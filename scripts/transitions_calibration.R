# ============================================================
# Adjust transition rates by PIF (age x sex x IMD), then plot
# calibrated (PIF-adjusted) vs non-calibrated (raw) rates.
#
# Runs sections 1-5 only:
#   1) read inputs        2) 2011->2021 mapping + GM universe
#   2b) LSOA21 -> IMD      3) mortality      4) incidence
#   5) apply PIFs by age x sex x IMD (pooled fallback)
# then ggplot rate vs rate_raw. (Depression / synth_hd / OLD-vs-
# NEW calibration / save steps removed.)
#
# 2026-08 FIX: (a) location_code is mixed LSOA11/LSOA21 vintage --
#   deaths are LSOA21, incidence is LSOA11 -- routing both through
#   the 2011->2021 lookup lost 66 GM areas from each (section 2a);
#   (b) mortality sex_age_group used "M_[40,45)" while the pifs CSV
#   uses "male_[40,45)", so no mortality row ever matched a PIF.
#
# 2026-05 FIX: section 4 (incidence) was joining on LAD22CD, but
# incidence location_code is reported at LSOA11 ("E01...").
# That made every LSOA21CD = NA after the 2021 match. Now joined
# on LSOA11CD via l11_21 with the same equal-split apportionment
# as mortality, and scoped to the GM universe for consistency.
# ============================================================

library(tidyverse)
library(here)
library(readr)
library(stringr)

# ----------------------------
# Paths + constants
# ----------------------------
PATHS <- list(
  pifs           = "manchester/health/processed/pifs_reference_norm052226.csv",
  lsoa_map       = here("manchester/health/original/ons/lsoa_2011_to_lsoa_2021.csv"),
  trans_raw      = "manchester/health/processed/health_transitions_manchester_raw.csv",
  zones          = here("manchester/synPop/sp_2021/zoneSystem.csv"),
  out_plots_base = "images/manchester"
)

GM_LADS <- c("Manchester","Salford","Bolton","Bury","Oldham",
             "Rochdale","Stockport","Tameside","Trafford","Wigan")

# ----------------------------
# Helpers
# ----------------------------
# [FIX] The curly apostrophe in "parkinson’s_disease" only matched if
# this file and the source CSV agreed on encoding. Normalise the
# apostrophe first, then match on the straight form, so either vintage
# of the transitions file collapses to "parkinson" (the label the pifs
# CSV uses).
clean_cause <- function(x) {
  x %>%
    str_replace_all("\u2019", "'") %>%
    str_replace_all("-", "_") %>%
    str_replace_all("parkinson's_disease", "parkinson") %>%
    str_replace_all("parkinsons_disease", "parkinson") %>%
    str_replace_all("head_and_neck_cancer", "head_neck_cancer")
}

sex_to_char <- function(x) {
  case_when(
    x %in% c(1, "1", "M", "Male", "male") ~ "male",
    x %in% c(2, "2", "F", "Female", "female") ~ "female",
    TRUE ~ as.character(x)
  )
}

sex_to_num <- function(x) {
  x2 <- sex_to_char(x)
  case_when(
    x2 == "male" ~ 1L,
    x2 == "female" ~ 2L,
    TRUE ~ NA_integer_
  )
}

# Top age band present in the pifs CSV. pif_calc.R builds labels with
# sprintf("[%d,%d)", ...), so its highest band is [95,100). Ages at or
# above 100 are capped into that band rather than falling into a
# [100,105) band that has no PIF.
PIF_MAX_AGE <- 99L

# [FIX] Two problems with the previous version:
#   (a) it used cut(), whose top interval becomes "[95,100]" (square
#       bracket) under include.lowest = TRUE, while pif_calc.R emits
#       "[95,100)". The keys therefore never matched at the top band,
#       which is what repeat_95_100() was working around.
#   (b) it pasted `sex` verbatim, so mortality (recoded to "M"/"F"
#       upstream) produced "M_[40,45)" while the pifs CSV carries
#       "male_[40,45)". EVERY mortality row missed both the stratified
#       and the pooled PIF and fell through to paf = 0.
# Both are fixed by normalising sex first and building the label with
# the same sprintf() form pif_calc.R uses.
add_age_groups <- function(df) {
  df %>%
    mutate(
      age = as.integer(age),
      sex = sex_to_char(sex),
      age_band_tmp  = pmin(age, PIF_MAX_AGE),
      age_group     = sprintf("[%d,%d)",
                              5L * (age_band_tmp %/% 5L),
                              5L * (age_band_tmp %/% 5L + 1L)),
      age_group     = if_else(is.na(age), NA_character_, age_group),
      sex_age_group = paste(sex, age_group, sep = "_")
    ) %>%
    select(-age_band_tmp)
}

safe_name <- function(x) str_replace_all(x, "[^A-Za-z0-9]+", "_")

# ============================================================
# 1) Read inputs ONCE
# ============================================================
# pifs CSV is produced by the run_pif_calc.R and MUST carry an
# imd_decile column (1..10 stratified rows + NA pooled rows) plus
# paf_combined_traditional.
# [FIX] repeat_95_100() removed. It copied the [90,95) PIF onto a
# "[95,100]" key to paper over the cut() label mismatch, which both
# (a) invented a PIF for 95+ when the CSV already carries a real
# [95,100) row, and (b) risked duplicating join keys. add_age_groups()
# now emits "[95,100)" directly and caps ages >= 100 into it.
pifs <- read_csv(PATHS$pifs, show_col_types = FALSE) %>%
  rename(cause = outcome) %>%
  mutate(cause = clean_cause(cause))

if (!"imd_decile" %in% names(pifs)) {
  stop("pifs CSV ('", PATHS$pifs, "') has no imd_decile column. ",
       "Regenerate it with the run_pif_calc.R (stratified by ",
       "sex_age_group x imd_decile, plus pooled imd_decile = NA ",
       "rows), then rerun.")
}
stopifnot("paf_combined_traditional" %in% names(pifs))

# [ADDED] A duplicated (sex_age_group, cause, imd_decile) key would
# silently multiply rows at the join in section 5 and inflate the
# transitions table.
if (anyDuplicated(pifs[, c("sex_age_group", "cause", "imd_decile")])) {
  stop("pifs CSV has duplicate sex_age_group x cause x imd_decile keys; ",
       "the PIF join would multiply transition rows.")
}

lsoa_11to21 <- read_csv(PATHS$lsoa_map, show_col_types = FALSE) %>%
  select(LSOA11CD, LSOA21CD, LAD22CD, LSOA21NM, LAD22NM)

transition_raw <- read_csv(PATHS$trans_raw, show_col_types = FALSE) %>%
  mutate(cause = clean_cause(cause)) %>%
  add_age_groups()

# ============================================================
# 2) Build 2011 -> 2021 equal-split mapping + GM 
# ============================================================
l11_21 <- lsoa_11to21 %>%
  transmute(
    LSOA11CD = as.character(LSOA11CD),
    LSOA21CD = as.character(LSOA21CD),
    LAD22CD  = as.character(LAD22CD),
    LAD22NM  = as.character(LAD22NM)
  ) %>%
  group_by(LSOA21CD) %>%
  mutate(w = 1 / n()) %>%
  ungroup()

lsoa21_universe <- l11_21 %>%
  distinct(LSOA21CD, LAD22CD, LAD22NM) %>%
  filter(LAD22NM %in% GM_LADS)

# An LSOA21 must appear exactly once here, or crossing() below would
# emit duplicate rows for it.
stopifnot(!anyDuplicated(lsoa21_universe$LSOA21CD))

# ============================================================
# 2a) [FIX] location_code vintage is SPLIT BY MEASURE
#
# In health_data_Manchester.Rmd (chunk combine-mortality-diseases):
#   allcause  -> location_code = lsoa21cd   (LSOA21, from
#                manchester_lsoa21_mxrates_calibrated.csv)
#   diseases  -> location_code = lsoa_code  (LSOA11, from
#                england_lsoa_deaths / manchester_diseases_lsoa.RDS)
#
# So deaths are ALREADY LSOA21 and need no 2011->2021 mapping at all,
# while incidence is LSOA11 and does. The two only looked alike
# because an LSOA that did not change in 2021 keeps an identical
# code string; the 66 GM areas that DID change are the ones that
# expose the difference.
#
# The old code sent both measures through inner_join(l11_21), which:
#   - for deaths, dropped the 66 LSOA21-coded areas (absent from
#     LSOA11CD), after which crossing() regenerated them and
#     coalesce(rate_raw, 0) filled every cell with zero;
#   - for incidence, dropped them outright with no grid to refill
#     them, losing 66 x 101 x 33 = 219,978 rows of real data.
#
# Neither showed up in a row count or n_distinct(lsoa21cd).
#
# NOTE: do NOT "fix" this by adding identity rows to l11_21. The 66
# areas legitimately appear under their LSOA21 code in deaths AND
# under their LSOA11 parent codes in incidence; a single shared
# lookup cannot serve both without either double counting or
# discarding one measure.
# ============================================================
deaths_codes    <- unique(as.character(
  transition_raw$location_code[transition_raw$measure == "deaths"]))
incidence_codes <- unique(as.character(
  transition_raw$location_code[transition_raw$measure == "incidence"]))

# Deaths must be LSOA21 already.
d_bad <- setdiff(deaths_codes, l11_21$LSOA21CD)
if (length(d_bad) > 0) {
  stop(length(d_bad), " mortality location_code(s) are not valid LSOA21 ",
       "codes: ", paste(head(d_bad, 5), collapse = ", "),
       "\nMortality is expected at LSOA21 (see the Rmd: location_code = ",
       "lsoa21cd). If this fires, that upstream convention changed.")
}

# Incidence must be LSOA11 and fully mappable.
i_bad <- setdiff(incidence_codes, l11_21$LSOA11CD)
if (length(i_bad) > 0) {
  warning(length(i_bad), " incidence location_code(s) are not LSOA11 codes ",
          "and will be dropped: ", paste(head(i_bad, 10), collapse = ", "),
          if (length(i_bad) > 10) " ..." else "")
}

message("location_code vintage: deaths = ", length(deaths_codes),
        " LSOA21 codes; incidence = ", length(incidence_codes),
        " LSOA11 codes.")

# ============================================================
# 2b) LSOA21 -> IMD decile lookup
#     Modal imd10 per LSOA21 (an LSOA21 can span OAs with
#     slightly different imd10). Forced to integer so it matches
#     the pifs CSV's imd_decile (1..10; 1 = most deprived).
# ============================================================
zones_raw <- read_csv(PATHS$zones, show_col_types = FALSE)

imd_lookup <- zones_raw %>%
  dplyr::filter(!is.na(lsoa21cd), !is.na(imd10)) %>%
  dplyr::group_by(lsoa21cd, imd10) %>%
  dplyr::summarise(w = sum(population, na.rm = TRUE), .groups = "drop_last") %>%
  dplyr::slice_max(order_by = w, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    lsoa21cd   = as.character(lsoa21cd),
    imd_decile = as.integer(imd10)
  )

# In the current zone file every LSOA21 is unanimous (imd10 is assigned
# at LSOA level and broadcast to its OAs), so the modal collapse above
# is lossless. These guards make that an error rather than a silent
# row-multiplying join if a future zone file ever differs.
stopifnot(!anyDuplicated(imd_lookup$lsoa21cd))
stopifnot(all(imd_lookup$imd_decile %in% 1:10))

# ============================================================
# 3) Mortality: deaths (2011 LSOA) -> 2021 LSOA grid
# ============================================================
# [FIX] sex was recoded to "M"/"F" here, which then flowed into
# add_age_groups() and produced sex_age_group = "M_[40,45)" -- a key
# that matches nothing in the pifs CSV ("male_[40,45)"). Every
# mortality row therefore fell through to paf = 0 and rate == rate_raw.
# Normalise to the same "male"/"female" vocabulary the pifs use.
mortality_11 <- transition_raw %>%
  filter(measure == "deaths") %>%
  mutate(
    sex           = sex_to_char(sex),
    location_code = as.character(location_code),
    rate_raw      = as.numeric(rate)
  ) %>%
  select(age, sex, cause, rate_raw, location_code)

stopifnot(all(mortality_11$sex %in% c("male", "female")))

# [FIX] Mortality is ALREADY at LSOA21 (see section 2a), so it needs no
# 2011->2021 apportionment -- location_code IS the LSOA21 code. Sending
# it through l11_21 dropped the 66 areas whose codes changed in 2021.
# Attach LAD via the LSOA21 side of the lookup instead.
mortality_21_agg <- mortality_11 %>%
  rename(LSOA21CD = location_code) %>%
  inner_join(lsoa21_universe, by = "LSOA21CD") %>%   # GM only, adds LAD22CD/NM
  group_by(LSOA21CD, LAD22CD, LAD22NM, sex, age, cause) %>%
  summarise(rate_raw = sum(rate_raw, na.rm = TRUE), .groups = "drop")

sex_levels   <- sort(unique(mortality_11$sex))
age_levels   <- sort(unique(mortality_11$age))
cause_levels <- sort(unique(mortality_11$cause))

# [ADDED] The coalesce() below turns any join miss into a plausible
# looking zero, so a whole LSOA can go missing without changing the row
# count. Check coverage BEFORE the zero-fill, where it is still visible.
missing21 <- setdiff(lsoa21_universe$LSOA21CD, mortality_21_agg$LSOA21CD)
if (length(missing21) > 0) {
  stop(length(missing21), " of ", nrow(lsoa21_universe),
       " GM LSOA21s have no mortality data and would be zero-filled ",
       "for every cause: ", paste(head(missing21, 5), collapse = ", "),
       if (length(missing21) > 5) " ..." else "",
       "\nMortality location_code should already be LSOA21; see the ",
       "section 2a guards.")
}

mortality <- tidyr::crossing(
  LSOA21CD = lsoa21_universe$LSOA21CD,
  sex      = sex_levels,
  age      = age_levels,
  cause    = cause_levels
) %>%
  left_join(lsoa21_universe, by = "LSOA21CD") %>%
  left_join(mortality_21_agg, by = c("LSOA21CD","LAD22CD","LAD22NM","sex","age","cause")) %>%
  # Remaining NAs are genuinely absent age/sex/cause cells, not lost
  # LSOAs -- the guard above rules that out.
  mutate(rate_raw = coalesce(rate_raw, 0)) %>%
  transmute(
    lsoa21cd = LSOA21CD,
    LAD22NM,
    sex, age, cause,
    rate_raw
  ) %>%
  add_age_groups()

# ============================================================
# 4) Incidence: 2011 LSOA -> 2021 LSOA grid
#     FIX: incidence is reported at LSOA11 (location_code is
#     "E01..."), NOT at LAD. The old join on LAD22CD matched
#     nothing and left every LSOA21CD = NA after the 2021 match.
#     Join on LSOA11CD via l11_21 instead, and use the same
#     equal-split apportionment as mortality -- rate is a RATE,
#     so it is averaged across the LSOA11s that compose an
#     LSOA21 (w = 1/n() by LSOA21), never summed.
#     The semi_join scopes incidence to the GM universe so it
#     matches mortality and the GM-only IMD lookup downstream;
#     drop that line if you want national incidence.
# ============================================================
incidence <- transition_raw %>%
  filter(measure == "incidence") %>%
  mutate(
    sex           = sex_to_char(sex),
    location_code = as.character(location_code),
    rate_raw      = as.numeric(rate)
  ) %>%
  inner_join(l11_21, by = c("location_code" = "LSOA11CD")) %>%
  semi_join(lsoa21_universe, by = "LSOA21CD") %>%   # GM only (drop for national)
  mutate(rate_w = rate_raw * w) %>%
  group_by(LSOA21CD, LAD22NM, sex, age, cause) %>%
  summarise(rate_raw = sum(rate_w, na.rm = TRUE), .groups = "drop") %>%
  rename(lsoa21cd = LSOA21CD) %>%
  add_age_groups()

# Quick guard: incidence should not be empty and should have no
# NA LSOA21 keys after the fix.
if (nrow(incidence) == 0L) {
  stop("incidence is empty after the LSOA11 join. The incidence ",
       "location_code values are not present in lsoa_11to21$LSOA11CD ",
       "(likely an LSOA11 vintage / GBD location id mismatch). ",
       "Inspect: setdiff(unique(transition_raw$location_code[",
       "transition_raw$measure=='incidence']), l11_21$LSOA11CD).")
}
stopifnot(sum(is.na(incidence$lsoa21cd)) == 0)
stopifnot(all(incidence$sex %in% c("male", "female")))

# [ADDED] Incidence is NOT expanded onto a complete grid the way
# mortality is, so a shortfall here is silent data loss rather than
# zero-fill. The 66 areas whose codes changed in 2021 are reachable
# only through their LSOA11 parents, which is why incidence must use
# l11_21 unchanged.
inc_missing <- setdiff(lsoa21_universe$LSOA21CD, incidence$lsoa21cd)
if (length(inc_missing) > 0) {
  stop(length(inc_missing), " of ", nrow(lsoa21_universe),
       " GM LSOA21s have no incidence rows at all: ",
       paste(head(inc_missing, 5), collapse = ", "),
       if (length(inc_missing) > 5) " ..." else "",
       "\nEach missing LSOA21 costs 101 ages x 33 sex-cause combos.")
}

# ============================================================
# 5) Apply PIFs => NEW adjusted transition_data
#    [CHANGED] PIF applied by age x sex x IMD with pooled fallback
# ============================================================
options(scipen = 999)

# Split the pifs table once:
#   pifs_imd  = IMD-stratified rows (imd_decile 1..10)
#   pifs_pool = pooled rows (imd_decile NA), fallback wherever a
#               sex_age_group x imd x cause cell is sparse/absent.
pifs_imd <- pifs %>%
  filter(!is.na(imd_decile)) %>%
  mutate(imd_decile = suppressWarnings(as.integer(imd_decile))) %>%
  select(sex_age_group, cause, imd_decile,
         paf_strat = paf_combined_traditional)

pifs_pool <- pifs %>%
  filter(is.na(imd_decile)) %>%
  select(sex_age_group, cause,
         paf_pool = paf_combined_traditional)

# Robust fallback: if the CSV carries no pooled (imd_decile NA)
# rows, rebuild them EXACTLY from the stratified rows. Combined
# PAF = 1 - total_pop / sum_rr_individual, so pooling across
# deciles = sum both components, then take the ratio.
if (nrow(pifs_pool) == 0) {
  message("No pooled (imd_decile NA) rows in pifs CSV; ",
          "reconstructing pooled fallback from stratified components.")
  stopifnot(all(c("total_pop","sum_rr_individual") %in% names(pifs)))
  pifs_pool <- pifs %>%
    filter(!is.na(imd_decile)) %>%
    group_by(sex_age_group, cause) %>%
    summarise(total_pop         = sum(total_pop,         na.rm = TRUE),
              sum_rr_individual = sum(sum_rr_individual, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(paf_pool = ifelse(sum_rr_individual > 0,
                             1 - total_pop / sum_rr_individual, 0)) %>%
    select(sex_age_group, cause, paf_pool)
}

# Guard the silent-failure case: all PIFs NA/~0 -> rate == rate_raw.
pif_vals <- c(pifs_imd$paf_strat, pifs_pool$paf_pool)
if (all(is.na(pif_vals)) ||
    isTRUE(max(abs(pif_vals), na.rm = TRUE) < 1e-12)) {
  stop("All PIFs are NA or zero: pifs CSV has no exposure effect, ",
       "so rate would equal rate_raw. Check pif_calc.R and ",
       "regenerate the CSV.")
}

transition_data <- bind_rows(
  incidence %>% select(lsoa21cd, LAD22NM, sex, age, cause, rate_raw, sex_age_group),
  mortality %>% select(lsoa21cd, LAD22NM, sex, age, cause, rate_raw, sex_age_group)
) %>%
  filter(cause != "myeloma") %>%
  # attach IMD decile per LSOA21 so the PIF applies by age x sex x IMD
  left_join(imd_lookup, by = "lsoa21cd") %>%
  mutate(imd_decile = suppressWarnings(as.integer(imd_decile))) %>%
  # IMD-stratified PIF (imd_decile 1..10)
  left_join(pifs_imd, by = c("cause", "sex_age_group", "imd_decile")) %>%
  # pooled (age x sex) PIF as fallback
  left_join(pifs_pool, by = c("cause", "sex_age_group")) %>%
  mutate(
    # stratified where available, else pooled, else 0
    paf_combined_traditional = coalesce(paf_strat, paf_pool, 0),
    paf_combined_traditional = if_else(age < 20, 0,
                                       paf_combined_traditional),
    pif_source = dplyr::case_when(
      age < 20          ~ "none_age<20",
      !is.na(paf_strat) ~ "imd_stratified",
      !is.na(paf_pool)  ~ "pooled_fallback",
      TRUE              ~ "missing_zeroed"
    ),
    rate = rate_raw * (1 - paf_combined_traditional)
  ) %>%
  mutate(
    # IMPORTANT: keep this numeric and consistent (fixes earlier error)
    sex = sex_to_num(sex)
  ) %>%
  select(age, sex, lsoa21cd, LAD22NM, cause, imd_decile,
         rate, rate_raw, pif_source)

# Provenance + did the PIF actually move the rates?
message("PIF source breakdown:")
print(transition_data %>% dplyr::count(pif_source, sort = TRUE))

# [ADDED] "missing_zeroed" means a row matched NEITHER the stratified
# NOR the pooled PIF, which can only be a key mismatch (cause,
# sex_age_group or imd_decile), not sparse data -- the pooled table
# covers every sex_age_group x cause. Before the sex fix this was ~100%
# of mortality rows, because sex_age_group read "M_[40,45)".
zeroed <- transition_data %>%
  filter(pif_source == "missing_zeroed")

if (nrow(zeroed) > 0) {
  warning(nrow(zeroed), " row(s) (",
          round(100 * nrow(zeroed) / nrow(transition_data), 2),
          "%) matched no PIF and were zeroed. Unmatched keys, e.g.:\n",
          paste(utils::capture.output(
            print(zeroed %>%
                    dplyr::distinct(cause, sex, imd_decile) %>%
                    head(10))),
            collapse = "\n"))
}

# Any LSOA that failed the IMD join falls back to pooled PIFs, which is
# survivable but should not be silent.
n_no_imd <- sum(is.na(transition_data$imd_decile))
if (n_no_imd > 0) {
  warning(n_no_imd, " row(s) have no IMD decile and used the pooled PIF. ",
          "Check that lsoa21cd matches between the zone file and the ",
          "transitions data.")
}

pif_effect <- transition_data %>%
  summarise(
    n_total     = dplyr::n(),
    n_changed   = sum(abs(rate - rate_raw) > 1e-12, na.rm = TRUE),
    pct_changed = round(100 * n_changed / dplyr::n(), 2)
  )
message("PIF effect on transition rates:")
print(pif_effect)
if (pif_effect$n_changed == 0L) {
  warning("PIF changed 0 rows: rate == rate_raw everywhere. Check ",
          "that cause / sex_age_group / imd_decile keys match ",
          "between pifs and transition_data.")
}

transition_data <- transition_data %>% select(-pif_source)

stopifnot(sum(is.na(transition_data$rate)) == 0)

# ============================================================
# 6) Post-processing: fix artefactual rate behaviour
#
# Three mutually exclusive treatments, applied per
# (lsoa21cd, sex, cause) group sorted by age:
#
# TREATMENT A - WITHDRAWN 2026-08. No cause now takes this treatment.
#   It applied an NDA-calibrated linear ramp to diabetes below age 40,
#   to remove a spike at ~15-25 with a trough at ~30-40. That shape is
#   NOT in GBD: the published band rates rise monotonically from 15-19
#   (0.00122 male) to 60-64 (0.00632). The spike was created by the
#   interpolating spline overshooting ages 5-24 by 1.5-2.1x, and was
#   removed at source by the mean-preserving band rescale in
#   health_data_Manchester.Rmd. See the note above fix_diabetes_nda().
#
# TREATMENT B - PEAK-FLATTEN  (all diseases in PEAK_FLATTEN_CAUSES)
#   Fix: hold the rate at its peak value for all ages beyond the peak.
#
#   REVISED 2026-08. The previous list was assembled from the expectation
#   that these diseases "are monotone increasing with age". Checking that
#   against the GBD band rates in gbdp.csv showed it was wrong in both
#   directions, so membership is now derived from the data.
#
#   Rule: flatten a cause only if BOTH
#     (i)  GBD declines by more than 20% from its peak band to 95+, and
#     (ii) the peak band starts at age 70 or later.
#
#   (ii) is the important half. A decline from a peak below 70 is the real
#   age gradient turning over, not a sparse-count wobble at the top of the
#   distribution, and flattening it would overwrite decades of genuine
#   curve. (i) alone would catch endometrial cancer and depression, both
#   of which decline for substantive reasons.
#
#   Diagnostic that produced the list (re-run it if gbdp.csv changes):
#
#     gbdp %>% filter(measure == "Incidence") %>%
#       group_by(cause, sex) %>% arrange(from_age, .by_group = TRUE) %>%
#       summarise(peak_from   = from_age[which.max(val)],
#                 pct_decline = 100 * (1 - dplyr::last(val) / max(val)),
#                 .groups = "drop")
#
#   NOTE ON CAUSE LABELS: gbdp.csv carries the raw GBD labels ("COPD",
#   "parkinson's_disease"); the transitions data has been through tolower()
#   and clean_cause(), so the equivalents here are "copd" and "parkinson".
#
#   NOTE ON GBD FIDELITY: flattening deliberately departs from the GBD band
#   means that the mean-preserving rescale in health_data_Manchester.Rmd
#   enforces to 1e-6. The two are in tension by design. Quantify the size of
#   the departure at 85+ and report it; do not present the rescale assertion
#   as though it survived this step.
#
# TREATMENT C - NO FIX  (diseases in NO_FIX_CAUSES)
#   Post-peak decline is REAL, or there is no decline to fix:
#     all_cause_mortality : exponential rise at old age, no artefact.
#     depression          : peaks at 15-19 and falls 60-65% thereafter.
#                           Flattening would hold the adolescent rate
#                           across the whole adult range.
#     endometrial_cancer  : -54% from a 75-79 peak. Oestrogen-driven;
#                           post-menopausal incidence genuinely declines.
#     liver_cancer        : -49% (male) from an 80-84 peak, plausibly real
#                           via competing mortality.
#     the remainder       : peak at 90-94 or 95+ with a 0-4% decline, so
#                           flattening was a no-op. Listed here to make the
#                           "nothing to fix" finding explicit rather than
#                           leaving it implied by absence.
#
# Applied to BOTH rate (calibrated) and rate_raw independently.
# ============================================================

# Diseases receiving the peak-flatten fix (Treatment B).
# Derived from the gbdp.csv diagnostic above, not from expectation.
# Figures in comments are pct decline from peak band to 95+, by sex.
PEAK_FLATTEN_CAUSES <- c(
  "myeloid_leukemia",   # -53 M / -48 F, peak 85-89
  "myeloma",            # -46 M / -36 F, peak 85-89
  "lung_cancer",        # -26 M / -16 F, peak 90-94 M
  "colon_cancer",       # -25 M /   0 F, peak 85-89 M
  "copd",               # -14 M /  -8 F, peak 85-89
  "breast_cancer",      # -38 M / -14 F, peak 85-89
  "bladder_cancer",     # -12 M /  -1 F, peak 90-94 M
  "head_neck_cancer"    #  -7 M /  -2 F, peak 90-94 M (borderline)
)

# Diabetes: deliberate override of rule (ii).
#
# Its peak band is 60-64, below the age-70 threshold, so the rule would
# exclude it. But the decline is -98% (632 -> 10.9 per 100k), which is
# categorically larger than anything else in the file and is not a credible
# epidemiological gradient -- incidence does not fall to a sixtieth of its
# peak. Flattening from the peak would overwrite 36 years of curve, so it is
# flattened from age 85 only: the real 65-85 decline is preserved and just
# the sparse-count tail is held flat.
#
# Set to NA to disable and leave diabetes wholly unmodified.
DIABETES_FLATTEN_FROM <- 85L

# Diseases with no fix (Treatment C).
# Grouped by reason; see the block comment above for the evidence.
NO_FIX_CAUSES <- c(
  "all_cause_mortality",
  # decline is real
  "depression",             # -65 F / -60 M from a 15-19 peak
  "endometrial_cancer",     # -54 F from a 75-79 peak
  "liver_cancer",           # -49 M from an 80-84 peak
  # nothing to fix: peak at 90-94 or 95+, decline 0-4%
  "all_cause_dementia",
  "stroke",
  "parkinson",
  "coronary_heart_disease",
  "esophageal_cancer",
  "gastric_cardia_cancer"
)

# The lists must not overlap, and diabetes must sit in exactly one regime.
stopifnot(length(intersect(PEAK_FLATTEN_CAUSES, NO_FIX_CAUSES)) == 0L)
stopifnot(!("diabetes" %in% c(PEAK_FLATTEN_CAUSES, NO_FIX_CAUSES)))

# ---- NDA calibration constants ----------------------------------------
# Source: NHS England National Diabetes Audit 2025-26 (April-December 2025)
# Sheet: "Type 2 and other registrations", England row.
# Age distribution of registered Type 2 / other diabetes patients:
#   Aged < 40 :  4.4 %  (band width 40 yrs => 0.110 % per yr)
#   Aged 40-64: 43.0 %  (band width 25 yrs => 1.720 % per yr)
#   Aged 65-79: 37.2 %  (band width 15 yrs => 2.480 % per yr)  <- peak band
#   Aged 80+  : 15.3 %  (band width 20 yrs => 0.765 % per yr)
#
# NDA_RAMP_RATIO: the rate just before age 40 (age 39) expressed as a
# fraction of the GBD rate at age 40.  Derived as:
#   2 * (pct_per_yr_under40 / pct_per_yr_40_64)
#   = 2 * (0.110 / 1.720)  = 0.128
# (Factor of 2 because a linear ramp has mean = endpoint/2, and the NDA
# pct/yr ratio gives the mean for the band, not the endpoint.)
NDA_RAMP_RATIO <- 2 * (4.4 / 40) / (43.0 / 25)   # ≈ 0.128

# ============================================================
# WITHDRAWN 2026-08 -- fix_diabetes_nda() IS NO LONGER CALLED.
#
# Retained for reference and reproducibility of earlier outputs. The
# artefact it targets was removed at source; calling it now would cut the
# rate at age 38 by ~84% and impose a discontinuous 8-fold jump at age 40.
#
# Evidence for withdrawal:
#  1. gbdp.csv band rates for diabetes incidence rise monotonically:
#     male 15-19 0.00122 -> 20-24 0.00206 -> ... -> 60-64 0.00632.
#     There is no spike at 15-25 and no trough at 30-40.
#  2. The 2026-05-15 output (pre-spline-fix) DOES have a local maximum at
#     age 21 and a trough at age 34 -- the shape described below. So the
#     hump came from the interpolating spline, not from GBD.
#  3. health_data_Manchester.Rmd documents that spline inflating ages 5-24
#     by 1.5-2.1x, and its mean-preserving band rescale fixed it.
#
# The NDA anchor was also weak on its own terms: it derives an INCIDENCE
# ramp from the age distribution of REGISTERED (prevalent) patients, which
# overstates young-adult incidence, and the register covers diagnosed cases
# while GBD models total burden including undiagnosed.
# ============================================================
# Original comment follows.
#
# Helper: fix the diabetes artefactual early hump.
#
# Problem: GBD data has two humps — a spurious spike at ~age 15-25
# (juvenile / Type-1 artefact) and the real Type-2 peak at ~age 50-65,
# with the rate dropping to near zero in between (~age 30-40).  The
# previous trough-finding ramp anchored at that near-zero trough, which
# produced a flat line from age 0 to ~40.
#
# NDA-informed strategy:
#   1. Identify the GBD rate at age 40 (val_at_40): the first point
#      where the real Type-2 signal begins.
#   2. Replace ALL values for ages < 40 with a LINEAR RAMP from
#      (age_min, 0) to (age 39, NDA_RAMP_RATIO * val_at_40).
#      This is calibrated to the NDA: rates under 40 average only
#      ~6.4 % of the 40-64 rate, producing a genuinely modest but
#      monotonically increasing curve before 40.
#   3. Leave ages >= 40 exactly as-is (GBD values).
#
# `vals` and `ages` must be parallel vectors, already sorted by age.
fix_diabetes_nda <- function(vals, ages) {
  if (all(is.na(vals)) || length(vals) < 2) return(vals)
  
  # Anchor: GBD rate at the first age >= 40
  idx_40 <- which(ages >= 40)
  if (length(idx_40) == 0) return(vals)          # no data >= 40, leave unchanged
  val_at_40 <- vals[idx_40[1]]
  
  # NDA-calibrated target value at age 39 (just before the boundary)
  target_at_39 <- NDA_RAMP_RATIO * val_at_40
  
  # Linear ramp for ages < 40
  pre_idx  <- which(ages < 40)
  if (length(pre_idx) == 0) return(vals)
  
  age_min  <- ages[1]
  age_span <- 39 - age_min                       # ramp spans age_min → 39
  
  if (age_span > 0) {
    vals[pre_idx] <- target_at_39 *
      (ages[pre_idx] - age_min) / age_span
  } else {
    vals[pre_idx] <- target_at_39
  }
  
  vals
}

# Helper: flatten rate at the peak value for all ages after the peak.
#
# `ages` must be supplied and sorted ascending alongside `vals`.
#
# min_peak_age: refuse to flatten when the peak sits below this age. A peak
#   that early means the decline is the real age gradient, not a sparse-count
#   artefact, and flattening would overwrite most of the curve. This is a
#   backstop independent of PEAK_FLATTEN_CAUSES: it would have caught
#   depression (peak 15-19) even if the list were wrong.
#
# from_age: when supplied, search for the peak only at ages >= from_age and
#   leave everything below untouched. Used for diabetes, where the tail is
#   artefactual but the mid-life decline is real. Bypasses min_peak_age,
#   since the caller has asserted where the artefact starts.
fix_peak_flatten <- function(vals, ages, min_peak_age = 70L, from_age = NA_integer_) {
  stopifnot(length(vals) == length(ages), !is.unsorted(ages))
  if (all(is.na(vals))) return(vals)
  
  if (!is.na(from_age)) {
    win <- which(ages >= from_age)
    if (length(win) < 2L) return(vals)
    if (all(is.na(vals[win]))) return(vals)
    peak_idx <- win[which.max(vals[win])]
  } else {
    peak_idx <- which.max(vals)
    if (ages[peak_idx] < min_peak_age) return(vals)   # refuse: peak too young
  }
  
  if (peak_idx < length(vals))
    vals[(peak_idx + 1L):length(vals)] <- vals[peak_idx]
  vals
}

message("Applying post-processing rate fixes ...")
message("  Treatment A (NDA ramp):    WITHDRAWN -- no causes")
message("  Treatment B (peak-flatten): ", paste(PEAK_FLATTEN_CAUSES, collapse = ", "))
if (!is.na(DIABETES_FLATTEN_FROM)) {
  message("  Treatment B (diabetes):     flattened from age ", DIABETES_FLATTEN_FROM)
} else {
  message("  Treatment B (diabetes):     disabled")
}
message("  Treatment C (no fix):      ", paste(NO_FIX_CAUSES, collapse = ", "))

# Sanity check: every cause in the data is accounted for
all_causes <- unique(transition_data$cause)
unaccounted <- setdiff(all_causes,
                       c(PEAK_FLATTEN_CAUSES, NO_FIX_CAUSES, "diabetes"))
if (length(unaccounted) > 0) {
  warning("The following causes are not assigned to any treatment in section 6 ",
          "and will receive NO fix by default. Add them to PEAK_FLATTEN_CAUSES ",
          "or NO_FIX_CAUSES as appropriate: ",
          paste(unaccounted, collapse = ", "))
}

# Keep the pre-fix series so the effect of section 6 can be measured. See the
# audit block below; this is what lets the departure from the GBD band means
# be reported rather than assumed small.
transition_data_prefix <- transition_data %>%
  select(lsoa21cd, sex, cause, age, rate_prefix = rate, rate_raw_prefix = rate_raw)

transition_data <- transition_data %>%
  group_by(lsoa21cd, sex, cause) %>%
  group_modify(~ {
    df <- .x %>% arrange(age)
    cz <- .y$cause
    
    # Treatment A withdrawn 2026-08.
    if (cz %in% PEAK_FLATTEN_CAUSES) {
      # Treatment B: hold at peak value after the peak age, provided the
      # peak is at or above min_peak_age.
      df$rate     <- fix_peak_flatten(df$rate,     df$age)
      df$rate_raw <- fix_peak_flatten(df$rate_raw, df$age)
      
    } else if (cz == "diabetes" && !is.na(DIABETES_FLATTEN_FROM)) {
      # Treatment B, restricted window: flatten only the sparse-count tail
      # and preserve the real mid-life decline below DIABETES_FLATTEN_FROM.
      df$rate     <- fix_peak_flatten(df$rate,     df$age,
                                      from_age = DIABETES_FLATTEN_FROM)
      df$rate_raw <- fix_peak_flatten(df$rate_raw, df$age,
                                      from_age = DIABETES_FLATTEN_FROM)
    }
    # Treatment C: no modification
    df
  }) %>%
  ungroup()

message("Post-processing complete.")

# ---- Audit: what did section 6 actually change? -----------------------
# Reports the mean shift in rate_raw at 85+ against the pre-fix series, i.e.
# how far the flattened old-age rates sit above the GBD band means that the
# mean-preserving rescale in health_data_Manchester.Rmd enforces. Report
# these numbers in the methods; they are the cost of Treatment B.
peak_flatten_audit <- transition_data %>%
  select(lsoa21cd, sex, cause, age, rate_raw) %>%
  inner_join(transition_data_prefix, by = c("lsoa21cd", "sex", "cause", "age")) %>%
  filter(age >= 85, rate_raw_prefix > 0) %>%
  group_by(cause, sex) %>%
  summarise(
    mean_pct_shift = round(100 * mean(rate_raw / rate_raw_prefix - 1), 1),
    max_pct_shift  = round(100 * max(rate_raw / rate_raw_prefix - 1), 1),
    .groups = "drop"
  ) %>%
  filter(mean_pct_shift != 0) %>%
  arrange(desc(mean_pct_shift))

message("Section 6 effect on rate_raw at ages 85+:")
print(peak_flatten_audit, n = Inf)

rm(transition_data_prefix)

# ============================================================
# Plot: calibrated (PIF-adjusted) vs non-calibrated (raw)
#   rate     = rate_raw * (1 - PIF)   -> "Rate (calibrated)"
#   rate_raw = original               -> "Rate (raw)"
#   x = age, two lines, facet = sex, one PNG per cause.
# ============================================================
plot_df <- transition_data %>%
  group_by(cause, sex, age) %>%
  summarise(
    `Rate (calibrated)` = mean(rate,     na.rm = TRUE),
    `Rate (raw)`        = mean(rate_raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(`Rate (calibrated)`, `Rate (raw)`),
               names_to = "measure", values_to = "value") %>%
  mutate(
    sex     = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    measure = factor(measure, levels = c("Rate (raw)", "Rate (calibrated)"))
  )

out_dir <- file.path(PATHS$out_plots_base, "pif_calibration_check")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

plot_cause <- function(cz) {
  d <- plot_df %>% filter(cause == cz)
  if (nrow(d) == 0) return(invisible(NULL))
  
  p <- ggplot(d, aes(age, value,
                     colour = measure, linetype = measure)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    facet_wrap(~ sex, ncol = 2, scales = "free_y") +
    scale_colour_manual(
      values = c("Rate (raw)" = "grey55",
                 "Rate (calibrated)" = "#1f6feb"), name = NULL) +
    scale_linetype_manual(
      values = c("Rate (raw)" = "solid",
                 "Rate (calibrated)" = "longdash"), name = NULL) +
    labs(
      title    = paste0("PIF calibration \u2014 ", cz),
      subtitle = "Raw vs PIF-calibrated (age x sex x IMD) transition rate",
      x = "Age (years)", y = "Rate"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold"),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
  
  ggsave(file.path(out_dir, paste0("pifcal_", safe_name(cz), ".png")),
         p, width = 11, height = 5, dpi = 200, bg = "white")
  p
}

# Save one plot per cause; show a few key ones in the plots pane.
purrr::walk(sort(unique(plot_df$cause)), \(cz) {
  message("Calibration plot: ", cz)
  plot_cause(cz)
})

plot_cause("all_cause_mortality")
plot_cause("all_cause_dementia")
plot_cause("lung_cancer")
plot_cause("breast_cancer")
plot_cause("stroke")

# ============================================================
# Plot BY IMD: same raw vs calibrated comparison, but split by
# deprivation. imd_decile -> imd_quintile (1 = most deprived,
# 5 = least deprived; ONS convention, decile 1 = most deprived).
# One PNG per cause, facet grid = sex (rows) x IMD quintile
# (cols), so the PIF effect can be read off within each
# deprivation band.
# ============================================================
plot_df_imd <- transition_data %>%
  filter(!is.na(imd_decile)) %>%
  mutate(imd_quintile = ceiling(imd_decile / 2)) %>%
  group_by(cause, sex, imd_quintile, age) %>%
  summarise(
    `Rate (calibrated)` = mean(rate,     na.rm = TRUE),
    `Rate (raw)`        = mean(rate_raw, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(`Rate (calibrated)`, `Rate (raw)`),
               names_to = "measure", values_to = "value") %>%
  mutate(
    sex     = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    measure = factor(measure, levels = c("Rate (raw)", "Rate (calibrated)")),
    imd_quintile = factor(
      imd_quintile, levels = 1:5,
      labels = c("Q1 (most deprived)", "Q2", "Q3",
                 "Q4", "Q5 (least deprived)"))
  )

out_dir_imd <- file.path(PATHS$out_plots_base, "pif_calibration_check_by_imd")
dir.create(out_dir_imd, recursive = TRUE, showWarnings = FALSE)

plot_cause_imd <- function(cz) {
  d <- plot_df_imd %>% filter(cause == cz)
  if (nrow(d) == 0) return(invisible(NULL))
  
  p <- ggplot(d, aes(age, value,
                     colour = measure, linetype = measure)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    facet_grid(sex ~ imd_quintile, scales = "free_y") +
    scale_colour_manual(
      values = c("Rate (raw)" = "grey55",
                 "Rate (calibrated)" = "#1f6feb"), name = NULL) +
    scale_linetype_manual(
      values = c("Rate (raw)" = "solid",
                 "Rate (calibrated)" = "longdash"), name = NULL) +
    labs(
      title    = paste0("PIF calibration by deprivation \u2014 ", cz),
      subtitle = "Raw vs PIF-calibrated transition rate, by IMD quintile",
      x = "Age (years)", y = "Rate"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold"),
      plot.title       = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
  
  ggsave(file.path(out_dir_imd, paste0("pifcal_imd_", safe_name(cz), ".png")),
         p, width = 15, height = 6, dpi = 200, bg = "white")
  p
}

# Save one IMD-split plot per cause; show a few key ones.
purrr::walk(sort(unique(plot_df_imd$cause)), \(cz) {
  message("Calibration-by-IMD plot: ", cz)
  plot_cause_imd(cz)
})

for (cz in c(
  "all_cause_mortality",
  "all_cause_dementia",
  "bladder_cancer",
  "breast_cancer",
  "colon_cancer",
  "copd",
  "coronary_heart_disease",
  "depression",
  "diabetes",
  "endometrial_cancer",
  "esophageal_cancer",
  "gastric_cardia_cancer",
  "head_neck_cancer",
  "liver_cancer",
  "lung_cancer",
  "myeloid_leukemia",
  "parkinson",
  "stroke"
)) {
  print(plot_cause_imd(cz))
}

# Numeric companion: median % change the PIF introduces, by
# cause x sex x IMD quintile (negative = PIF lowers the rate).
# Lets you see whether the PIF distorts the deprivation gradient.
pif_by_imd_summary <- plot_df_imd %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  filter(!is.na(`Rate (raw)`), `Rate (raw)` > 0) %>%
  mutate(pct_change = 100 * (`Rate (calibrated)` - `Rate (raw)`) /
           `Rate (raw)`) %>%
  group_by(cause, sex, imd_quintile) %>%
  summarise(median_pct_change = median(pct_change, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(cause, sex, imd_quintile)

print(pif_by_imd_summary, n = 50)
write_csv(pif_by_imd_summary,
          file.path(out_dir_imd, "pif_by_imd_summary.csv"))

write_csv(
  transition_data,
  here("manchester/health/processed/health_transitions_manchester_280826.csv")
)

## Check expected number of lsoas (should be)

dplyr::n_distinct(transition_data$lsoa21cd)

# ============================================================
# Post-run checks
#
# These run on transition_data alone and should ALWAYS pass. There is
# deliberately no comparison against an earlier vintage: the files
# produced before the mean-preserving spline fix in
# health_data_Manchester.Rmd carry a one-directional bias in
# val_interpolated (rates inflated ~12-20% across the main disease
# onset ages, more below 40), so they are not a valid reference. Once
# two clean runs exist, a vintage comparison can be reintroduced.
# ============================================================

library(data.table)

CHK <- new.env()
CHK$fail <- character(0)

chk <- function(ok, msg) {
  cat(if (isTRUE(ok)) "  PASS  " else "  FAIL  ", msg, "\n", sep = "")
  if (!isTRUE(ok)) CHK$fail <- c(CHK$fail, msg)
  invisible(ok)
}

dt <- as.data.table(transition_data)
KEY <- c("lsoa21cd", "sex", "age", "cause")   # used by the duplicate-key check

cat("\n============ INTERNAL CHECKS ============\n")

# ---- Shape -------------------------------------------------
n_lsoa  <- uniqueN(dt$lsoa21cd)
n_cause <- uniqueN(dt$cause)
n_age   <- uniqueN(dt$age)

cat(sprintf("rows=%s  lsoa=%d  cause=%d  age=%d  sex=%d\n",
            format(nrow(dt), big.mark = ","), n_lsoa, n_cause,
            n_age, uniqueN(dt$sex)))

chk(n_lsoa == 1702L,
    sprintf("1702 GM LSOA21s present (got %d)", n_lsoa))
chk(anyDuplicated(dt, by = KEY) == 0L,
    "no duplicate lsoa21cd x sex x age x cause keys")
chk(!any(is.na(dt$rate)),   "no NA in rate")
chk(!any(is.na(dt$rate_raw)), "no NA in rate_raw")
chk(all(dt$rate >= 0) && all(dt$rate_raw >= 0), "no negative rates")

# Every LSOA should carry the same number of rows: a partial LSOA is
# the signature of a join that dropped some but not all of its cells.
per_lsoa <- dt[, .N, by = lsoa21cd]
chk(uniqueN(per_lsoa$N) == 1L,
    sprintf("every LSOA has the same row count (%s)",
            paste(sort(unique(per_lsoa$N)), collapse = "/")))
if (uniqueN(per_lsoa$N) > 1L) print(per_lsoa[, .N, by = N][order(-N.1)])

# ---- Sex-specific causes -----------------------------------
# endometrial_cancer must be female-only (sex == 2). breast_cancer is
# NOT checked: GBD reports male breast cancer and the pipeline keeps it.
endo_male <- dt[cause == "endometrial_cancer" & sex == 1L, .N]
chk(endo_male == 0L,
    sprintf("endometrial_cancer has no male rows (got %d)", endo_male))

# ---- Did the PIF actually apply? ---------------------------
# rate == rate_raw means no adjustment. Expect exactly ages 0-19,
# i.e. 20/101 = 19.80%, for EVERY cause. The diabetes exemption was
# removed in 2026-08 with Treatment A: nothing now re-anchors rate and
# rate_raw separately below age 40, so diabetes must satisfy the same
# rule as everything else. If it does not, investigate rather than exempt.
unc <- dt[, .(pct_unadj = round(100 * mean(abs(rate - rate_raw) < 1e-12), 2)),
          by = cause][order(-pct_unadj)]
print(unc)

expected_pct <- round(100 * 20 / n_age, 2)
off <- unc[abs(pct_unadj - expected_pct) > 0.5]
chk(nrow(off) == 0L,
    sprintf("all causes ~%.2f%% unadjusted (ages 0-19)",
            expected_pct))
if (nrow(off)) print(off)

# Under-20 rows must be untouched; 20+ rows should nearly all move.
# No cause is exempt as of 2026-08 (Treatment A withdrawn).
chk(dt[age < 20, all(abs(rate - rate_raw) < 1e-12)],
    "ages 0-19 are never PIF-adjusted")
chk(dt[age >= 20, mean(abs(rate - rate_raw) > 1e-12)] > 0.99,
    "ages 20+ are PIF-adjusted")

# ---- Plausibility of the adjustment ------------------------
# rate = rate_raw * (1 - PAF). A PAF outside roughly [-1, 1] would be
# a red flag, so the implied ratio should sit in a sane band.
ratio <- dt[rate_raw > 0, rate / rate_raw]
cat(sprintf("rate/rate_raw: min=%.3f  p01=%.3f  median=%.3f  p99=%.3f  max=%.3f\n",
            min(ratio), quantile(ratio, .01), median(ratio),
            quantile(ratio, .99), max(ratio)))
chk(min(ratio) >= 0 && max(ratio) < 3,
    "implied (1 - PAF) stays within [0, 3)")

# ---- IMD gradient ------------------------------------------
# The point of stratifying is that deprivation matters. If the median
# adjustment is identical across deciles, the strata are not binding
# and something upstream collapsed to the pooled PIF.
grad <- dt[age >= 20 & rate_raw > 0,
           .(median_pct = round(median(100 * (rate - rate_raw) / rate_raw), 2)),
           by = .(imd_decile)][order(imd_decile)]
print(grad)
chk(uniqueN(grad$median_pct) > 1L,
    "PIF adjustment varies across IMD deciles")

# ---- Cause coverage --------------------------------------------
# transition_data and the pifs CSV must cover exactly the same causes:
# a cause with no PIF silently comes out uncalibrated (rate == rate_raw),
# and a PIF with no cause is dead weight. myeloma is deliberately
# excluded from transition_data upstream, so it should be in neither.
chk(setequal(unique(dt$cause), unique(pifs$cause)),
    sprintf("transition_data causes match the pifs CSV (%d vs %d)",
            uniqueN(dt$cause), uniqueN(pifs$cause)))
if (!setequal(unique(dt$cause), unique(pifs$cause))) {
  cat("  only in transition_data: ",
      paste(setdiff(unique(dt$cause), unique(pifs$cause)), collapse = ", "), "\n")
  cat("  only in pifs: ",
      paste(setdiff(unique(pifs$cause), unique(dt$cause)), collapse = ", "), "\n")
}

# ---- Changed-boundary LSOAs ------------------------------------
# The 66 GM areas whose codes changed in 2021 are LSOA21-coded in
# deaths but reachable only via their LSOA11 parents in incidence.
# Both routes have been broken at different times: mortality was once
# zero-filled for them, incidence once dropped them entirely. Neither
# changed the row count, so check their VALUES.
changed66 <- setdiff(unique(dt$lsoa21cd), l11_21$LSOA11CD)
cat(sprintf("changed-boundary LSOAs: %d\n", length(changed66)))

if (length(changed66)) {
  n_expected <- nrow(dt) / n_lsoa * length(changed66)
  chk(abs(dt[lsoa21cd %in% changed66, .N] - n_expected) < 1,
      "changed-boundary LSOAs carry a full complement of rows")
  chk(dt[lsoa21cd %in% changed66, mean(rate_raw == 0)] < 0.5,
      sprintf("changed-boundary LSOAs are not zero-filled (%.1f%% zero)",
              100 * dt[lsoa21cd %in% changed66, mean(rate_raw == 0)]))
}

# ============================================================
cat("\n============ SUMMARY ============\n")
if (length(CHK$fail) == 0L) {
  cat("All checks passed.\n")
} else {
  cat(length(CHK$fail), "check(s) failed:\n")
  cat(paste0("  - ", CHK$fail, collapse = "\n"), "\n")
}