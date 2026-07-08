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
  pifs           = "manchester/health/processed/pifs_baseline_150526.csv",
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
clean_cause <- function(x) {
  x %>%
    str_replace_all("-", "_") %>%
    str_replace_all("parkinson’s_disease", "parkinson") %>%
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

add_age_groups <- function(df) {
  df %>%
    mutate(
      age = as.integer(age),
      age_group = cut(age, breaks = seq(0, max(age, na.rm = TRUE), by = 5),
                      right = FALSE, include.lowest = TRUE),
      sex_age_group = paste(sex, age_group, sep = "_")
    )
}

repeat_95_100 <- function(pifs) {
  pifs_older <- pifs |>
    filter(sex_age_group %in% c("male_[90,95)", "female_[90,95)")) |>
    mutate(sex_age_group = case_when(
      sex_age_group == "male_[90,95)"   ~ "male_[95,100]",
      sex_age_group == "female_[90,95)" ~ "female_[95,100]",
      TRUE ~ sex_age_group
    ))
  bind_rows(pifs, pifs_older)
}

safe_name <- function(x) str_replace_all(x, "[^A-Za-z0-9]+", "_")

# ============================================================
# 1) Read inputs ONCE
# ============================================================
# pifs CSV is produced by the new pif_calc.R and MUST carry an
# imd_decile column (1..10 stratified rows + NA pooled rows) plus
# paf_combined_traditional.
pifs <- read_csv(PATHS$pifs, show_col_types = FALSE) %>%
  repeat_95_100() %>%
  rename(cause = outcome) %>%
  mutate(cause = clean_cause(cause))

if (!"imd_decile" %in% names(pifs)) {
  stop("pifs CSV ('", PATHS$pifs, "') has no imd_decile column. ",
       "Regenerate it with the updated pif_calc.R (stratified by ",
       "sex_age_group x imd_decile, plus pooled imd_decile = NA ",
       "rows), then rerun.")
}
stopifnot("paf_combined_traditional" %in% names(pifs))

lsoa_11to21 <- read_csv(PATHS$lsoa_map, show_col_types = FALSE) %>%
  select(LSOA11CD, LSOA21CD, LAD22CD, LSOA21NM, LAD22NM)

transition_raw <- read_csv(PATHS$trans_raw, show_col_types = FALSE) %>%
  mutate(cause = clean_cause(cause)) %>%
  add_age_groups()

# ============================================================
# 2) Build 2011 -> 2021 equal-split mapping + GM universe
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

# ============================================================
# 2b) LSOA21 -> IMD decile lookup  [NEW]
#     Modal imd10 per LSOA21 (an LSOA21 can span OAs with
#     slightly different imd10). Forced to integer so it matches
#     the pifs CSV's imd_decile (1..10; 1 = most deprived).
# ============================================================
imd_lookup <- read_csv(PATHS$zones, show_col_types = FALSE) %>%
  count(lsoa21cd, imd10) %>%
  group_by(lsoa21cd) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    lsoa21cd   = as.character(lsoa21cd),
    imd_decile = suppressWarnings(as.integer(imd10))
  )

# ============================================================
# 3) Mortality: deaths (2011 LSOA) -> 2021 LSOA grid
# ============================================================
mortality_11 <- transition_raw %>%
  filter(measure == "deaths") %>%
  mutate(
    sex = case_when(
      sex %in% c("M","Male") ~ "M",
      sex %in% c("F","Female") ~ "F",
      TRUE ~ as.character(sex)
    ),
    location_code = as.character(location_code),
    rate_raw = as.numeric(rate)
  ) %>%
  select(age, sex, cause, rate_raw, location_code)

mortality_21_agg <- mortality_11 %>%
  inner_join(l11_21, by = c("location_code" = "LSOA11CD")) %>%
  mutate(rate_w = rate_raw * w) %>%
  group_by(LSOA21CD, LAD22CD, LAD22NM, sex, age, cause) %>%
  summarise(rate_raw = sum(rate_w, na.rm = TRUE), .groups = "drop")

sex_levels   <- sort(unique(mortality_11$sex))
age_levels   <- sort(unique(mortality_11$age))
cause_levels <- sort(unique(mortality_11$cause))

mortality <- tidyr::crossing(
  LSOA21CD = lsoa21_universe$LSOA21CD,
  sex      = sex_levels,
  age      = age_levels,
  cause    = cause_levels
) %>%
  left_join(lsoa21_universe, by = "LSOA21CD") %>%
  left_join(mortality_21_agg, by = c("LSOA21CD","LAD22CD","LAD22NM","sex","age","cause")) %>%
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
print(transition_data %>% count(pif_source) %>% arrange(desc(n)))

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
# TREATMENT A - DIABETES ONLY
#   NDA-calibrated linear ramp for ages < 40 (see fix_diabetes_nda).
#   Ages >= 40 are left exactly as-is (GBD values).
#
# TREATMENT B - PEAK-FLATTEN  (all diseases in PEAK_FLATTEN_CAUSES)
#   These diseases are expected to be monotone increasing with age.
#   Any decline after the peak is an artefact of sparse counts at
#   very old ages.
#   Fix: hold the rate at its peak value for all ages beyond the peak.
#   Peak age is detected automatically per group, so it is flexible
#   across diseases and sex/IMD strata.
#
# TREATMENT C - NO FIX  (diseases in NO_FIX_CAUSES)
#   Post-peak decline is REAL and must be preserved:
#     all_cause_mortality : exponential rise at old age, no artefact.
#     depression          : genuinely peaks in young adults (~age 20)
#                           then declines; flattening would wrongly hold
#                           the young-adult rate at all older ages.
#
# Applied to BOTH rate (calibrated) and rate_raw independently.
# ============================================================

# Diseases receiving the peak-flatten fix (Treatment B).
# Update this list if new causes are added to the model.
PEAK_FLATTEN_CAUSES <- c(
  "all_cause_dementia",
  "bladder_cancer",
  "breast_cancer",
  "colon_cancer",
  "copd",
  "coronary_heart_disease",
  "endometrial_cancer",
  "esophageal_cancer",
  "gastric_cardia_cancer",
  "head_neck_cancer",
  "liver_cancer",
  "lung_cancer",
  "myeloid_leukemia",
  "parkinson",
  "stroke"
)

# Diseases with no fix (Treatment C) - documented for clarity.
NO_FIX_CAUSES <- c("all_cause_mortality", "depression")

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
fix_peak_flatten <- function(vals) {
  if (all(is.na(vals))) return(vals)
  peak_idx <- which.max(vals)
  if (peak_idx < length(vals))
    vals[(peak_idx + 1L):length(vals)] <- vals[peak_idx]
  vals
}

message("Applying post-processing rate fixes ...")
message("  Treatment A (NDA ramp):    diabetes")
message("  Treatment B (peak-flatten): ", paste(PEAK_FLATTEN_CAUSES, collapse = ", "))
message("  Treatment C (no fix):      ", paste(NO_FIX_CAUSES, collapse = ", "))

# Sanity check: every cause in the data is accounted for
all_causes <- unique(transition_data$cause)
unaccounted <- setdiff(all_causes, c("diabetes", PEAK_FLATTEN_CAUSES, NO_FIX_CAUSES))
if (length(unaccounted) > 0) {
  warning("The following causes are not assigned to any treatment in section 6 ",
          "and will receive NO fix by default. Add them to PEAK_FLATTEN_CAUSES ",
          "or NO_FIX_CAUSES as appropriate: ",
          paste(unaccounted, collapse = ", "))
}

transition_data <- transition_data %>%
  group_by(lsoa21cd, sex, cause) %>%
  group_modify(~ {
    df <- .x %>% arrange(age)
    cz <- .y$cause

    if (cz == "diabetes") {
      # Treatment A: NDA-calibrated ramp for ages < 40
      df$rate     <- fix_diabetes_nda(df$rate,     df$age)
      df$rate_raw <- fix_diabetes_nda(df$rate_raw, df$age)
    } else if (cz %in% PEAK_FLATTEN_CAUSES) {
      # Treatment B: hold at peak value after the peak age
      df$rate     <- fix_peak_flatten(df$rate)
      df$rate_raw <- fix_peak_flatten(df$rate_raw)
    }
    # Treatment C (all_cause_mortality, depression): no modification
    df
  }) %>%
  ungroup()

message("Post-processing complete.")

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
  here("manchester/health/processed/health_transitions_manchester_190526.csv")
)

## Check expected number of lsoas (should be)

dplyr::n_distinct(transition_data$lsoa21cd)