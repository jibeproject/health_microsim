#### Assign disease state for baseline population
####
#### PART A  - assign disease status to the synthetic population
#### PART B  - compare assigned rates against GBD (North West England)
#### PART C  - compare assigned CANCER rates against NDRS registry
####            prevalence (tables 1 and 2, NHS England 2022)

library(tidyverse)
library(here)


# Synthetic population baseline
synth_pop <- read_csv(
  here("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_rr_2021.csv"),
  show_col_types = FALSE)

# Zones data

zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv"),
                  show_col_types = FALSE)
# Some vintages of zoneSystem.csv already call this column `zone`.
if ("oaID" %in% names(zones)) zones <- rename(zones, zone = oaID)
stopifnot(all(c("zone", "LSOA11CD", "lsoa21cd") %in% names(zones)))

# Disease prevalence data
#
# NOTE ON GEOGRAPHY: location_code in this file is an LSOA11 code, not
# an LSOA21 code and not a LAD. It is produced by the Rmd chunk
# disease-lsoa-expansion, where regional GBD rates are cross-joined onto
# gm_lsoas (keyed on england_lsoa_deaths$lsoa_code = LSOA11) and scaled
# by RR_used. Greater Manchester has 1,673 LSOA11s, which is why this
# file has 1,673 unique codes rather than the 1,702 LSOA21s used
# elsewhere. That is correct for its vintage, not a shortfall:
#   1,673 LSOA11  -  37 changed parents  +  66 new LSOA21 children  =  1,702
# zoneSystem.csv carries BOTH LSOA11CD and lsoa21cd, so the join below
# uses LSOA11CD and no 2011->2021 mapping is needed.
prevalence_raw <- read_csv(here("manchester/health/processed/health_transitions_manchester_prevalence.csv"),
                           show_col_types = FALSE)

if ("measure" %in% names(prevalence_raw) &&
    n_distinct(prevalence_raw$measure) > 1) {
  stop("This file holds more than one measure (",
       paste(sort(unique(prevalence_raw$measure)), collapse = ", "),
       "). The proportion reading below applies to Prevalence only -- ",
       "branch on `measure` before proceeding.")
}


n_over <- sum(prevalence_raw$rate > 1, na.rm = TRUE)
if (n_over > 0) {
  over <- prevalence_raw %>% filter(rate > 1)
  warning(n_over, " prevalence values exceed 1 (",
          sprintf("%.2f%%", 100 * n_over / nrow(prevalence_raw)),
          " of rows, max ", round(max(prevalence_raw$rate, na.rm = TRUE), 3),
          ") and are capped at 0.99. Affected causes: ",
          paste(sprintf("%s (n=%d, ages %s)",
                        names(table(over$cause)),
                        as.integer(table(over$cause)),
                        tapply(over$age, over$cause,
                               function(a) paste0(min(a), "-", max(a)))),
                collapse = "; "),
          ". This is spline overshoot at the top age band -- fix it in ",
          "health_data_Manchester.Rmd rather than relying on this cap.")
}

prevalence <- prevalence_raw %>%
  mutate(prob = pmin(pmax(rate, 0), 0.99),
         # normalise the apostrophe first: gbd_process.R has emitted both
         # the curly (U+2019) and straight forms depending on vintage
         cause = str_replace_all(cause, "\u2019", "'"),
         cause = str_replace_all(cause, fixed("parkinson's_disease"), "parkinson"),
         cause = str_replace_all(cause, fixed("head_and_neck_cancer"), "head_neck_cancer"),
         # sex is written as 1/2 by the Rmd; make it explicit
         sex = case_when(sex == 1 ~ "male",
                         sex == 2 ~ "female",
                         TRUE ~ as.character(sex)))

# How much the old transform was costing, by cause. Keep this printing:
# it is the fastest way to notice if the upstream file ever switches
# back to reporting a hazard, in which case these numbers collapse
# toward zero and the causes below stop being high-prevalence ones.
message("effect of dropping 1 - exp(-rate), by cause (max shortfall at ",
        "the top of each gradient):")
print(as.data.frame(
  prevalence %>%
    group_by(cause) %>%
    summarise(max_prob = round(max(prob, na.rm = TRUE), 4),
              old_prob = round(1 - exp(-max(prob, na.rm = TRUE)), 4),
              shortfall_pct = round(100 * (1 - (1 - exp(-max(prob, na.rm = TRUE))) /
                                             max(prob, na.rm = TRUE)), 1),
              .groups = "drop") %>%
    arrange(desc(shortfall_pct)) %>%
    head(6)
), row.names = FALSE)

### Assign geographies

synth_pop <- synth_pop %>%
  left_join(zones, by = c("zone" = "zone"))

stopifnot("LSOA11CD" %in% names(synth_pop))
if (any(is.na(synth_pop$LSOA11CD))) {
  stop(sum(is.na(synth_pop$LSOA11CD)), " people have no LSOA11CD after the ",
       "zone join; check that synth_pop$zone matches zones$zone.")
}

# [NEW] All-ages denominator, captured BEFORE the age > 18 filter below.
# PART C needs it: NDRS table 1 crude rates are per 100,000 of the WHOLE
# resident population, children included. Comparing an adults-only
# numerator against an adults-only denominator would overstate the
# assigned rate by roughly the child share of the population (~22% in GM).
synth_pop_denom <- synth_pop %>%
  rename(sex = gender) %>%
  mutate(sex = case_when(sex == 1 ~ "male",
                         sex == 2 ~ "female",
                         TRUE ~ as.character(sex)))

### Join prevalence rates to synthetic population

# [FIX] The join was on ladcd, but prevalence is reported at LSOA11
# (see the note above), so no location_code ever matched a LAD code.
# Every prevalence column came back NA, every disease status resolved
# to 0, and the baseline population was assigned no disease at all.
# This mirrors the same fix already applied in functions/pif_calc.R.
#
# [FIX 2] pivot_wider() silently returns LIST-COLUMNS when the
# id_cols x names_from combination is not unique -- it packs the
# colliding values into a list rather than erroring. Downstream,
# `runif(n()) < .x` then fails with
#     'list' object cannot be coerced to type 'double'
# and it fails on whichever cause happens to be alphabetically first,
# which is misleading: the fault is in the prevalence file, not that
# cause. Check for the collision explicitly and de-duplicate before
# pivoting, so the wide frame is guaranteed to be plain doubles.
dup_keys <- prevalence %>%
  count(age, sex, location_code, cause, name = "n_rows") %>%
  filter(n_rows > 1)

if (nrow(dup_keys) > 0) {
  warning(nrow(dup_keys), " (age, sex, location_code, cause) keys appear ",
          "more than once in the prevalence file (max ", max(dup_keys$n_rows),
          " rows). Collapsing to the mean prob per key. Affected causes: ",
          paste(sort(unique(dup_keys$cause)), collapse = ", "))
  prevalence <- prevalence %>%
    group_by(age, sex, location_code, cause) %>%
    summarise(prob = mean(prob, na.rm = TRUE), .groups = "drop")
}

synth_pop_wprob <- synth_pop |>
  rename(sex = gender) |>
  mutate(sex = case_when(sex == 1 ~ "male",
                         sex == 2 ~ "female",
                         TRUE ~ as.character(sex))) |>
  select(id, age, sex, ladcd, ladnm, lsoa21cd, LSOA11CD) |>
  rownames_to_column() |>
  left_join(
    prevalence |>
      pivot_wider(id_cols = c(age, sex, location_code),
                  names_from = cause, values_from = prob),
    by = c("age", "sex", "LSOA11CD" = "location_code")
  ) |> filter(age > 18)

# Guard the silent-failure case the ladcd join produced: if the keys do
# not match, every disease probability is NA and everyone ends up healthy.
disease_cols <- setdiff(names(synth_pop_wprob),
                        c("rowname", "id", "age", "sex", "ladcd", "ladnm",
                          "lsoa21cd", "LSOA11CD"))
if (length(disease_cols) == 0L) {
  stop("No disease columns after the prevalence join -- the pivot produced ",
       "nothing. Check that `cause` is populated in the prevalence file.")
}
# [FIX] as.matrix() on a frame containing list-columns produces a
# character matrix, so is.na() there measures nothing useful. Test each
# column on its own terms instead, treating a zero-length list element
# (pivot_wider's filler for an absent combination) as missing.
na_frac <- function(x) {
  if (is.list(x)) mean(lengths(x) == 0L | vapply(x, function(e) {
    length(e) == 0L || is.na(e[[1]])
  }, logical(1)))
  else mean(is.na(x))
}
pct_na <- mean(vapply(synth_pop_wprob[disease_cols], na_frac, numeric(1)))
message(sprintf("prevalence join: %d disease columns, %.1f%% NA",
                length(disease_cols), 100 * pct_na))
if (pct_na > 0.5) {
  stop(sprintf("%.1f%% of disease probabilities are NA after the join. ",
               100 * pct_na),
       "Everyone would be assigned zero diseases. Check the age / sex / ",
       "LSOA11CD keys against the prevalence file.")
}

# Function to allocate disease statuses based on probability

set.seed(123)

# [FIX] `across(copd:stroke, ...)` selected a POSITIONAL range, so it
# silently depended on pivot_wider's column order and would grab the
# wrong columns (or error) if a cause were added, removed or reordered.
# Use an explicit list intersected with what is actually present --
# the same approach as functions/pif_calc.R.
disease_vars_all <- c(
  "copd", "all_cause_dementia", "bladder_cancer", "breast_cancer",
  "colon_cancer", "coronary_heart_disease", "depression", "diabetes",
  "endometrial_cancer", "esophageal_cancer", "gastric_cardia_cancer",
  "head_neck_cancer", "liver_cancer", "lung_cancer", "myeloid_leukemia",
  "parkinson", "stroke"
)

# The subset of the above that NDRS counts as cancer. PART C sums these.
cancer_vars_all <- c(
  "bladder_cancer", "breast_cancer", "colon_cancer", "endometrial_cancer",
  "esophageal_cancer", "gastric_cardia_cancer", "head_neck_cancer",
  "liver_cancer", "lung_cancer", "myeloid_leukemia"
)

# Female-only causes: a male must never draw these. Extend if prostate
# or cervical cancer enter disease_vars_all later.
female_only_vars <- c("endometrial_cancer")

missing_dv <- setdiff(disease_vars_all, names(synth_pop_wprob))
if (length(missing_dv)) {
  warning("Disease(s) expected but not present after the prevalence join: ",
          paste(missing_dv, collapse = ", "))
}

# [NEW] Safety net for the list-column case. With the de-duplication
# above this should be a no-op, but it also rescues a stale
# synth_pop_wprob left in the session from an earlier run. A
# zero-length element means "no matching row in the prevalence file"
# and becomes NA, which the allocator then treats as zero risk.
flatten_prob <- function(x) {
  if (!is.list(x)) return(as.numeric(x))
  if (any(lengths(x) > 1L)) {
    stop("A probability column still holds multiple values per person. ",
         "De-duplicate the prevalence file on (age, sex, location_code, cause).")
  }
  vapply(x, function(e) if (length(e) == 0L) NA_real_ else as.numeric(e)[1],
         numeric(1))
}

allocate_disease <- function(df) {
  dv <- intersect(disease_vars_all, names(df))
  
  df <- df %>% mutate(dplyr::across(all_of(dv), flatten_prob))
  
  # A probability outside [0, 1] means the rate -> prob conversion or the
  # RR scaling has gone wrong upstream; runif() would silently clamp the
  # behaviour to always/never rather than erroring.
  oob <- dv[vapply(df[dv], function(x) any(x < 0 | x > 1, na.rm = TRUE), logical(1))]
  if (length(oob)) {
    stop("Probability outside [0,1] in: ", paste(oob, collapse = ", "))
  }
  
  # Sex-specific causes: zero the probability BEFORE the draw, so the
  # zero is visible in the probability column too, not just the status.
  for (v in intersect(female_only_vars, dv)) {
    df[[v]] <- dplyr::if_else(df$sex == "male", 0, df[[v]])
  }
  
  df %>%
    # [FIX] runif(n()) < NA yields NA, and ifelse then returns NA rather
    # than 0 or 1. A single unmatched key would therefore put NA into a
    # status column, which sum(value, na.rm = TRUE) later silently drops
    # from the numerator while the denominator still counts the person.
    # Treat a missing probability as zero risk, explicitly.
    mutate(dplyr::across(all_of(dv),
                         ~ as.integer(!is.na(.x) & runif(n()) < .x),
                         .names = "{.col}_status"))
}

# Apply function to assign diseases
synth_pop_assigned <- allocate_disease(synth_pop_wprob)

synth_pop_prev <- synth_pop_assigned %>%
  select(id, age, sex, ladcd, ladnm, lsoa21cd, LSOA11CD, ends_with("_status"))

# Post-conditions. These are cheap and catch the failure modes that are
# otherwise invisible until the results look odd months later.
stopifnot(!anyNA(synth_pop_prev %>% select(ends_with("_status"))))
if ("endometrial_cancer_status" %in% names(synth_pop_prev)) {
  stopifnot(sum(synth_pop_prev$endometrial_cancer_status[
    synth_pop_prev$sex == "male"]) == 0)
}

# Sanity check on the draw: assigned cases should track the assigned
# probabilities within binomial noise. A systematic gap means the
# allocation, not the source rates, is at fault.
draw_chk <- synth_pop_assigned %>%
  summarise(across(all_of(intersect(disease_vars_all, names(.))),
                   ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "cause", values_to = "expected")
drawn <- synth_pop_prev %>%
  summarise(across(ends_with("_status"), sum)) %>%
  pivot_longer(everything(), names_to = "cause", values_to = "drawn") %>%
  mutate(cause = str_remove(cause, "_status"))
message("expected vs drawn cases (ratio should be ~1.00):")
print(as.data.frame(
  draw_chk %>% inner_join(drawn, by = "cause") %>%
    mutate(ratio = round(drawn / expected, 3)) %>%
    arrange(desc(abs(ratio - 1)))
), row.names = FALSE)

# Save

synth_pop_prev_long <- synth_pop_prev |> pivot_longer(
  cols = ends_with("_status"),
  names_to = "diseases",
  values_to = "values"
) |>
  mutate(diseases = str_remove(diseases, "_status")) |>
  select(id, diseases, values) |>
  filter(values != 0) |>
  select(!values) |>
  group_by(id) %>%
  summarise(diseases = paste(diseases, collapse = " "), .groups = "drop")

# Output path carries a date stamp; set it once here so a re-run does
# not silently overwrite a previous vintage.
OUT_DATE <- format(Sys.Date(), "%d%m%y")
OUT_DIR  <- "manchester/health/processed/"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
write_csv(synth_pop_prev_long,
          file.path(OUT_DIR, paste0("base_prevalence_id_clean_", OUT_DATE, ".csv")))
message("wrote ", nrow(synth_pop_prev_long), " people with >=1 disease to ",
        file.path(OUT_DIR, paste0("base_prevalence_id_clean_", OUT_DATE, ".csv")))


# ============================================================
# PART B
# Compare assigned prevalence with GBD, by AGE GROUP x SEX.
#
# gbdp.csv is regional (location = "North West England" only), so
# there is no LAD dimension to compare on. Pooling the assigned
# population across GM and comparing by age x sex is the honest
# match to what the reference data actually is.
#
# Expect assigned to sit NEAR gbd, not on top of it: the prevalence
# file's rates are regional GBD scaled by RR_used (NDRS quintile RR,
# else the LSOA mortality RR), plus binomial noise from the runif()
# draw. A ratio near 1 is success; an exact 1.000 would be suspicious.
#
# NOTE: after the mean-preserving spline fix in
# health_data_Manchester.Rmd, the age gradient in this ratio should be
# roughly FLAT at about the population-weighted mean RR_used (~1.05-1.12).
# Before that fix it ran 1.89 -> 0.88 across age bands, which was the
# spline failing to conserve band means, not a deprivation effect.
# ============================================================

AGE_LEVELS <- c("20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                "55-59","60-64","65-69","70-74","75-79","80-84","85-89",
                "90-94","95+")

# ---- GBD reference ---------------------------------------------
gbd <- read_csv(here("manchester/health/processed/gbdp.csv"),
                show_col_types = FALSE) %>%
  filter(measure == "Prevalence") %>%          # file also holds Incidence
  mutate(
    # gbdp.csv keeps gbd_process()'s raw labels: COPD is uppercase and
    # parkinson uses a curly apostrophe. The assigned side has been
    # through tolower() and the parkinson collapse.
    cause = tolower(cause),
    cause = str_replace_all(cause, "\u2019", "'"),
    cause = str_replace_all(cause, fixed("parkinson's_disease"), "parkinson"),
    sex   = tolower(sex)
  ) %>%
  select(sex, cause, agegroup, rate_gbd = val)   # val is already per 100k

# ---- Assigned, pooled across GM --------------------------------
add_agegroup <- function(df) {
  df %>% mutate(agegroup = cut(
    age, breaks = c(seq(0, 95, 5), Inf), right = FALSE,
    labels = c("<5","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
               "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
               "75-79","80-84","85-89","90-94","95+")) %>% as.character())
}

assigned <- synth_pop_prev %>%
  add_agegroup() %>%
  pivot_longer(ends_with("_status"), names_to = "cause", values_to = "value") %>%
  mutate(cause = str_remove(cause, "_status")) %>%
  # [FIX] pop must be computed BEFORE grouping by cause. Grouping a
  # long-format frame by (sex, cause, agegroup) and then taking
  # n_distinct(id) happens to give the right answer only because every
  # person contributes exactly one row per cause -- but that breaks the
  # moment a cause is missing for some people (endometrial_cancer is
  # female-only, so its denominator would silently become the female
  # count while the numerator stays correct). Compute the denominator
  # once, from the person-level frame, and join it on.
  group_by(sex, cause, agegroup) %>%
  summarise(cases = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    synth_pop_prev %>%
      add_agegroup() %>%
      group_by(sex, agegroup) %>%
      summarise(pop = n_distinct(id), .groups = "drop"),
    by = c("sex", "agegroup")
  ) %>%
  mutate(rate_assigned = cases / pop * 100000)

# ---- Join and summarise ----------------------------------------
# Ages 0-18 are excluded upstream (filter(age > 18)), so 15-19 holds
# only 19-year-olds and its rate is not comparable to a full band.
cmp <- assigned %>%
  inner_join(gbd, by = c("sex", "cause", "agegroup")) %>%
  filter(agegroup %in% AGE_LEVELS) %>%
  mutate(ratio = rate_assigned / rate_gbd,
         agegroup = factor(agegroup, levels = AGE_LEVELS))

# Causes on one side only -- how a renamed GBD cause goes unnoticed
message("assigned but not in GBD: ",
        paste(setdiff(assigned$cause, gbd$cause), collapse = ", "))
message("in GBD but not assigned: ",
        paste(setdiff(gbd$cause, assigned$cause), collapse = ", "))

# Sparse cells produce ratios of 0 or 10+ that are binomial noise, not
# signal: a rare cancer in the 95+ band may draw zero cases from a
# handful of expected ones. Flag them rather than letting them distort
# the min/max columns.
cmp <- cmp %>% mutate(expected_cases = rate_gbd / 1e5 * pop)
n_sparse <- sum(cmp$expected_cases < 5, na.rm = TRUE)
message(sprintf("%d of %d cells have <5 expected cases (%.1f%%); ",
                n_sparse, nrow(cmp), 100 * n_sparse / nrow(cmp)),
        "their ratios are noise -- read the medians, not min/max.")

cat("\n== assigned / GBD ratio by cause (1.0 = exact reproduction) ==\n")
print(as.data.frame(
  cmp %>%
    filter(rate_gbd > 0) %>%
    group_by(cause) %>%
    summarise(
      median_ratio = round(median(ratio, na.rm = TRUE), 3),
      # min/max over well-populated cells only, so a single sparse
      # cell cannot make a healthy cause look broken
      min_ratio = round(suppressWarnings(
        min(ratio[expected_cases >= 5], na.rm = TRUE)), 3),
      max_ratio = round(suppressWarnings(
        max(ratio[expected_cases >= 5], na.rm = TRUE)), 3),
      n_sparse  = sum(expected_cases < 5, na.rm = TRUE),
      .groups = "drop") %>%
    arrange(desc(abs(median_ratio - 1)))
), row.names = FALSE)

cat("\n== by age group (all causes pooled) ==\n")
print(as.data.frame(
  cmp %>%
    filter(rate_gbd > 0) %>%
    group_by(agegroup, sex) %>%
    summarise(median_ratio = round(median(ratio, na.rm = TRUE), 3),
              .groups = "drop") %>%
    pivot_wider(names_from = sex, values_from = median_ratio)
), row.names = FALSE)

write_csv(cmp, here("manchester/health/processed/prevalence_vs_gbd.csv"))

# ---- Plots: one PNG per cause, facet by sex --------------------
out_dir <- here("images/manchester/prevalence")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

plot_df <- cmp %>%
  select(sex, cause, agegroup, assigned = rate_assigned, gbd = rate_gbd) %>%
  pivot_longer(c(assigned, gbd), names_to = "type", values_to = "rate")

for (cs in sort(unique(plot_df$cause))) {
  d <- plot_df %>% filter(cause == cs)
  
  p <- ggplot(d, aes(agegroup, rate, fill = type)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.85) +
    facet_wrap(~ sex, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = c(assigned = "#1f6feb", gbd = "#d1495b"),
                      labels = c(assigned = "Assigned", gbd = "GBD (North West)"),
                      name = NULL) +
    labs(x = "Age group", y = "Rate per 100,000") +
    theme_classic(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(face = "bold"))
  
  ggsave(file.path(out_dir, paste0("prev_", cs, ".png")),
         p, width = 10, height = 4.5, dpi = 200, bg = "white")
}

message("wrote ", length(unique(plot_df$cause)), " plots to ", out_dir)


# ---- Combined figure for the paper -----------------------------
# One panel per cause, both sexes overlaid. Bars do not survive
# being shrunk to 1/18th of a page, so the combined version uses
# lines: source is colour, sex is linetype. Each panel keeps its own
# y scale because the causes span four orders of magnitude (myeloid
# leukaemia peaks near 30 per 100,000, COPD near 50,000).
pretty_cause <- function(x) {
  x <- gsub("_", " ", x)
  x <- sub("^copd$", "COPD", x)
  x <- sub("^coronary heart disease$", "Coronary heart disease", x)
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

# Age band -> numeric midpoint, so the x axis is continuous and the
# tick labels do not collide at panel width.
band_midpoint <- function(f) {
  i <- match(as.character(f), AGE_LEVELS)
  20 + 5 * (i - 1) + 2.5
}

combined_df <- cmp %>%
  mutate(age_mid = band_midpoint(agegroup),
         cause_lab = pretty_cause(cause)) %>%
  select(sex, cause_lab, age_mid, Assigned = rate_assigned, GBD = rate_gbd) %>%
  pivot_longer(c(Assigned, GBD), names_to = "source", values_to = "rate")

p_all <- ggplot(combined_df,
                aes(age_mid, rate, colour = source, linetype = sex)) +
  geom_line(linewidth = 0.55) +
  facet_wrap(~ cause_lab, ncol = 4, scales = "free_y") +
  scale_colour_manual(values = c(Assigned = "#1f6feb", GBD = "#d1495b"),
                      labels = c(Assigned = "Assigned baseline",
                                 GBD = "GBD (North West)"),
                      name = NULL) +
  scale_linetype_manual(values = c(female = "solid", male = "22"),
                        name = NULL) +
  scale_x_continuous(breaks = seq(20, 100, 20)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(x = "Age (years)", y = "Prevalence per 100,000") +
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 8, hjust = 0),
        panel.spacing = unit(0.7, "lines"),
        axis.text = element_text(size = 7))

ggsave(file.path(out_dir, "fig_prevalence_vs_gbd_all_causes.png"),
       p_all, width = 9, height = 10, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "fig_prevalence_vs_gbd_all_causes.pdf"),
       p_all, width = 9, height = 10, bg = "white")

# Supplementary figure: the same comparison expressed as a ratio.
# A flat series near 1.0 is the pass condition and a sloped one is
# not. Kept out of the main text as it duplicates Figure 1; sparse
# cells (<5 expected cases) are dropped rather than plotted as noise.
p_ratio <- cmp %>%
  filter(rate_gbd > 0, expected_cases >= 5) %>%
  mutate(age_mid = band_midpoint(agegroup),
         cause_lab = pretty_cause(cause)) %>%
  ggplot(aes(age_mid, ratio, colour = sex)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.9, ymax = 1.1,
           fill = "grey85", alpha = 0.5) +
  geom_hline(yintercept = 1, linewidth = 0.3, colour = "grey40") +
  geom_line(linewidth = 0.5) +
  geom_point(size = 0.6) +
  facet_wrap(~ cause_lab, ncol = 4) +
  scale_colour_manual(values = c(female = "#b5179e", male = "#1f6feb"),
                      name = NULL) +
  scale_x_continuous(breaks = seq(20, 100, 20)) +
  scale_y_continuous(trans = "log2",
                     breaks = c(0.5, 0.75, 1, 1.5, 2)) +
  coord_cartesian(ylim = c(0.5, 2)) +
  labs(x = "Age (years)",
       y = "Assigned / GBD rate ratio (log scale)") +
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 8, hjust = 0),
        panel.spacing = unit(0.7, "lines"),
        axis.text = element_text(size = 7))

ggsave(file.path(out_dir, "figS1_prevalence_vs_gbd_ratio.png"),
       p_ratio, width = 9, height = 10, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "figS1_prevalence_vs_gbd_ratio.pdf"),
       p_ratio, width = 9, height = 10, bg = "white")

message("wrote combined figures (png + pdf) to ", out_dir)


# ============================================================
# PART C
# Compare assigned CANCER prevalence with the NDRS registry counts.
#
# WHAT THE REFERENCE DATA IS
#   table_1_prevalence.csv  Cancer prevalence 2022, all ages, by
#                           Local Authority x sex x prevalence duration.
#   table_2_prevalence.csv  Cancer prevalence 2022, by 5-year age band
#                           x sex x duration, but only down to Cancer
#                           Alliance / ICB level -- NOT by LAD.
#   Both: NHS England / NDRS, crude rate per 100,000 resident population.
#
# So the two tables answer different questions and are used differently:
#   table 1 -> does the geographic (LAD) distribution look right?
#   table 2 -> does the age gradient look right?
#
# THREE THINGS MAKE THIS NOT A LIKE-FOR-LIKE COMPARISON. All three
# push the assigned rate DOWN relative to NDRS, and all three are
# handled or quantified below rather than ignored.
#
# 1. SITE COVERAGE. NDRS counts every registered malignancy. The
#    synthetic population carries only the GBD sites listed in
#    cancer_vars_all. Prostate, cervical, ovarian, kidney, pancreatic,
#    melanoma, thyroid, brain/CNS, non-Hodgkin and Hodgkin lymphoma,
#    testicular and mesothelioma are all absent. Prostate alone is
#    roughly a quarter of male cancer prevalence. Expect the assigned
#    total to be somewhere near half of NDRS, and read the RATIO OF
#    RATIOS across ages and LADs rather than the absolute level.
#
# 2. PREVALENCE DURATION. NDRS reports 5, 10, 20 year and Maximum
#    (lifetime) prevalence. GBD's cancer model treats survivors beyond
#    10 years as cured and does not carry them as prevalent cases --
#    see GBD 2019 nonfatal appendix p807, "survivors beyond 10 years
#    were considered cured". The 10-year NDRS series is therefore the
#    matching definition, and NDRS_DURATION is set to it below. Set it
#    to "Maximum" and the assigned side will look far too low for a
#    reason that has nothing to do with this script.
#
# 3. DENOMINATOR. NDRS crude rates are per 100,000 of the whole
#    resident population, children included. synth_pop_prev is filtered
#    to age > 18. PART C therefore uses synth_pop_denom (captured before
#    the filter) for table 1, and restricts to adult age bands for
#    table 2. Childhood cancer is <1% of registry prevalence, so the
#    adults-only numerator is a minor understatement; the denominator
#    would have been a ~22% overstatement if left uncorrected.
# ============================================================

NDRS_DIR      <- here("manchester/health/original")   # where the two CSVs live
NDRS_DURATION <- "10"    # see note 2 above; "5", "10", "20" or "Maximum"

# The published CSVs carry seven rows of title//contents matter, a
# header on row 9 (1-based) and a copyright line at the foot. They are
# Windows-1252, not UTF-8 -- the copyright symbol will error under a
# UTF-8 locale.
read_ndrs <- function(path) {
  read_csv(path, skip = 9, show_col_types = FALSE,
           locale = locale(encoding = "Windows-1252"),
           na = c("", "NA", "*", ":")) %>%
    rename_with(~ str_replace(.x, "^Georaphy$", "Geography")) %>%  # sic, in source
    filter(!is.na(`Area code`), str_detect(`Area code`, "^E")) %>%
    mutate(sex = case_when(Gender == "Females" ~ "female",
                           Gender == "Males"   ~ "male",
                           Gender == "Persons" ~ "persons",
                           TRUE ~ tolower(Gender)),
           across(any_of(c("Count", "Crude rate", "Lower CI", "Upper CI")),
                  ~ suppressWarnings(as.numeric(str_remove_all(as.character(.x), ","))))) %>%
    rename(duration = `Prevalence duration`)
}

t1_path <- file.path(NDRS_DIR, "table_1_prevalence.csv")
t2_path <- file.path(NDRS_DIR, "table_2_prevalence.csv")

if (!file.exists(t1_path) || !file.exists(t2_path)) {
  warning("NDRS tables not found under ", NDRS_DIR,
          " -- skipping PART C. Expected table_1_prevalence.csv and ",
          "table_2_prevalence.csv.")
} else {
  
  ndrs_t1 <- read_ndrs(t1_path)
  ndrs_t2 <- read_ndrs(t2_path)
  
  stopifnot(NDRS_DURATION %in% unique(ndrs_t1$duration))
  
  # ---- Assigned: one row per person, any modelled cancer site ----
  # NDRS prevalence counts PEOPLE, not tumours: someone on the registry
  # with two primaries appears once. Summing the status columns would
  # double-count them, so reduce to a single any-cancer indicator.
  cv <- intersect(paste0(cancer_vars_all, "_status"), names(synth_pop_prev))
  message("PART C: pooling ", length(cv), " cancer sites -> any_cancer")
  
  assigned_person <- synth_pop_prev %>%
    mutate(any_cancer = as.integer(rowSums(across(all_of(cv))) > 0)) %>%
    select(id, age, sex, ladcd, ladnm, any_cancer)
  
  message(sprintf("  %d people with >=1 modelled cancer (%.2f%% of adults)",
                  sum(assigned_person$any_cancer),
                  100 * mean(assigned_person$any_cancer)))
  
  # ---------------------------------------------------------------
  # C1. By LOCAL AUTHORITY x sex  (table 1)
  # ---------------------------------------------------------------
  # Denominator is the full all-ages synthetic population, to match
  # NDRS's resident-population base.
  denom_lad <- synth_pop_denom %>%
    group_by(ladcd, sex) %>%
    summarise(pop_all_ages = n(), .groups = "drop")
  
  assigned_lad <- assigned_person %>%
    group_by(ladcd, ladnm, sex) %>%
    summarise(cases = sum(any_cancer), .groups = "drop") %>%
    left_join(denom_lad, by = c("ladcd", "sex")) %>%
    mutate(rate_assigned = cases / pop_all_ages * 100000)
  
  ndrs_lad <- ndrs_t1 %>%
    filter(str_detect(Geography, "Local Authority"),
           duration == NDRS_DURATION,
           sex %in% c("male", "female")) %>%
    select(ladcd = `Area code`, area_name = `Area name`, sex,
           ndrs_count = Count, rate_ndrs = `Crude rate`,
           ndrs_lo = `Lower CI`, ndrs_hi = `Upper CI`)
  
  cmp_lad <- assigned_lad %>%
    inner_join(ndrs_lad, by = c("ladcd", "sex")) %>%
    mutate(coverage = rate_assigned / rate_ndrs)
  
  if (nrow(cmp_lad) == 0) {
    warning("No LAD matched between the synthetic population and NDRS table 1. ",
            "Check that ladcd holds E08xxxxxx codes.")
  } else {
    message(sprintf("  matched %d LAD x sex cells (%d LADs)",
                    nrow(cmp_lad), n_distinct(cmp_lad$ladcd)))
  }
  
  cat("\n== PART C1: any modelled cancer vs NDRS ", NDRS_DURATION,
      "-year prevalence, by LAD ==\n", sep = "")
  cat("   coverage = assigned / NDRS. Below 1 by design: modelled sites vs all registered sites.\n")
  cat("   What matters is that coverage is FLAT across LADs. A LAD that\n",
      "   deviates from the others is a geography or deprivation-scaling bug.\n\n", sep = "")
  print(as.data.frame(
    cmp_lad %>%
      select(ladnm, sex, cases, rate_assigned, rate_ndrs, coverage) %>%
      mutate(across(c(rate_assigned, rate_ndrs), ~ round(.x, 1)),
             coverage = round(coverage, 3)) %>%
      arrange(sex, desc(coverage))
  ), row.names = FALSE)
  
  cat("\n   coverage spread across LADs (tight = good):\n")
  print(as.data.frame(
    cmp_lad %>%
      group_by(sex) %>%
      summarise(median_coverage = round(median(coverage), 3),
                iqr             = round(IQR(coverage), 3),
                min             = round(min(coverage), 3),
                max             = round(max(coverage), 3),
                cv_pct          = round(100 * sd(coverage) / mean(coverage), 1),
                .groups = "drop")
  ), row.names = FALSE)
  
  # Rank correlation asks the question the level cannot: even though the
  # assigned rate is low in absolute terms, does it order the LADs the way
  # the registry does? That is the part the deprivation scaling controls.
  cat("\n   Spearman rank correlation of LAD ordering (assigned vs NDRS):\n")
  print(as.data.frame(
    cmp_lad %>%
      group_by(sex) %>%
      summarise(rho = round(cor(rate_assigned, rate_ndrs, method = "spearman"), 3),
                n_lads = n(), .groups = "drop")
  ), row.names = FALSE)
  
  # ---------------------------------------------------------------
  # C2. By AGE BAND x sex  (table 2, Greater Manchester)
  # ---------------------------------------------------------------
  # Table 2 stops at Cancer Alliance / ICB level. GM appears as Cancer
  # Alliance E56000032 and as ICB E54000057; either is the whole
  # conurbation, so take the Cancer Alliance row and fall back to the ICB.
  GM_T2_AREA <- c("E56000032", "E54000057")
  
  ndrs_gm <- ndrs_t2 %>%
    filter(`Area code` %in% GM_T2_AREA,
           duration == NDRS_DURATION,
           sex %in% c("male", "female"))
  
  if (n_distinct(ndrs_gm$`Area code`) > 1) {
    keep <- if ("E56000032" %in% ndrs_gm$`Area code`) "E56000032" else "E54000057"
    ndrs_gm <- filter(ndrs_gm, `Area code` == keep)
  }
  message("  table 2 reference area: ", unique(ndrs_gm$`Area name`))
  
  ndrs_gm <- ndrs_gm %>%
    select(sex, agegroup = `Age at index date`,
           ndrs_count = Count, rate_ndrs = `Crude rate`)
  
  # NDRS bands are zero-padded and top out at 90+; the assigned side uses
  # unpadded labels and splits 90-94 / 95+. Harmonise onto the NDRS grid.
  add_agegroup_ndrs <- function(df) {
    df %>% mutate(agegroup = cut(
      age, breaks = c(seq(0, 90, 5), Inf), right = FALSE,
      labels = c("00-04","05-09","10-14","15-19","20-24","25-29","30-34",
                 "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                 "70-74","75-79","80-84","85-89","90+")) %>% as.character())
  }
  
  # Adults only: age > 18 upstream means 15-19 is a partial band and
  # everything below it is empty, so neither is comparable.
  ADULT_BANDS <- c("20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                   "55-59","60-64","65-69","70-74","75-79","80-84","85-89","90+")
  
  denom_age <- synth_pop_denom %>%
    add_agegroup_ndrs() %>%
    group_by(sex, agegroup) %>%
    summarise(pop = n(), .groups = "drop")
  
  assigned_age <- assigned_person %>%
    add_agegroup_ndrs() %>%
    group_by(sex, agegroup) %>%
    summarise(cases = sum(any_cancer), .groups = "drop") %>%
    left_join(denom_age, by = c("sex", "agegroup")) %>%
    mutate(rate_assigned = cases / pop * 100000)
  
  cmp_age <- assigned_age %>%
    inner_join(ndrs_gm, by = c("sex", "agegroup")) %>%
    filter(agegroup %in% ADULT_BANDS) %>%
    mutate(coverage = rate_assigned / rate_ndrs,
           agegroup = factor(agegroup, levels = ADULT_BANDS)) %>%
    arrange(sex, agegroup)
  
  cat("\n== PART C2: any modelled cancer vs NDRS ", NDRS_DURATION,
      "-year prevalence, by age band (GM) ==\n", sep = "")
  cat("   Read the SHAPE of coverage down the age bands, not its level.\n")
  cat("   Flat  -> the age gradient is right, the gap is missing sites.\n")
  cat("   Rising or falling -> the age gradient itself is wrong.\n\n")
  print(as.data.frame(
    cmp_age %>%
      select(sex, agegroup, cases, pop, rate_assigned, rate_ndrs, coverage) %>%
      mutate(across(c(rate_assigned, rate_ndrs), ~ round(.x, 1)),
             coverage = round(coverage, 3))
  ), row.names = FALSE)
  
  # A monotone trend in coverage across age is the signal worth having:
  # it separates "missing sites" (flat) from "wrong age gradient" (sloped).
  cat("\n   trend in coverage across age bands (Spearman vs band index):\n")
  print(as.data.frame(
    cmp_age %>%
      group_by(sex) %>%
      summarise(
        median_coverage = round(median(coverage, na.rm = TRUE), 3),
        trend_rho = round(suppressWarnings(
          cor(as.integer(agegroup), coverage, method = "spearman",
              use = "complete.obs")), 3),
        verdict = case_when(
          abs(trend_rho) < 0.4 ~ "flat - consistent with missing sites only",
          trend_rho >= 0.4     ~ "RISING with age - check the age spline",
          TRUE                 ~ "FALLING with age - check the age spline"),
        .groups = "drop")
  ), row.names = FALSE)
  
  # ---- Write out and plot ----------------------------------------
  write_csv(cmp_lad, here("manchester/health/processed/cancer_vs_ndrs_lad.csv"))
  write_csv(cmp_age, here("manchester/health/processed/cancer_vs_ndrs_age.csv"))
  
  p_age <- cmp_age %>%
    select(sex, agegroup, Assigned = rate_assigned, NDRS = rate_ndrs) %>%
    pivot_longer(c(Assigned, NDRS), names_to = "type", values_to = "rate") %>%
    ggplot(aes(agegroup, rate, fill = type)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.85) +
    facet_wrap(~ sex, ncol = 2) +
    scale_fill_manual(values = c(Assigned = "#1f6feb", NDRS = "#0b7a5b"),
                      labels = c(Assigned = paste0("Assigned (", length(cv),
                                                   " GBD sites)"),
                                 NDRS = paste0("NDRS (all sites, ",
                                               NDRS_DURATION, "-yr)")),
                      name = NULL) +
    labs(x = "Age group",
         y = paste0("Prevalence per 100,000 (", NDRS_DURATION, "-year)")) +
    theme_classic(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text = element_text(face = "bold"))
  
  ggsave(file.path(out_dir, "cancer_vs_ndrs_age.png"),
         p_age, width = 10, height = 4.5, dpi = 200, bg = "white")
  
  p_lad <- ggplot(cmp_lad, aes(rate_ndrs, rate_assigned, colour = sex)) +
    geom_point(size = 2.4, alpha = 0.9) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.5, linetype = "dashed") +
    scale_colour_manual(values = c(female = "#b5179e", male = "#1f6feb"),
                        name = NULL) +
    labs(x = paste0("NDRS rate per 100,000, all sites (", NDRS_DURATION,
                    "-year)"),
         y = paste0("Assigned rate per 100,000 (", length(cv),
                    " GBD sites)")) +
    theme_classic(base_size = 11) +
    theme(legend.position = "bottom")
  
  # ggrepel keeps the 10 LAD labels from colliding, but it is optional --
  # fall back to plain text labels rather than losing the plot.
  p_lad <- if (requireNamespace("ggrepel", quietly = TRUE)) {
    p_lad + ggrepel::geom_text_repel(aes(label = ladnm), size = 2.9,
                                     show.legend = FALSE, max.overlaps = 20)
  } else {
    message("  install.packages('ggrepel') for non-overlapping LAD labels.")
    p_lad + geom_text(aes(label = ladnm), size = 2.9, vjust = -0.8,
                      show.legend = FALSE)
  }
  
  ggsave(file.path(out_dir, "cancer_vs_ndrs_lad.png"),
         p_lad, width = 8, height = 6, dpi = 200, bg = "white")
  
  message("PART C complete: wrote cancer_vs_ndrs_lad.csv, ",
          "cancer_vs_ndrs_age.csv and plots to ", out_dir)
  
  
  # ============================================================
  # PART D
  # Every figure quoted in the validation text of the paper, printed
  # in one place so the prose and the run cannot drift apart. Re-run
  # this and update the manuscript whenever the pipeline changes.
  # ============================================================
  
  cancer_causes <- intersect(cancer_vars_all, unique(cmp$cause))
  noncancer_causes <- setdiff(unique(cmp$cause), cancer_causes)
  
  cat("\n\n=============== NUMBERS FOR THE PAPER ===============\n")
  
  cat("\n-- Internal validation (vs GBD) --\n")
  int_all <- cmp %>% filter(rate_gbd > 0, expected_cases >= 5)
  cat(sprintf("  overall median ratio    : %.2f (IQR %.2f-%.2f, n=%d cells)\n",
              median(int_all$ratio), quantile(int_all$ratio, .25),
              quantile(int_all$ratio, .75), nrow(int_all)))
  cat(sprintf("  non-cancer median ratio : %.2f\n",
              median(int_all$ratio[int_all$cause %in% noncancer_causes])))
  cat(sprintf("  cancer median ratio     : %.2f\n",
              median(int_all$ratio[int_all$cause %in% cancer_causes])))
  cat(sprintf("  %% of cells within 10%%    : %.0f%%\n",
              100 * mean(abs(int_all$ratio - 1) <= 0.10)))
  cat(sprintf("  sparse cells excluded   : %d of %d (%.0f%%)\n",
              sum(cmp$expected_cases < 5, na.rm = TRUE), nrow(cmp),
              100 * mean(cmp$expected_cases < 5, na.rm = TRUE)))
  
  cat("\n  median ratio by cause:\n")
  print(as.data.frame(
    int_all %>% group_by(cause) %>%
      summarise(median_ratio = round(median(ratio), 2),
                min = round(min(ratio), 2), max = round(max(ratio), 2),
                .groups = "drop") %>% arrange(desc(median_ratio))
  ), row.names = FALSE)
  
  # Age drift is the pass/fail test: a flat series means the gradient is
  # right and any offset is the deprivation adjustment.
  cat("\n  age drift (Spearman of ratio vs age band, non-cancer):\n")
  print(as.data.frame(
    int_all %>% filter(cause %in% noncancer_causes) %>%
      group_by(cause) %>%
      summarise(rho = round(suppressWarnings(cor(as.integer(agegroup), ratio,
                                                 method = "spearman")), 2),
                .groups = "drop") %>% arrange(desc(abs(rho)))
  ), row.names = FALSE)
  
  cat("\n-- External validation (vs NDRS) --\n")
  cat(sprintf("  duration matched        : %s-year prevalence\n", NDRS_DURATION))
  cat(sprintf("  modelled sites          : %d\n", length(cv)))
  print(as.data.frame(
    cmp_lad %>% group_by(sex) %>%
      summarise(median_coverage = round(median(coverage), 2),
                iqr = round(IQR(coverage), 3),
                cv_pct = round(100 * sd(coverage) / mean(coverage), 1),
                spearman_rho = round(cor(rate_assigned, rate_ndrs,
                                         method = "spearman"), 2),
                .groups = "drop")
  ), row.names = FALSE)
  
  cat("\n  coverage by age band (GM):\n")
  print(as.data.frame(
    cmp_age %>%
      select(sex, agegroup, coverage) %>%
      mutate(coverage = round(coverage, 2)) %>%
      pivot_wider(names_from = sex, values_from = coverage)
  ), row.names = FALSE)
  
  cat("\n  coverage trend across age (Spearman):\n")
  print(as.data.frame(
    cmp_age %>% group_by(sex) %>%
      summarise(rho = round(suppressWarnings(
        cor(as.integer(agegroup), coverage, method = "spearman",
            use = "complete.obs")), 2), .groups = "drop")
  ), row.names = FALSE)
  
  # Breast is the dominant female site, so its share is the quickest
  # test of whether GBD and the registry disagree on that cause.
  cat("\n  breast share of assigned female cancer (registry ~30-35%):\n")
  print(as.data.frame(
    assigned_person %>% filter(sex == "female") %>%
      left_join(synth_pop_prev %>% select(id, breast_cancer_status),
                by = "id") %>%
      summarise(breast     = sum(breast_cancer_status),
                any_cancer = sum(any_cancer),
                share_pct  = round(100 * sum(breast_cancer_status) /
                                     sum(any_cancer), 1))
  ), row.names = FALSE)
  
  cat("\n=====================================================\n")
  
}  # end PART C availability guard