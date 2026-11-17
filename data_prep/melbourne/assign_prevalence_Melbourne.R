#### Assign baseline disease state to the synthetic population
#### Reads : pp_2018.csv (chosen interactively), SA2_2016_AUST.csv,
####         health_transitions_melbourne_prevalence_SA2.csv
#### Writes: {scenario}_prevalence_id_clean_{date}_{region}.csv + validation plots

library(tidyverse)
library(here)
# data.table is optional; it makes the 1.48 GB population read much faster

# ---------------------------------------------------------------- config ----
SYNTH_POP_COLS <- c("id", "age", "gender", "zone")
MIN_AGE        <- 19
EXTEND_TOP_AGE <- TRUE    # carry age-95 prevalence forward to 96-100

# GBD prevalence is a proportion, but the Rmd applies prob = 1 - exp(-rate),
# the incidence-rate-to-probability formula. It shrinks large values (0.98x at
# p = 0.04, 0.87x at p = 0.29), which is what produced the apparent drift with
# age. -log(1 - prob) inverts it exactly (dementia F age 95 -> 0.28909 against
# GBD 0.28908). Set FALSE once the Rmd stops transforming prevalence.
FIX_PREVALENCE_PROB <- TRUE

USE_CACHE  <- TRUE
CACHE_DIR  <- file.path(tempdir(), "synthpop_cache")
OUT_SUBDIR <- "preprocessing/health/processed"
PLOT_DIR   <- here("images", "Melbourne")
GBD_YEAR   <- 2018

# ----------------------------------------------------------- file picker ----
f <- normalizePath(file.choose(), winslash = "/")
scenario_name <- str_split_i(str_extract(f, "(?<=scenOutput/)[^/]+"), " ", 1)
study_region  <- str_extract(f, "(?<=/)[^/]+(?=/scenOutput/)")
if (is.na(scenario_name) || is.na(study_region))
  stop("Path must look like .../{region}/scenOutput/{scenario}/pp_2018.csv\nGot: ", f)
cat("Region:", study_region, "| Scenario:", scenario_name, "\n")

find_file <- function(subdirs, filename, label) {
  hits <- file.path(here(study_region), subdirs, filename)
  hits <- hits[file.exists(hits)]
  if (!length(hits)) stop(label, " not found in: ", paste(subdirs, collapse = ", "))
  if (length(hits) > 1)
    warning(label, " exists in ", length(hits), " locations; using ", hits[1],
            call. = FALSE)
  hits[1]
}

# ------------------------------------------------ read population (cached) ---
read_synth_pop <- function(path, cols) {
  i  <- file.info(path)
  cf <- file.path(CACHE_DIR, paste0(gsub("[^A-Za-z0-9_.-]", "_",
                                         paste0(basename(path), i$size, as.integer(i$mtime))), ".rds"))
  if (USE_CACHE && file.exists(cf)) { cat("  (cached)\n"); return(readRDS(cf)) }
  out <- if (requireNamespace("data.table", quietly = TRUE)) {
    as_tibble(data.table::fread(path, select = cols, showProgress = FALSE))
  } else {
    read_csv(path, col_select = all_of(cols), show_col_types = FALSE,
             progress = FALSE)
  }
  if (USE_CACHE) {
    dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, cf, compress = FALSE)
  }
  out
}

cat("Reading synthetic population...\n")
synth_pop <- read_synth_pop(f, SYNTH_POP_COLS)
if (length(setdiff(SYNTH_POP_COLS, names(synth_pop))))
  stop("Missing columns - did you pick pp_2018.csv?")
cat("  ", nrow(synth_pop), "agents\n")

# ----------------------------------------------------------- zone -> SA2 ----
# zone is SA1_7DIGITCODE_2016 = STATE(1) + SA2(4) + SA1(2), so the first five
# characters are the SA2 5-digit code. The 5->9 digit step needs the ABS file.
sa2_geo <- read_csv(find_file(c("preprocessing/health/original/ABS",
                                "preprocessing/original/ABS"),
                              "SA2_2016_AUST.csv", "SA2 geography"),
                    col_types = cols(.default = col_character())) %>%
  filter(GCCSA_NAME_2016 == "Greater Melbourne") %>%
  transmute(sa2_5dig   = SA2_5DIGITCODE_2016,
            SA2_MAIN16 = as.numeric(SA2_MAINCODE_2016))

synth_pop <- synth_pop %>%
  mutate(sa2_5dig = substr(as.character(zone), 1, 5)) %>%
  left_join(sa2_geo, by = "sa2_5dig")
if (any(is.na(synth_pop$SA2_MAIN16)))
  warning(sum(is.na(synth_pop$SA2_MAIN16)), " agents have an unmatched zone",
          call. = FALSE)

# ------------------------------------------------------------- prevalence ---
prev_path <- find_file(c("preprocessing/health/processed",
                         "preprocessing/processed", "health/processed"),
                       "health_transitions_melbourne_prevalence_SA2.csv",
                       "Prevalence file")
cat("Prevalence:", prev_path, "\n")
prevalence <- read_csv(prev_path, show_col_types = FALSE)

if (FIX_PREVALENCE_PROB) {
  if (max(prevalence$prob) > 0.60)
    stop("max prob = ", round(max(prevalence$prob), 3),
         " - this looks already corrected. Set FIX_PREVALENCE_PROB <- FALSE.")
  stopifnot(max(prevalence$prob) < 1)
  prevalence <- mutate(prevalence, prob = -log(1 - prob))
  cat("  applied -log(1 - prob); max prob now",
      round(max(prevalence$prob), 3), "\n")
}

if (EXTEND_TOP_AGE && max(synth_pop$age) > max(prevalence$age)) {
  top <- max(prevalence$age)
  prevalence <- bind_rows(prevalence,
                          crossing(filter(prevalence, age == top) %>% dplyr::select(-age),
                                   age = (top + 1):max(synth_pop$age)))
  cat("  extended prevalence from age", top, "to", max(synth_pop$age), "\n")
}

# ------------------------------------------------------------------- join ---
synth_pop_wprob <- synth_pop %>%
  rename(sex = gender) %>%
  filter(age >= MIN_AGE, !is.na(SA2_MAIN16)) %>%
  dplyr::select(id, age, sex, SA2_MAIN16) %>%
  left_join(pivot_wider(prevalence, id_cols = c(age, sex, SA2_MAIN16),
                        names_from = cause, values_from = prob),
            by = c("age", "sex", "SA2_MAIN16"))

# must match keep_causes in the Rmd, minus all_cause_mortality
disease_cols <- c("all_cause_dementia", "bladder_cancer", "breast_cancer",
                  "colon_cancer", "copd", "coronary_heart_disease",
                  "depression", "diabetes", "endometrial_cancer",
                  "esophageal_cancer", "gastric_cardia_cancer",
                  "head_neck_cancer", "liver_cancer", "lung_cancer",
                  "myeloid_leukemia", "parkinson", "rectum_cancer", "stroke")

dcols   <- intersect(disease_cols, names(synth_pop_wprob))
absent  <- setdiff(disease_cols, dcols)
if (length(absent))
  warning("Absent from prevalence file: ", paste(absent, collapse = ", "),
          call. = FALSE)
n_na <- sum(is.na(synth_pop_wprob[dcols]))
if (n_na) cat("  ", n_na, "agent-disease cells have no probability (treated as 0)\n")

# --------------------------------------------------------------- allocate ---
set.seed(123)
status_cols <- paste0(dcols, "_status")

synth_pop_alloc <- synth_pop_wprob %>%
  mutate(across(all_of(dcols), ~ ifelse(runif(n()) < coalesce(.x, 0), 1L, 0L),
                .names = "{.col}_status"))

synth_pop_prev <- synth_pop_alloc %>%
  dplyr::select(id, age, sex, SA2_MAIN16, all_of(status_cols)) %>%
  mutate(sex = if_else(sex == 1, "male", "female"))
stopifnot(!anyNA(synth_pop_prev[status_cols]))

out_long <- synth_pop_prev %>%
  pivot_longer(all_of(status_cols), names_to = "diseases", values_to = "v") %>%
  filter(v == 1) %>%
  mutate(diseases = str_remove(diseases, "_status")) %>%
  group_by(id) %>%
  summarise(diseases = paste(diseases, collapse = " "), .groups = "drop")

cat("\nAgents with >=1 disease:", nrow(out_long),
    sprintf("(%.1f%%)\n", 100 * nrow(out_long) / nrow(synth_pop_prev)))

out_dir <- here(study_region, OUT_SUBDIR)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, paste0(scenario_name, "_prevalence_id_clean_",
                                      Sys.Date(), "_", study_region, ".csv"))
write_csv(out_long, out_path)
cat("Wrote:", out_path, "\n")

# =============================================================== validation ==
# Assigned vs original GBD is end-to-end: a gap can come from the interpolation,
# the SA2 disaggregation, the draw, or a real Melbourne-vs-Australia difference.
# GBD is national, so a flat offset is expected; drift with age is not.
# Plot 04 isolates the draw from everything upstream.

dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

age_band <- function(a) cut(a, c(seq(15, 95, 5), Inf), right = FALSE,
                            labels = c(paste0(seq(15, 90, 5), "-", seq(19, 94, 5)), "95+"))

# GBD label -> pipeline cause. GBD splits head/neck across four sites and
# reports colon+rectum together, so those are summed on the matching side.
gbd_map <- tribble(
  ~gbd_cause,                                ~cause,
  "Alzheimer's disease and other dementias", "all_cause_dementia",
  "Parkinson's disease",                     "parkinson",
  "Ischemic heart disease",                  "coronary_heart_disease",
  "Stroke",                                  "stroke",
  "Diabetes mellitus type 2",                "diabetes",
  "Chronic obstructive pulmonary disease",   "copd",
  "Depressive disorders",                    "depression",
  "Breast cancer",                           "breast_cancer",
  "Bladder cancer",                          "bladder_cancer",
  "Colon and rectum cancer",                 "colon_and_rectum_cancer",
  "Esophageal cancer",                       "esophageal_cancer",
  "Stomach cancer",                          "gastric_cardia_cancer",
  "Uterine cancer",                          "endometrial_cancer",
  "Liver cancer",                            "liver_cancer",
  "Tracheal, bronchus, and lung cancer",     "lung_cancer",
  "Larynx cancer",                           "head_neck_cancer",
  "Lip and oral cavity cancer",              "head_neck_cancer",
  "Nasopharynx cancer",                      "head_neck_cancer",
  "Other pharynx cancer",                    "head_neck_cancer",
  "Chronic myeloid leukemia",                "myeloid_leukemia")

COMBINE   <- list(colon_and_rectum_cancer = c("colon_cancer", "rectum_cancer"))
SEX_ONLY  <- list(breast_cancer = "female", endometrial_cancer = "female")
MIN_CASES <- 20   # below this a cell is noise (myeloid leukemia is 76 cases total)

agg <- synth_pop_alloc %>%
  mutate(agegroup = age_band(age), sex = if_else(sex == 1, "male", "female")) %>%
  filter(!is.na(agegroup)) %>%
  group_by(agegroup, sex) %>%
  summarise(n = n(),
            across(all_of(status_cols), ~ sum(.x),           .names = "o_{.col}"),
            across(all_of(dcols), ~ mean(coalesce(.x, 0)),   .names = "e_{.col}"),
            .groups = "drop")

assigned <- agg %>%
  dplyr::select(agegroup, sex, n, starts_with("o_")) %>%
  pivot_longer(starts_with("o_"), names_to = "cause", values_to = "cases") %>%
  mutate(cause = str_remove(str_remove(cause, "^o_"), "_status$"),
         assigned_100k = cases / n * 1e5)

expected <- agg %>%
  dplyr::select(agegroup, sex, starts_with("e_")) %>%
  pivot_longer(starts_with("e_"), names_to = "cause", values_to = "p_input") %>%
  mutate(cause = str_remove(cause, "^e_"))

gbd_dir <- file.path(here(study_region),
                     c("preprocessing/health/original/GBD",
                       "preprocessing/original/GBD"))
gbd_dir <- gbd_dir[dir.exists(gbd_dir)][1]

if (!is.na(gbd_dir)) {
  gbd <- list.files(gbd_dir, pattern = "prevalence|parkinson",
                    full.names = TRUE) %>%
    map_dfr(read_csv, show_col_types = FALSE, progress = FALSE) %>%
    filter(metric == "Rate", measure == "Prevalence", year == GBD_YEAR,
           age != "All ages", cause != "All causes") %>%
    mutate(agegroup = gsub(" years", "", age), sex = tolower(sex)) %>%
    inner_join(gbd_map, by = c("cause" = "gbd_cause")) %>%
    group_by(cause = cause.y, agegroup, sex) %>%
    summarise(gbd_100k = sum(val), .groups = "drop")
  
  for (g in names(COMBINE)) {
    mem <- COMBINE[[g]]
    if (all(mem %in% assigned$cause))
      assigned <- assigned %>%
        filter(!cause %in% mem) %>%
        bind_rows(assigned %>% filter(cause %in% mem) %>%
                    group_by(agegroup, sex, n) %>%
                    summarise(cases = sum(cases), .groups = "drop") %>%
                    mutate(cause = g, assigned_100k = cases / n * 1e5))
  }
  
  cmp <- assigned %>%
    inner_join(gbd, by = c("cause", "agegroup", "sex")) %>%
    mutate(ratio = assigned_100k / gbd_100k,
           reliable = gbd_100k / 1e5 * n >= MIN_CASES)
  for (cs in names(SEX_ONLY))
    cmp <- filter(cmp, !(cause == cs & sex != SEX_ONLY[[cs]]))
  
  cmp %>% filter(reliable) %>%
    group_by(cause, sex) %>%
    summarise(cells = n(), median_ratio = round(median(ratio), 3),
              p05 = round(quantile(ratio, .05), 3),
              p95 = round(quantile(ratio, .95), 3), .groups = "drop") %>%
    arrange(desc(abs(median_ratio - 1))) %>% print(n = Inf)
  
  write_csv(cmp, file.path(PLOT_DIR, "assigned_vs_gbd.csv"))
  
  for (sx in unique(cmp$sex)) {
    p <- cmp %>% filter(sex == sx) %>%
      pivot_longer(c(assigned_100k, gbd_100k), names_to = "type",
                   values_to = "rate") %>%
      mutate(type = recode(type, assigned_100k = "Assigned", gbd_100k = "GBD")) %>%
      ggplot(aes(agegroup, rate, fill = type)) +
      geom_col(position = "dodge", alpha = .85) +
      facet_wrap(~ cause, scales = "free_y", ncol = 4) +
      scale_fill_manual(values = c(Assigned = "blue", GBD = "red")) +
      labs(title = paste("Assigned vs GBD prevalence -", sx),
           x = "Age group", y = "Rate per 100,000", fill = NULL) +
      theme_bw(9) + theme(axis.text.x = element_text(angle = 60, hjust = 1),
                          legend.position = "bottom")
    ggsave(file.path(PLOT_DIR, paste0("01_assigned_vs_gbd_", sx, ".png")), p,
           width = 15, height = 12, dpi = 150)
  }
  
  p2 <- cmp %>% filter(reliable) %>%
    ggplot(aes(agegroup, ratio, colour = sex, group = sex)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_line() + geom_point(size = .9) +
    facet_wrap(~ cause, scales = "free_y", ncol = 4) +
    labs(title = "Assigned / GBD prevalence ratio",
         subtitle = paste0(">= ", MIN_CASES, " expected cases only; a flat ",
                           "offset is Melbourne vs Australia, drift with age is not"),
         x = "Age group", y = "Assigned / GBD") +
    theme_bw(9) + theme(axis.text.x = element_text(angle = 60, hjust = 1),
                        legend.position = "bottom")
  ggsave(file.path(PLOT_DIR, "02_ratio_vs_gbd.png"), p2,
         width = 15, height = 12, dpi = 150)
  
  p3 <- ggplot(cmp, aes(gbd_100k, assigned_100k, colour = sex)) +
    geom_abline(linetype = "dashed") + geom_point(alpha = .7, size = 1.4) +
    scale_x_log10() + scale_y_log10() +
    labs(title = "Assigned vs GBD, all causes and ages",
         x = "GBD per 100,000 (log)", y = "Assigned per 100,000 (log)") +
    theme_bw() + theme(legend.position = "bottom")
  ggsave(file.path(PLOT_DIR, "03_scatter_vs_gbd.png"), p3,
         width = 8, height = 7, dpi = 150)
} else {
  warning("GBD source files not found; skipping the GBD comparison.",
          call. = FALSE)
}

# 04: assigned vs the input probabilities of the same agents. Isolates the
# Bernoulli draw - these must agree to within binomial noise whatever else is
# wrong upstream.
alloc <- assigned %>%
  inner_join(expected, by = c("agegroup", "sex", "cause")) %>%
  mutate(input_100k = p_input * 1e5,
         se = sqrt(p_input * (1 - p_input) / n) * 1e5,
         lo = pmax(0, input_100k - 1.96 * se),
         hi = input_100k + 1.96 * se,
         ok = assigned_100k >= lo & assigned_100k <= hi)

cat("\nAllocation check:", sum(alloc$ok, na.rm = TRUE), "of",
    sum(!is.na(alloc$ok)), "cells within the 95% binomial interval\n")

p4 <- ggplot(alloc, aes(input_100k, assigned_100k, colour = sex)) +
  geom_abline(linetype = "dashed") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, alpha = .35) +
  geom_point(alpha = .7, size = 1.4) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Allocation check: assigned vs input probabilities",
       x = "Input per 100,000 (log)", y = "Assigned per 100,000 (log)") +
  theme_bw() + theme(legend.position = "bottom")
ggsave(file.path(PLOT_DIR, "04_allocation_check.png"), p4,
       width = 8, height = 7, dpi = 150)
write_csv(alloc, file.path(PLOT_DIR, "allocation_check.csv"))

cat("Plots ->", PLOT_DIR, "\n")