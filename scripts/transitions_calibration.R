# ============================================================
# Adjust transition rates (PIF), build RR products on synth pop,
# calibrate rates (rate * rr_all), and plot OLD vs NEW with
# Rate / Rate (raw) / Rate (calibrated)
#

# ============================================================

library(tidyverse)
library(here)
library(readr)
library(stringr)

# ----------------------------
# Paths + constants
# ----------------------------
PATHS <- list(
  # Inputs
  pifs         = "manchester/health/processed/pifs_baseline_06122025.csv",
  lsoa_map     = here("manchester/health/original/ons/lsoa_2011_to_lsoa_2021.csv"),
  trans_raw    = "manchester/health/processed/health_transitions_manchester_raw.csv",
  inc_prev_dep = "manchester/health/original/gbd/inc_prev_depression.csv",
  trans_old    = "X:/HealthImpact/Data/Country/UK/JIBE_health_input_data/health_transitions_manchester_11112025.csv",
  
  synth_pop    = "X:/HealthImpact/Data/Country/UK/JIBE/manchester/scenOutput/061225_100%/base/microData/pp_rr_2021.csv",
  zones        = here("manchester/synPop/sp_2021/zoneSystem.csv"),
  prevalence   = "manchester/health/processed/health_transitions_manchester_prevalence.csv",
  
  # Outputs
  out_trans_local = "manchester/health/processed/health_transitions_manchester.csv",
  out_trans_y     = "X:/HealthImpact/Data/Country/UK/JIBE_health_input_data/health_transitions_manchester_05122025.csv",
  out_plots_base  = "images/manchester",
  out_plots_sub   = "rates_adjusted"  # all plots saved here (no disease subfolders)
)

GM_LADS <- c("Manchester","Salford","Bolton","Bury","Oldham",
             "Rochdale","Stockport","Tameside","Trafford","Wigan")

DISEASES <- c(
  "copd", "all_cause_dementia", "bladder_cancer", "breast_cancer",
  "colon_cancer", "coronary_heart_disease", "diabetes", "endometrial_cancer",
  "esophageal_cancer", "gastric_cardia_cancer", "head_neck_cancer", "liver_cancer",
  "lung_cancer", "myeloid_leukemia", "parkinson", "stroke",
  "all_cause_mortality", "depression"
)

OUTCOMES <- c(
  "all_cause_dementia","all_cause_mortality","copd","coronary_heart_disease",
  "lung_cancer","stroke","bladder_cancer","breast_cancer","colon_cancer",
  "depression","diabetes","endometrial_cancer","esophageal_cancer",
  "gastric_cardia_cancer","head_neck_cancer","liver_cancer","myeloid_leukemia","parkinson"
)

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
  # returns integer 1/2, else NA
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

make_age_name <- function(age) {
  case_when(
    age < 5 ~ "<5 years",
    age < 10 ~ "5-9 years",
    age < 15 ~ "10-14 years",
    age < 20 ~ "15-19 years",
    age < 25 ~ "20-24 years",
    age < 30 ~ "25-29 years",
    age < 35 ~ "30-34 years",
    age < 40 ~ "35-39 years",
    age < 45 ~ "40-44 years",
    age < 50 ~ "45-49 years",
    age < 55 ~ "50-54 years",
    age < 60 ~ "55-59 years",
    age < 65 ~ "60-64 years",
    age < 70 ~ "65-69 years",
    age < 75 ~ "70-74 years",
    age < 80 ~ "75-79 years",
    age < 85 ~ "80-84 years",
    age < 90 ~ "85-89 years",
    age < 95 ~ "90-94 years",
    TRUE ~ "95+ years"
  )
}

safe_name <- function(x) str_replace_all(x, "[^A-Za-z0-9]+", "_")

# ============================================================
# 1) Read inputs ONCE
# ============================================================
pifs <- read_csv(PATHS$pifs, show_col_types = FALSE) %>%
  repeat_95_100() %>%
  rename(cause = outcome) %>%
  mutate(cause = clean_cause(cause))

lsoa_11to21 <- read_csv(PATHS$lsoa_map, show_col_types = FALSE) %>%
  select(LSOA11CD, LSOA21CD, LAD22CD, LSOA21NM, LAD22NM)

transition_raw <- read_csv(PATHS$trans_raw, show_col_types = FALSE) %>%
  mutate(cause = clean_cause(cause)) %>%
  add_age_groups()

trans_old <- read_csv(PATHS$trans_old, show_col_types = FALSE) %>%
  mutate(cause = clean_cause(cause))

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
# 4) Incidence: expand LAD -> LSOA21 via mapping
# ============================================================
incidence <- transition_raw %>%
  filter(measure == "incidence") %>%
  left_join(lsoa_11to21, join_by(location_code == LAD22CD)) %>%
  transmute(
    lsoa21cd = LSOA21CD,
    LAD22NM,
    sex, age,
    cause,
    rate_raw = rate,
    sex_age_group
  )

# ============================================================
# 5) Apply PIFs => NEW adjusted transition_data
# ============================================================
options(scipen = 999)

transition_data <- bind_rows(
  incidence %>% select(lsoa21cd, LAD22NM, sex, age, cause, rate_raw, sex_age_group),
  mortality %>% select(lsoa21cd, LAD22NM, sex, age, cause, rate_raw, sex_age_group)
) %>% 
  filter(cause != "myeloma") %>%
  left_join(pifs %>% select(sex_age_group, cause, paf_combined_traditional),
            by = c("cause", "sex_age_group")) %>%
  mutate(
    paf_combined_traditional = if_else(age < 20, 0, paf_combined_traditional),
    rate = rate_raw * (1 - paf_combined_traditional)
  ) %>%
  mutate(
    # IMPORTANT: keep this numeric and consistent (fixes your earlier error)
    sex = sex_to_num(sex)
  ) %>%
  select(age, sex, lsoa21cd, LAD22NM, cause, rate, rate_raw)

stopifnot(sum(is.na(transition_data$rate)) == 0)

# ============================================================
# 6) Depression adjustment (NEW only)
# ============================================================
adj_dep <- read_csv(PATHS$inc_prev_dep, show_col_types = FALSE) %>%
  filter(metric_name == "Percent") %>%
  select(location_name, sex_name, age_name, measure_name, val) %>%
  pivot_wider(names_from = measure_name, values_from = val) %>%
  mutate(ratio = if_else(is.nan(Incidence / Prevalence), 0, Incidence / Prevalence)) %>%
  transmute(
    LAD22NM = location_name,
    sex = case_when(sex_name == "Male" ~ 1L, sex_name == "Female" ~ 2L, TRUE ~ NA_integer_),
    age_name,
    ratio
  )

depression <- transition_data %>%
  filter(cause == "depression") %>%
  mutate(age_name = make_age_name(age)) %>%
  left_join(adj_dep, by = c("age_name", "sex", "LAD22NM")) %>%
  mutate(rate = rate * ratio) %>%
  select(age, sex, lsoa21cd, LAD22NM, cause, rate, rate_raw)

transition_data <- transition_data %>%
  filter(cause != "depression") %>%
  bind_rows(depression) %>%
  filter(cause != "myeloma")

# ============================================================
# 7) Build synth_hd = per-person × outcome with rr_all
#    NOTE: disease RR affects ONLY all_cause_mortality
# ============================================================
build_synth_hd <- function() {
  synth_pop <- read_csv(PATHS$synth_pop, show_col_types = FALSE)
  
  zones <- read_csv(PATHS$zones, show_col_types = FALSE) %>%
    rename(zone = oaID)
  
  prevalence <- read_csv(PATHS$prevalence, show_col_types = FALSE) %>%
    mutate(
      prob = 1 - exp(-rate),
      sex  = sex_to_char(sex)
    ) %>%
    rename(location = location_code)
  
  synth_pop_geo <- synth_pop %>%
    left_join(zones, by = "zone") %>%
    rename(sex = gender) %>%
    mutate(sex = sex_to_char(sex)) %>%
    select(id, age, sex, ladcd, ladnm, lsoa21cd, starts_with("RR"), starts_with("rr"))
  
  prev_wide <- prevalence %>%
    select(age, sex, location, cause, prob) %>%
    pivot_wider(id_cols = c(age, sex, location),
                names_from = cause, values_from = prob)
  
  synth_pop_wprob <- synth_pop_geo %>%
    left_join(prev_wide, by = c("age", "sex", "ladcd" = "location"))
  
  # Allocate disease statuses (for multimorbidity RR -> all-cause mortality only)
  set.seed(123)
  
  disease_vars <- c(
    "copd","bladder_cancer","breast_cancer","colon_cancer","coronary_heart_disease",
    "diabetes","endometrial_cancer","esophageal_cancer","gastric_cardia_cancer",
    "head_neck_cancer","liver_cancer","lung_cancer","myeloid_leukemia","parkinson",
    "stroke","depression"
  )
  disease_vars <- intersect(disease_vars, names(synth_pop_wprob))
  
  synth_pop_prev <- synth_pop_wprob %>%
    mutate(
      across(all_of(disease_vars),
             ~ ifelse(runif(n()) < .x, 1L, 0L),
             .names = "{.col}_status")
    ) %>%
    mutate(
      endometrial_cancer_status = ifelse(sex == "male", 0L, endometrial_cancer_status),
      breast_cancer_status      = ifelse(sex == "male", 0L, breast_cancer_status)
    )
  
  status_cols <- grep("_status$", names(synth_pop_prev), value = TRUE)
  
  disease_rr_lookup <- synth_pop_prev %>%
    select(id, age, sex, ladcd, ladnm, lsoa21cd, all_of(status_cols)) %>%
    mutate(
      n_diseases = rowSums(across(all_of(status_cols)), na.rm = TRUE),
      rr_disease_all_cause_mortality = case_when(
        n_diseases == 0 ~ 1.00,
        n_diseases == 1 ~ 1.23,
        n_diseases == 2 ~ 1.62,
        n_diseases == 3 ~ 2.09,
        n_diseases == 4 ~ 2.77,
        n_diseases == 5 ~ 3.46,
        n_diseases >= 6 ~ 5.14
      )
    ) %>%
    select(id, age, sex, ladcd, ladnm, lsoa21cd, rr_disease_all_cause_mortality)
  
  # Keep exposure RR columns; standardise names
  sample_rr <- synth_pop_prev %>%
    select(id, age, sex, ladcd, ladnm, lsoa21cd, starts_with("RR"), starts_with("rr")) %>%
    rename_with(~ gsub("-", "_", tolower(.x)))
  
  # Convert rr_*_OUTCOME -> rr_*^OUTCOME using known outcomes at end of string
  outcome_rx <- paste0("(", paste(OUTCOMES, collapse = "|"), ")$")
  
  rr_cols <- names(sample_rr)[startsWith(names(sample_rr), "rr_")]
  rr_new  <- rr_cols %>%
    str_remove("^rr_") %>%
    (\(x) {
      out <- str_extract(x, outcome_rx)
      risk <- str_remove(x, paste0("_", outcome_rx))
      paste0("rr_", risk, "^", out)
    })()
  
  names(sample_rr)[match(rr_cols, names(sample_rr))] <- rr_new
  
  # Add disease RR ONLY for all-cause mortality, as rr_disease^all_cause_mortality
  sample_rr <- sample_rr %>%
    left_join(disease_rr_lookup, by = c("id","age","sex","ladcd","ladnm","lsoa21cd")) %>%
    mutate(`rr_disease^all_cause_mortality` = rr_disease_all_cause_mortality) %>%
    select(-rr_disease_all_cause_mortality)
  
  # Long
  data_long_rr <- sample_rr %>%
    select(id, age, sex, ladcd, ladnm, lsoa21cd, starts_with("rr_")) %>%
    pivot_longer(
      cols = starts_with("rr_"),
      names_to = c("risk_type", "outcome"),
      names_pattern = "rr_([^\\^]+)\\^(.*)",
      values_to = "relative_risk"
    ) %>%
    filter(!is.na(relative_risk), !is.na(outcome))
  
  # Wide => rr_all
  pif_ind <- data_long_rr %>%
    pivot_wider(
      id_cols = c(id, age, sex, ladcd, ladnm, lsoa21cd, outcome),
      names_from = risk_type,
      values_from = relative_risk,
      values_fill = list(relative_risk = 1)
    )
  
  # Ensure expected columns exist
  for (nm in c("air_pollution_pm25","air_pollution_no2","physical_activity","noise","ndvi","disease")) {
    if (!nm %in% names(pif_ind)) pif_ind[[nm]] <- 1
  }
  
  pif_ind %>%
    mutate(
      rr_all = air_pollution_pm25 * air_pollution_no2 * physical_activity * noise * ndvi * disease,
      sex = sex_to_num(sex),
      LAD22NM = str_squish(as.character(ladnm))
    )
}

synth_hd <- build_synth_hd()

# ============================================================
# 8) Calibrate rates at individual level: rate_calibrated = rate * rr_all
#    for BOTH OLD and NEW. Then aggregate to LAD×sex×age×cause.
# ============================================================

make_individual_rates <- function(trans_df, synth_hd, dataset_label) {
  trans_key <- trans_df %>%
    transmute(
      age = as.integer(age),
      sex = as.integer(sex),
      lsoa21cd = as.character(lsoa21cd),
      outcome = cause,
      rate = as.numeric(rate),
      rate_raw = as.numeric(rate_raw)
    )
  
  synth_hd %>%
    transmute(
      id, age = as.integer(age), sex = as.integer(sex),
      lsoa21cd = as.character(lsoa21cd),
      LAD22NM = str_squish(as.character(LAD22NM)),
      outcome,
      rr_all = as.numeric(rr_all)
    ) %>%
    left_join(trans_key, by = c("age","sex","lsoa21cd","outcome")) %>%
    mutate(
      dataset = dataset_label,
      rate_calibrated = rate * rr_all
    )
}

# Prepare OLD to have consistent types + names for joining
trans_old_clean <- trans_old %>%
  transmute(
    age = as.integer(age),
    sex = as.integer(sex),
    lsoa21cd = as.character(lsoa21cd),
    LAD22NM = str_squish(as.character(LAD22NM)),
    cause = clean_cause(cause),
    rate = as.numeric(rate),
    rate_raw = as.numeric(rate_raw)
  )

trans_new_clean <- transition_data %>%
  transmute(
    age = as.integer(age),
    sex = as.integer(sex),
    lsoa21cd = as.character(lsoa21cd),
    LAD22NM = str_squish(as.character(LAD22NM)),
    cause,
    rate = as.numeric(rate),
    rate_raw = as.numeric(rate_raw)
  )

ind_old <- make_individual_rates(trans_old_clean, synth_hd, "old")
ind_new <- make_individual_rates(trans_new_clean, synth_hd, "new")

# Aggregate to LAD × sex × age × outcome
lad_age_sex_outcome <- bind_rows(ind_old, ind_new) %>%
  group_by(dataset, LAD22NM, sex, age, outcome) %>%
  summarise(
    rate = mean(rate, na.rm = TRUE),
    rate_raw = mean(rate_raw, na.rm = TRUE),
    rate_calibrated = mean(rate_calibrated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(rate, rate_raw, rate_calibrated),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    measure = recode(
      measure,
      rate = "Rate",
      rate_raw = "Rate (raw)",
      rate_calibrated = "Rate (calibrated)"
    ),
    sex = factor(sex, levels = c(1,2), labels = c("Male","Female"))
  )

# ============================================================
# 9) Plotting: for each disease + LAD, lines for OLD/NEW and
#    linetype for Rate / Rate (raw) / Rate (calibrated)
# ============================================================

make_lad_rate_plots_measures_newonly <- function(
    lad_age_sex_outcome_long,
    disease,
    lad = NULL,
    out_base = PATHS$out_plots_base,
    out_subfolder = PATHS$out_plots_sub,
    width = 9,
    height = 8,
    dpi = 220
) {
  stopifnot(is.character(disease), length(disease) == 1)
  
  out_dir <- file.path(out_base, out_subfolder)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  df <- lad_age_sex_outcome_long %>%
    dplyr::filter(outcome == disease, dataset == "new") %>%   # NEW only
    dplyr::mutate(
      measure = factor(
        measure,
        levels = c("Rate", "Rate (calibrated)", "Rate (raw)")
      )
    )
  
  if (nrow(df) == 0) return(invisible(NULL))
  
  lad_list <- if (is.null(lad)) sort(unique(df$LAD22NM)) else stringr::str_squish(lad)
  
  plot_one_lad <- function(lad_name) {
    ggplot(
      df %>% dplyr::filter(LAD22NM == lad_name),
      aes(
        x = age, y = value,
        linetype = measure,
        linewidth = measure,
        alpha = measure
      )
    ) +
      geom_line(na.rm = TRUE) +
      facet_wrap(~ sex, ncol = 1, scales = "free_y") +
      scale_linetype_manual(values = c(
        "Rate" = "solid",
        "Rate (calibrated)" = "longdash",
        "Rate (raw)" = "dotdash"
      )) +
      scale_linewidth_manual(values = c(
        "Rate" = 0.9,
        "Rate (calibrated)" = 1.4,
        "Rate (raw)" = 0.6
      )) +
      scale_alpha_manual(values = c(
        "Rate" = 1,
        "Rate (calibrated)" = 1,
        "Rate (raw)" = 0.65
      )) +
      labs(
        title = paste0(lad_name, " — ", disease, " (NEW only)"),
        x = "Age (single year)",
        y = "Rate",
        linetype = "Measure",
        linewidth = "Measure",
        alpha = "Measure"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        plot.background   = element_rect(fill = "white", colour = NA),
        panel.background  = element_rect(fill = "white", colour = NA),
        legend.background = element_rect(fill = "white", colour = NA),
        legend.key        = element_rect(fill = "white", colour = NA)
      )
  }
  
  saved <- character(0)
  for (lad_name in lad_list) {
    if (!any(df$LAD22NM == lad_name)) next
    
    fn <- file.path(
      out_dir,
      paste0("rates_NEW_", safe_name(disease), "_", safe_name(lad_name), ".png")
    )
    
    ggsave(fn, plot_one_lad(lad_name), width = width, height = height, dpi = dpi, bg = "white")
    saved <- c(saved, fn)
  }
  
  invisible(list(out_dir = out_dir, n_saved = length(saved), files = saved))
}



# Run all diseases, all LADs, save in ONE folder
dir.create(PATHS$out_plots_base, recursive = TRUE, showWarnings = FALSE)

purrr::walk(DISEASES, \(d) {
  message("Plotting (NEW only): ", d)
  make_lad_rate_plots_measures_newonly(lad_age_sex_outcome, disease = d)
})


# ============================================================
# 10) Save NEW transition data once
# ============================================================

transition_data %>% filter(cause == "all_cause_mortality") %>%
  dplyr::summarise(n_lsoa = dplyr::n_distinct(lsoa21cd))

transition_data |> group_by(cause) |> reframe(transition_data = mean(rate))


write_csv(transition_data, PATHS$out_trans_local)
write_csv(transition_data, PATHS$out_trans_y)

# data_check <- read_csv(PATHS$out_trans_y)