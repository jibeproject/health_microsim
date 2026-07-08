### Function to calculate PIFs (scenario-specific, using multi-disease RR)

library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(data.table)
library(here)

calculate_pif <- function(synth_pop) {
  
  # ------------------------------------------------------------
  # 1. Read synthetic population and lookup tables
  # ------------------------------------------------------------
  
  # synth_pop <- read_csv(
  #   "Y:/HealthImpact/Data/Country/UK/JIBE/manchester/scenOutput/061225_100%/base/microData/pp_rr_2021.csv",
  #   show_col_types = FALSE
  # )
  
  zones <- read_csv(
    here("manchester/synPop/sp_2021/zoneSystem.csv"),
    show_col_types = FALSE
  ) %>%
    rename(zone = oaID)
  
  # Disease prevalence data
  prevalence <- read.csv("manchester/health/processed/health_transitions_manchester_prevalence.csv") %>%
    mutate(
      prob = 1 - exp(-rate),                    # Convert rates to probabilities
      sex  = case_when(
        sex == 1 ~ "male",
        sex == 2 ~ "female",
        TRUE     ~ as.character(sex)
      )
    ) %>%
    rename(location = location_code)
  
  # ------------------------------------------------------------
  # 2. Attach geography and prevalence to synthetic population
  # ------------------------------------------------------------
  
  synth_pop <- synth_pop %>%
    left_join(zones, by = c("zone" = "zone"))
  
  synth_pop_wprob <- synth_pop %>%
    rename(sex = gender) %>%
    mutate(
      sex = case_when(
        sex == 1 ~ "male",
        sex == 2 ~ "female",
        TRUE     ~ as.character(sex)
      )
    ) %>%
    select(id, age, sex, ladcd, ladnm, starts_with("rr")) %>%
    rownames_to_column() %>%
    left_join(
      prevalence %>%
        pivot_wider(
          id_cols    = c(age, sex, location),
          names_from = cause,
          values_from = prob
        ),
      by = c("age", "sex", "ladcd" = "location")
    )
  
  # ------------------------------------------------------------
  # 3. Allocate disease statuses (0/1) and clean sex-specific cancers
  # ------------------------------------------------------------
  
  set.seed(123)
  
  allocate_disease <- function(df) {
    df %>%
      mutate(
        across(
          copd:stroke,
          ~ ifelse(runif(n()) < ., 1, 0),
          .names = "{.col}_status"
        )
      )
  }
  
  synth_pop_prev <- allocate_disease(synth_pop_wprob) %>%
    select(
      id, age, sex, ladcd, ladnm,
      ends_with("_status"),
      starts_with("RR")
    ) %>%
    mutate(
      endometrial_cancer_status = ifelse(sex == "male", 0, endometrial_cancer_status),
      breast_cancer_status      = ifelse(sex == "male", 0, breast_cancer_status)
    )
  
  # Use full sample (no sub-sampling here)
  sample <- synth_pop_prev
  
  # ------------------------------------------------------------
  # 4. NEW: assign disease_rr based on number of diseases
  # ------------------------------------------------------------
  #  1 disease  → 1.23
  #  2 diseases → 1.62
  #  3 diseases → 2.09
  #  4 diseases → 2.77
  #  5 diseases → 3.46
  #  6+         → 5.14   (for "5 or more")
  
  disease_rr_lookup <- sample %>%
    select(id, age, sex, ends_with("_status")) %>%
    mutate(
      n_diseases = rowSums(across(ends_with("_status")), na.rm = TRUE),
      `rr_disease^all_cause_mortality` = case_when(
        n_diseases == 0 ~ 1.00,
        n_diseases == 1 ~ 1.23,
        n_diseases == 2 ~ 1.62,
        n_diseases == 3 ~ 2.09,
        n_diseases == 4 ~ 2.77,
        n_diseases == 5 ~ 3.46,
        n_diseases >= 6 ~ 5.14
      )
    ) %>%
    select(id, age, sex, `rr_disease^all_cause_mortality`)
  
  # ------------------------------------------------------------
  # Prepare exposure RR columns for PIFs
  # ------------------------------------------------------------
  
  # Drop disease status columns; keep id/age/sex + RR exposure columns
  sample <- sample %>%
    select(id, age, sex, starts_with("RR"))
  
  # Clean column names (lowercase, replace hyphens)
  colnames(sample) <- gsub("-", "_", tolower(colnames(sample)))
  
  # Define exposure keywords
  keywords <- c(
    "air_pollution_no2",
    "air_pollution_pm25",
    "physical_activity",
    "noise",
    "ndvi"
  )
  
  # Convert names like rr_air_pollution_pm25_all_cause_mortality
  # into rr_air_pollution_pm25^all_cause_mortality
  pattern       <- paste0("(", paste0(keywords, collapse = "|"), ")_(.*)")
  new_col_names <- gsub(pattern, "\\1^\\2", colnames(sample))
  colnames(sample) <- new_col_names
  
  # Attach per-person disease_rr
  sample <- sample %>%
    left_join(disease_rr_lookup, by = c("id", "age", "sex")) %>%
    # Keep only id/age/sex, disease_rr, and RR columns
    select(id, age, sex, starts_with("rr_"))
  
  # ------------------------------------------------------------
  # Long → wide RR for PIF calculation
  # ------------------------------------------------------------
  
  data_long_rr <- sample %>%
    pivot_longer(
      cols = starts_with("rr_"),
      names_to   = c("risk_type", "outcome"),
      names_pattern = "rr_([^\\^]+)\\^(.*)",
      values_to  = "relative_risk"
    ) %>%
    drop_na()
  
  pif_ind <- data_long_rr %>%
    pivot_wider(
      names_from  = risk_type,
      values_from = relative_risk,
      values_fill = list(relative_risk = 1)
    ) %>%
    rename(
      pm25 = air_pollution_pm25,
      no2  = air_pollution_no2
    ) 
  
  # ------------------------------------------------------------
  # Aggregate to groups and compute PIFs
  # ------------------------------------------------------------
  
  setDT(pif_ind)
  
  # Create age groups and sex_age_group labels
  pif_ind[, `:=`(
    age_group     = sprintf("[%d,%d)", 5 * floor(age / 5), 5 * (floor(age / 5) + 1)),
    sex_age_group = sprintf("%s_[%d,%d)", sex, 5 * floor(age / 5), 5 * (floor(age / 5) + 1))
  )]
  
  # Ensure disease_rr is always defined
  pif_ind[is.na(disease), disease := 1]
  
  options(scipen = 999)
  
  # Group-level sums by sex_age_group and outcome
  pif_group <- pif_ind[, .(
    total_pop         = .N,
    sum_rr_pa         = sum(physical_activity),
    sum_rr_ndvi       = sum(ndvi),
    sum_rr_pm25       = sum(pm25),
    sum_rr_no2        = sum(no2),
    sum_rr_noise      = sum(noise),
    sum_rr_disease    = sum(disease),
    sum_rr_individual = sum(
      physical_activity * ndvi * pm25 * no2 * noise * disease
    )
  ), by = .(sex_age_group, outcome)]
  
  # PIFs by exposure and disease
  pif_group[, `:=`(
    pif_pa       = 1 - (total_pop / sum_rr_pa),
    pif_ndvi     = 1 - (total_pop / sum_rr_ndvi),
    pif_pm25     = 1 - (total_pop / sum_rr_pm25),
    pif_no2      = 1 - (total_pop / sum_rr_no2),
    pif_noise    = 1 - (total_pop / sum_rr_noise),
    pif_disease  = 1 - (total_pop / sum_rr_disease),
    paf_combined_traditional = 1 - (total_pop / sum_rr_individual)
  )]
  
  # Correct combined PAF (assuming independence of components)
  pif_group[, paf_combined_correct := 1 - (
    (1 - pif_pa) *
      (1 - pif_pm25) *
      (1 - pif_no2) *
      (1 - pif_noise) *
      (1 - pif_ndvi) *
      (1 - pif_disease)
  )]
  
  return(pif_group)
}
