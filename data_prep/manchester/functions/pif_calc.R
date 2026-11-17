### Function to calculate PIFs (scenario-specific, using multi-disease RR)
### + diabetes-conditional RR for coronary heart disease and stroke
###
### Fixes vs previous version:
###  - prevalence joined on LSOA11 (zones carries both LSOA11CD and lsoa21cd),
###    not on ladcd (which silently produced all-NA prevalence -> all disease
###    statuses 0 -> multimorbidity RR collapsed to 1)
###  - allocate_disease() uses an explicit disease list + intersect() instead of
###    the fragile positional range copd:stroke
###  - adds diabetes-conditional RR for coronary_heart_disease and stroke,
###    mirroring the multi-disease RR for all_cause_mortality
###  - PIF formula unchanged: explicit summing form 1 - (N / sum_rr)

library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(data.table)
library(here)
library(stringr)

calculate_pif <- function(synth_pop) {
  
  # ------------------------------------------------------------
  # 1. Read lookup tables
  # ------------------------------------------------------------
  # synth_pop <-  read_csv("X:/HealthImpact/Data/Country/UK/JIBE/manchester/scenOutput/nonTransportPA_050526/RR/reference/pp_rr_2021.csv")
  # zones carries oaID (-> zone), lsoa21cd, ladcd, ladnm, AND LSOA11CD.
  # LSOA11CD is the key that matches the prevalence file's location_code.
  zones <- read_csv(
    here("manchester/synPop/sp_2021/zoneSystem.csv"),
    show_col_types = FALSE
  ) %>%
    rename(zone = oaID)
  
  # Disease prevalence (LSOA11-level; location_code is E01... LSOA11 codes)
  prevalence <- read.csv(
    here("manchester/health/processed/health_transitions_manchester_prevalence.csv")
  ) %>%
    mutate(
      prob = 1 - exp(-rate),                       # rates -> probabilities
      sex  = case_when(
        sex == 1 ~ "male",
        sex == 2 ~ "female",
        TRUE     ~ as.character(sex)
      ),
      # normalise cause names so pivoted column names are predictable
      # (curly apostrophe -> straight; head_and_neck -> head_neck)
      cause = str_replace_all(cause, "\u2019", "'"),
      cause = str_replace_all(cause, "head_and_neck_cancer", "head_neck_cancer")
    ) %>%
    rename(location = location_code)
  
  # ------------------------------------------------------------
  # 2. Attach geography (incl. LSOA11CD) and prevalence to synth pop
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
      ),
      # IMD decile (1..10) from the person's zone; carried through so PIFs
      # can be stratified by deprivation. NA imd kept out of the stratified
      # table (handled via the pooled fallback in calibration).
      imd_decile = suppressWarnings(as.integer(imd10))
    ) %>%
    # KEEP LSOA11CD (and lsoa21cd) so we can join prevalence correctly
    select(id, age, sex, ladcd, ladnm, lsoa21cd, LSOA11CD, imd_decile,
           starts_with("rr")) %>%
    rownames_to_column() %>%
    left_join(
      prevalence %>%
        pivot_wider(
          id_cols     = c(age, sex, location),
          names_from  = cause,
          values_from = prob
        ),
      by = c("age", "sex", "LSOA11CD" = "location")   # <- THE FIX (was ladcd)
    )
  
  # ------------------------------------------------------------
  # 3. Allocate disease statuses (0/1) and clean sex-specific cancers
  # ------------------------------------------------------------
  
  set.seed(123)
  
  # Explicit disease list (robust to column order / missing causes).
  # Excludes all_cause_mortality (not a disease state) and myeloma
  # (dropped downstream). diabetes IS included -> needed for the
  # diabetes-conditional CHD/stroke RR below.
  disease_vars_all <- c(
    "copd", "all_cause_dementia", "bladder_cancer", "breast_cancer",
    "colon_cancer", "coronary_heart_disease", "depression", "diabetes",
    "endometrial_cancer", "esophageal_cancer", "gastric_cardia_cancer",
    "head_neck_cancer", "liver_cancer", "lung_cancer", "myeloid_leukemia",
    "parkinson's_disease", "stroke"
  )
  
  allocate_disease <- function(df) {
    dv <- intersect(disease_vars_all, names(df))   # only those present
    df %>%
      mutate(
        across(
          all_of(dv),
          ~ ifelse(runif(n()) < .x, 1L, 0L),
          .names = "{.col}_status"
        )
      )
  }
  
  synth_pop_prev <- allocate_disease(synth_pop_wprob) %>%
    select(
      id, age, sex, ladcd, ladnm, lsoa21cd, LSOA11CD, imd_decile,
      ends_with("_status"),
      starts_with("RR")
    ) %>%
    mutate(
      endometrial_cancer_status = ifelse(sex == "male", 0L, endometrial_cancer_status),
      breast_cancer_status      = ifelse(sex == "male", 0L, breast_cancer_status)
    )
  
  # Use full sample (no sub-sampling here)
  sample <- synth_pop_prev
  
  # ------------------------------------------------------------
  # 4. Multi-disease RR -> all_cause_mortality
  #    (corresponds to adjustments applied in microsim_model.R)
  #    1 disease 1.23 | 2 1.62 | 3 2.09 | 4 2.77 | 5 3.46 | 6+ 5.14
  # ------------------------------------------------------------
  
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
  # 4b. NEW: diabetes-conditional RR for CHD and stroke
  #     (corresponds to adjustments applied in microsim_model.R)
  #     CHD:    male 2.16, female 2.82
  #     Stroke: male 1.83, female 2.28
  # ------------------------------------------------------------
  
  diabetes_rr_lookup <- sample %>%
    select(id, age, sex, diabetes_status) %>%
    mutate(
      `rr_diabetes^coronary_heart_disease` = case_when(
        is.na(diabetes_status) | diabetes_status == 0 ~ 1.00,
        diabetes_status == 1 & sex == "male"          ~ 2.16,
        diabetes_status == 1 & sex == "female"        ~ 2.82
      ),
      `rr_diabetes^stroke` = case_when(
        is.na(diabetes_status) | diabetes_status == 0 ~ 1.00,
        diabetes_status == 1 & sex == "male"          ~ 1.83,
        diabetes_status == 1 & sex == "female"        ~ 2.28
      )
    ) %>%
    select(id, age, sex,
           `rr_diabetes^coronary_heart_disease`,
           `rr_diabetes^stroke`)
  
  # ------------------------------------------------------------
  # Prepare exposure RR columns for PIFs
  # ------------------------------------------------------------
  
  # Drop disease status columns; keep id/age/sex + imd_decile + RR exposure cols
  sample <- sample %>%
    select(id, age, sex, imd_decile, starts_with("RR"))
  
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
  
  # Attach per-person disease_rr AND diabetes_rr (keep imd_decile)
  sample <- sample %>%
    left_join(disease_rr_lookup,  by = c("id", "age", "sex")) %>%
    left_join(diabetes_rr_lookup, by = c("id", "age", "sex")) %>%
    select(id, age, sex, imd_decile, starts_with("rr_"))
  
  # ------------------------------------------------------------
  # Long -> wide RR for PIF calculation
  # ------------------------------------------------------------
  
  data_long_rr <- sample %>%
    pivot_longer(
      cols          = starts_with("rr_"),
      names_to      = c("risk_type", "outcome"),
      names_pattern = "rr_([^\\^]+)\\^(.*)",
      values_to     = "relative_risk"
    ) %>%
    # scope drop_na to the RR fields only: a missing imd_decile must NOT
    # remove the person from the pooled (non-stratified) PIF population
    drop_na(risk_type, outcome, relative_risk)
  
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
  
  # Plain 5-year age groups + sex_age_group labels
  pif_ind[, `:=`(
    age_group     = sprintf("[%d,%d)", 5 * floor(age / 5), 5 * (floor(age / 5) + 1)),
    sex_age_group = sprintf("%s_[%d,%d)", sex, 5 * floor(age / 5), 5 * (floor(age / 5) + 1)),
    imd_decile    = suppressWarnings(as.integer(imd_decile))
  )]
  
  # Ensure disease / diabetes multipliers are always defined
  if (!"disease"  %in% names(pif_ind)) pif_ind[, disease  := 1]
  if (!"diabetes" %in% names(pif_ind)) pif_ind[, diabetes := 1]
  pif_ind[is.na(disease),  disease  := 1]
  pif_ind[is.na(diabetes), diabetes := 1]
  
  options(scipen = 999)
  
  # ------------------------------------------------------------
  # Aggregate to groups and compute PIFs.
  #   - IMD-stratified : sex_age_group x imd_decile x outcome
  #   - Pooled fallback: sex_age_group x outcome (imd_decile = NA)
  # The PIF math is unchanged; it is just applied within each IMD
  # decile as well as pooled. The pooled rows let calibration fall
  # back to the non-stratified PIF for any (age,sex,imd,outcome)
  # cell that is too sparse or absent after the 10-way split,
  # instead of zeroing the attributable burden there.
  # ------------------------------------------------------------
  paf_cols <- c("pif_pa","pif_ndvi","pif_pm25","pif_no2","pif_noise",
                "pif_disease","pif_diabetes",
                "paf_combined_traditional","paf_combined_correct")

  .agg_pif <- function(dt, by_cols) {
    g <- dt[, .(
      total_pop         = .N,
      sum_rr_pa         = sum(physical_activity),
      sum_rr_ndvi       = sum(ndvi),
      sum_rr_pm25       = sum(pm25),
      sum_rr_no2        = sum(no2),
      sum_rr_noise      = sum(noise),
      sum_rr_disease    = sum(disease),
      sum_rr_diabetes   = sum(diabetes),
      sum_rr_individual = sum(
        physical_activity * ndvi * pm25 * no2 * noise * disease * diabetes
      )
    ), by = by_cols]

    # PIFs by exposure, disease and diabetes (explicit summing form)
    g[, `:=`(
      pif_pa       = 1 - (total_pop / sum_rr_pa),
      pif_ndvi     = 1 - (total_pop / sum_rr_ndvi),
      pif_pm25     = 1 - (total_pop / sum_rr_pm25),
      pif_no2      = 1 - (total_pop / sum_rr_no2),
      pif_noise    = 1 - (total_pop / sum_rr_noise),
      pif_disease  = 1 - (total_pop / sum_rr_disease),
      pif_diabetes = 1 - (total_pop / sum_rr_diabetes),
      paf_combined_traditional = 1 - (total_pop / sum_rr_individual)
    )]

    # Correct combined PAF (assuming independence of components)
    g[, paf_combined_correct := 1 - (
      (1 - pif_pa) *
        (1 - pif_pm25) *
        (1 - pif_no2) *
        (1 - pif_noise) *
        (1 - pif_ndvi) *
        (1 - pif_disease) *
        (1 - pif_diabetes)
    )]

    # Guard against -Inf / NaN where a (sparser) group's sum_rr is 0
    for (cc in paf_cols) {
      set(g, which(!is.finite(g[[cc]])), cc, 0)
    }
    g[]
  }

  # Persons with missing IMD are excluded from the stratified table
  # but kept in the pooled table so their burden is still calibrated.
  pif_ind_imd <- pif_ind[!is.na(imd_decile)]

  pif_group_imd <- .agg_pif(pif_ind_imd,
                            c("sex_age_group", "imd_decile", "outcome"))
  pif_group_all <- .agg_pif(pif_ind,
                            c("sex_age_group", "outcome"))
  pif_group_all[, imd_decile := NA_integer_]

  # One table: imd_decile 1..10 = stratified; imd_decile NA = pooled
  pif_group <- rbind(pif_group_imd, pif_group_all,
                     use.names = TRUE, fill = TRUE)

  return(pif_group[])
}
