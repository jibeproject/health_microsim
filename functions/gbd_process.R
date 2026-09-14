gbd_process <- function(data) {
  
  gbdp <- data %>%
    filter(year == "2018") %>%                 # decreases file size
    filter(metric %in% "Rate") %>%
    select(-c(upper, lower)) %>%
    filter(!age %in% "All ages") %>%
    mutate(rate_1 = val / 100000) %>%
    # some ages have 'years' (eg 5-9 years), others don't (eg 80-84); strip ' years'
    mutate(age = gsub(" years", "", age)) %>%
    tidyr::extract(age, c("from_age", "to_age"), "(.+)-(.+)",
                   remove = FALSE, convert = TRUE) %>%
    mutate(from_age = case_when(age == "95+" ~ 95L,
                                age == "<5"  ~ 0L,
                                TRUE         ~ from_age),
           to_age   = case_when(age == "95+" ~ 99L,
                                age == "<5"  ~ 4L,
                                TRUE         ~ to_age),
           agediff  = to_age - from_age + 1,
           val1yr   = rate_1) %>%
    # we do not distribute within age groups (it's a rate); assume constant within band
    rename(agegroup = age)
  
  # ---- Pool head-and-neck sub-sites IF the data provides them separately ----
  # Newer GBD downloads provide a single "Head and neck cancer"; older ones
  # provide 4 sub-sites. Handle both: if the sub-sites are present, sum them
  # into "Head and neck cancer" so the downstream whitelist/mapping is uniform.
  hanc_subsites <- c("Larynx cancer", "Lip and oral cavity cancer",
                     "Nasopharynx cancer", "Other pharynx cancer")
  
  if (any(hanc_subsites %in% gbdp$cause)) {
    gbdp_hanc <- gbdp %>%
      filter(cause %in% hanc_subsites) %>%
      group_by(measure, location, sex, agegroup, from_age, to_age,
               metric, year, agediff) %>%
      summarise(val    = sum(val,    na.rm = TRUE),
                rate_1 = sum(rate_1, na.rm = TRUE),
                val1yr = sum(val1yr, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(cause = "Head and neck cancer")
    
    gbdp <- gbdp %>%
      filter(!cause %in% hanc_subsites) %>%
      bind_rows(gbdp_hanc)
  }
  
  # ---- Whitelist + rename to standardised JIBE disease names ----
  gbdp <- gbdp %>%
    filter(cause %in% c(
      "Stroke",                          
      "Ischemic heart disease",
      "Breast cancer",
      "Uterine cancer",
      "Tracheal, bronchus, and lung cancer",
      "Colon and rectum cancer",
      "Esophageal cancer",
      "Liver cancer",
      "Stomach cancer",
      "Chronic myeloid leukemia",
      "Multiple myeloma",
      "Head and neck cancer",                     # ← combined (pooled above if needed)
      "Bladder cancer",
      "Depressive disorders",
      "Alzheimer's disease and other dementias",
      "Diabetes mellitus type 2",
      "Chronic obstructive pulmonary disease",
      "Parkinson's disease"
    )) %>%
    mutate(cause = case_when(
      cause == "Ischemic heart disease"                  ~ "coronary_heart_disease",
      cause == "Uterine cancer"                          ~ "endometrial_cancer",
      cause == "Stomach cancer"                          ~ "gastric_cardia_cancer",
      cause == "Chronic myeloid leukemia"                ~ "myeloid_leukemia",
      cause == "Multiple myeloma"                        ~ "myeloma",
      cause == "Depressive disorders"                    ~ "depression",
      cause == "Alzheimer's disease and other dementias" ~ "all_cause_dementia",
      cause == "Diabetes mellitus type 2"                ~ "diabetes",
      cause == "Stroke"                                  ~ "stroke",            
      cause == "Head and neck cancer"                    ~ "head_neck_cancer",  
      cause == "Tracheal, bronchus, and lung cancer"     ~ "lung_cancer",
      cause == "Breast cancer"                           ~ "breast_cancer",
      cause == "Colon and rectum cancer"                 ~ "colon_cancer",
      cause == "Bladder cancer"                          ~ "bladder_cancer",
      cause == "Esophageal cancer"                       ~ "esophageal_cancer",
      cause == "Liver cancer"                            ~ "liver_cancer",
      cause == "Chronic obstructive pulmonary disease"   ~ "COPD",
      cause == "Parkinson's disease" ~ "parkinson's_disease",
      .default = cause
    )) %>%
    mutate(val = case_when(
      cause == "colon_cancer" ~ val * 2/3,    # colon-only (American Cancer Society)
      cause == "lung_cancer"  ~ val * 0.54,   # lung-only (England vs GBD totals)
      TRUE                    ~ val
    ),
    # keep rate_1 / val1yr consistent with the adjusted val for these two
    rate_1 = case_when(
      cause == "colon_cancer" ~ rate_1 * 2/3,
      cause == "lung_cancer"  ~ rate_1 * 0.54,
      TRUE                    ~ rate_1
    ),
    val1yr = case_when(
      cause == "colon_cancer" ~ val1yr * 2/3,
      cause == "lung_cancer"  ~ val1yr * 0.54,
      TRUE                    ~ val1yr
    ))
  
  return(gbdp)
}