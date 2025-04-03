# load libraries
require("tidyverse")
require("data.table")
require("drpa")
require("ithimr")
require("here")

# set seed - as current year
set.seed(2025)

for (scen in c("green", "both", "base", "safestreet"))
  {
  # Define name of the scenario
  SCEN_SHORT_NAME <<- scen
  
  #SCEN_SHORT_NAME <<- 'safestreet'
  
  # load pm2.5_dose_response
  source(here("scripts/pm2.5_dose_response.R"))
  # Read all_cause_no2 DR
  all_cause_no2 <- read_csv(here("jibe health/health1/all_cause_no.csv")) |> 
    rename(RR = rr)
  
  # all_cause_no2 <- read_csv("health/all_cause_no.csv") |> 
  #   rename(RR = rr)
  
  # Load NO2_dose_response function
  source(here("scripts/NO2_dose_response.R"))
  
  # Load NDVI_dose_response function
  source(here("scripts/ndvi_dose_response.R"))
  
  # Load noise_dose_response functiom
  source(here("scripts/noise_dose_response.R"))
  
  list_of_files <- list.files(
    path = "jibe health/health1/",
    recursive = TRUE, pattern = "(pm|noise|ndvi)\\.csv$",
    full.names = TRUE
  )
  
  # list_of_files <- list.files(
  #   path = "health/",
  #   recursive = TRUE, pattern = "(pm|noise|ndvi)\\.csv$",
  #   full.names = TRUE
  # )
  
  # Make them available as global datasets
  for (i in 1:length(list_of_files)) {
    assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
           read.csv(list_of_files[[i]]),
           pos = 1
    )
  }
  
  # Fix object name
  diabetes_noise <- diabates_noise
  rm(diabates_noise)
  
  # Read the default discease outcomes table from the ITHIM package
  DISEASE_INVENTORY <<- read_csv("jibe health/health1/disease_outcomes_lookup.csv") |> 
    #DISEASE_INVENTORY <- read_csv("health/disease_outcomes_lookup.csv") |>  
    mutate(pa_acronym = acronym_inJava) |> mutate(pa_acronym = str_replace_all(pa_acronym, "_", "-"),
                                                  outcome = str_replace_all(outcome, "_", "-")) |> 
    mutate(pa_acronym = case_when(pa_acronym == "parkinson" ~ "parkinson's-disease",
                                  pa_acronym == "head-neck-cancer" ~ "head-and-neck-cancer",
                                  TRUE ~ pa_acronym))
  
  # Read per person exposure
  if (SCEN_SHORT_NAME == "base"){
    ppdf <- read_csv(here("jibe health/1_reference/health/04_exposure_and_rr/pp_exposure_2021.csv"))
  }else if (SCEN_SHORT_NAME == "safestreet"){
    ppdf <- read_csv(here("jibe health/2_safestreet/health/04_exposure_and_rr/pp_exposure_2021.csv"))
  }else if (SCEN_SHORT_NAME == "green"){
    ppdf <- read_csv(here("jibe health/3_green/health/04_exposure_and_rr/pp_exposure_2021.csv"))
  }else if (SCEN_SHORT_NAME == "both"){
    ppdf <- read_csv(here("jibe health/4_both/health/04_exposure_and_rr/pp_exposure_2021.csv"))
  }
  
  # ppdf <- read_csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_exposure_2021.csv")
  
  # Recalculate base_mmet and rename pm_conc_base variable (although it is exposure but ITHIM pacakge expects this name)
  ppdf <- ppdf |> 
    mutate(!!paste0(SCEN_SHORT_NAME, "_mmet") := mmetHr_walk + mmetHr_cycle + mmetHr_otherSport)
  
  # Calculate PA RRs
  pa <- ithimr::gen_pa_rr(mmets_pp = ppdf, conf_int = F)
  
  # Rename parkinson's disease to parkinson
  colnames(pa) <- gsub("parkinson's_disease", "parkinson", colnames(pa))
  
  
  # Calculate NO2 RRs
  assign(paste0("RR_no2_", SCEN_SHORT_NAME, "_all_cause_mortality"), NO2_dose_response(cause = "all_cause_no2", dose = ppdf |> 
                                                                                         arrange(id) |> 
                                                                                         dplyr::select(exposure_normalised_no2) |> 
                                                                                         pull()) |> 
           rename(!!paste0("RR_no2_", SCEN_SHORT_NAME, "_all_cause_mortality") := rr))
  
  # Calculate all_cause_mortality RRs for pm2.5
  assign(paste0("RR_pm_", SCEN_SHORT_NAME, "_all_cause_mortality"), pm2.5_dose_response(cause = "all_cause_pm", dose = ppdf |> 
                                                                                          arrange(id) |> 
                                                                                          dplyr::select(exposure_normalised_pm25) |> 
                                                                                          pull()) |> 
           rename(!!paste0("RR_pm_", SCEN_SHORT_NAME, "_all_cause_mortality") := rr))
  
  # Calculate copd RRs for pm2.5
  assign(paste0("RR_pm_", SCEN_SHORT_NAME, "_copd"), pm2.5_dose_response(cause = "copd_pm", dose = ppdf |> 
                                                                           arrange(id) |> 
                                                                           dplyr::select(exposure_normalised_pm25) |> 
                                                                           pull()) |> 
           rename(!!paste0("RR_pm_", SCEN_SHORT_NAME, "_copd") := rr))
  
  
  # Calculate ihd (or chd) RRs for pm2.5
  assign(paste0("RR_pm_", SCEN_SHORT_NAME, "_coronary_heart_disease"), pm2.5_dose_response(cause = "ihd_pm", dose = ppdf |> 
                                                                                             arrange(id) |> 
                                                                                             dplyr::select(exposure_normalised_pm25) |> 
                                                                                             pull()) |> 
           rename(!!paste0("RR_pm_", SCEN_SHORT_NAME, "_coronary_heart_disease") := rr))
  
  # Calculate lung cancer RRs for pm2.5
  assign(paste0("RR_pm_", SCEN_SHORT_NAME, "_lung_cancer"), pm2.5_dose_response(cause = "lc_pm", dose = ppdf |> 
                                                  arrange(id) |> 
                                                  dplyr::select(exposure_normalised_pm25) |> 
                                                  pull()) |> 
    rename(!!paste0("RR_pm_", SCEN_SHORT_NAME, "_lung_cancer") := rr))
  
  # Calculate pm2.5 ERF for stroke
  assign(paste0("RR_pm_", SCEN_SHORT_NAME, "_stroke"), pm2.5_dose_response(cause = "stroke_pm", dose = ppdf |> 
                                             arrange(id) |> 
                                             dplyr::select(exposure_normalised_pm25) |> 
                                             pull()) |> 
    rename(!!paste0("RR_pm_", SCEN_SHORT_NAME, "_stroke_cancer") := rr))
  
  # Create a named vector for mapping
  outcome_mapping <- c(
    "all_cause_noise" = paste0("RR_noise_", SCEN_SHORT_NAME, "_all_cause_mortality"),
    "ihd_noise" = paste0("RR_noise_", SCEN_SHORT_NAME, "_coronary_heart_disease"),
    "diabetes_noise" = paste0("RR_noise_", SCEN_SHORT_NAME, "_diabetes"),
    "stroke_noise" = paste0("RR_noise_", SCEN_SHORT_NAME, "_stroke")
  )
  
  # Calculate and assign in one step
  list2env(
    setNames(
      lapply(names(outcome_mapping), function(noise_dis_outcome) {
        result <- noise_dose_response(
          cause = noise_dis_outcome,
          dose = ppdf |> arrange(id) |> 
            dplyr::select(exposure_normalised_noise_Lden) |> 
            pull()
        )
        # Rename the column to match the outcome_mapping name
        colnames(result) <- outcome_mapping[noise_dis_outcome]
        result
      }),
      outcome_mapping
    ),
    envir = .GlobalEnv
  )
  
  
  # Create a named vector for mapping
  outcome_mapping <- c(
    "all_cause_ndvi" = paste0("RR_ndvi_", SCEN_SHORT_NAME, "_all_cause_mortality"),
    "diabetes_ndvi" = paste0("RR_ndvi_", SCEN_SHORT_NAME, "_diabetes"),
    "stroke_ndvi" = paste0("RR_ndvi_", SCEN_SHORT_NAME, "_stroke")
  )
  
  # Calculate and assign in one step
  list2env(
    setNames(
      lapply(names(outcome_mapping), function(ndvi_dis_outcome) {
        result <- NDVI_dose_response(
          cause = ndvi_dis_outcome,
          dose = ppdf |> arrange(id) |> 
            dplyr::select(exposure_normalised_ndvi) |> 
            pull()
        )
        # Rename the column to match the outcome_mapping name
        colnames(result) <- outcome_mapping[ndvi_dis_outcome]
        result
      }),
      outcome_mapping
    ),
    envir = .GlobalEnv
  )
  
  
  # Assign PA columns with RR by individual IDs and sort them by their IDs
  rr <- pa |> dplyr::select(id, contains("RR")) |> 
    arrange(id)

  all_RR_objects <- ls(pattern = paste0("^RR.*", SCEN_SHORT_NAME))
  all_RR_list <- mget(all_RR_objects)
  result <- do.call(cbind, all_RR_list)
  
  rr <- cbind(rr, result)
  
  # Rename 
  # Combine all columns that have the same ending by multiplying them together
  combine_rr <- rr
  
  # Read zones dataset with LSOA and LAD for each zones (Belen: no need of dd file as now exposures files comes with zones)
  zones <- read_csv("jibe health/zoneSystem.csv")
  # zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv"))
  
  # Join zones with oaID (for households) to bring LSOA and LAD codes
  ppdf <- left_join(ppdf, zones |> dplyr::select(oaID, lsoa21cd, ladcd) |> rename(zone = oaID))
  
  # Add age, gender, lsoa and lad columns to RRs dataset
  combine_rr <- left_join(combine_rr, ppdf |> dplyr::select(id, age, gender, lsoa21cd, ladcd))
  
  # write combine_rr
  if (ncol(combine_rr) == 36)
    write_csv(combine_rr, paste0("jibe health/", SCEN_SHORT_NAME, "_pp_exposure_RR_2021.csv"))
  # write_csv(combine_rr, "manchester/health/processed/base_pp_exposure_RR_2021.csv")
  
  # Disease prevalence data
  prevalence <- read_csv(here("jibe health/health_transitions_manchester_prevalence.csv")) |> 
    #prevalence <- read_csv("manchester/health/processed/health_transitions_manchester_prevalence.csv") %>%  
    mutate(prob = 1 - exp(-rate))  # Convert rates to probabilities
  
  ### Syntehtic population for scenario
  
  # 1 make use of 
  # Assign prevalence to synthetic population
  synth_pop_wprob <- combine_rr %>% 
    rename(sex = gender) %>%
    rownames_to_column() %>%
    left_join(
      prevalence %>%
        pivot_wider(id_cols = c(age, sex, location_code), names_from = cause, values_from = prob),
      by = c("age", "sex", "ladcd" = "location_code")) %>%
    dplyr::select(!rowname)
  
  # 1 make use of 
  # Function to allocate disease statuses based on probability
  allocate_disease <- function(df) {
    df %>%
      mutate(across(copd:stroke, ~ ifelse(runif(dplyr::n()) < ., 1, 0), .names = "{.col}_diseased"))
  }
  
  # 1 make use of 
  # Apply function to assign diseases
  synth_pop_prev <- allocate_disease(synth_pop_wprob) %>%
    dplyr::select(id, age, sex, ladcd, ends_with("_diseased"), starts_with("RR"))  %>%# Ensure we only keep relevant columns
    mutate(sex = case_when(
      sex == 1 ~ "male",
      sex == 2 ~ "female",
      TRUE ~ as.character(sex)))
  
  
  synth_pop_prev |> 
    dplyr::select(id, age, sex, contains("disease") & !contains("RR")) |> 
    pivot_longer(cols = -c(id, age, sex)) |> 
    filter(value == 1)  |> 
    group_by(id) |> 
    summarize(diseases = paste(unique(name), collapse = " ")) %>%
    ungroup() |> 
    mutate(across(diseases, ~gsub("_diseased", "", .))) |> 
    write_csv(paste0("jibe health/", SCEN_SHORT_NAME, "_prevalence_id.csv"))
  
  rm(list = ls())
  #write_csv("manchester/health/processed/prevalence_id.csv")
}