#### Silo output processing function

# It takes individual level exposure file from silo output and returns data fore prevalence and input file with rrs for health microsim

# load libraries
require("tidyverse")
require("data.table")
require("drpa")
require("ithimr")
require("here")

##### Input data

# DISEASE_INVENTORY <- read_csv("health/disease_outcomes_lookup.csv")
# # DISEASE_INVENTORY <- read_csv("jibe health/health1/disease_outcomes_lookup.csv") 
# exposures <- read_csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_exposure_2021.csv")
# # ppdf <- read_csv("jibe health/pp_exposure_2021.csv")
# prevalence <- read_csv("manchester/health/processed/health_transitions_manchester_prevalence.csv")
# # prevalence <- read.csv("jibe health/health_transitions_manchester_prevalence.csv")
# zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv"))
# # zones <- read_csv("jibe health/zoneSystem.csv")
# 
# # Global variables
# 
# # list_of_files <- list.files(
# #   path = "jibe health/health1/",
# #   recursive = TRUE, pattern = "(pm|no|noise|ndvi)\\.csv$",
# #   full.names = TRUE
# # )
# 
# list_of_files <- list.files(
#   path = "health/",
#   recursive = TRUE, pattern = "(pm|no|noise|ndvi)\\.csv$",
#   full.names = TRUE
# )
# 
# # Make them available as global datasets
# for (i in 1:length(list_of_files)) {
#   assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
#          read.csv(list_of_files[[i]]),
#          pos = 1
#   )
# }
# 
# # # Read all_cause_no2 DR
# # all_cause_no2 <- read_csv("jibe health/health1/all_cause_no.csv") |>
# # rename(RR = rr)
# # 
# all_cause_no2 <- read_csv("health/all_cause_no.csv") |>
#   rename(RR = rr)
# 
# SCEN_SHORT_NAME <<- 'base' # check if need changing for scenarios

#### Function


process_output_silo <- function(disease_inventory, population_exposures, prevalence_baseline, zone_baseline) {

  
# disease_inventory <- DISEASE_INVENTORY
# population_exposures <- ppdf
# prevalence_baseline <- prevalence
# zone_baseline <- zones

  

# load pm2.5_dose_response
source("scripts/pm2.5_dose_response.R")

# Load NO2_dose_response function
source("scripts/NO2_dose_response.R")

# Load NDVI_dose_response function
source("scripts/ndvi_dose_response.R")

# Load noise_dose_response function
source("scripts/noise_dose_response.R")

# Fix object name
# diabetes_noise <- diabates_noise
# rm(diabates_noise)

# Read the default discease outcomes table from the ITHIM package

DISEASE_INVENTORY <<- disease_inventory |>  
  mutate(pa_acronym = acronym_inJava) |> mutate(pa_acronym = str_replace_all(pa_acronym, "_", "-"),
                                                outcome = str_replace_all(outcome, "_", "-")) |> 
  mutate(pa_acronym = case_when(pa_acronym == "parkinson" ~ "parkinson's-disease",
                                pa_acronym == "head-neck-cancer" ~ "head-and-neck-cancer",
                                TRUE ~ pa_acronym))

ppdf <- population_exposures

# Recalculate base_mmet and rename pm_conc_base variable (although it is exposure but ITHIM pacakge expects this name)
ppdf <- ppdf |> 
  mutate(base_mmet = mmetHr_walk + mmetHr_cycle + mmetHr_otherSport)

# Calculate PA RRs
pa <- ithimr::gen_pa_rr(mmets_pp = ppdf, conf_int = F)
# Calculate NO2 RRs
no2_all_cause <- NO2_dose_response(cause = "all_cause_no2", dose = ppdf |> 
                                     arrange(id) |> 
                                     dplyr::select(exposure_normalised_no2) |> 
                                     pull()) |> 
  rename(RR_no2_base_all_cause_mortality = rr)

# Calculate all_cause_mortality RRs for pm2.5
RR_pm_base_all_cause_mortality <- pm2.5_dose_response(cause = "all_cause_pm", dose = ppdf |> 
                                                        arrange(id) |> 
                                                        dplyr::select(exposure_normalised_pm25) |> 
                                                        pull()) |> 
  rename(RR_pm_base_all_cause_mortality = rr)

# Calculate copd RRs for pm2.5
RR_pm_base_copd <- pm2.5_dose_response(cause = "copd_pm", dose = ppdf |> 
                                         arrange(id) |> 
                                         dplyr::select(exposure_normalised_pm25) |> 
                                         pull()) |> 
  rename(RR_pm_base_copd = rr)

# Calculate ihd (or chd) RRs for pm2.5
RR_pm_base_coronary_heart_disease <- pm2.5_dose_response(cause = "ihd_pm", dose = ppdf |> 
                                                           arrange(id) |> 
                                                           dplyr::select(exposure_normalised_pm25) |> 
                                                           pull()) |> 
  rename(RR_pm_base_coronary_heart_disease = rr)

# Calculate lung cancer RRs for pm2.5
RR_pm_base_lung_cancer <- pm2.5_dose_response(cause = "lc_pm", dose = ppdf |> 
                                                arrange(id) |> 
                                                dplyr::select(exposure_normalised_pm25) |> 
                                                pull()) |> 
  rename(RR_pm_base_lung_cancer = rr)

# Calculate pm2.5 ERF for stroke
RR_pm_base_stroke <- pm2.5_dose_response(cause = "lc_pm", dose = ppdf |> 
                                           arrange(id) |> 
                                           dplyr::select(exposure_normalised_pm25) |> 
                                           pull()) |> 
  rename(RR_pm_base_stroke = rr)


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


# Multiply similar ending columns
multiply_similar_columns <- function(df) {
  # Get all column names
  col_names <- names(df)
  
  # Function to extract the common part of column names
  extract_common <- function(name) {
    parts <- strsplit(name, "_")[[1]]
    paste(parts[3:length(parts)], collapse = "_")
  }
  
  # Get unique common parts
  common_parts <- unique(sapply(col_names, extract_common))
  
  # For each common part, multiply corresponding columns
  for (part in common_parts) {
    matching_cols <- col_names[sapply(col_names, function(x) grepl(part, x))]
    
    if (length(matching_cols) > 1) {
      new_col_name <- paste0("RR_", part)
      df[[new_col_name]] <- Reduce(`*`, df[matching_cols])
    }
  }
  
  return(df)
}

# Assign PA columns with RR by individual IDs and sort them by their IDs
rr <- pa |> dplyr::select(id, contains("RR")) |> 
  arrange(id)
# Add no2_all_cause (already sorted by ID)
rr <- cbind(rr, no2_all_cause)

all_RR_objects <- ls(pattern = "^RR")
all_RR_list <- mget(all_RR_objects)
result <- do.call(cbind, all_RR_list)

rr <- cbind(rr, result)

# Rename 
# Combine all columns that have the same ending by multiplying them together
combine_rr <- rr#multiply_similar_columns(rr)

zones <- zone_baseline

# Join zones with oaID (for households) to bring LSOA and LAD codes
ppdf <- left_join(ppdf, zones |> dplyr::select(oaID, lsoa21cd, ladcd) |> rename(zone = oaID))

# Add age, gender, lsoa and lad columns to RRs dataset
combine_rr <- left_join(combine_rr, ppdf |> dplyr::select(id, age, gender, lsoa21cd, ladcd))


# Disease prevalence data
# prevalence <- read.csv("jibe health/health_transitions_manchester_prevalence.csv") %>%
prevalence <- prevalence_baseline %>%  
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
  mutate(across(diseases, ~gsub("_diseased", "", .)))
  # write_csv("jibe health/prevalence_id.csv")
  # write_csv("manchester/health/processed/prevalence_id.csv")


# write combine_rr
# write_csv(combine_rr, "jibe health/base_pp_exposure_RR_2021.csv")
# write_csv(combine_rr, "manchester/health/processed/base_pp_exposure_RR_2021.csv")

output <- list(synth_pop_prev, combine_rr)

}



output_reference <- process_output_silo(DISEASE_INVENTORY, exposures, prevalence, zones)
##### Output
###### Synthetic population with exposures for each scenario
# write_csv(combine_rr, "jibe health/base_pp_exposure_RR_2021.csv")

# ##### Prevalence save once only with basline
# write_csv(output[combine_rr], "manchester/health/processed/ref_pp_exposure_RR_2021.csv")
# # write_csv("jibe health/prevalence_id.csv")
# write_csv(output[synth_pop_prev],"manchester/health/processed/prevalence_id.csv")