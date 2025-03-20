# load libraries
require("tidyverse")
require("data.table")
require("drpa")

# Define name of the scenario
SCEN_SHORT_NAME <- 'base'


# load pm2.5_dose_response
source("scripts/pm2.5_dose_response.R")
# Read all_cause_no2 DR
all_cause_no2 <- read_csv("jibe health/health1/all_cause_no.csv") |> 
  rename(RR = rr)
# Load NO2_dose_response functiom
source("scripts/NO2_dose_response.R")

# Load NDVI_dose_response functiom
source("scripts/ndvi_dose_response.R")

# Load noise_dose_response functiom
source("scripts/noise_dose_response.R")

list_of_files <- list.files(
  path = "jibe health/health1/",
  recursive = TRUE, pattern = "(pm|noise|ndvi)\\.csv$",
  full.names = TRUE
)


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
DISEASE_INVENTORY <- read_csv("jibe health/health1/disease_outcomes_lookup.csv") |> 
  mutate(pa_acronym = acronym_inJava) |> mutate(pa_acronym = str_replace_all(pa_acronym, "_", "-"),
                                                outcome = str_replace_all(outcome, "_", "-")) |> 
  mutate(pa_acronym = case_when(pa_acronym == "parkinson" ~ "parkinson's-disease",
                                pa_acronym == "head-neck-cancer" ~ "head-and-neck-cancer",
                           TRUE ~ pa_acronym))




# |> 
#   rename("air_pollution" = "air_pollution_pm25") |> 
#   mutate(ap_acronym = acronym) |> 
#   mutate(ap_acronym = case_when(ap_acronym == "all_cause_mortality" ~ "all_cause_ap",
#                                 ap_acronym == "coronary_heart_disease" ~ "cvd_ihd",
#                                 ap_acronym == "COPD" ~ "resp_copd",
#                                 ap_acronym == "lung_cancer" ~ "neo_lung",
#                                 ap_acronym == "stroke" ~ "cvd_stroke",
#                                 # neo_lung, resp_copd, cvd_stroke, t2_dm, cvd, respiratory
#                                 TRUE ~ ap_acronym)) 
  
# Read per person exposure
ppdf <- read_csv("jibe health/pp_exposure_2021.csv")
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

# # Calculate noise DR
# for (noise_dis_outcome in c(
#   "all_cause_noise",
#   "ihd_noise",
#   "diabetes_noise",
#   "stroke_noise"
# )){
#   noise_RR <- noise_dose_response(cause = noise_dis_outcome,
#                                          dose = ppdf |> arrange(id) |> 
#                                            dplyr::select(exposure_normalised_noise_Lden) |> 
#                                            pull())
#   
#   if (noise_dis_outcome == "all_cause_noise")
#     assign(paste0("RR_noise_", SCEN_SHORT_NAME, "all_cause_mortality"), noise_RR)
#   else if (noise_dis_outcome == "ihd_noise")
#     assign(paste0("RR_noise_", SCEN_SHORT_NAME, "coronary_heart_disease"), noise_RR)
#   else if (noise_dis_outcome == "diabetes_noise")
#     assign(paste0("RR_noise_", SCEN_SHORT_NAME, "diabetes"), noise_RR)
#   else if (noise_dis_outcome == "stroke_noise")
#     assign(paste0("RR_noise_", SCEN_SHORT_NAME, "stroke"), noise_RR)
#   
# }

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


# 
# for (ndvi_dis_outcome in c(
#   "all_cause_ndvi",
#   "diabetes_ndvi",
#   "stroke_ndvi"
# )){
#   ndvi_RR <- NDVI_dose_response(cause = ndvi_dis_outcome,
#                                    dose = ppdf |> arrange(id) |> 
#                                      dplyr::select(exposure_normalised_ndvi) |> 
#                                      pull())
#   #assign(noise_noise_diseases
#   
# }


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

# Hist of RR_all_cause
#hist(combine_rr$RR_base_all_cause)

# Read dwelling dataset
dd <- read_csv("jibe health/dd_2021.csv")

# Attach household IDs and zone to per person data frame
ppdf <- left_join(ppdf, dd |> dplyr::select(hhID, zone) |> rename(hhid = hhID))

# Read zones dataset with LSOA and LAD for each zones
zones <- read_csv("jibe health/zoneSystem.csv")

# Join zones with oaID (for households) to bring LSOA and LAD codes
ppdf <- left_join(ppdf, zones |> dplyr::select(oaID, lsoa21cd, ladcd) |> rename(zone = oaID))

# Add age, gender, lsoa and lad columns to RRs dataset
combine_rr <- left_join(combine_rr, ppdf |> dplyr::select(id, age, gender, lsoa21cd, ladcd))

# write combine_rr
write_csv(combine_rr, "jibe health/base_pp_exposure_RR_2021.csv")

# Disease prevalence data
prevalence <- read.csv("jibe health/health_transitions_manchester_prevalence.csv") %>%
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
  write_csv("jibe health/prevalence_id.csv")


## DISEASE RRs ##

disease_risk <- read.csv("jibe health/health/disease risks.csv")

# Convert sex in sample_long from numeric (1,2) to character ("male", "female")
sample_long <- synth_pop_prev %>%
  rename(
    "parkinsons_disease_diseased" = "parkinson’s_disease_diseased"
  ) %>%
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex)  # Keep other values unchanged
  )) %>%
  pivot_longer(cols = c(copd_diseased:stroke_diseased), names_to = "risk_factor", values_to = "has_risk_factor") 


# Expand disease_risk_expanded by duplicating "male female" rows
disease_risk_expanded <- disease_risk %>%
  filter(sex == "male female") %>%
  dplyr::select(-sex) %>%   
  crossing(sex = c("male", "female")) %>%  
  bind_rows(disease_risk %>% filter(sex %in% c("male", "female")))

# Join sample_long with expanded disease risk data
sample_with_rr <- sample_long %>%
  mutate(risk_factor = gsub("_diseased$", "", risk_factor)) %>%
  left_join(disease_risk_expanded, by = c("sex", "risk_factor")) %>%
  
  # Modify outcome for cancer-related risk factors
  mutate(outcome = if_else(grepl("cancer", risk_factor, ignore.case = TRUE), "all-cause-mortality", outcome)) %>%
  
  # Join again to fetch relative risks for cancer-related risk factors
  left_join(
    disease_risk_expanded %>% filter(grepl("cancer", risk_factor, ignore.case = TRUE)),  
    by = c("sex", "outcome"),  
    suffix = c("", "_cancer")  
  ) %>%
  
  # Assign relative risk values only if age is between 40 and 70
  mutate(relative_risk = case_when(
    age >= 40 & age <= 70 & has_risk_factor == 1 & !grepl("cancer", risk_factor, ignore.case = TRUE) ~ relative_risk,
    age >= 40 & age <= 70 & has_risk_factor == 1 & grepl("cancer", risk_factor, ignore.case = TRUE) ~ coalesce(relative_risk_cancer, 1),
    TRUE ~ 1  # Assign neutral RR (1) if age is outside the 40-70 range
  )) %>%
  
  # Create column names for pivoting
  mutate(risk_outcome = paste0("rr_", risk_factor, "^", outcome)) %>%
  
  # Select only relevant columns (remove risk_factor and outcome before pivoting)
  dplyr::select(id, age, sex, risk_outcome, relative_risk) %>%
  
  # Pivot wider to reduce dataset length and fill missing values with 1
  pivot_wider(
    names_from = risk_outcome, 
    values_from = relative_risk,
    values_fill = list(relative_risk = 1)
  ) %>% dplyr::select(!matches("na", ignore.case = TRUE))


# Merge with sample
synth_pop_prev <- left_join(synth_pop_prev, sample_with_rr, by = c("id", "sex", "age")) %>%
  dplyr::select(id, age, sex, starts_with("rr"))

# Make long

colnames(synth_pop_prev) <- gsub("-", "_", tolower(colnames(synth_pop_prev)))

# Get the current column names
col_names <- colnames(synth_pop_prev)

# Define the keywords
keywords <- c("no2", "ap", "pa", "noise", "ndvi")

# Create a pattern that matches an underscore following any of the keywords
pattern <- paste0("(", paste(keywords, collapse = "|"), ")_")

# Replace the matched pattern with the keyword followed by a caret
new_col_names <- gsub(pattern, "\\1^", col_names)

# Assign the modified column names back to the data frame
colnames(synth_pop_prev) <- new_col_names

# Convert to data long

data_long_rr <- synth_pop_prev %>% 
  pivot_longer(
    cols = starts_with("rr_"),  # Select all columns that start with "rr_"
    names_to = c("risk_type", "outcome"),  
    names_pattern = "rr_([^\\^]+)\\^(.*)",  # Extract text before and after ^
    values_to = "relative_risk"
  ) %>% dplyr::select(where(~ !all(is.na(.))))  # This is to remove rows for which diseases have no risk outcomes. 