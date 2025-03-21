## This function is to assign relative risks to synthetic population if rrs are not alrady attached. 

library(drpa)    
library(readxl)
library(tidyverse)  
library(dbplyr) 
# Example data

synth_pop <- read_csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_exposure_2021.csv")


synth_pop_with_rr <- function(data) {
  
  # data <- synth_pop

# 1) Assign mmets

synth_pop_mmets <- data %>% 
  mutate(mmets = mmetHr_walk + mmetHr_cycle + mmetHr_otherSport) %>%
  rename(sex = gender) %>% 
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex)  # This line handles any other cases
  ))


# 2) Assign Relative Risks mmets

DISEASE_SHORT_NAMES <- read_csv("health/disease_outcomes_lookup.csv")

pa_diseases <- DISEASE_SHORT_NAMES %>%
   filter(physical_activity == 1) %>%
   mutate(acronym = gsub("_", "-", acronym)) 

 for (i in seq_len(nrow(pa_diseases))) {
   disease_acronym <- pa_diseases$acronym[i]
   new_colname <- paste0("rr_PHYSICAL_ACTIVITY_", disease_acronym)
   
   # Apply conditions:
   # - Age must be greater than 20
   # - Males should not have 'breast_cancer' or 'endometrial_cancer'
   synth_pop_mmets[[new_colname]] <- ifelse(
     synth_pop_mmets$age > 20 & !(synth_pop_mmets$sex == "male" & disease_acronym %in% c("breast-cancer", "endometrial-cancer")),
     drpa::dose_response(
       cause = disease_acronym,
       outcome_type = case_when(
         disease_acronym == "all-cause-mortality" ~ "fatal",
         disease_acronym == "diabetes" ~ "non-fatal",
         TRUE ~ "fatal-and-non-fatal"
       ),
       dose = synth_pop_mmets$mmets,
       confidence_intervals = FALSE, 
       censor_method = "WHO-DRL"
     )$rr,
     1  # Assign 1 instead of NA when conditions are not met
   )
 }


# 3) Assign relative risks exposure

# Function to find the closest value in a vector
 find_closest <- function(value, vector) {
   vector[which.min(abs(vector - value))]
 }
 
# Function to assign RRs only if age > 20
find_rr <- function(df_1, df_2, exposure_name, new_col_name) {
 
   df_1 %>%
     rowwise() %>%
     mutate(!!new_col_name := ifelse(
       age > 20,  # Apply RR only if age > 20
       {
         closest_dose <- find_closest({{exposure_name}}, df_2$dose)
         df_2$rr[df_2$dose == closest_dose]
       },
       1  # Assign neutral RR (1) if age <= 20
     )) %>%
     ungroup()
 }
 
# # Assign RRs for PM2.5 & NO2, greenspace, and noise
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/all_cause_pm.csv"), exposure_normalised_pm25, "rr_AIR_POLLUTION_PM25_all-cause-mortality")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/copd_pm.csv"), exposure_normalised_pm25, "rr_AIR_POLLUTION_PM25_copd")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/ihd_pm.csv"), exposure_normalised_pm25, "rr_AIR_POLLUTION_PM25_coronary-heart-disease")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/stroke_pm.csv"), exposure_normalised_pm25, "rr_AIR_POLLUTION_PM25_stroke")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/lc_pm.csv"), exposure_normalised_pm25, "rr_AIR_POLLUTION_PM25_lung-cancer")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/all_cause_no.csv"), exposure_normalised_no2, "rr_AIR_POLLUTION_NO2_all-cause-mortality")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/diabetes_ndvi.csv"), exposure_normalised_ndvi, "rr_NDVI_diabetes")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/stroke_ndvi.csv"), exposure_normalised_ndvi, "rr_NDVI_stroke")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/all_cause_ndvi.csv"), exposure_normalised_ndvi, "rr_NDVI_all-cause-mortality")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/all_cause_noise.csv"), exposure_normalised_noise_Lden, "rr_NOISE_all-cause-mortality")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/diabates_noise.csv"), exposure_normalised_noise_Lden, "rr_NOISE_diabetes")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/ihd_noise.csv"), exposure_normalised_noise_Lden, "rr_NOISE_coronary-heart-disease")
synth_pop_mmets <- find_rr(synth_pop_mmets, read_csv("health/stroke_noise.csv"), exposure_normalised_noise_Lden, "rr_NOISE_stroke")


}

#### Compare with java assigned rrs #####


synth_pop_rr <- read_csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_rr_2021.csv") %>% 
  mutate(scen="rr") %>% 
  select(id, age, scen, starts_with(("rr"))) %>% filter(age >20)

synth_pop_exp <- synth_pop_mmets %>% 
  mutate(scen="exp") %>%  
  select(id, age, scen, starts_with("rr")) %>%
  rename(rr_PHYSICAL_ACTIVITY_parkinson = `rr_PHYSICAL_ACTIVITY_parkinson's-disease`,
        rr_PHYSICAL_ACTIVITY_head_neck_cancer = `rr_PHYSICAL_ACTIVITY_head-and-neck-cancer`) %>% 
  filter(age >20)


colnames(synth_pop_exp) <- gsub("-", "_", colnames(synth_pop_exp))

compare_rr <- bind_rows(synth_pop_rr, synth_pop_exp)

library(dplyr)
library(tidyr)

data_long <- compare_rr %>%
  pivot_longer(
    cols = -scen,           # Pivot all columns except 'scen'
    names_to = "variable",    # Name of the new column containing original column names
    values_to = "value"       # Name of the new column containing values
  ) %>%
  pivot_wider(
    names_from = scen,      # Create new columns based on 'scen' values
    values_from = value       # Fill new columns with 'value'
  )





