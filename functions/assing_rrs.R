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

library(data.table)

# 1. Preprocess all CSV files first
rr_tables <- list(
  pm25_allcause = read_csv("health/all_cause_pm.csv") %>% arrange(dose),
  pm25_copd = read_csv("health/copd_pm.csv") %>% arrange(dose),
  pm25_ihd = read_csv("health/ihd_pm.csv") %>% arrange(dose),
  pm25_stroke = read_csv("health/stroke_pm.csv") %>% arrange(dose),
  pm25_lc = read_csv("health/lc_pm.csv") %>% arrange(dose),
  no2_allcause = read_csv("health/all_cause_no.csv") %>% arrange(dose),
  ndvi_diabetes = read_csv("health/diabetes_ndvi.csv") %>% arrange(dose),
  ndvi_stroke = read_csv("health/stroke_ndvi.csv") %>% arrange(dose),
  ndvi_allcause = read_csv("health/all_cause_ndvi.csv") %>% arrange(dose),
  noise_allcause = read_csv("health/all_cause_noise.csv") %>% arrange(dose),
  noise_diabetes = read_csv("health/diabates_noise.csv") %>% arrange(dose),
  noise_ihd = read_csv("health/ihd_noise.csv") %>% arrange(dose),
  noise_stroke = read_csv("health/stroke_noise.csv") %>% arrange(dose)
)

# 2. Vectorized closest value finder using binary search
find_closest_vectorized <- function(values, ref_doses) {
  indices <- findInterval(values, ref_doses, all.inside = TRUE)
  ref_doses[indices]
}

# 3. Optimized RR assignment function using data.table

assign_rrs <- function(dt, rr_table, exposure_col, new_col_name) {
  # Convert to data.table if not already
  setDT(dt)
  
  # Create sorted reference vectors
  ref_doses <- rr_table$dose
  ref_rr <- rr_table$rr
  
  # Vectorized closest dose calculation
  closest_doses <- find_closest_vectorized(dt[[exposure_col]], ref_doses)
  
  # Direct index mapping instead of searching again
  dt[age > 20, (new_col_name) := ref_rr[match(closest_doses[.I], ref_doses)]]
  dt[age <= 20, (new_col_name) := 1]
  
  return(dt)
}

# 4. Convert synth_pop_mmets to data.table once
setDT(synth_pop_mmets)

# 5. Apply all assignments in one pass using column operations
synth_pop_mmets <- synth_pop_mmets %>%
  assign_rrs(rr_tables$pm25_allcause, "exposure_normalised_pm25", "rr_AIR_POLLUTION_PM25_all-cause-mortality") %>%
  assign_rrs(rr_tables$pm25_copd, "exposure_normalised_pm25", "rr_AIR_POLLUTION_PM25_copd") %>%
  assign_rrs(rr_tables$pm25_ihd, "exposure_normalised_pm25", "rr_AIR_POLLUTION_PM25_coronary-heart-disease") %>%
  assign_rrs(rr_tables$pm25_stroke, "exposure_normalised_pm25", "rr_AIR_POLLUTION_PM25_stroke") %>%
  assign_rrs(rr_tables$pm25_lc, "exposure_normalised_pm25", "rr_AIR_POLLUTION_PM25_lung-cancer") %>%
  assign_rrs(rr_tables$no2_allcause, "exposure_normalised_no2", "rr_AIR_POLLUTION_NO2_all-cause-mortality") %>%
  assign_rrs(rr_tables$ndvi_diabetes, "exposure_normalised_ndvi", "rr_NDVI_diabetes") %>%
  assign_rrs(rr_tables$ndvi_stroke, "exposure_normalised_ndvi", "rr_NDVI_stroke") %>%
  assign_rrs(rr_tables$ndvi_allcause, "exposure_normalised_ndvi", "rr_NDVI_all-cause-mortality") %>%
  assign_rrs(rr_tables$noise_allcause, "exposure_normalised_noise_Lden", "rr_NOISE_all-cause-mortality") %>%
  assign_rrs(rr_tables$noise_diabetes, "exposure_normalised_noise_Lden", "rr_NOISE_diabetes") %>%
  assign_rrs(rr_tables$noise_ihd, "exposure_normalised_noise_Lden", "rr_NOISE_coronary-heart-disease") %>%
  assign_rrs(rr_tables$noise_stroke, "exposure_normalised_noise_Lden", "rr_NOISE_stroke")


}

#### Compare with java assigned rrs #####

# 
# synth_pop_rr <- read_csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_rr_2021.csv") %>%
#   mutate(scen="rr") %>%
#   select(id, age, scen, starts_with(("rr"))) %>% filter(age >20)
# 
# synth_pop_exp <- synth_pop_mmets %>% 
#   mutate(scen="exp") %>%  
#   select(id, age, scen, starts_with("rr")) %>%
#   rename(rr_PHYSICAL_ACTIVITY_parkinson = `rr_PHYSICAL_ACTIVITY_parkinson's-disease`,
#         rr_PHYSICAL_ACTIVITY_head_neck_cancer = `rr_PHYSICAL_ACTIVITY_head-and-neck-cancer`) %>% 
#   filter(age >20)
# 
# 
# colnames(synth_pop_exp) <- gsub("-", "_", colnames(synth_pop_exp))
# 
# compare_rr <- bind_rows(synth_pop_rr, synth_pop_exp)
# 
# library(dplyr)
# library(tidyr)
# 
# data_long <- compare_rr %>%
#   pivot_longer(
#     cols = -c(scen, id, age),           # Pivot all columns except 'scen'
#     names_to = "variable",    # Name of the new column containing original column names
#     values_to = "value"       # Name of the new column containing values
#   )
# 
# 
# 
# summary_stats <- data_long %>%
#   group_by(scen, variable) %>%
#   summarise(
#     mean_value = mean(value, na.rm = TRUE),
#     median_value = median(value, na.rm = TRUE),
#     .groups = "keep"  # Preserves grouping structure
#   ) %>%
#   ungroup()  # Remove grouping for subsequent operations
# 
# 


