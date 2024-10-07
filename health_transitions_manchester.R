rm(list = ls())

packages <- c("tidyverse",
              "dplyr")
lapply(packages, library, character.only = TRUE)

# Add data Greater Manchester for all cause mortality by LSOA and for diseases by LAD

diseases <- readRDS("manchester/health/processed/manchester_diseases_lad.RDS")
allcause <- readRDS("manchester/health/processed/manchester_mortality_lsoa.RDS")

# Process data 

allcause_data <- allcause_data %>%
  mutate(cause = "all cause",
         measure = "deaths",
         sex = if_else(sex == "Females", "Female", "Male")) %>%  
  rename(location_code = lsoa_code,
         location_name = lsoa_name,
         socio = imd_decile) %>%
  select(age, sex, location_code, location_name, prob, cause, measure, socio)

diseases_data <- diseases_data %>% 
  mutate(measure = tolower(measure),
         cause = tolower(cause),
         socio = NA) %>% 
  rename(location_code = LAD20CD,
         location_name = location,
         age = ageyr,
         prob = val_interpolated) %>% 
  select(age,sex,location_code,location_name,prob,cause,measure,socio)

health_transitions_manchester <- bind_rows(allcause_data, diseases_data)

saveRDS(health_transitions_manchester, "manchester/health/processed/health_transitions_manchester.RDS")

