rm(list = ls())

packages <- c("tidyverse",
              "dplyr")
lapply(packages, library, character.only = TRUE)

# Add data Greater Manchester for all cause mortality by LSOA and for diseases by LAD

diseases <- readRDS("manchester/health/processed/manchester_diseases_lad.RDS")
allcause <- readRDS("manchester/health/processed/manchester_mortality_lsoa.RDS")

# Process data 

allcause_data <- allcause %>%
  mutate(cause = "all_cause_mortality",
         measure = "deaths") %>%  
  rename(location_code = lsoa_code,
         location_name = lsoa_name) %>%
  mutate(location_type="lsoa") %>%
  mutate(sex=as.numeric(case_when(sex == "Males" ~ 1,
                                  sex=="Females" ~ 2))) %>%
  select(age, sex, location_code, location_type, cause, prob)

diseases_data <- diseases %>% 
  filter(measure == "Incidence") %>% 
  mutate(measure = tolower(measure),
         cause = tolower(cause),
         socio = NA) %>% 
  rename(location_code = LAD20CD,
         location_name = location,
         age = ageyr,
         prob = val_interpolated) %>%
  mutate(location_type="lad")  %>%
  mutate(sex=as.numeric(case_when(sex == "Male" ~ 1,
                                  sex=="Female" ~ 2))) %>%
  select(age, sex, measure, location_code, location_type, cause, prob)


health_transitions_manchester <- bind_rows(allcause_data, diseases_data)

saveRDS(health_transitions_manchester, "manchester/health/processed/health_transitions_manchester_raw.RDS")
write.csv(health_transitions_manchester, "manchester/health/processed/health_transitions_manchester_raw.csv")
