library(tidyverse)
library(data.table)
library(readr)


##Prepareation dose-response relationships air pollution

# 2) Second assign RRs for PM2.5 and NO2. Note: need to add diseases RRs and also NO2. 

# 2.1) Create table with ERFs and save as csv. same as with ITHIMR format

# Create dose sequences (this is similar to ITHIMR RRs for PM AP)
dose <- seq(0, 0.99, 0.01)
dose_2 <- seq(1, 9.9, 0.1)
dose_3 <- seq(10, 84, 1)

combined_dose <- data.frame(dose = c(dose, dose_2, dose_3))

# PM2.5

## All-cause mortality Orellano et al 2024 update for WHO AQ guidelines. RR 1.095 (1.064-1.127). Exposure levels median (min-max) 
# 16.58 (4.49, 72.4). Same as from document "Health risks of air pollution in Europe - HARPIE-2"
# RRs reflect on exposure range

rr_pm_mort <- 1.095

all_cause_pm <- combined_dose %>% 
  select(dose) %>%  # Select the columns dose and rr_pm_mort
  mutate(rr = as.numeric(exp(log(rr_pm_mort) * (dose / 10)))) %>%  # Adjust rr for the exposure range
  mutate(rr = case_when(
    dose < 4.48 ~ 1,   # If dose is less than 4.48, set rr to 1
    dose > 72.4 ~ exp(log(rr_pm_mort) * (72.4 / 10)),  # Cap rr at the value for dose = 72.4
    TRUE ~ rr          # For all other cases, keep the calculated rr
  ))

write.csv(all_cause_pm, "health/all_cause_pm.csv")

## Disease incidence from Forastiere et al., 2024 for ERFs rated as A from the overall confidence assessment. 
## The authors, in the absence of shape for the CRFs from the studies, indicated that linear CRFs could be assumed. 


### COPD. 1.18 (1.13, 1.21) per 10 ug/m3. Exposure range 5-26

rr_pm_copd <- 1.18

copd_pm <- combined_dose %>% 
  select(dose) %>%  # Select the columns dose and rr_pm_mort
  mutate(rr = as.numeric(exp(log(rr_pm_copd) * (dose / 10)))) %>%  # Adjust rr for the exposure range
  mutate(rr = case_when(
    dose < 4.99 ~ 1,   # If dose is less than 5, set rr to 1
    dose > 25.99 ~ exp(log(rr_pm_copd) * (25.99 / 10)),  # Cap rr at the value for dose = 26
    TRUE ~ rr          # For all other cases, keep the calculated rr
  ))

write.csv(copd_pm, "health/copd_pm.csv")

### IHD 1.13 (1.05, 1.22). Exposure range is 5-65

rr_pm_ihd <- 1.13

ihd_pm <- combined_dose %>% 
  select(dose) %>%  # Select the columns dose and rr_pm_mort
  mutate(rr = as.numeric(exp(log(rr_pm_ihd) * (dose / 10)))) %>%  # Adjust rr for the exposure range
  mutate(rr = case_when(
    dose < 4.99 ~ 1,   # If dose is less than 5, set rr to 1
    dose > 64.99 ~ exp(log(rr_pm_ihd) * (64.99 / 10)),  # Cap rr at the value for dose = 65
    TRUE ~ rr          # For all other cases, keep the calculated rr
  ))

write.csv(ihd_pm, "health/ihd_pm.csv")

## Stroke 1.16 (1.12, 1.20). Exposure range 5-36

rr_pm_stroke <- 1.16

stroke_pm <- combined_dose %>% 
  select(dose) %>%  # Select the columns dose and rr_pm_mort
  mutate(rr = as.numeric(exp(log(rr_pm_stroke) * (dose / 10)))) %>%  # Adjust rr for the exposure range
  mutate(rr = case_when(
    dose < 4.99 ~ 1,   # If dose is less than 5, set rr to 1
    dose > 35.99 ~ exp(log(rr_pm_stroke) * (35.99 / 10)),  # Cap rr at the value for dose = 36
    TRUE ~ rr          # For all other cases, keep the calculated rr
  ))

write.csv(stroke_pm, "health/stroke_pm.csv")

## Lung cancer 1.16 (1.10, 1.23). Exposure range 5-44

rr_pm_lc <- 1.16

lc_pm <- combined_dose %>% 
  select(dose) %>%  # Select the columns dose and rr_pm_mort
  mutate(rr = as.numeric(exp(log(rr_pm_lc) * (dose / 10)))) %>%  # Adjust rr for the exposure range
  mutate(rr = case_when(
    dose < 4.99 ~ 1,   # If dose is less than 5, set rr to 1
    dose > 43.99 ~ exp(log(rr_pm_lc) * (43.99 / 10)),  # Cap rr at the value for dose = 44
    TRUE ~ rr          # For all other cases, keep the calculated rr
  ))

write.csv(lc_pm, "health/lc_pm.csv")

## 

#NO2 

## All-cause mortality. Kasdagli et al. 2024 (25).RR 1.05 (1.03, 1.07)
# Exposure range and shape of the ERF is linear. 
# Exposure range 7.1 to 129.9

rr_no_mort <- 1.05


all_cause_no <- combined_dose %>% 
  select(dose) %>%  # Select the columns dose and rr_pm_mort
  mutate(rr = as.numeric(exp(log(rr_no_mort) * (dose / 10)))) %>%  # Adjust rr for the exposure range
  mutate(rr = case_when(
    dose < 7.1 ~ 1,   # If dose is less than 7.1, set rr to 1
    dose > 129.9 ~ exp(log(rr_no_mort) * (129.9 / 10)),  # Cap rr at the value for dose = 44
    TRUE ~ rr          # For all other cases, keep the calculated rr
  ))

write.csv(all_cause_no, "health/all_cause_no.csv")


# Noise

