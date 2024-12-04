library(tidyverse)
# remotes::install_github("meta-analyses/drpa")
library(drpa)
library(data.table)
library(readr)

# In this code we adjust the reference probabilities for deaths and diseases by the proportion
# attributable to modeled risk factors (PA to start with, then add PM2.5 and NO2)


# Read file for synthetic population with baseline exposures

synth_pop <- read.csv("manchester/simulationResults/ForUrbanTransition/reference/health/03_exposure_and_rr/pp_health_2021.csv")

# Create mmets variable for calculation of pif

synth_pop <- synth_pop %>%  mutate(mmets8=8.75, mmets17=17.5)

# ----- Function to assign RRs for PA  -----

# 1) First assign RRs for each synth individual mmets

DISEASE_SHORT_NAMES_PA <- read_csv("health/disease_outcomes_lookup.csv") %>%
  mutate(acronym = ifelse(grepl("_", acronym), gsub("_", "-", acronym), acronym)) %>%
  filter(!acronym %in% "COPD")

SCEN_SHORT_NAME <- c("mmets8", "mmets17")

#####
for (s in SCEN_SHORT_NAME) {
  for (i in 1:nrow(DISEASE_SHORT_NAMES_PA)) {

    synth_pop[,paste("rr_PHYSICAL_ACTIVITY", s, DISEASE_SHORT_NAMES_PA$acronym[i], sep = "_")] <-
      drpa::dose_response(cause = DISEASE_SHORT_NAMES_PA$acronym[i],
                          outcome_type = case_when(
                            DISEASE_SHORT_NAMES_PA$acronym[i] == "all-cause-mortality" ~ "fatal",
                            DISEASE_SHORT_NAMES_PA$acronym[i] == "diabetes" ~ "non-fatal",
                            TRUE ~ "fatal-and-non-fatal"
                          ),
                          dose = synth_pop[,paste0(s)],confidence_intervals = F)
  }
}

# Changes to match synthetic popuation convention
synth_pop <- synth_pop %>%
  rename_with(~ gsub("-", "_", .)) %>%
  rename("rr_PHYSICAL_ACTIVITY_mmets17_parkinson_disease" = "rr_PHYSICAL_ACTIVITY_mmets17_parkinson's_disease",
         "rr_PHYSICAL_ACTIVITY_mmets8_parkinson_disease" = "rr_PHYSICAL_ACTIVITY_mmets8_parkinson's_disease", 
         "rr_PHYSICAL_ACTIVITY_mmets8_head_neck_cancer" = "rr_PHYSICAL_ACTIVITY_mmets8_head_and_neck_cancer",
         "rr_PHYSICAL_ACTIVITY_mmets17_head_neck_cancer" = "rr_PHYSICAL_ACTIVITY_mmets17_head_and_neck_cancer")

# Add age and sex variable for later pif calculation.
# for age groups () means exclusion and [] means inclusion, it means lower bound included and upper bound excluded
  
  synth_pop <- synth_pop %>% mutate(
    age_group = cut(age, breaks = seq(0, max(age, na.rm = TRUE), by = 5), right = FALSE, include.lowest = TRUE),
    age_gender = paste(gender, age_group, sep = "_")
  ) %>%
    mutate(age_gender = case_when(age_gender == "2_[90,95]" ~ "2_[90,95)",
                                  age_gender == "1_[90,95]" ~ "1_[90,95)",
                                  TRUE ~ age_gender))



# 2) calculate PIFs for each cause

# Convert data to long

  synth_pop_long <- synth_pop %>%
    pivot_longer(
      # Capture all columns starting with rr_PHYSICAL_ACTIVITY, rr_AIR_POLLUTION_PM25, or rr_AIR_POLLUTION_NO2
      cols = matches("^rr_PHYSICAL_ACTIVITY|^rr_AIR_POLLUTION_PM25|^rr_AIR_POLLUTION_NO2"),  
      names_to = c("risk_type", "cause"),  
      # Use a simpler pattern to separate risk_type and cause
      names_pattern = "^(rr_PHYSICAL_ACTIVITY_mmets8|rr_PHYSICAL_ACTIVITY_mmets17|rr_PHYSICAL_ACTIVITY|rr_AIR_POLLUTION_PM25|rr_AIR_POLLUTION_NO2)_(.*)",  
      values_to = "value"
    ) %>%
    drop_na(risk_type) %>%  # Drop rows where risk_type is missing (if any)
    select(id, age, gender, age_gender, risk_type, cause, value)
  


# Pif per individuals
## PIFs for physical activity (pif PA with reference desired level 8.75 mmtes per week and 1 for AP)

# Step 1: Calculate PIF for each individual
pif_ind <- synth_pop_long %>%
  pivot_wider(
    names_from = risk_type,   
    values_from = value        
  ) %>%
  mutate(rr_PHYSICAL_ACTIVITY_mmets8revised = case_when( # Use revised for pif calculations
    rr_PHYSICAL_ACTIVITY < rr_PHYSICAL_ACTIVITY_mmets8 ~ rr_PHYSICAL_ACTIVITY_mmets8,  # change name and add for mmets17
    TRUE ~ rr_PHYSICAL_ACTIVITY
  ),
  rr_PHYSICAL_ACTIVITY_mmets17revised = case_when( # Use revised for pif calculations
    rr_PHYSICAL_ACTIVITY < rr_PHYSICAL_ACTIVITY_mmets17 ~ rr_PHYSICAL_ACTIVITY_mmets17,  # change name and add for mmets17
    TRUE ~ rr_PHYSICAL_ACTIVITY
  )) %>% 
  mutate(
    pif_pa8 = (rr_PHYSICAL_ACTIVITY_mmets8revised - rr_PHYSICAL_ACTIVITY_mmets8) / rr_PHYSICAL_ACTIVITY_mmets8revised,
    pif_pa17 = (rr_PHYSICAL_ACTIVITY_mmets17revised - rr_PHYSICAL_ACTIVITY_mmets17) / rr_PHYSICAL_ACTIVITY_mmets17revised,
    pif_pm = (rr_AIR_POLLUTION_PM25 - 1) / 1,
    pif_no = (rr_AIR_POLLUTION_NO2 - 1) / 1
  ) %>%
  mutate(
    pif_pa8 = coalesce(pif_pa8, 0),
    pif_pa17 = coalesce(pif_pa17, 0),
    pif_pm = coalesce(pif_pm, 0),
    pif_no = coalesce(pif_no, 0),
    pif8 = 1 - (1 - pif_pa8) * (1 - pif_pm) * (1 - pif_no),
    pif17 = 1 - (1 - pif_pa17) * (1 - pif_pm) * (1 - pif_no))  # Combined PIF
  

## pifs by age and sex

pif <- pif_ind %>%
  group_by(age_gender, cause) %>%
  summarize(pif8=mean(pif8),
            pif17=mean(pif17)) %>%
   mutate(diff=(pif8-pif17)/pif8*100) 

## Synthetic pop max age is 95, but health data is up to 100 years old. Repeat older age groups

pif_older <- pif %>% filter(age_gender %in% c("1_[90,95)", "2_[90,95)")) %>%
  mutate(age_gender = (case_when(age_gender == "1_[90,95)" ~ "1_[95,100]",
                              age_gender == "2_[90,95)" ~ "2_[95,100]")))

pif <- pif %>% bind_rows(pif_older)

## Modify transition probabilities

transition_data <- readRDS("manchester/health/processed/health_transitions_manchester_raw.RDS") %>%
  mutate(cause = case_when(
    cause == "parkinson’s_disease" ~ "parkinson",
    cause == "head_and_neck_cancer" ~ "head_neck_cancer",
    TRUE ~ cause
  )) %>%
  mutate(
    age_group = cut(age, breaks = seq(0, max(age, na.rm = TRUE), by = 5), right = FALSE, include.lowest = TRUE),
    age_gender = paste(sex, age_group, sep = "_")
  ) %>%
  left_join(pif, by = c("cause", "age_gender")) %>%
  select(!c("pif17", "diff")) %>%
  mutate(pif = case_when(
    age < 20 ~ 0,  # Assign zero to pif for age < 20
    TRUE ~ pif8
  )) %>%
  select(!c("age_group", "pif8")) %>%
  mutate(prob = prob * (1 - pif)) %>%
  select(!c("age_gender", "pif"))

# ## Compare graphs with estimates
# 
# library(ggplot2)
# library(ggforce)
# 
# # Calculate the number of pages needed for 6 graphs per page
# n_pages <- ceiling(length(unique(plot_data$location_code)) / 6)
# 
# # Create the base plot with a white background
# plot <- ggplot(plot_data) +
#   aes(x = age, y = prob, colour = name) +
#   geom_line(size = 1) +
#   scale_color_manual(values = c("blue", "red")) +
#   theme_minimal(base_family = "Arial") +   # White background, minimal theme
#   labs(x = "Age", y = "Probability", title = "Probability by Age and Location")
# 
# # Loop to save each page of facets with a white background
# for (i in seq_len(n_pages)) {
#   ggsave(
#     filename = paste0("images/prob/probability_plots_page_", i, ".png"),
#     plot = plot + facet_wrap_paginate(~ location_code, ncol = 2, nrow = 3, page = i),
#     width = 10, height = 8,
#     bg = "white"   # Ensures white background in saved images
#   )
# }



## Save final version 

##CHANGE PROB ADJUSTED FOR PROB

write.csv(transition_data, "manchester/health/processed/health_transitions_manchester.csv")

write_rds(transition_data, "manchester/health/processed/health_transitions_manchester.RDS")