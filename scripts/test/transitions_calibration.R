###  STEP 1: Load Required Libraries  ###
library(tidyverse)  
library(dbplyr)    
library(duckdb)    
library(arrow)     
library(here)      
library(drpa)    
library(readxl)    

source("functions/pif_calc.R")

###  STEP 2: Load Data  ###

# Synthetic population with zones and prevalence calculation
synth_pop <- read.csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_rr_2021.csv")


### NEED to do by age and sex
pifs_baseline <- calculate_pif(synth_pop)

write.csv(pifs_baseline, "manchester/health/processed/pifs_baseline.csv")

## Modify transition probabilities

pifs <- read.csv("manchester/health/processed/pifs_baseline.csv")

## Repeat last age groups for age group 95 to 100

pifs_older <- pifs %>% 
  
  filter(sex_age_group %in% c("male_[90,95)", "female_[90,95)")) %>%
  mutate(sex_age_group = case_when(
    sex_age_group == "male_[90,95)" ~ "male_[95,100]",
    sex_age_group == "female_[90,95)" ~ "female_[95,100]",
    TRUE ~ sex_age_group  # Keep other values unchanged (optional)
  ))

pifs <- pifs %>% bind_rows(pifs_older) %>%
  rename(cause = outcome) %>%
  mutate(cause = str_replace_all(cause, "-", "_")) %>%  
  mutate(cause = case_when(
    cause == "parkinson’s_disease" ~ "parkinson",
    cause == "head_and_neck_cancer" ~ "head_neck_cancer",
    TRUE ~ cause
  ))

#### Add age 95 to 100
transition_data <- read.csv("manchester/health/processed/health_transitions_manchester_raw.csv") %>%
  mutate(cause = case_when(
    cause == "parkinson’s_disease" ~ "parkinson",
    cause == "head_and_neck_cancer" ~ "head_neck_cancer",
    TRUE ~ cause
  )) %>%
  mutate(
    age_group = cut(age, breaks = seq(0, max(age, na.rm = TRUE), by = 5), right = FALSE, include.lowest = TRUE),
    # sex = case_when(
    #   sex == 1 ~ "male",
    #   sex == 2 ~ "female",
    #   TRUE ~ as.character(sex)),
    sex_age_group = paste(sex, age_group, sep = "_")
  ) %>%
  left_join(pifs, by = c("cause", "sex_age_group")) %>%
  mutate(paf_combined_traditional = case_when(
    age < 20 ~ 0,  # Assign zero to pif for age < 20
    TRUE ~ paf_combined_traditional
  )) %>%
  select(!c("age_group")) %>%
  mutate(rate_adj = rate * (1 - paf_combined_traditional)) %>%
  select(!c("sex_age_group", "paf_combined_traditional", "X.x", "X.y", "rate")) %>%
  rename("rate" = "rate_adj") %>%
  mutate(sex = case_when(
    sex == "male" ~ 1,
    sex == "female" ~ 2)) %>%
  select(age, sex, location_code, location_type, cause, measure, rate)

## Compare graphs with estimates


# Create a function to generate plots for each cause and save them
plot_and_save <- function(data, cause_name) {
  # Filter data for the specific cause
  cause_data <- data %>% filter(cause == cause_name)
  
  # Check if data is empty
  if (nrow(cause_data) == 0) {
    warning(paste("No data available for cause:", cause_name))
    return(NULL)
  }
  
  # Ensure directory exists
  dir.create("images/manchester/inc_gbd_age_sex", recursive = TRUE, showWarnings = FALSE)
  
  # Create plots for males and females
  plot_male <- ggplot(cause_data %>% filter(sex == 1), aes(x = age, y = rate, color = location_code)) +
    geom_line() +
    labs(title = paste("Rates by Age for Males -", cause_name), x = "Age", y = "Rate") +
    theme_minimal(base_family = "sans") +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  
  plot_female <- ggplot(cause_data %>% filter(sex == 2), aes(x = age, y = rate, color = location_code)) +
    geom_line() +
    labs(title = paste("Rates by Age for Females -", cause_name), x = "Age", y = "Rate") +
    theme_minimal(base_family = "sans") +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA))
  
  # Save plots to separate files
  male_filename <- paste0("images/manchester/inc_gbd_age_sex/plot_male_", cause_name, ".png")
  female_filename <- paste0("images/manchester/inc_gbd_age_sex/plot_female_", cause_name, ".png")
  
  print(paste("Saving male plot to:", male_filename))
  ggsave(filename = male_filename, plot = plot_male, width = 8, height = 6, bg = "white")
  
  print(paste("Saving female plot to:", female_filename))
  ggsave(filename = female_filename, plot = plot_female, width = 8, height = 6, bg = "white")
}


# Get unique causes
unique_causes <- unique(transition_data$cause)

# Generate and save plots for each cause
lapply(unique_causes, function(cause) plot_and_save(transition_data, cause))


## Save final version 

##CHANGE PROB ADJUSTED FOR PROB

write.csv(transition_data, "manchester/health/processed/health_transitions_manchester.csv")

# write_rds(transition_data, "manchester/health/processed/health_transitions_manchester.RDS")