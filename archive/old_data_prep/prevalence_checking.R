library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)


# Synthetic population baseline with diseases

synth_pop_prev <- read_csv("manchester/health/processed/synth_pop_prev_2021.csv")

# Load standard population data (GBD uses the WHO world standardised popuation)
european_standard_population <- read.csv(here('manchester/health/original/ons/european_standard_population_by_sex.csv')) %>%
  rename(sex = Sex, standard_pop = EuropeanStandardPopulation) %>%
  mutate(sex=tolower(sex))

# WHO standard population (by age but not sex, repeated same weights). This standard pop used in GBD. 
library(tidyverse)

who_standard_population <- tibble(
  AgeGroup = rep(c("0-4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39", "40-44", 
                    "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", 
                    "85-89 years", "90plus years"), 2),
  standard_pop = rep(c(8.86, 8.69, 8.60, 8.47, 8.22, 7.93, 7.61, 7.15, 6.59, 6.04, 5.37, 4.55, 3.72, 
                 2.96, 2.21, 1.52, 0.91, 0.44, 0.195), 2),
  sex = rep(c("male", "female"), each = 19)
) %>%
  arrange(sex, AgeGroup)


# Function to calculate age-standardized rates
calculate_asr <- function(data, std_pop, group_vars = NULL) {
  
data <- df
std_pop <- who_standard_population
test <-   data %>%
    left_join(std_pop, by = c("AgeGroup", "sex")) %>%
    mutate(AgeGroup = gsub(" years", "", AgeGroup),
           AgeGroup = recode(AgeGroup, "<5" = "0-4", "90plus" = "90+")) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(ends_with("status"), 
                     ~ sum(.x * standard_pop, na.rm = TRUE) / sum(standard_pop, na.rm = TRUE) * 100000, 
                     .names = "ASR_{col}_per100k"), .groups = "drop")
}

## Add age groups
df <- synth_pop_prev %>%
  mutate(AgeGroup = case_when(
    age >= 0 & age <= 4 ~ "0-4 years",
    age >= 5 & age <= 9 ~ "5-9 years",
    age >= 10 & age <= 14 ~ "10-14 years",
    age >= 15 & age <= 19 ~ "15-19 years",
    age >= 20 & age <= 24 ~ "20-24 years",
    age >= 25 & age <= 29 ~ "25-29 years",
    age >= 30 & age <= 34 ~ "30-34 years",
    age >= 35 & age <= 39 ~ "35-39 years",
    age >= 40 & age <= 44 ~ "40-44 years",
    age >= 45 & age <= 49 ~ "45-49 years",
    age >= 50 & age <= 54 ~ "50-54 years",
    age >= 55 & age <= 59 ~ "55-59 years",
    age >= 60 & age <= 64 ~ "60-64 years",
    age >= 65 & age <= 69 ~ "65-69 years",
    age >= 70 & age <= 74 ~ "70-74 years",
    age >= 75 & age <= 79 ~ "75-79 years",
    age >= 80 & age <= 84 ~ "80-84 years",
    age >= 85 & age <= 89 ~ "85-89 years",
    age >= 90 ~ "90plus years",
    TRUE ~ "Unknown"))

# Compute ASR by different groupings
asr_list <- list(
  whole_population = calculate_asr(df, who_standard_population),
  by_sex = calculate_asr(df, who_standard_population, group_vars = "sex"),
  by_lad = calculate_asr(df, who_standard_population, group_vars = "ladnm"),
  by_sex_agegroup = calculate_asr(df, who_standard_population, group_vars = c("sex", "AgeGroup")),
  by_sex_age_lad = calculate_asr(df, who_standard_population, group_vars = c("sex", "AgeGroup", "ladnm"))
)

# Load and merge GBD datasets
gbd_files <- list.files(path = "manchester/health/original/gbd/", pattern = "*.csv", full.names = TRUE)
gbd <- bind_rows(lapply(gbd_files, read.csv))

# Process GBD data
gbdp <- gbd %>%
  filter(metric == "Rate", measure == "Prevalence", year == 2018, age != "All ages") %>%
  select(-c(upper, lower)) %>%
  rename(AgeGroup = age) %>%
  mutate(AgeGroup = case_when(
    AgeGroup == "<5 years" ~ "0-4",
    TRUE ~ AgeGroup
  )) %>%
  mutate(AgeGroup = str_remove(AgeGroup, " years")) %>%
  mutate(sex=tolower(sex))%>%
  filter(cause %in% c("Stroke", "Ischemic heart disease", "Breast cancer", 
                      "Uterine cancer", "Tracheal, bronchus, and lung cancer", 
                      "Colon and rectum cancer", "Esophageal cancer", 
                      "Liver cancer", "Stomach cancer", "Chronic myeloid leukemia", 
                      "Multiple myeloma", "Larynx cancer", "Lip and oral cavity cancer", 
                      "Nasopharynx cancer", "Other pharynx cancer", "Bladder cancer",  
                      "Depressive disorders", "Alzheimer's disease and other dementias", 
                      "Diabetes mellitus type 2", "Chronic obstructive pulmonary disease", 
                      "Parkinson's disease")) %>%
  mutate(cause = recode(cause, 
                        "Ischemic heart disease" = "coronary_heart_disease",
                        "Uterine cancer" = "endometrial_cancer",
                        "Stomach cancer" = "gastric_cardia_cancer",
                        "Chronic myeloid leukemia" = "myeloid_leukemia",
                        "Multiple myeloma" = "myeloma",
                        "Depressive disorders" = "depression",
                        "Alzheimer's disease and other dementias" = "all_cause_dementia",
                        "Diabetes mellitus type 2" = "diabetes",
                        "Stroke" = "stroke",
                        "Tracheal, bronchus, and lung cancer" = "lung_cancer",
                        "Breast cancer" = "breast_cancer",
                        "Colon and rectum cancer" = "colon_cancer",
                        "Bladder cancer" = "bladder_cancer",
                        "Esophageal cancer" = "esophageal_cancer",
                        "Liver cancer" = "liver_cancer",
                        "Chronic obstructive pulmonary disease" = "copd",
                        "Parkinson's disease" = "parkinson’s_disease"))

# Sum rates for head and neck cancers
hanc <- c("Larynx cancer", "Lip and oral cavity cancer", "Nasopharynx cancer", "Other pharynx cancer")
gbdp_hanc <- gbdp %>%
  filter(cause %in% hanc) %>%
  group_by(measure, location, sex, AgeGroup, metric, year) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(cause = "head_and_neck_cancer")

gbdp <- gbdp %>%
  filter(!cause %in% hanc) %>%
  bind_rows(gbdp_hanc)

# Convert ASR data to long format
asr_long <- asr_list$by_sex_age_lad %>%
  pivot_longer(cols = starts_with("ASR_"), names_to = "cause", values_to = "values_assigned") %>%
  mutate(cause = gsub("ASR_|_diseased_per100k", "", cause),
         AgeGroup = recode(AgeGroup, "90+" = "90-94"))

# older group

asr_long_older <- asr_long %>% filter(AgeGroup == "90-94") %>%
  mutate(AgeGroup = recode(AgeGroup, "90-94"= "95+"))

asr_long <- bind_rows(asr_long, asr_long_older) %>% rename(location=ladnm) %>% 
  mutate(cause = gsub("_status_per100k", "", cause))

# Compare with GBD
compare <- asr_long  %>%
  left_join(gbdp, by = join_by(sex, AgeGroup, location  , cause)) %>%
  select(sex, AgeGroup, location, cause, values_assigned, val_gbd = val) %>%
  mutate(AgeGroup = factor(AgeGroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                                                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                                                "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
                                                "90-94", "95+"), ordered = TRUE),
         diff = values_assigned - val_gbd) 
  

write.csv(compare, "manchester/health/processed/prevalence_compare.csv")

# Generate plots for each location, sex, and cause
combinations <- compare %>% 
  select(location, sex, cause) %>% 
  distinct()

for (i in seq_len(nrow(combinations))) {
  loc <- combinations$location[i]
  sx <- combinations$sex[i]
  cs <- combinations$cause[i]
  
  df_filtered <- compare %>% 
    filter(location == loc, sex == sx, cause == cs) %>%
    pivot_longer(cols = c(values_assigned, val_gbd),
                 names_to = "variable",
                 values_to = "value")
  
  plot <- ggplot(df_filtered, aes(x = factor(AgeGroup), y = value, fill = variable)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    scale_fill_manual(values = c("values_assigned" = "blue", "val_gbd" = "red"),
                      labels = c("Values Assigned", "Val GBD")) +
    labs(title = paste("Location:", loc, "| Sex:", sx, "| Cause:", cs),
         x = "Age Group",
         y = "Values",
         fill = "Legend") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("images/manchester/prevalence/plot_", loc, "_", sx, "_", cs, ".png"),
         plot = plot, width = 8, height = 6)
}

