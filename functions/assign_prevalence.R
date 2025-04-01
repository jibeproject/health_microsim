#### Assign disease state for baseline population

library(tidyverse)  
library(here)      

# Synthetic population baseline
synth_pop <- read.csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_rr_2021.csv")

# Zones data

zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv")) %>%
  rename(zone = oaID)

# Disease prevalence data
prevalence <- read.csv("manchester/health/processed/health_transitions_manchester_prevalence.csv") %>%
  mutate(prob = 1 - exp(-rate))  # Convert rates to probabilities

### Assign geographies

synth_pop <- synth_pop %>%
  left_join(zones, by = c("zone" = "zone")) 

### Join prevalence rates to synthetic population

# Assign prevalence to synthetic population
synth_pop_wprob <- synth_pop %>% 
  rename(sex=gender) %>%
  select(id, age, sex, ladcd, ladnm,  starts_with("rr")) %>%
  rownames_to_column() %>%
  left_join(
    prevalence %>%
      pivot_wider(id_cols = c(age, sex, location), names_from = cause, values_from = prob),
    by = c("age", "sex", "ladnm" = "location")
  )

# Function to allocate disease statuses based on probability

set.seed(123)

allocate_disease <- function(df) {
  df %>%
    mutate(across(copd:stroke, ~ ifelse(runif(n()) < ., 1, 0), .names = "{.col}_status"))
}

# Apply function to assign diseases
synth_pop_prev <- allocate_disease(synth_pop_wprob) %>%
  select(id, age, sex, ladcd, ladnm, ends_with("_status"), starts_with("rr"))  %>%# Ensure we only keep relevant columns
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex)))

# Save

write_csv(synth_pop_prev, "manchester/health/processed/synth_pop_prev_2021.csv")
