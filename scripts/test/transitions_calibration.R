###  STEP 1: Load Required Libraries  ###
library(tidyverse)  
library(dbplyr)    
library(duckdb)    
library(arrow)     
library(here)      
library(drpa)    
library(readxl)    


###  STEP 2: Load Data  ###

# Synthetic population with zones and prevalence calculation
synth_pop <- read.csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_exposure_2021.csv")

# Household & dwelling data
hh <- read_csv(here("manchester/simulationResults/ForUrbanTransition/reference/sp_2021_2050/hh_2021.csv"))
dd <- read_csv(here("manchester/simulationResults/ForUrbanTransition/reference/sp_2021_2050/dd_2021.csv"))
zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv")) %>%
  rename(zone = oaID)

# Disease prevalence data
prevalence <- read.csv("manchester/health/processed/health_transitions_manchester_prevalence.csv") %>%
  mutate(prob = 1 - exp(-rate))  # Convert rates to probabilities

# Join synthetic population with household & zone info
synth_pop <- synth_pop %>%
  left_join(hh, by = c("hhid" = "id")) %>%
  left_join(dd, by = c("dwelling" = "id")) %>%
  left_join(zones, by = "zone") %>%
  rename(sex = gender)

###  STEP 3: Assign Disease Prevalence  ###

# Assign prevalence to synthetic population
synth_pop_wprob <- synth_pop %>%
  select(age, sex, ladcd, ladnm,  mmetHr_walk, mmetHr_cycle, mmetHr_otherSport,
         exposure_normalised_pm25, exposure_normalised_no2) %>%
  rownames_to_column() %>%
  left_join(
    prevalence %>%
      pivot_wider(id_cols = c(age, sex, location_code), names_from = cause, values_from = prob),
    by = c("age", "sex", "ladcd" = "location_code")
  )

# Function to allocate disease statuses based on probability
allocate_disease <- function(df) {
  df %>%
    mutate(across(copd:stroke, ~ ifelse(runif(1) < ., 1, 0), .names = "{.col}_diseased"))
}

# Apply function to assign diseases
synth_pop_prev <- allocate_disease(synth_pop_wprob) %>%
  rename_with(~ gsub("_diseased", "_status", .x), ends_with("_diseased")) %>%  # Append _status instead of removing
  rename(id = rowname) %>%
  select(id, age, sex, ladcd, ladnm, ends_with("_status"), mmetHr_walk, mmetHr_cycle, mmetHr_otherSport,
         exposure_normalised_pm25, exposure_normalised_no2)  %>%# Ensure we only keep relevant columns
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex)))

# Sample 1000 individuals for calculations
# sample <- sample_n(synth_pop_prev, 1000)

### Start pif funciton here so we can check prevalence allocation


sample <- synth_pop_prev

# Compute physical activity variable
sample <- sample %>% mutate(mmets = mmetHr_walk + mmetHr_cycle + mmetHr_otherSport)

###  STEP 4: Assign Relative Risks  ###

DISEASE_SHORT_NAMES <- read_csv("health/disease_outcomes_lookup.csv")

pa_diseases <- DISEASE_SHORT_NAMES %>%
  filter(physical_activity == 1) %>%
  mutate(acronym = gsub("_", "-", acronym))

# Assign PA RRs
for (i in seq_len(nrow(pa_diseases))) {
  disease_acronym <- pa_diseases$acronym[i]
  new_colname <- paste0("rr_physical_activity^", disease_acronym)
  
  # Apply conditions:
  # - Age must be greater than 20
  # - Males should not have 'breast_cancer' or 'endometrial_cancer'
  sample[[new_colname]] <- ifelse(
    sample$age > 20 & !(sample$sex == "male" & disease_acronym %in% c("breast-cancer", "endometrial-cancer")),
    drpa::dose_response(
      cause = disease_acronym,
      outcome_type = case_when(
        disease_acronym == "all-cause-mortality" ~ "fatal",
        disease_acronym == "diabetes" ~ "non-fatal",
        TRUE ~ "fatal-and-non-fatal"
      ),
      dose = sample$mmets,
      confidence_intervals = FALSE
    )$rr,
    1  # Assign 1 instead of NA when conditions are not met
  )
}



## 🌫️ AIR POLLUTION RRs (PM2.5 & NO2) ##
# Function to find the closest value in a vector
find_closest <- function(value, vector) {
  vector[which.min(abs(vector - value))]
}

# Optimized function to assign RRs only if age > 20
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

# Assign RRs for PM2.5 & NO2, greenspace, and noise
sample <- find_rr(sample, read_csv("health/all_cause_pm.csv"), exposure_normalised_pm25, "rr_pm25^all-cause-mortality")
sample <- find_rr(sample, read_csv("health/copd_pm.csv"), exposure_normalised_pm25, "rr_pm25^copd")
sample <- find_rr(sample, read_csv("health/ihd_pm.csv"), exposure_normalised_pm25, "rr_pm25^coronary-heart-disease")
sample <- find_rr(sample, read_csv("health/stroke_pm.csv"), exposure_normalised_pm25, "rr_pm25^stroke")
sample <- find_rr(sample, read_csv("health/lc_pm.csv"), exposure_normalised_pm25, "rr_pm25^lung-cancer")
sample <- find_rr(sample, read_csv("health/all_cause_no.csv"), exposure_normalised_no2, "rr_no2^all-cause-mortality")


### PLACEHOLDER: ADD NOISE AND GREENSPACE


## DISEASE RRs ##

disease_risk <- read.csv("health/disease risks.csv")

# Convert sex in sample_long from numeric (1,2) to character ("male", "female")
sample_long <- sample %>%
  rename(
    "rr_physical_activity^parkinsons-disease" = "rr_physical_activity^parkinson's-disease", 
    "parkinsons_disease_status" = "parkinson’s_disease_status"
  ) %>%
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex)  # Keep other values unchanged
  )) %>%
  pivot_longer(cols = c(copd_status:stroke_status), names_to = "risk_factor", values_to = "has_risk_factor")

# Expand disease_risk_expanded by duplicating "male female" rows
disease_risk_expanded <- disease_risk %>%
  filter(sex == "male female") %>%
  select(-sex) %>%   
  crossing(sex = c("male", "female")) %>%  
  bind_rows(disease_risk %>% filter(sex %in% c("male", "female")))

# Join sample_long with expanded disease risk data
sample_with_rr <- sample_long %>%
  mutate(risk_factor = gsub("_status$", "", risk_factor)) %>%
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
  select(id, age, sex, risk_outcome, relative_risk) %>%
  
  # Pivot wider to reduce dataset length and fill missing values with 1
  pivot_wider(
    names_from = risk_outcome, 
    values_from = relative_risk,
    values_fill = list(relative_risk = 1)
  ) %>% select(where(~ !all(is.na(.))))


# Merge with sample
sample <- left_join(sample, sample_with_rr, by = c("id", "sex", "age")) %>%
  select(id, age, sex, starts_with("rr"))

# Make long

data_long_rr <- sample %>% 
  pivot_longer(
    cols = starts_with("rr_"),  # Select all columns that start with "rr_"
    names_to = c("risk_type", "outcome"),  
    names_pattern = "rr_([^\\^]+)\\^(.*)",  # Extract text before and after ^
    values_to = "relative_risk"
  ) %>% select(where(~ !all(is.na(.)))) # This is to remove rows for which diseases have no risk outcomes. 


# Create a DuckDB connection
# con <- dbConnect(duckdb::duckdb())

# Convert Arrow Table to a data frame (or tibble) for DuckDB
# data_df <- as.data.frame(data_long_rr)

# Copy data to DuckDB (temporary table)
# copy_to(con, data_df, "data_long_rr", temporary = TRUE, overwrite = TRUE)



### Interpretation of negative pif

### Calculate teh PAFs separatly for PA and rest of risk factors as PA is less than one. 
### Combine pafs per outcome as 1-prod(1-pafs)
# Process data in DuckDB
pif_ind <- data_long_rr %>%
  pivot_wider(
    names_from = risk_type,  # Create separate columns for each risk type
    values_from = relative_risk,  # Fill values with relative risk
    values_fill = list(relative_risk = 1)  # Fill missing values with 1 (neutral RR)
  ) # %>% ### added calculation here to check too
  # rowwise() %>%  
  # mutate(pif = prod(c_across(physical_activity:stroke), na.rm = TRUE)) %>%  
  # ungroup() 

# Collect the data back into R after processing in DuckDB
# pif_ind <- collect(pif_ind_db)

library(duckdb)
library(dplyr)

# Connect to DuckDB
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Register data as a DuckDB table
duckdb_register(con, "pif_ind", pif_ind)

# Identify environmental RR columns
env_start <- which(names(pif_ind) == "pm25")
env_end <- which(names(pif_ind) == "no2")

if (length(env_start) == 0 | length(env_end) == 0) {
  stop("Error: Columns 'pm25' and 'no2' must exist in the dataset.")
}

env_cols <- names(pif_ind)[env_start:env_end]
env_mult <- paste(sprintf("COALESCE(%s, 1)", env_cols), collapse = " * ")

# Identify disease RR columns
disease_start <- which(names(pif_ind) == "copd")
disease_end <- which(names(pif_ind) == "stroke")

if (length(disease_start) == 0 | length(disease_end) == 0) {
  stop("Error: Columns 'copd' and 'stroke' must exist in the dataset.")
}

disease_cols <- names(pif_ind)[disease_start:disease_end]
disease_mult <- paste(sprintf("COALESCE(%s, 1)", disease_cols), collapse = " * ")

# Identify all columns from physical_activity to stroke
all_start <- which(names(pif_ind) == "physical_activity")
all_end <- which(names(pif_ind) == "stroke")

if (length(all_start) == 0 | length(all_end) == 0) {
  stop("Error: Columns 'physical_activity' and 'stroke' must exist in the dataset.")
}

all_cols <- names(pif_ind)[all_start:all_end]
all_mult <- paste(sprintf("COALESCE(%s, 1)", all_cols), collapse = " * ")

# Build SQL query
query <- sprintf("
  WITH transformed AS (
    SELECT 
      id,
      sex,
      outcome,
      physical_activity,

      -- Age groups
      CONCAT(
        '[', CAST(FLOOR(age / 5) * 5 AS INT), ',', CAST(FLOOR(age / 5) * 5 + 5 AS INT), ')'
      ) AS age_group,

      CONCAT(
        sex, '_', 
        '[', CAST(FLOOR(age / 5) * 5 AS INT), ',', CAST(FLOOR(age / 5) * 5 + 5 AS INT), ')'
      ) AS age_gender,

      -- Product of environmental RRs
      %s AS env_prodrr,

      -- Product of disease RRs
      %s AS disease_prodrr,

      -- Product of all factors (physical_activity to stroke)
      %s AS all_prodrr

    FROM pif_ind
  ),

  grouped AS (
    SELECT 
      age_gender,
      outcome,
      COUNT(DISTINCT id) AS total_ids,
      SUM(physical_activity) AS sum_pa_rr,
      SUM(env_prodrr) AS sum_env_prodrr,
      SUM(disease_prodrr) AS sum_disease_prodrr,
      SUM(all_prodrr) AS sum_all_prodrr
    FROM transformed
    GROUP BY age_gender, outcome
  )

  SELECT 
    age_gender,
    outcome,
    total_ids,

    -- PAF for physical activity
    1 - (sum_pa_rr / total_ids) AS paf_pa,

    -- PAF for environmental risks (pm25 & no2)
    1 - (total_ids / sum_env_prodrr) AS paf_env,

    -- PAF for diseases (copd to stroke)
    1 - (total_ids / sum_disease_prodrr) AS paf_disease,

    -- PAF for all factors (physical_activity to stroke)
    1 - (total_ids / sum_all_prodrr) AS paf_all,

    -- Combined PAF for physical activity and environment
    1 - ((1 + paf_pa) * (1 - paf_env)) AS paf_pa_env,

    -- Combined PAF for all three factors
    1 - ((1 + paf_pa) * (1 - paf_env) * (1 - paf_disease)) AS paf_combined

  FROM grouped
", env_mult, disease_mult, all_mult)

# Run the query
pif_group <- dbGetQuery(con, query)

# Disconnect DuckDB
dbDisconnect(con, shutdown = TRUE)

# View results
print(pif_group)

### TO DO, some NAs along the process that end up in final file. 


### The correct pif to use is paf_combined. That can be seen by comparing paf_all and 
# paf_combined and assessing cases with only one risk factor present (or combined env,d disease).
# We should see paf_combined and paf_all as the pif of the individual risk factors, but does
# not happen with paf_all. 

write.csv(pif_group, "manchester/health/processed/pifs_baseline.csv")

## Modify transition probabilities

pifs <- read.csv("manchester/health/processed/pifs_baseline.csv")

## Repeat last age groups for age group 95 to 100

pifs_older <- pifs %>% 
  filter(age_gender %in% c("1_[90,95)", "2_[90,95)")) %>%
  mutate(age_gender = case_when(
    age_gender == "1_[90,95)" ~ "1_[95,100]",
    age_gender == "2_[90,95)" ~ "2_[95,100]",
    TRUE ~ age_gender  # Keep other values unchanged (optional)
  ))

pifs <- pifs %>% bind_rows(pifs_older) %>%
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
    age_gender = paste(sex, age_group, sep = "_")
  ) %>%
  left_join(pifs, by = c("cause", "age_gender")) %>%
  mutate(pif = case_when(
    age < 20 ~ 0,  # Assign zero to pif for age < 20
    TRUE ~ pif
  )) %>%
  select(!c("age_group")) %>%
  mutate(rate_adj = rate * (1 - pif)) %>%
  select(!c("age_gender", "pif", "X.x", "X.y", "total_ids", "sum_prodrrs", "rate")) %>%
  rename("rate" = "rate_adj")

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

# write_rds(transition_data, "manchester/health/processed/health_transitions_manchester.RDS")