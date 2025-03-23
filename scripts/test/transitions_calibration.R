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
synth_pop <- read.csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_rr_2021.csv")

# Zones data

zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv")) %>%
  rename(zone = oaID)

# Disease prevalence data
prevalence <- read.csv("manchester/health/processed/health_transitions_manchester_prevalence.csv") %>%
  mutate(prob = 1 - exp(-rate))  # Convert rates to probabilities

# Join synthetic population with zones
synth_pop <- synth_pop %>%
  left_join(zones, by = "zone") %>%
  rename(sex = gender)

###  STEP 3: Assign Disease Prevalence  ###

# Assign prevalence to synthetic population
synth_pop_wprob <- synth_pop %>%
  select(id, age, sex, ladcd, ladnm,  starts_with("rr")) %>%
  rownames_to_column() %>%
  left_join(
    prevalence %>%
      pivot_wider(id_cols = c(age, sex, location_code), names_from = cause, values_from = prob),
    by = c("age", "sex", "ladcd" = "location_code")
  )

# Function to allocate disease statuses based on probability
set.seed(123)

allocate_disease <- function(df) {
  df %>%
    mutate(across(copd:stroke, ~ ifelse(runif(n()) < ., 1, 0), .names = "{.col}_status"))
}

# Apply function to assign diseases
synth_pop_prev <- allocate_disease(synth_pop_wprob) %>%
  select(id, age, sex, ladcd, ladnm, ends_with("_status"), starts_with("RR"))  %>%# Ensure we only keep relevant columns
  # mutate(sex = case_when(
  # sex == 1 ~ "male",
  # sex == 2 ~ "female",
  # TRUE ~ as.character(sex))) %>%
  mutate(
    `endometrial_cancer_status` = ifelse(sex == "male", 0, `endometrial_cancer_status`),
    `breast_cancer_status` = ifelse(sex == "male", 0, `breast_cancer_status`)
  )


# Sample 1000 individuals for calculations
# sample <- sample_n(synth_pop_prev, 1000)

### Start pif funciton here so we can check prevalence allocation

sample <- synth_pop_prev


## DISEASE RRs ##

disease_risk <- read.csv("health/disease risks.csv")

# Expand disease_risk_expanded by duplicating "male female" rows
disease_risk_expanded <- disease_risk %>%
  filter(sex == "male female") %>%
  select(-sex) %>%   
  crossing(sex = c("male", "female")) %>%  
  bind_rows(disease_risk %>% filter(sex %in% c("male", "female")))

# Convert sex in sample_long from numeric (1,2) to character ("male", "female")
sample_long <- sample %>%
  rename(
    "parkinsons_disease_status" = "parkinson’s_disease_status"
  ) %>%
   mutate(sex = case_when(
     sex == 1 ~ "male",
     sex == 2 ~ "female",
     TRUE ~ as.character(sex)  # Keep other values unchanged
   )) %>%
  pivot_longer(cols = c(copd_status:stroke_status), names_to = "risk_factor", values_to = "has_risk_factor")


# Join sample_long with expanded disease risk data
sample_with_rr <- sample_long %>%
  mutate(risk_factor = gsub("_status$", "", risk_factor)) %>%
  left_join(disease_risk_expanded, by = c("sex", "risk_factor")) %>%
  
  # Modify outcome for cancer-related risk factors
  mutate(outcome = if_else(
    grepl("cancer|myeloid_leukemia|myeloma", risk_factor, ignore.case = TRUE), 
    "all_cause_mortality", 
    outcome
  )) %>%
  
  # Join again to fetch relative risks for cancer-related risk factors
  left_join(
    disease_risk_expanded %>% filter(grepl("cancer|myeloid_leukemia|myeloma", risk_factor, ignore.case = TRUE)),  
    by = c("sex", "outcome"),  
    suffix = c("", "_cancer")  
  ) %>%
  
  # Assign relative risk values only if age is between 40 and 70
  mutate(relative_risk = case_when(
    age >= 40 & age <= 70 & has_risk_factor == 1 & !grepl("cancer|myeloid_leukemia|myeloma", risk_factor, ignore.case = TRUE) ~ relative_risk,
    age >= 40 & age <= 70 & has_risk_factor == 1 & grepl("cancer|myeloid_leukemia|myeloma", risk_factor, ignore.case = TRUE) ~ coalesce(relative_risk_cancer, 1),
    TRUE ~ 1  # Assign neutral RR (1) if age is outside the 40-70 range
  )) %>%
  
  # Create column names for pivoting (CHECK RISK_FACTORS AND OUTCOMES PAIR, chd MISSING)
  mutate(risk_outcome = paste0("rr_", risk_factor, "^", outcome)) %>%
  
  # Select only relevant columns (remove risk_factor and outcome before pivoting)
  select(id, age, sex, risk_outcome, relative_risk) %>%
  
  # Pivot wider to reduce dataset length and fill missing values with 1
  pivot_wider(
    names_from = risk_outcome, 
    values_from = relative_risk,
    values_fill = list(relative_risk = 1))

# Modify column names to facilitate pif calculations

sample <-  sample %>% select(!contains("status"))

# Assuming 'sample' is your data frame

# Clean column names (lowercase, replace hyphens with underscores)
colnames(sample) <- gsub("-", "_", tolower(colnames(sample)))

# Get the current column names
col_names <- colnames(sample)

# Define the keywords
keywords <- c("air_pollution_no2", "air_pollution_pm25", "physical_activity", "noise", "ndvi")

# Create a more robust pattern
pattern <- paste0("(", paste0(keywords, collapse = "|"), ")_(.*)")

# Replace the matched pattern with the keyword followed by a caret
new_col_names <- gsub(pattern, "\\1\\^\\2", col_names)

# Assign the modified column names back to the data frame
colnames(sample) <- new_col_names

# print(colnames(sample))  # Check the result

# Assign the modified column names back to the data frame
colnames(sample) <- new_col_names

# Merge with sample
sample <- sample %>%
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex)  # Keep other values unchanged
  )) %>% left_join(sample_with_rr, by = c("id", "sex", "age")) %>%
  select(id, age, sex, starts_with("rr"))

# Convert to data long

data_long_rr <- sample %>% 
  pivot_longer(
    cols = starts_with("rr_"),  # Select all columns that start with "rr_"
    names_to = c("risk_type", "outcome"),  
    names_pattern = "rr_([^\\^]+)\\^(.*)",  # Extract text before and after ^
    values_to = "relative_risk"
  ) %>% drop_na() 


### Calculate pifs by age and sex (then add areas, need to bring back to data)
pif_ind <- data_long_rr %>%
  pivot_wider(
    names_from = risk_type,  # Create separate columns for each risk type
    values_from = relative_risk,  # Fill values with relative risk
    values_fill = list(relative_risk = 1)) %>%
  rename(pm25 = air_pollution_pm25, 
         no2 = air_pollution_no2)

### Calculate group Pif


library(DBI)
library(duckdb)

# Ensure the DuckDB connection is properly reset
if (exists("con") && dbIsValid(con)) {
  dbDisconnect(con, shutdown = TRUE)
}

# Connect to DuckDB
con <- dbConnect(duckdb::duckdb(), ":memory:")

# Check if `pif_ind` exists before proceeding
if (!exists("pif_ind")) stop("Dataset 'pif_ind' is missing. Please load it first.")

# Register the dataset with DuckDB
duckdb_register(con, "pif_ind", pif_ind)

# Get column names
all_cols <- colnames(pif_ind)

# Identify cancer-related columns
cancer_keywords <- c("cancer", "leukemia", "myeloma")
cancer_cols <- all_cols[grepl(paste(cancer_keywords, collapse = "|"), all_cols, ignore.case = TRUE)]

# Identify non-cancer columns (excluding id, outcome, age, and sex)
non_cancer_cols <- setdiff(all_cols, c(cancer_cols, "id", "outcome", "age", "sex"))

# Construct SQL expressions
cancer_condition <- paste(cancer_cols, "> 1", collapse = " OR ")
cancer_greatest <- paste("GREATEST(", paste(cancer_cols, collapse = ", "), ")")
cancer_coalesce <- paste("COALESCE(NULLIF(", cancer_cols, ", 0), 1)", collapse = " * ")
non_cancer_expr <- paste(non_cancer_cols, collapse = " * ")

# SQL Query
query <- sprintf("
    WITH preprocessed AS (
      SELECT 
        *,  
        -- Create 5-year age groups like [0,5), [5,10), etc.
        CONCAT(
          '[', CAST(FLOOR(age / 5) * 5 AS INT), ',', CAST(FLOOR(age / 5) * 5 + 5 AS INT), ')'
        ) AS age_group,

        -- Create combined sex-age groups like M_[0,5), F_[5,10), etc.
        CONCAT(
          sex, '_', 
          '[', CAST(FLOOR(age / 5) * 5 AS INT), ',', CAST(FLOOR(age / 5) * 5 + 5 AS INT), ')'
        ) AS sex_age_group
      FROM pif_ind
    ),
    disease_rr AS (
      SELECT 
        outcome,
        sex_age_group,
        physical_activity,
        ndvi,
        pm25,
        no2,
        noise,
        CASE 
          WHEN (%s) THEN %s  -- Apply cancer condition
          ELSE 1  -- Default to 1 if no cancer condition met
        END * (%s) AS disease_rr  -- Multiply by non-cancer factors
      FROM preprocessed
    ),
    transformed AS (
      SELECT 
        sex_age_group,
        outcome,
        COUNT(*) AS total_pop,
        SUM(physical_activity) AS sum_rr_pa,
        SUM(ndvi) AS sum_rr_ndvi,
        SUM(pm25) AS sum_rr_pm25,
        SUM(no2) AS sum_rr_no2,
        SUM(noise) AS sum_rr_noise,
        SUM(disease_rr) AS sum_rr_disease,
        SUM(
          COALESCE(NULLIF(physical_activity, 0), 1) * 
          COALESCE(NULLIF(ndvi, 0), 1) * 
          COALESCE(NULLIF(pm25, 0), 1) * 
          COALESCE(NULLIF(no2, 0), 1) * 
          COALESCE(NULLIF(noise, 0), 1) * 
          disease_rr
        ) AS sum_rr_individual
      FROM disease_rr
      GROUP BY sex_age_group, outcome
    ),
    pif_calc AS (
      SELECT
        sex_age_group,
        outcome,
        1 - (total_pop / NULLIF(sum_rr_pa, 0)) AS pif_pa,
        1 - (total_pop / NULLIF(sum_rr_ndvi, 0)) AS pif_ndvi,
        1 - (total_pop / NULLIF(sum_rr_pm25, 0)) AS pif_pm25,
        1 - (total_pop / NULLIF(sum_rr_no2, 0)) AS pif_no2,
        1 - (total_pop / NULLIF(sum_rr_noise, 0)) AS pif_noise,
        1 - (total_pop / NULLIF(sum_rr_disease, 0)) AS pif_disease
      FROM transformed
    )
    SELECT 
      t.sex_age_group,
      t.outcome,
      p.pif_pa,
      p.pif_ndvi,
      p.pif_pm25,
      p.pif_no2,
      p.pif_noise,
      p.pif_disease,
      1 - (t.total_pop / NULLIF(t.sum_rr_individual, 0)) AS paf_combined_traditional,
      1 - ((1 - p.pif_pa) * (1 - p.pif_pm25) * (1 - p.pif_no2) * 
           (1 - p.pif_noise) * (1 - p.pif_ndvi) * (1 - p.pif_disease)) AS paf_combined_correct
    FROM transformed t
    JOIN pif_calc p 
    ON t.sex_age_group = p.sex_age_group 
    AND t.outcome = p.outcome;", 
                 
                 # Formatting placeholders with actual values
                 cancer_condition, cancer_greatest, cancer_coalesce, non_cancer_expr
)

# Run query safely
tryCatch({
  if (!dbIsValid(con)) stop("Database connection is invalid. Please reconnect.")
  pif_group <- dbGetQuery(con, query)
  print(head(pif_group))
}, error = function(e) {
  message("SQL Error:\n", e$message)
  message("\nGenerated Query:\n", query)
})

# Close connection properly
dbDisconnect(con, shutdown = TRUE)





### The correct pif to use is paf_combined. That can be seen by comparing paf_all and 
# paf_combined and assessing cases with only one risk factor present (or combined env,d disease).
# We should see paf_combined and paf_all as the pif of the individual risk factors, but does
# not happen with paf_all. 

write.csv(pif_group, "manchester/health/processed/pifs_baseline.csv")

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
    sex = case_when(
      sex == 1 ~ "male", 
      sex == 2 ~ "female", 
      TRUE ~ as.character(sex)),
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