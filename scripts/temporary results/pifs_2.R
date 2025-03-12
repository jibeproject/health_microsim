## Temporary results

## Libraries

library(tidyverse)  
library(dbplyr)    
library(duckdb)    
library(arrow)     
library(here)      
library(drpa)    
library(readxl)    

# source("functions/compute_pif.R") to be done

##### Read files

# Scenario specific
## Reference (all mmets are zero)

reference <- read_csv("manchester/simulationResults/ForPaper/2_safestreet/health/04_exposure_and_rr/pp_rr_2021.csv")

reference_exp <- read_csv("manchester/simulationResults/ForPaper/2_safestreet/health/04_exposure_and_rr/pp_exposure_2021.csv")

# General files: Household & dwelling data 

zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv")) %>%
  rename(zone = oaID)

# Disease prevalence data
prevalence <- read.csv("manchester/health/processed/health_transitions_manchester_prevalence.csv") %>%
  mutate(prob = 1 - exp(-rate))  # Convert rates to probabilities

# ## For older data we need to add zones to the synthetic population. Zones are in ForPaper results
# hh <- read_csv("manchester/simulationResults/ForUrbanTransition/reference/sp_2021_2050/hh_2021.csv")
# dd <- read_csv("manchester/simulationResults/ForUrbanTransition/reference/sp_2021_2050/dd_2021.csv")
# 
# synth_pop <- synth_pop %>%
#   left_join(hh, by = c("hhid" = "id")) %>%
#   left_join(dd, by = c("dwelling" = "id"))

### Syntehtic population for scenario

synth_pop <- reference

### Assign geographies

synth_pop <- synth_pop %>%
  left_join(zones, by = c("zone" = "zone")) 

## For missing exposures give exposure level for which rr are 1 (this is just to manage missing exposures)

# synth_pop <- synth_pop %>% mutate(exposure_normalised_noise = 0,
#                                    exposure_normalised_green = 0)

###  STEP 3: Assign Disease Prevalence  ###

# Assign prevalence to synthetic population
synth_pop_wprob <- synth_pop %>% 
  rename(sex = gender) %>%
  rownames_to_column() %>%
  left_join(
    prevalence %>%
      pivot_wider(id_cols = c(age, sex, location_code), names_from = cause, values_from = prob),
    by = c("age", "sex", "ladcd" = "location_code")) %>%
    select(!rowname)

# Function to allocate disease statuses based on probability
allocate_disease <- function(df) {
  df %>%
    mutate(across(copd:stroke, ~ ifelse(runif(n()) < ., 1, 0), .names = "{.col}_diseased"))
}

# Apply function to assign diseases
synth_pop_prev <- allocate_disease(synth_pop_wprob) %>%
  select(id, age, sex, ladcd, ladnm, ends_with("_diseased"), income, ethnicity, starts_with("RR"))  %>%# Ensure we only keep relevant columns
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex)))

# Sample 1000 individuals for calculations
# sample <- sample_n(synth_pop_prev, 1000)

sample <- synth_pop_prev
#
# Compute physical activity variable
# sample <- sample %>% mutate(mmets = mmetHr_walk + mmetHr_cycle + mmetHr_otherSport)


### Apply pif function from here

# pifs_groups_reference <- compute_pif(sample)

###  STEP 4: Assign Relative Risks  ###

# DISEASE_SHORT_NAMES <- read_csv("health/disease_outcomes_lookup.csv")

# pa_diseases <- DISEASE_SHORT_NAMES %>%
#   filter(physical_activity == 1) %>%
  # mutate(acronym = gsub("_", "-", acronym)) 

# Assign PA RRs
# for (i in seq_len(nrow(pa_diseases))) {
#   disease_acronym <- pa_diseases$acronym[i]
#   new_colname <- paste0("rr_physical_activity^", disease_acronym)
#   
#   # Apply conditions:
#   # - Age must be greater than 20
#   # - Males should not have 'breast_cancer' or 'endometrial_cancer'
#   sample[[new_colname]] <- ifelse(
#     sample$age > 20 & !(sample$sex == "male" & disease_acronym %in% c("breast-cancer", "endometrial-cancer")),
#     drpa::dose_response(
#       cause = disease_acronym,
#       outcome_type = case_when(
#         disease_acronym == "all-cause-mortality" ~ "fatal",
#         disease_acronym == "diabetes" ~ "non-fatal",
#         TRUE ~ "fatal-and-non-fatal"
#       ),
#       dose = sample$mmets,
#       confidence_intervals = FALSE
#     )$rr,
#     1  # Assign 1 instead of NA when conditions are not met
#   )
# }



## 🌫Exposures RRs (PM2.5, NO2, noise and greenspace) ##
# Function to find the closest value in a vector
# find_closest <- function(value, vector) {
#   vector[which.min(abs(vector - value))]
# }
# 
# # Optimized function to assign RRs only if age > 20
# find_rr <- function(df_1, df_2, exposure_name, new_col_name) {
# 
#   df_1 %>%
#     rowwise() %>%
#     mutate(!!new_col_name := ifelse(
#       age > 20,  # Apply RR only if age > 20
#       {
#         closest_dose <- find_closest({{exposure_name}}, df_2$dose)
#         df_2$rr[df_2$dose == closest_dose]
#       },
#       1  # Assign neutral RR (1) if age <= 20
#     )) %>%
#     ungroup()
# }
# 
# # Assign RRs for PM2.5 & NO2, greenspace, and noise
# sample <- find_rr(sample, read_csv("health/all_cause_pm.csv"), exposure_normalised_pm25, "rr_pm25^all-cause-mortality")
# sample <- find_rr(sample, read_csv("health/copd_pm.csv"), exposure_normalised_pm25, "rr_pm25^copd")
# sample <- find_rr(sample, read_csv("health/ihd_pm.csv"), exposure_normalised_pm25, "rr_pm25^coronary-heart-disease")
# sample <- find_rr(sample, read_csv("health/stroke_pm.csv"), exposure_normalised_pm25, "rr_pm25^stroke")
# sample <- find_rr(sample, read_csv("health/lc_pm.csv"), exposure_normalised_pm25, "rr_pm25^lung-cancer")
# sample <- find_rr(sample, read_csv("health/all_cause_no.csv"), exposure_normalised_no2, "rr_no2^all-cause-mortality")
# sample <- find_rr(sample, read_csv("health/diabetes_ndvi.csv"), exposure_normalised_ndvi, "rr_ndvi^diabetes")
# sample <- find_rr(sample, read_csv("health/stroke_ndvi.csv"), exposure_normalised_ndvi, "rr_ndvi^stroke")
# sample <- find_rr(sample, read_csv("health/all_cause_ndvi.csv"), exposure_normalised_ndvi, "rr_ndvi^all-cause-mortality")
# sample <- find_rr(sample, read_csv("health/all_cause_noise.csv"), exposure_normalised_noise, "rr_noise^all-cause-mortality")
# sample <- find_rr(sample, read_csv("health/diabates_noise.csv"), exposure_normalised_noise, "rr_noise^diabetes")
# sample <- find_rr(sample, read_csv("health/ihd_noise.csv"), exposure_normalised_noise, "rr_noise^coronary-heart-disease")
# sample <- find_rr(sample, read_csv("health/stroke_noise.csv"), exposure_normalised_noise, "rr_noise^stroke")


## DISEASE RRs ##

disease_risk <- read.csv("health/disease risks.csv")

# Convert sex in sample_long from numeric (1,2) to character ("male", "female")
sample_long <- sample %>%
  rename(
    "parkinsons_disease_diseased" = "parkinson’s_disease_diseased"
  ) %>%
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex)  # Keep other values unchanged
  )) %>%
  pivot_longer(cols = c(copd_diseased:stroke_diseased), names_to = "risk_factor", values_to = "has_risk_factor") 
  

# Expand disease_risk_expanded by duplicating "male female" rows
disease_risk_expanded <- disease_risk %>%
  filter(sex == "male female") %>%
  select(-sex) %>%   
  crossing(sex = c("male", "female")) %>%  
  bind_rows(disease_risk %>% filter(sex %in% c("male", "female")))

# Join sample_long with expanded disease risk data
sample_with_rr <- sample_long %>%
  mutate(risk_factor = gsub("_diseased$", "", risk_factor)) %>%
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
  ) %>% select(!matches("na", ignore.case = TRUE))


# Merge with sample
sample <- left_join(sample, sample_with_rr, by = c("id", "sex", "age")) %>%
  select(id, age, sex, starts_with("rr"))

# Make long

colnames(sample) <- gsub("-", "_", tolower(colnames(sample)))

# Get the current column names
col_names <- colnames(sample)

# Define the keywords
keywords <- c("no2", "pm25", "physical_activity", "noise", "ndvi")

# Create a pattern that matches an underscore following any of the keywords
pattern <- paste0("(", paste(keywords, collapse = "|"), ")_")

# Replace the matched pattern with the keyword followed by a caret
new_col_names <- gsub(pattern, "\\1^", col_names)

# Assign the modified column names back to the data frame
colnames(sample) <- new_col_names

# Convert to data long

data_long_rr <- sample %>% 
  pivot_longer(
    cols = starts_with("rr_"),  # Select all columns that start with "rr_"
    names_to = c("risk_type", "outcome"),  
    names_pattern = "rr_([^\\^]+)\\^(.*)",  # Extract text before and after ^
    values_to = "relative_risk"
  ) %>% select(where(~ !all(is.na(.))))  # This is to remove rows for which diseases have no risk outcomes. 




### Calculate pifs by age and sex (then add areas, need to bring back to data)
# Create a DuckDB connection


### Calculate teh PAFs separatly for PA and rest of risk factors as PA is less than one. 
### Combine pafs per outcome as 1-prod(1-pafs)
# Process data in DuckDB
pif_ind <- data_long_rr %>%
  pivot_wider(
    names_from = risk_type,  # Create separate columns for each risk type
    values_from = relative_risk,  # Fill values with relative risk
    values_fill = list(relative_risk = 1))  %>%# Fill missing values with 1 (neutral RR)
    rename(pm25 = air_pollution_pm25, 
           no2 = air_pollution_no2)
    
    
library(duckdb)

# Create 5-year age groups
pif_ind$age_group <- cut(pif_ind$age, breaks = seq(0, 100, by = 5), right = FALSE, 
                         labels = paste(seq(0, 95, by = 5), seq(5, 100, by = 5), sep = "-"))

# Combine 'age_group' with 'sex' to create 'sex_age_group'
pif_ind$sex_age_group <- paste(pif_ind$age_group, pif_ind$sex, sep = "_")

# Connect to DuckDB
con <- dbConnect(duckdb::duckdb(), ":memory:")
duckdb_register(con, "pif_ind", pif_ind)

# Identify disease-related columns dynamically
disease_cols <- names(pif_ind)
disease_start <- match("copd", disease_cols)
disease_end <- match("stroke", disease_cols)

if (is.na(disease_start) | is.na(disease_end)) {
  stop("Error: 'copd' and/or 'stroke' columns are missing in the dataset.")
}

disease_cols <- disease_cols[disease_start:disease_end]

# Construct SQL expression for the product of disease RRs
disease_mult <- paste(sprintf("COALESCE(NULLIF(%s, 0), 1)", disease_cols), collapse = " * ")
disease_product_expr <- sprintf("(%s)", disease_mult)

# Final SQL query with detailed PIF and PAF for each risk factor by sex_age_group and outcome
query <- sprintf("
    WITH transformed AS (
      SELECT 
        sex,
        outcome,
        COUNT(*) AS total_pop,
        SUM(physical_activity) AS sum_rr_pa,
        SUM(ndvi) AS sum_rr_ndvi,
        SUM(pm25) AS sum_rr_pm25,
        SUM(no2) AS sum_rr_no2,
        SUM(noise) AS sum_rr_noise,
        SUM(%s) AS sum_rr_disease,

        -- Multiply the relative risks for each individual and sum them
        SUM(
          COALESCE(NULLIF(physical_activity, 0), 1) * 
          COALESCE(NULLIF(ndvi, 0), 1) * 
          COALESCE(NULLIF(pm25, 0), 1) * 
          COALESCE(NULLIF(no2, 0), 1) * 
          COALESCE(NULLIF(noise, 0), 1) * 
          %s
        ) AS sum_rr_individual
      FROM pif_ind
      GROUP BY sex, outcome
    ),
    pif_calc AS (
      SELECT
        sex,
        outcome,
        -- PIF for each risk factor (handle potential division by zero)
        1 - (total_pop / NULLIF(sum_rr_pa, 0)) AS pif_pa,
        1 - (total_pop / NULLIF(sum_rr_ndvi, 0)) AS pif_ndvi,
        1 - (total_pop / NULLIF(sum_rr_pm25, 0)) AS pif_pm25,
        1 - (total_pop / NULLIF(sum_rr_no2, 0)) AS pif_no2,
        1 - (total_pop / NULLIF(sum_rr_noise, 0)) AS pif_noise,
        1 - (total_pop / NULLIF(sum_rr_disease, 0)) AS pif_disease
      FROM transformed
    )
    SELECT 
      t.sex,
      t.outcome,

      -- PIF for each risk factor
      p.pif_pa,
      p.pif_ndvi,
      p.pif_pm25,
      p.pif_no2,
      p.pif_noise,
      p.pif_disease,

      -- Traditional PIF for combined risks
      1 - (t.total_pop / NULLIF(t.sum_rr_individual, 0)) AS paf_combined_traditional,  

      -- Corrected PIF using product of combined risks
      1 - ((1 - p.pif_pa) * (1 - p.pif_pm25) * (1 - p.pif_no2) * (1 - p.pif_noise) * (1 - p.pif_ndvi)) AS paf_combined_correct,


    FROM transformed t
    JOIN pif_calc p
    ON t.sex = p.sex AND t.outcome = p.outcome;
  ", disease_product_expr, disease_product_expr)


# Run the query
pif_group <- dbGetQuery(con, query)

# Disconnect DuckDB
dbDisconnect(con, shutdown = TRUE)

write.csv(pif_group, "manchester/health/processed/pif_safer_streets_sex.csv")


### Figures and tables (save csv files)



library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)  # For percentage formatting

df <- read_csv("manchester/health/processed/pif_sex_presentation.csv")

# Load necessary library
library(dplyr)

####

# Gather the data to long format for easier plotting
df_long <- df %>%
  pivot_longer(cols = c(pif_pa, pif_pm25, pif_no2, paf_combined_correct), 
               names_to = "pif",  # Ensure consistency
               values_to = "value")

# Add a column for the percentage difference between reference and cycling scenarios
df_long <- df_long %>%
  group_by(outcome, sex, pif) %>%
  mutate(difference_percentage = (value[scenario == "cycling"] - value[scenario == "reference"]) / value[scenario == "reference"] * 100) %>%
  ungroup()

# Define unique outcomes and sexes
outcomes <- unique(df$outcome)
sexes <- unique(df$sex)

# Create a list to store plots
plots <- list()

# Define directory and ensure it exists
output_dir <- "images/manchester/pifs/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  
}

# Loop through each outcome and sex to create and save plots
for (outcome in outcomes) {
  for (sex in sexes) {
    # Filter the data for the current outcome and sex
    df_filtered <- df_long %>% filter(outcome == !!outcome, sex == !!sex)
    
    # Create the plot with a white background
    p <- ggplot(df_filtered, aes(x = scenario, y = value * 100, fill = pif)) +  # Multiply by 100 for percentage
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste("Outcome:", outcome, "- Sex:", sex), y = "PIF Values (%)", fill = "Scenario") +
      scale_y_continuous(labels = label_percent(scale = 1)) +  # Set y-axis as percentages
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),  # White background
        plot.background = element_rect(fill = "white", color = NA),   # White outer background
        legend.background = element_rect(fill = "white", color = NA)  # White legend background
      ) +
      # Optionally add text labels for the difference in percentages
      geom_text(aes(label = sprintf("%.1f%%", difference_percentage)), vjust = -0.5, color = "black", size = 3)
    
    # Add the plot to the list
    plot_name <- paste(outcome, sex, sep = "_")
    plots[[plot_name]] <- p
    
    # Save the plot as a PNG file
    ggsave(filename = file.path(output_dir, paste0(plot_name, ".png")), 
           plot = p, width = 8, height = 6, dpi = 300)
  }
}

# Print all plots
for (plot in plots) {
  print(plot)
}
