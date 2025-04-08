### Funciton to calculate pifs

# Scenario specific without rrs (assign using function) 
# General files: Household & dwelling data 

calculate_pif <- function(data) {
  
  
synth_pop <- data

zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv")) %>%
  rename(zone = oaID)

# Disease prevalence data
prevalence <- read.csv("manchester/health/processed/health_transitions_manchester_prevalence.csv") %>%
  mutate(prob = 1 - exp(-rate)) %>% # Convert rates to probabilities
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex)  # This line handles any other cases
  ))


### Assign relative risks if exposures input file

## Exposure risks if working with exposures files

# synth_pop <- synth_pop_with_rr(synth_pop)

### Assign geographies

synth_pop <- synth_pop %>%
  left_join(zones, by = c("zone" = "zone")) 



### Assign Disease Prevalence

# Assign prevalence to synthetic population
synth_pop_wprob <- synth_pop %>% 
  rename(sex=gender) %>%
  mutate(sex = case_when(sex == 1 ~ "male",
                         sex == 2 ~ "female")) %>%
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
  select(id, age, sex, ladcd, ladnm, ends_with("_status"), starts_with("RR"))  %>%# Ensure we only keep relevant columns
  # mutate(sex = case_when(
  # sex == 1 ~ "male",
  # sex == 2 ~ "female",
  # TRUE ~ as.character(sex))) %>%
  mutate(
    `endometrial_cancer_status` = ifelse(sex == "male", 0, `endometrial_cancer_status`),
    `breast_cancer_status` = ifelse(sex == "male", 0, `breast_cancer_status`)
  )



# Sample 1000 individuals for calculations (TO RUN FULL SAMPLE FROM HERE, DO NOT ALLOCATE RRS AGAIN)

# sample <- sample_n(synth_pop_prev, 1000)

sample <- synth_pop_prev


## DISEASE RRs ##

disease_risk <- read.csv("health/disease_risks.csv")

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
  # mutate(sex = case_when(
  #   sex == 1 ~ "male",
  #   sex == 2 ~ "female",
  #   TRUE ~ as.character(sex)  # Keep other values unchanged
  # )) %>%
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

# Create an easier patterns for pif calculations
pattern <- paste0("(", paste0(keywords, collapse = "|"), ")_(.*)")

# Replace the matched pattern with the keyword followed by a caret
new_col_names <- gsub(pattern, "\\1\\^\\2", col_names)

# Assign the modified column names back to the data frame
colnames(sample) <- new_col_names

# print(colnames(sample))  # Check the result

# Assign the modified column names back to the data frame
colnames(sample) <- new_col_names

# Merge with sample
sample <- left_join(sample, sample_with_rr, by = c("id", "sex", "age")) %>%
  select(id, age, sex, starts_with("rr"))

# Convert to data long

data_long_rr <- sample %>% 
  pivot_longer(
    cols = starts_with("rr_"),  # Select all columns that start with "rr_"
    names_to = c("risk_type", "outcome"),  
    names_pattern = "rr_([^\\^]+)\\^(.*)",  # Extract text before and after ^
    values_to = "relative_risk"
  ) %>% drop_na() 




### Calculate pifs by age and sex 

pif_ind <- data_long_rr %>%
  pivot_wider(
    names_from = risk_type,  # Create separate columns for each risk type
    values_from = relative_risk,  # Fill values with relative risk
    values_fill = list(relative_risk = 1))  %>% # Fill missing values with 1 (neutral RR)
  rename(pm25 = air_pollution_pm25, 
         no2 = air_pollution_no2)


## pif groups 

library(data.table)

# Convert pif_ind to data.table if it's not already
setDT(pif_ind)

# Identify cancer-related columns
cancer_keywords <- c("cancer", "leukemia", "myeloma")
cancer_cols <- grep(paste(cancer_keywords, collapse = "|"), names(pif_ind), value = TRUE, ignore.case = TRUE)

# Identify non-cancer columns (excluding id, outcome, age, sex, physical activity and exposures)
non_cancer_cols <- setdiff(names(pif_ind), c(cancer_cols, "id", "outcome", "age", "sex", "pm25", "no2", "ndvi", "noise", "physical_activity", "age_group", "sex_age_group", "disease_rr"))

# Create age groups and sex_age_groups
pif_ind[, `:=`(
  age_group = sprintf("[%d,%d)", 5 * floor(age/5), 5 * (floor(age/5) + 1)),
  sex_age_group = sprintf("%s_[%d,%d)", sex, 5 * floor(age/5), 5 * (floor(age/5) + 1))
)]

# Calculate disease_rr
pif_ind[, disease_rr := do.call(pmax, c(.SD, list(1))), .SDcols = cancer_cols]
pif_ind[, disease_rr := disease_rr * Reduce(`*`, .SD), .SDcols = non_cancer_cols]


options(scipen = 999)
# Calculate group PIFs
pif_group <- pif_ind[, .(
  total_pop = .N,
  sum_rr_pa = sum(physical_activity),
  sum_rr_ndvi = sum(ndvi),
  sum_rr_pm25 = sum(pm25),
  sum_rr_no2 = sum(no2),
  sum_rr_noise = sum(noise),
  sum_rr_disease = sum(disease_rr),
  sum_rr_individual = sum(physical_activity * ndvi * pm25 * no2 * noise * disease_rr)
), by = .(sex_age_group, outcome)]  # change to sex_age_group if we want by age and sex groups

# Calculate PIFs
pif_group[, `:=`(
  pif_pa = 1 - (total_pop / sum_rr_pa),
  pif_ndvi = 1 - (total_pop / sum_rr_ndvi),
  pif_pm25 = 1 - (total_pop / sum_rr_pm25),
  pif_no2 = 1 - (total_pop / sum_rr_no2),
  pif_noise = 1 - (total_pop / sum_rr_noise),
  pif_disease = 1 - (total_pop / sum_rr_disease),
  paf_combined_traditional = 1 - (total_pop / sum_rr_individual)
)]

pif_group[, paf_combined_correct := 1 - ((1 - pif_pa) * (1 - pif_pm25) * (1 - pif_no2) * 
                                           (1 - pif_noise) * (1 - pif_ndvi) * (1 - pif_disease))]

}