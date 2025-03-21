## Temporary results

## Libraries

library(tidyverse)  
library(dbplyr)    
library(duckdb)    
library(arrow)     
library(here)      
library(drpa)    
library(readxl)    

source("functions/assing_rrs.R")

##### Read files

# Scenario specific data with rrs (input might be with exposures or with exposures and rrs)
# Change per scenario

# reference <- read_csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_rr_2021.csv")

safestreet <- read_csv("manchester/simulationResults/ForPaper/2_safestreet/health/04_exposure_and_rr/pp_rr_2021.csv")

# Change per scenario
synth_pop <- safestreet

# Scenario specific without rrs (assign using function) 
# General files: Household & dwelling data 

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

synth_pop <- safestreet

### Assign relative risks if exposures input file

## Exposure risks if working with exposures files

synth_pop <- synth_pop_with_rr(synth_pop)

### Assign geographies

synth_pop <- synth_pop %>%
  left_join(zones, by = c("zone" = "zone")) 



### Assign Disease Prevalence

# Assign prevalence to synthetic population
synth_pop_wprob <- synth_pop %>% 
  rename(sex=gender) %>%
  mutate(sex = case_when(sex == 1 ~ "male", 
                         sex == 2 ~ "female")) %>%
  rownames_to_column() %>%
  left_join(
    prevalence %>%
      pivot_wider(id_cols = c(age, sex, location_code), names_from = cause, values_from = prob),
    by = c("age", "sex", "ladcd" = "location_code")) %>%
    select(!rowname)

# Function to allocate disease statuses based on probability

set.seed(123)

allocate_disease <- function(df) {
  df %>%
    mutate(across(copd:stroke, ~ ifelse(runif(n()) < ., 1, 0), .names = "{.col}_status"))
}

# Apply function to assign diseases
synth_pop_prev <- allocate_disease(synth_pop_wprob) %>%
  select(id, age, sex, ladcd, ladnm, ends_with("_status"), income, ethnicity, starts_with("RR"))  %>%# Ensure we only keep relevant columns
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex))) %>%
      mutate(
        `endometrial_cancer_status` = ifelse(sex == "male", 0, `endometrial_cancer_status`),
        `breast_cancer_status` = ifelse(sex == "male", 0, `breast_cancer_status`)
      )
  


# Sample 1000 individuals for calculations (TO RUN FULL SAMPLE FROM HERE, DO NOT ALLOCATE RRS AGAIN)

# sample <- sample_n(synth_pop_prev, 1000)

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




### Calculate pifs by age and sex (then add areas, need to bring back to data)
# Create a DuckDB connection


### Calculate teh PAFs separatly for PA and rest of risk factors as PA is less than one. 
### Combine pafs per outcome as 1-prod(1-pafs)
# Process data in DuckDB
pif_ind <- data_long_rr %>%
  pivot_wider(
    names_from = risk_type,  # Create separate columns for each risk type
    values_from = relative_risk,  # Fill values with relative risk
    values_fill = list(relative_risk = 1))  %>% # Fill missing values with 1 (neutral RR)
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
      1 - ((1 - p.pif_pa) * (1 - p.pif_pm25) * (1 - p.pif_no2) * (1 - p.pif_noise) * (1 - p.pif_ndvi) * (1 - p.pif_disease)) AS paf_combined_correct,


    FROM transformed t
    JOIN pif_calc p
    ON t.sex = p.sex AND t.outcome = p.outcome;
  ", disease_product_expr, disease_product_expr)


# Run the query
pif_group <- dbGetQuery(con, query)

# Disconnect DuckDB
dbDisconnect(con, shutdown = TRUE)

write.csv(pif_group, "manchester/health/processed/pif_saferstreet_sex.csv")



##### Figures comparing PIFs #####

pif_ref <- read_csv("manchester/health/processed/pif_reference_sex.csv") %>% mutate(scenario = "reference")

pif_safe <- read_csv("manchester/health/processed/pif_saferstreet_sex.csv") %>% mutate(scenario = "safer")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)  # For percentage formatting


# Load necessary library
library(dplyr)

####

pif_compare <- bind_rows(pif_ref, pif_safe) %>% 
              rename(pif_combined = paf_combined_traditional)

# Gather the data to long format for easier plotting
df_long <- pif_compare %>%
  pivot_longer(cols = c(pif_pa, pif_ndvi, pif_pm25, pif_no2, pif_noise, pif_disease, pif_combined), 
               names_to = "pif",  # Ensure consistency
               values_to = "value") 

# Add a column for the percentage difference between reference and cycling scenarios (revise, not very intuitive)
df_long <- df_long %>%
  group_by(outcome, sex, pif) %>%
  mutate(difference_percentage = (value[scenario == "safer"] - value[scenario == "reference"]) / value[scenario == "reference"] * 100) %>%
  ungroup()

# Define unique outcomes and sexes
outcomes <- unique(pif_compare$outcome)
sexes <- unique(pif_compare$sex)

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
      labs(title = paste("Outcome:", outcome, "- Sex:", sex), y = "PIF Values ", fill = "Scenario") +
      scale_y_continuous(labels = label_percent(scale = 1)) +  # Set y-axis as percentages
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),  # White background
        plot.background = element_rect(fill = "white", color = NA),   # White outer background
        legend.background = element_rect(fill = "white", color = NA)  # White legend background
      ) #+
      # Optionally add text labels for the difference in percentages
      #geom_text(aes(label = sprintf("%.1f%%", difference_percentage)), vjust = -0.5, color = "black", size = 3)
    
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



### TO DO: 1) GRAPHS OTHER EXPOSURES; 2) COMPARE AGAINTS ORIGINAL DISTRIBUTIONS, 3) PROCESS TO SAVE LONG DATA TO THEN COMPARE BETWEEN SCANRIOS. 
# 5) plot distribution variable

##### Calculate burden of disease by area, sex and outcome type #####

### Deaths total for Greater Manchester by sex

# Import data from ONS
deaths_males <- read_xlsx(
  here("manchester/health/original/ons", "DeathsbyLSOAmidyear11to21.xlsx"),
  sheet = "2",
  skip = 2
) %>% 
  filter(`Mid-year` %in% c(2018)) %>%
  filter(
    str_starts(`Local Authority name`, "Bolton") |
      str_starts(`Local Authority name`, "Bury") |
      str_starts(`Local Authority name`, "Manchester") |
      str_starts(`Local Authority name`, "Oldham") |
      str_starts(`Local Authority name`, "Rochdale") |
      str_starts(`Local Authority name`, "Salford") |
      str_starts(`Local Authority name`, "Stockport") |
      str_starts(`Local Authority name`, "Tameside") |
      str_starts(`Local Authority name`, "Trafford") |
      str_starts(`Local Authority name`, "Wigan"))

deaths_GM_male <- deaths_males %>% 
  mutate(total_deaths = rowSums(across(`Males under 1`:`Males over 85`), na.rm = TRUE)) %>%
  summarise(total_val=sum(total_deaths)) %>%
  mutate(cause="all_cause_mortality",
         sex = "male")

deaths_females <- read_xlsx(
  here("manchester/health/original/ons", "DeathsbyLSOAmidyear11to21.xlsx"),
  sheet = "3",
  skip = 2
) %>% 
  filter(`Mid-year` %in% c(2018)) %>%
  filter(
    str_starts(`Local Authority name`, "Bolton") |
      str_starts(`Local Authority name`, "Bury") |
      str_starts(`Local Authority name`, "Manchester") |
      str_starts(`Local Authority name`, "Oldham") |
      str_starts(`Local Authority name`, "Rochdale") |
      str_starts(`Local Authority name`, "Salford") |
      str_starts(`Local Authority name`, "Stockport") |
      str_starts(`Local Authority name`, "Tameside") |
      str_starts(`Local Authority name`, "Trafford") |
      str_starts(`Local Authority name`, "Wigan"))

deaths_GM_female <- deaths_females %>% 
  mutate(total_deaths = rowSums(across(`Females under 1`:`Females over 85`), na.rm = TRUE)) %>%
  summarise(total_val=sum(total_deaths)) %>%
  mutate(cause="all_cause_mortality",
         sex = "female")

## Disease incidence total for Greater Manchester by sex

# Load and merge GBD datasets
gbd_files <- list.files(path = "manchester/health/original/gbd/", pattern = "*.csv", full.names = TRUE)
gbd <- bind_rows(lapply(gbd_files, read.csv))

# Process GBD data
gbdp <- gbd %>%
  filter(metric == "Number", measure == "Incidence", year == 2018, age != "All ages") %>%
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


incidence_males <- gbdp %>% 
  filter(sex == "male") %>%
  group_by(cause) %>% 
  summarise(total_val = sum(val, na.rm = TRUE)) %>%
  mutate(sex = "male")

incidence_females <- gbdp %>% 
  filter(sex == "female") %>%
  group_by(cause) %>% 
  summarise(total_val = sum(val, na.rm = TRUE)) %>%
  mutate(sex = "female")


burden_GM <- bind_rows(deaths_GM_female, deaths_GM_male, incidence_females, incidence_males)

## Calculate burden

pif_compare <- pif_compare %>% rename(cause=outcome)

burden_pif <- left_join(burden_GM, pif_compare, by = c("sex", "cause")) %>%
  select(!c(paf_combined_correct, `...1`)) %>%
  mutate(burden_pa = total_val*pif_pa,
         burden_ndvi=total_val*pif_ndvi,
         burden_pm25=total_val*pif_pm25,
         burden_noise=total_val*pif_noise,
         burden_no2=total_val*pif_no2,
         burden_disease=total_val*pif_disease,
         burden_total=total_val*pif_combined)


burden_diff <- burden_pif %>% 
  select(cause, sex, scenario, burden_pa:burden_total) %>%
  pivot_longer(
    cols = burden_pa:burden_total,
    names_to = "burden",
    values_to = "scenario_value"
  ) %>% 
  pivot_wider(
    names_from = "scenario",
    values_from = "scenario_value"
  ) %>% 
  mutate(diff = ceiling(reference - safer)) %>%  # Calculate difference
  filter(reference != 0) %>% 
  select(cause, sex, burden, cases_prevented = diff) %>% 
  arrange(cause, sex, burden) %>% 
  pivot_wider(
    names_from = "burden",
    values_from = "cases_prevented"
  ) %>% 
  mutate(across(everything(), ~replace_na(., 0))) %>%  # Replace all NAs with 0
  relocate(burden_pa, .after = burden_total)  # Swap burden_total and burden_pa columns

write.csv(burden_diff, "manchester/health/processed/cra_age_sex.csv")


##### Graphs: Compare exposures reference and scenario and overlay relative risks #####

# Reattached exposures to study patterns

# pop_exposures <- synth_pop %>% select(id, mmets, exposure_normalised_pm25, exposure_normalised_no2, 
                                      # exposure_normalised_noise_Lden, exposure_normalised_ndvi)

# sample <- sample %>% left_join(pop_exposures)

# write_csv(sample, "manchester/health/processed/sample_with_rr.csv") # for additional checking in excel

##### Plot rrs mmets and exposures against assign rrs #####

reference_exp <- read_csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_exposure_2021.csv") %>%
  mutate(mmets=mmetHr_walk + mmetHr_cycle + mmetHr_otherSport) %>% 
  mutate(scen = "reference")

safestreet_exp <- read_csv("manchester/simulationResults/ForPaper/2_safestreet/health/04_exposure_and_rr/pp_exposure_2021.csv") %>%
  mutate(mmets=mmetHr_walk + mmetHr_cycle + mmetHr_otherSport) %>%
  mutate(scen="safestreets")

## Compare distributions

### mmets

exp_mmets_compare <- bind_rows(reference_exp, safestreet_exp) %>% select(age, gender, mmets, scen) %>%
  filter(age > 20)

library(ggplot2)

plot_mmets_compare <- ggplot(exp_mmets_compare, aes(x = mmets, fill = scen)) +
  geom_density(alpha = 0.7) +
  labs(x = "MMETs", y = "Density", title = "Distribution of MMETs by Scenario") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


plot_mmets_compare


### noise

exp_noise_compare <- bind_rows(reference_exp, safestreet_exp) %>% 
  select(age, gender, exposure_normalised_noise_Lden, scen) %>%
  filter(age > 20)

# Calculate stats
stats <- exp_noise_compare %>%
  group_by(scen) %>%
  summarize(
    mean = mean(exposure_normalised_noise_Lden, na.rm = TRUE),
    median = median(exposure_normalised_noise_Lden, na.rm = TRUE),
    min = min(exposure_normalised_noise_Lden, na.rm = TRUE),
    max = max(exposure_normalised_noise_Lden, na.rm = TRUE)
  )

# Create the plot
plot_noise_compare <- ggplot(exp_noise_compare, aes(x = exposure_normalised_noise_Lden, fill = scen)) +
  geom_density(alpha = 0.7) +
  geom_vline(data = stats, aes(xintercept = mean, color = scen), linetype = "dashed", linewidth = 1) +
  labs(x = "Noise Lden", y = "Density", title = "Distribution of Noise Lden by Scenario") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

plot_noise_compare

## pm25

exp_pm25_compare <- bind_rows(reference_exp, safestreet_exp) %>% 
  select(age, gender, exposure_normalised_pm25, scen) %>%
  filter(age > 20)

# Calculate stats
stats <- exp_pm25_compare %>%
  group_by(scen) %>%
  summarize(
    mean = mean(exposure_normalised_pm25, na.rm = TRUE),
    median = median(exposure_normalised_pm25, na.rm = TRUE),
    min = min(exposure_normalised_pm25, na.rm = TRUE),
    max = max(exposure_normalised_pm25, na.rm = TRUE)
  )

# Create the plot
plot_pm25_compare <- ggplot(exp_pm25_compare, aes(x = exposure_normalised_pm25, fill = scen)) +
  geom_density(alpha = 0.7) +
  geom_vline(data = stats, aes(xintercept = mean, color = scen), linetype = "dashed", linewidth = 1) +
  labs(x = "PM25", y = "Density", title = "Distribution of pm25 by Scenario") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(xlim = c(7, 20)) # Adjust xlim as needed

plot_pm25_compare

##no2

exp_no2_compare <- bind_rows(reference_exp, safestreet_exp) %>% 
  select(age, gender, exposure_normalised_no2, scen) %>%
  filter(age > 20)

# Calculate stats
stats <- exp_no2_compare %>%
  group_by(scen) %>%
  summarize(
    mean = mean(exposure_normalised_no2, na.rm = TRUE),
    median = median(exposure_normalised_no2, na.rm = TRUE),
    min = min(exposure_normalised_no2, na.rm = TRUE),
    max = max(exposure_normalised_no2, na.rm = TRUE)
  ) 

# Create the plot
plot_no2_compare <- ggplot(exp_no2_compare, aes(x = exposure_normalised_no2, fill = scen)) +
  geom_density(alpha = 0.7) +
  geom_vline(data = stats, aes(xintercept = mean, color = scen), linetype = "dashed", linewidth = 1) +
  labs(x = "No2", y = "Density", title = "Distribution of NO2 by Scenario") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(xlim = c(10, 70)) #

plot_no2_compare

##ndvi

exp_ndvi_compare <-  bind_rows(reference_exp, safestreet_exp) %>% 
  select(age, gender, exposure_normalised_ndvi, scen) %>%
  filter(age > 20)

# Calculate stats
stats <- exp_ndvi_compare %>%
  group_by(scen) %>%
  summarize(
    mean = mean(exposure_normalised_ndvi, na.rm = TRUE),
    median = median(exposure_normalised_ndvi, na.rm = TRUE),
    min = min(exposure_normalised_ndvi, na.rm = TRUE),
    max = max(exposure_normalised_ndvi, na.rm = TRUE)
  ) 

# Create the plot
plot_ndvi_compare <- ggplot(exp_ndvi_compare, aes(x = exposure_normalised_ndvi, fill = scen)) +
  geom_density(alpha = 0.7) +
  geom_vline(data = stats, aes(xintercept = mean, color = scen), linetype = "dashed", linewidth = 1) +
  labs(x = "NDVI", y = "Density", title = "Distribution of NDVI by Scenario") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

plot_ndvi_compare


library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

# Reshape the data
sample_plots_long_mmets <- sample %>%
  pivot_longer(
    cols = starts_with("rr_PHYSICAL_ACTIVITY"),
    names_to = "variable",
    values_to = "value"
  ) %>%
  # Filter out male observations for specific cancer types
  filter(!(variable %in% c("rr_PHYSICAL_ACTIVITY_endometrial-cancer", 
                           "rr_PHYSICAL_ACTIVITY_breast-cancer") & 
             sex == "male"))

# Create the ggplot
p <- ggplot(sample_plots_long_mmets, aes(x = mmets, y = value, color = variable)) +
  geom_line() +
  labs(x = "mmets", y = "RR Physical Activity", color = "Outcome") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convert to interactive plotly graph
plot_mmets_interactive <- ggplotly(p, tooltip = c("x", "y", "color"))

# Improve layout
plot_mmets_interactive <- plot_mmets_interactive %>% 
  layout(legend = list(orientation = "h", y = -0.2),
         hovermode = "closest")

# Display the interactive plot
plot_mmets_interactive

#### Plot exposures ####

## Plot rrs mmets and exposures against assign rrs. 

# Reshape the data
sample_plots_long_exposures <- sample_plots %>%
  pivot_longer(
    cols = starts_with("rr_AIR_POLLUTION_PM25"),
    names_to = "variable",
    values_to = "value"
  )



# Create the ggplot
p <- ggplot(sample_plots_long_exposures, aes(x = exposure_normalised_pm25, y = value, color = variable)) +
  geom_line() +
  labs(x = "Exposure normalised PM25", y = "RR PM25", color = "Outcome") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Convert to interactive plotly graph
plot_exposures_interactive <- ggplotly(p, tooltip = c("x", "y", "color"))

# Improve layout
plot_mmets_interactive <- plot_exposures_interactive %>% 
  layout(legend = list(orientation = "h", y = -0.2),
         hovermode = "closest")

# Display the interactive plot
plot_exposures_interactive

# Plot distribution and overlay with risk. I think of more interest for comparisons. Move to the end
# Save files at the end to be able to do comparisons, overlay distributions reference and scenario. 

library(ggplot2)

# Create the base density plot
plot_pm25 <- ggplot(sample_plots_long_exposures, aes(x = exposure_normalised_pm25)) +
  geom_density(fill = "skyblue", alpha = 0.5, color = "black") +  # Add color for clarity
  labs(
    x = "Exposure Normalised PM2.5 (Dose)",
    y = "Density",
    title = "Distribution of Exposure Normalised PM2.5 with Risk Ratio Overlay"
  ) +
  theme_minimal()



# Overlay the risk ratio line plot

rr_data <- read_csv("health/all_cause_pm.csv")

p_pm25_risk <- plot_pm25 +
  geom_line(data = rr_data, aes(x = dose, y = rr * 0.1), color = "red", size = 1) +   # Scale to fit
  scale_y_continuous(
    name = "Density",
    sec.axis = sec_axis(~./0.1, name="Risk Ratio") # The number after the dot is a scaling factor
  )

p_pm25_risk

