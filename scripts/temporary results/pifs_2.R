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
source("functions/pif_calc.R")

##### Read files

# Scenario specific data with rrs (input might be with exposures or with exposures and rrs)
# Change per scenario

reference <- read_csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_rr_2021.csv")

safestreet <- read_csv("manchester/simulationResults/ForPaper/2_safestreet/health/04_exposure_and_rr/pp_rr_2021.csv")

green <- read_csv("manchester/simulationResults/ForPaper/3_green/health/04_exposure_and_rr/pp_rr_2021.csv")

both <- read_csv("manchester/simulationResults/ForPaper/4_both/health/04_exposure_and_rr/pp_rr_2021.csv")

# Calculate pifs by sex (needs synthetic population with rrs)

ref_pifs <- calculate_pif(reference)

write.csv(ref_pifs, "manchester/health/processed/pif_reference_sex.csv")

safe_pif <- calculate_pif(safestreet)

write.csv(safe_pif, "manchester/health/processed/pif_safestreet_sex.csv")

green_pif <- calculate_pif(green)

write.csv(green_pif, "manchester/health/processed/pif_green_sex.csv")

both_pif <- calculate_pif(both)

write.csv(both_pif, "manchester/health/processed/pif_both_sex.csv")

##### Figures and tables for comparing scenarios pifs, burden and distributions ####

## Get saved pifs and burden

pif_ref <- read_csv("manchester/health/processed/pif_reference_sex.csv") %>% mutate(scenario = "reference")

pif_safe <- read_csv("manchester/health/processed/pif_safestreet_sex.csv") %>% mutate(scenario = "safer")

pif_green <- read_csv("manchester/health/processed/pif_green_sex.csv") %>% mutate(scenario = "green")

pif_both <- read_csv("manchester/health/processed/pif_both_sex.csv") %>% mutate(scenario = "both")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)  # For percentage formatting

pif_compare <- bind_rows(pif_ref, pif_safe, pif_green, pif_both) %>% 
              rename(pif_combined = paf_combined_traditional)

# Gather the data to long format for easier plotting
df_long <- pif_compare %>%
  pivot_longer(cols = c(pif_pa, pif_ndvi, pif_pm25, pif_no2, pif_noise, pif_disease, pif_combined), 
               names_to = "pif",  
               values_to = "value") 

#### PIFs plots ####

# Define unique outcomes and sexes
causes <- unique(pif_compare$cause)
sexes <- unique(pif_compare$sex)

# Create a list to store plots
plots <- list()

# Define directory and ensure it exists
output_dir <- "manchester/health/processed/preliminary_results/pifs/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  
}

for (cause in causes) {
  for (sex in sexes) {
    # Filter and order scenarios, and remove burden types with all zero values
    df_filtered <- df_long %>% 
      filter(cause == !!cause, sex == !!sex) %>%
      group_by(pif) %>%
      filter(any(value != 0)) %>%  # Keep only burden types with non-zero values
      ungroup() %>%
      mutate(scenario = factor(scenario, levels = c("reference", "safer", "green", "both")))
    
    # Calculate reference values for each pif type
    ref_values <- df_filtered %>% 
      filter(scenario == "reference") %>% 
      group_by(pif) %>%
      summarise(ref_value = value * 100) %>%
      ungroup()
    
    # Create plot with burden breakdown
    p <- ggplot(df_filtered, aes(x = scenario, y = value * 100, fill = pif)) +
      geom_col(position = position_dodge(width = 0.8)) +
      # Add reference lines for each pif type, excluding lines at y=0
      geom_hline(data = ref_values %>% filter(ref_value != 0), aes(yintercept = ref_value, color = pif),
                 linetype = "dashed", linewidth = 0.8) +
      labs(title = paste("Cause:", cause, "- Sex:", sex), 
           y = "PIF Values (%)", 
           fill = "Burden Type",
           color = "Reference Lines") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom"
      )
    
    plots[[paste(cause, sex, sep = "_")]] <- p
    ggsave(file.path(output_dir, paste0(cause, "_", sex, ".png")), p, width = 10, height = 6, dpi = 300)
  }
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

### fix to comapre all sceanrios
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
  mutate(safer_diff = ceiling(reference - safer),
         green_diff = ceiling (reference - green),
         both_diff = ceiling (reference - both)) %>%  # Calculate difference
  filter(reference != 0) %>% 
   select(cause, sex, burden, safer_diff, green_diff, both_diff) #%>% 
  # arrange(cause, sex, burden) %>% 
  # pivot_wider(
  #   names_from = "burden",
  #   values_from = safer_diff:both_diff
  # ) %>% 
  # mutate(across(everything(), ~replace_na(., 0))) %>%  # Replace all NAs with 0

write.csv(burden_diff, "manchester/health/processed/cra_age_sex.csv")

##### Graphs: Compare exposures reference and scenario and overlay relative risks #####

library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(htmlwidgets)

data <- burden_diff %>% filter(!burden %in% "burden_disease")


# Reshape the data for plotting
df_long <- data %>%
  pivot_longer(cols = c(safer_diff, green_diff, both_diff), 
               names_to = "diff_type", 
               values_to = "value")

# Create the ggplot for females
plot_female <- ggplot(df_long %>% filter(sex == "female"), aes(x = cause, y = value, fill = diff_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ burden, scales = "free") +
  labs(title = "Burden Type Differences by Cause for Females", x = "Cause", y = "Difference Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the ggplot for males
plot_male <- ggplot(df_long %>% filter(sex == "male"), aes(x = cause, y = value, fill = diff_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ burden, scales = "free") +
  labs(title = "Burden Type Differences by Cause for Males", x = "Cause", y = "Difference Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert ggplot to plotly for interactivity
interactive_plot_female <- ggplotly(plot_female)
interactive_plot_male <- ggplotly(plot_male)

# Print the interactive plots
interactive_plot_female
interactive_plot_male

saveWidget(interactive_plot_female, "manchester/health/processed/preliminary_results/interactive_plot_female.html")
saveWidget(interactive_plot_male, "manchester/health/processed/preliminary_results/interactive_plot_male.html")

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

