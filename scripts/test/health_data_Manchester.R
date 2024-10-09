# Diseases data for Greater Manchester
# Data is from the GBD for incidence, prevalence and mortality. 
# Data is by 5 year age groups, sex and local district area (10 in GM)
# This code is to interpolate 5 year age group data to 1 year age group and match names of diseases to Cambridge PA MA

rm(list = ls())

library(tidyverse)
library(readxl)
library(readr)
library(esquisse) #ai graphs tool

# functions
source("docs/functions/interpolation.R")

# Avoid scientific notation
options("scipen"=100, "digits"=4)


# GBD data for Manchester 
gbd <- bind_rows(
  read.csv("manchester/health/original/GBD/IHME-GBD_2021_data-bfdf8007-1.csv"), 
  read.csv("manchester/health/original/gbd/IHME-GBD_2021_DATA-125d4b23-1.csv"), 
  read.csv("manchester/health/original/gbd/IHME-GBD_2021_DATA-1316cd33-1.csv"),
  read.csv("manchester/health/original/gbd/IHME-GBD_2021_DATA-cffadb37-1.csv"),
  read.csv("manchester/health/original/gbd/IHME-GBD_2021_DATA-131cf659-1.csv"),
  read.csv("manchester/health/original/gbd/IHME-GBD_2021_DATA-4fbe0549-1.csv"), 
  read.csv("manchester/health/original/gbd/parkinsons1.csv"),
  read.csv("manchester/health/original/gbd/parkinsons3.csv"))

# Filter data to needed variables and remove strings from GBD age variable and create from_age and to_age needed for interpolation. 

gbdp <- gbd %>%
  filter(metric %in% "Rate") %>%
  select(-c(upper, lower)) %>%
  filter(!age %in% "All ages") %>%
  mutate(rate_1=val/100000) %>% 
  # some ages have 'years' (eg 5-9 years), while others don't (eg 80-84); omit 'years'
  mutate(age = gsub(" years", "", age)) %>%
  tidyr::extract(age, c("from_age", "to_age"), "(.+)-(.+)", remove=FALSE, convert=TRUE) %>%
  mutate(from_age = case_when(age=="95+"  ~  95L,
                              age=="<5"  ~  0L,
                              TRUE  ~  from_age),
         to_age = case_when(age=="95+"  ~  99L,
                            age=="<5"  ~  4L,
                            TRUE  ~  to_age),
         agediff = to_age - from_age + 1,
         val1yr = rate_1) %>% 
  #we do not distribute among age groups as it is a rate but assume same within age group
  rename(agegroup = age) 

# Data preparation to match diseases and names from Cambridge physical activity meta-analysis

# Some specific cancers need adjustment to match GBD data to standardised
# JIBE disease list. Apart from name changes, the following cancers need transformation:
  
#  -   Priority: Head and neck cancer: sum of Larynx cancer + Lip and oral cavity cancer + Nasopharynx cancer + Other pharynx cancer

#  -   Less priority: Colon cancer, Rectum cancer: Colon and rectum cancer, apportioned based on ? incidence/prevalence rates

# -   Less priority: Lung cancer: Tracheal, bronchus, and lung cancer, apportioned based on comparison of ?

gbdp <- gbdp %>%
  filter (cause %in% c("Stroke", "Ischemic heart disease", "Breast cancer", 
                       "Uterine cancer", "Tracheal, bronchus, and lung cancer", 
                       "Colon and rectum cancer", "Esophageal cancer", 
                       "Liver cancer",  
                       "Stomach cancer", "Chronic myeloid leukemia", "Multiple myeloma", 
                       "Larynx cancer", "Lip and oral cavity cancer", "Nasopharynx cancer",
                       "Other pharynx cancer", "Bladder cancer",  
                        "Depressive disorders",  "Alzheimer's disease and other dementias", 
                       "Diabetes mellitus type 2", "Chronic obstructive pulmonary disease", "Parkinson's disease")) %>%
  mutate(cause = case_when(
    cause == "Ischemic heart disease"    ~ "coronary_heart_disease",
    cause == "Uterine cancer"            ~ "endometrial_cancer",
    cause == "Stomach cancer"            ~ "gastric_cardia_cancer",
    cause == "Chronic myeloid leukemia"  ~ "myeloid_leukemia",
    cause == "Multiple myeloma"          ~ "myeloma",
    cause == "Depressive disorders" ~ "depression",
    cause == "Alzheimer's disease and other dementias" ~ "all_cause_dementia",
    cause == "Diabetes mellitus type 2"  ~ "diabetes",
    cause == "Stroke" ~ "stroke", 
    cause == "Tracheal, bronchus, and lung cancer" ~ "lung_cancer", 
    cause == "Breast cancer" ~ "breast_cancer", 
    cause == "Colon and rectum cancer" ~ "colon_cancer", 
    cause == "Bladder cancer" ~ "bladder_cancer", 
    cause == "Esophageal cancer" ~ "esophageal_cancer",
    cause == "Liver cancer" ~ "liver_cancer", 
    cause == "Chronic obstructive pulmonary disease" ~ "COPD",
    cause == "Parkinson's disease"  ~ "parkinson’s_disease",
    .default = cause))


# Sum rates for head an neck cancers
hanc <- c("Larynx cancer", "Lip and oral cavity cancer", "Nasopharynx cancer",
          "Other pharynx cancer")

gbdp_hanc <- gbdp %>%
  filter(cause %in% hanc) %>%
  group_by(measure, location, sex, agegroup, from_age, to_age, metric, year, agediff) %>%
  summarise(val = sum(val),
            rate_1 = sum(rate_1),
            val1yr = sum(val1yr),.groups = "drop") %>%
  mutate(cause = "head_and_neck_cancer") %>% 
  select(1:6,13,7,8,10,11,9,12)

gbdp <- gbdp %>%
  filter(!cause %in% hanc) %>%
  bind_rows(gbdp_hanc) 

# split rates for colon and rectum cancers by reference to the AIHW incidence
# rates for those two diseases
# AIHW incidence proportions for colon and rectum cancer
# inc_crc <- incyr %>% 
#   filter(site %in% c("colon cancer", "rectum cancer")) %>%
#   dplyr::select(site, sex, ageyr, rate_1) %>%
#   mutate(site = case_when(site == "colon cancer"  ~ "colon",
#                           site == "rectum cancer" ~ "rectum"),
#          sex = case_when(sex == "Females" ~ "Female",
#                          sex == "Males"   ~ "Male")) %>%
#   pivot_wider(names_from = site, values_from = rate_1) %>%
#   # total of the two rates (with small constant to avoid zeros)
#   mutate(total = colon + rectum) %>%
#   mutate(total = ifelse(total == 0, 1e-6, total)) %>%
#   # proportions
#   mutate(colon_prop = colon/total,
#          rectum_prop = rectum/total) %>%
#   dplyr::select(sex, age = ageyr, colon_prop, rectum_prop)
# 
# # apportion colon and rectum cancer by rates
# prev_agesex_crc <-
#   prev_agesex_all %>%
#   filter(cause == "Colon and rectum cancer") %>%
#   # join the proportions and calculate proportionate rates
#   left_join(inc_crc, by = c("sex", "age")) %>%
#   mutate(Colon = rate * colon_prop,
#          Rectum = rate * rectum_prop) %>%
#   # pivot into separate rows for colon and rectum
#   dplyr::select(year, age, sex, Colon, Rectum) %>%
#   pivot_longer(cols = c("Colon", "Rectum"), names_to = "cause",
#                names_transform = ~ paste(., "cancer"),
#                values_to = "rate")
# 
# prev_agesex_all <- prev_agesex_all %>%
#   filter(!cause == "Colon and rectum cancer") %>%
#   bind_rows(prev_agesex_crc)
# 
# 
# # lung cancer: use incidence numbers from AIHW (lung) and GBD (tracheal, bronchus
# # and lung) to determine a proportion for lung, and apply to GBD prevalence (tracheal,
# # bronchus and lung)
# lung_proportion <- incyr_interp %>%  # AIHW incidence - use 2018 to match GBD
#   filter(year == 2018 & site == "lung cancer") %>%
#   mutate(sex = case_when(sex == "Females" ~ "Female", sex == "Males" ~ "Male")) %>%
#   dplyr::select(sex, ageyr, lung_val = val_interpolated) %>%
#   left_join(gbdpyr_interp %>%  # GBD incidence for 2018
#               filter(measure == "Incidence" & year == 2018) %>%
#               filter(cause == "Tracheal, bronchus, and lung cancer") %>%
#               dplyr::select(sex, ageyr, tbalc_val = val_interpolated),
#             by = c("sex", "ageyr")) %>%
#   mutate(lung_prop = lung_val / tbalc_val)
# 
# # plot the respective rates
# ggplot(lung_proportion %>%
#          rename(lung_val_AIHW = lung_val, tbalc_val_GBD = tbalc_val) %>%
#          pivot_longer(cols = c("lung_val_AIHW", "tbalc_val_GBD"), 
#                       names_to = "site", values_to = "value") %>%
#          mutate(sex_site = paste0(sex, "_", site))) +
#   geom_line(aes(x = ageyr, y = value, colour = sex_site), lwd = 1) +
#   scale_color_brewer(palette = "Paired") +
#   labs(title = "AIHW lung cancer incidence vs GBD tbalc incidence",
#        x = "Age (years)",
#        y = "Incidence rate (per 100,000)") +
#   theme_minimal()
# 
# # multiply tbalc_rate by lung_prop to get lung rate - but some years have higher 
# # lung than tbalc, so in that case just use the tbalc figure
# prev_agesex_lung <- prev_agesex_all %>%
#   filter(cause == "Tracheal, bronchus, and lung cancer") %>%
#   rename(tbalc_rate = rate) %>%
#   left_join(lung_proportion %>%
#               dplyr::select(sex, age = ageyr, lung_prop),
#             by = c("sex", "age")) %>%
#   mutate(rate = ifelse(lung_prop < 1, tbalc_rate * lung_prop, tbalc_rate)) %>%
#   dplyr::select(-tbalc_rate, -lung_prop) %>%
#   mutate(cause = "Lung cancer")
# 
# prev_agesex_all <- prev_agesex_all %>%
#   filter(!cause == "Tracheal, bronchus, and lung cancer") %>%
#   bind_rows(prev_agesex_lung)
# 
# # filter to 2018 values
# prev_agesex <- prev_agesex_all %>%
#   filter(year == 2018) %>%
#   dplyr::select(-year)
# 
# 
# 

# Now stretch the data out using an index, to create a data frame with 1 row per year of age and create a variable for year of age. 
# The age group rate repeats within single years of age in the group. 
index <- rep(1:nrow(gbdp), gbdp$agediff)
gbdpyrd5 <- gbdp[index,] %>%
  mutate(ageyr = from_age + sequence(gbdp$agediff) - 1)
gbdpyrd5 <- gbdpyrd5 %>% 
  select(measure, ageyr, sex, agegroup, from_age, to_age, cause, year, val1yr,rate_1, location)

# Apply the disaggregation function

# Group data for dissaggregation

gbdp_grp <- gbdp %>%
  group_by(measure, year, sex, cause, location) %>%
  arrange(measure, year, sex, cause, from_age)

# Apply function (select a function from 'functions/interpolation.R'). Smooth_spline does a good job.
gbdpyr_interp <- group_modify(gbdp_grp, disagg_smooth_spline) %>%
  ungroup()

# Add LAD code
lad <- fread('manchester/health/original/ons/lsoa_to_msoa.csv')
lad <- lad %>%
  select(6,7) %>%
  distinct()

gbdpyr_interp <- gbdpyr_interp %>%
  left_join(lad, by = c("location" = "LAD20NM"))

saveRDS(gbdpyr_interp, "manchester/health/processed/manchester_diseases_lad.RDS")

write.csv(gbdpyr_interp, "manchester/health/processed/manchester_diseases_lad.csv")

#Join with original data where rates are the same within groups to validate interpolated data
gbdpyr_interp <- gbdpyr_interp %>%
  left_join(gbdpyrd5, by = c("measure", "year", "ageyr", "sex", "cause", "location"))

# Plot data to check interpolated values against 5-year age group values (original data)
## Four interpolation function could be use, the best fit should be used. For now, best fit to the data assessed visually.

# Melt the data to long format for ggplot
plot_data <- gbdpyr_interp %>%
  select(measure, cause, sex, ageyr, rate_1, val_interpolated, location) %>%
  pivot_longer(cols = c(rate_1, val_interpolated), names_to = "type", values_to = "value")


library(ggplot2)
library(dplyr)

# Assuming df is already loaded in your environment

# Define output directory
output_dir <- "images/manchester/inc_gbd_age_sex/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Get unique combinations of measure, cause, sex, and location
combinations <- plot_data %>%
  distinct(measure, cause, sex, location)

# Generate and save plots for each combination
for (i in 1:nrow(combinations)) {
  subset_df <- plot_data %>%
    filter(measure == combinations$measure[i],
           cause == combinations$cause[i],
           sex == combinations$sex[i],
           location == combinations$location[i])
  
  p <- ggplot(subset_df, aes(x = ageyr, y = value, color = type)) +
    geom_line() +
    labs(title = paste("Measure:", combinations$measure[i], 
                       "Cause:", combinations$cause[i], 
                       "Sex:", combinations$sex[i],
                       "Location:", combinations$location[i]),
         x = "Age (years)",
         y = "Value") +
    theme_minimal(base_family = "Arial") +
    theme(panel.background = element_rect(fill = "white", color = "white"),
          plot.background = element_rect(fill = "white", color = "white"))
  
  # Save the plot
  ggsave(filename = paste0(output_dir, "plot_", i, ".png"), plot = p, width = 8, height = 6)
}