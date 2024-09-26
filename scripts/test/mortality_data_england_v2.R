# setup 
rm(list = ls())
packages <- c("tidyverse",
              "haven",
              "dplyr",
              "ggplot2",
              "data.table",
              "readxl")
lapply(packages, library, character.only = TRUE)

# Disable scientific notation globally
options(scipen = 999)

# Data for England
# Mortality and Population Data by LSOA by age and sex (Average for 2016-2019). BZD: is this 2026-2019 (4 years) or 2017-2019 (3 years)
# [Source]: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/1028deathregistrationsbysexfiveyearagegroupandlowerlayersuperoutputareaslsoa2011censusboundariesenglandandwales2001to2021
deaths_england <- fread('manchester/health/processed/ons/deaths_england.csv')
population_england <- fread('manchester/health/processed/ons/population_england.csv')

# Geographical Hierarchy (as of 2021)
# [Source]: https://geoportal.statistics.gov.uk/datasets/e7c49b62898a417192a336aca17e3a3f/about
msoa <- fread('manchester/health/original/ons/lsoa_to_msoa.csv')

# Life Tables for England (2017-2019)
# [Source]: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandreferencetables/current
eng <- fread('manchester/health/original/ons/life_tables_20172019.csv',skip = 5)

# English Indices of Deprivation by LSOA (2019)
# [Source]: https://opendatacommunities.org/resource?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices
deprivation <- fread('manchester/health/original/ons/imd2019lsoa.csv') 

## Mortality by year of age and sex, for England (not by LSOAs)

# The mortality rate `mx` is defined as "the number of deaths at age x 
# last birthday in the three year period (2016-2019) 
# divided by the average population at that age over the same period."

eng <- eng[ , -c(3,7,8,10)] 


mf <- rep(c("male","female"),each=4)
names1 <- rep(c("rate","denom","deaths","le"), 2)
names(eng) <- c("age",paste(mf, names1, sep="_"))

eng <- eng %>% 
  mutate(male_personyears = round(male_denom - male_deaths),
         female_personyears = round(female_denom - female_deaths)) %>% 
  select(
    age, male_denom, male_rate, male_personyears, male_le,
    female_denom, female_rate, female_personyears, female_le)

england_lifetable <- eng |>
  pivot_longer(-age, 
               names_to = c("sex","measure"),
               names_sep = "_") |>
  pivot_wider(names_from="measure", values_from=value) |>
  mutate(rate1000 = rate*1000,
         sex = factor(sex,
                      labels=stringr::str_to_title(sort(unique(.data$sex)))),
         sex = relevel(sex, "Male"))

## Mortality by LSOA small areas in England (not by age and sex)

# The age-standardized mortality rate (ASMR) is calculated by first finding 
# the age-specific (mortality) rates for each age group by dividing the number 
# of deaths by the respective population, then multiplying the result by 1000,
# then multiplying each of the age-specific rates by the proportion of 
# the population belonging to the particular age group (standard population weight)
## BZD: for what population is the proportion by age group that you are multiplying by?
# Marina: I multiply the age-standardized mortality rate for each age group by the population weight of each age group in a given lsoa

# Source: https://www.statcan.gc.ca/en/dai/btd/asr

deaths_lsoa_eng <- merge(deaths_england, population_england, by = c("lsoa_code", "lsoa_name", "gender", "age"))

# Age-specific (mortality) rates
deaths_lsoa_eng  <- deaths_lsoa_eng  %>%
  mutate(age_specific_rate = deaths/population*1000)

# Instead of per 100,000 it is per 1000 similar to Victoria 


### BZD: made changes here to change the reference population
# Population weight for each age group within all lsoas in England. England is our reference population, since the RRs will then be
# multiplied by death rates by age and sex for the whole of England. 

# Calculate total population by gender
population_gender_England <- deaths_lsoa_eng %>%
  group_by(gender) %>%
  summarise(gender_population = sum(population))  # Total population by gender

# Calculate age-specific population and join with gender totals
population_weights_England <- deaths_lsoa_eng %>%
  select(gender, age, population) %>%
  group_by(gender, age) %>%
  summarise(age_population = sum(population)) %>%  # Total population by gender and age
  ungroup() %>%
  left_join(population_gender_England, by = "gender") %>% # Join with total population by gender
  mutate(weight=age_population/gender_population) 


# Population by lsoas to then calculate SMDR average
population_lsoas <- population_england %>%
  group_by(lsoa_code, lsoa_name, gender) %>%
  summarise(population_lsoa=sum(population)) %>%
  ungroup()

# Deaths lsoas to then assess whether enough (at least 10?) or replace with higher level data. 
deaths_lsoas <- deaths_lsoa_eng %>%
  group_by(lsoa_code, lsoa_name, gender) %>%
  summarise(deaths_lsoa=sum(deaths)) %>%
  ungroup()


# Now this is adding 
# Age-standardized Rate per lsoa
# Calculate the age-standardized rate per LSOA
deaths_lsoa_eng_asdr  <- deaths_lsoa_eng %>%
  left_join(population_weights_England, by = c("age", "gender"))  %>%
  # filter(population != 0) %>%  # BZD: some lsoas have a population of 0 but some deaths?
  group_by(lsoa_code, lsoa_name, gender) %>%
  summarise(
    stdrate = sum(age_specific_rate * weight, na.rm = TRUE)) %>%
    ungroup() %>% 
  left_join(population_lsoas)

# Merge with index of deprivation 

socio <- deprivation %>% 
  filter(`Indices of Deprivation` == "a. Index of Multiple Deprivation (IMD)") %>% 
  filter(Measurement == "Decile") %>%
  mutate(deprivation="IMD") %>%
  rename(lsoa_code=FeatureCode) %>%
  select(lsoa_code, imd_decile=Value, deprivation)


# Add to lsoa_deaths dataset. 

deaths_lsoa_eng_asdr <- deaths_lsoa_eng_asdr %>% left_join(socio) %>%
  mutate(rr=stdrate/stdrate_ave)

# BZD: given the many 0s we discussed replacing with MSOA when strrate is 0
# Marina: done, please verify 

##  Mortality by MSOA small areas in England (not by age)

# Population estimates by age and sex by MSOA 

msoa <- msoa %>%
  select(2:7) %>%
  distinct()

deaths_msoa <- merge(deaths_england, msoa, by.x = "lsoa_code", by.y = "LSOA11CD", all.x = TRUE)

deaths_msoa <- deaths_msoa %>% 
  select(-c(1,2,6,9,10)) %>% 
  rename("msoa_code" = "MSOA11CD",
         "msoa_name" = "MSOA11NM") %>% 
  group_by(msoa_code, msoa_name, gender, age) %>% 
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
  ungroup()

population_msoa <- merge(population_england, msoa, by.x = "lsoa_code", by.y = "LSOA11CD", all.x = TRUE)

population_msoa <- population_msoa %>% 
  select(-c(1,2,6,9,10)) %>% 
  rename("msoa_code" = "MSOA11CD",
         "msoa_name" = "MSOA11NM") %>% 
  group_by(msoa_code, msoa_name, gender, age) %>% 
  summarise(population = sum(population, na.rm = TRUE)) %>%
  ungroup()

deaths_msoa_eng <- merge(deaths_msoa, population_msoa, by = c("msoa_code", "msoa_name", "gender", "age"), all.x = TRUE)

# Age-specific (mortality) rates
deaths_msoa_eng_crude  <- deaths_msoa_eng  %>%
  mutate(age_specific_rate = deaths/population*1000)

# Instead of per 100,000 it is per 1000 similar to Victoria 

population_msoa_gender <- population_msoa %>%
  group_by(msoa_code, gender) %>%
  summarise(population=sum(population))


# Deaths msoas to then assess whether enough (at least 10?) or replace with higher level data. 
deaths_msoa_gender <- deaths_msoa %>%
  group_by(msoa_code, gender) %>%
  summarise(deaths_msoa=sum(deaths)) %>%
  ungroup()

# Population weight for each age group
deaths_msoa_eng_asdr  <- deaths_msoa_eng_crude  %>%
  left_join(population_weights_England) %>% #added, the reference population is england
 filter(population != 0) %>% 
  group_by(msoa_code, gender) %>%
  summarise(
    stdrate = sum(age_specific_rate * weight, na.rm = TRUE)) %>%
  ungroup() %>% 
  left_join(population_msoa_gender) %>%
  group_by(gender) %>%
  mutate(stdrate_ave = sum(stdrate*population,na.rm = TRUE)/sum(population)) %>%
  left_join(deaths_msoa_gender)


##  Mortality by (local authority districts) LAD areas in England 

# Population estimates by age and sex by LAD 

deaths_lad <- merge(deaths_england, msoa, by.x = "lsoa_code", by.y = "LSOA11CD", all.x = TRUE)

deaths_lad <- deaths_lad %>% 
  select(-c(1,2,6,7,8)) %>% 
  rename("lad_code" = "LAD20CD",
         "lad_name" = "LAD20NM") %>% 
  group_by(lad_code, lad_name, gender, age) %>% 
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
  ungroup()

population_lad <- merge(population_england, msoa, by.x = "lsoa_code", by.y = "LSOA11CD", all.x = TRUE)

population_lad <- population_lad %>% 
  select(-c(1,2,6,7,8)) %>% 
  rename("lad_code" = "LAD20CD",
         "lad_name" = "LAD20NM") %>% 
  group_by(lad_code, lad_name, gender, age) %>% 
  summarise(population = sum(population, na.rm = TRUE)) %>%
  ungroup()

deaths_lad_eng <- merge(deaths_lad, population_lad, by = c("lad_code", "lad_name", "gender", "age"), all.x = TRUE)

# Age-specific (mortality) crude rates
deaths_lad_eng  <- deaths_lad_eng  %>%
  mutate(age_specific_rate = deaths/population*1000)

# Instead of per 100,000 it is per 1000 similar to Victoria 

population_lad_gender <- population_lad %>%
  group_by(lad_code, gender) %>%
  summarise(population=sum(population))


# Deaths msoas to then assess whether enough (at least 10?) or replace with higher level data. 
deaths_lad_gender <- deaths_lad %>%
  group_by(lad_code, gender) %>%
  summarise(deaths_lad=sum(deaths)) %>%
  ungroup()

# Age-standardized Rate
deaths_lad_eng_asdr  <- deaths_lad_eng  %>%
  left_join(population_weights_England) %>%
  group_by(lad_code, lad_name, gender) %>%
  summarise(
    pop = sum(population),                       
    deaths = sum(deaths),
    stdrate = sum(age_specific_rate*weight)) %>%
  ungroup() %>% 
  left_join(population_lad_gender) %>%
  group_by(gender) %>%
  mutate(stdrate_ave = sum(stdrate*population,na.rm = TRUE)/sum(population)) %>%
  left_join(deaths_lad_gender)


# From Chris' code: A relative mortality rate is produced for each area,
# defined as the rate relative to the average mortality in the state.
# This average mortality is defined as a weighted average
# of the area-specific rates, weighted by the population of each area.

deaths_lsoa_eng_asdr <- deaths_lsoa_eng_asdr %>%
  rename(stdrate_lsoa=stdrate,
         population = population_lsoa) %>%
  select(lsoa_code, lsoa_name, gender, stdrate_lsoa, population, deaths_lsoa, imd_decile) %>%
  left_join(msoa, join_by(lsoa_code == LSOA11CD)) %>%
  rename(msoa_code=MSOA11CD,
         lad_code=LAD20CD) %>%
  select(lsoa_code, lsoa_name, msoa_code, lad_code, gender, stdrate_lsoa, population, deaths_lsoa, imd_decile)

deaths_msoa_eng_asdr <- deaths_msoa_eng_asdr %>%
  rename(stdrate_msoa = stdrate) %>%
  select(msoa_code, gender, stdrate_msoa, deaths_msoa) %>%
  ungroup()


deaths_lad_eng_asdr <- deaths_lad_eng_asdr %>%
  rename(stdrate_lad=stdrate) %>%
  select(lad_code, gender, stdrate_lad, deaths_lad) %>%
  ungroup()


# All three data
# BZD: check the sensitivity of the results to the min deaths rules below. 
deaths_england_final <- deaths_lsoa_eng_asdr %>%
  left_join(deaths_msoa_eng_asdr, by = c("gender", "msoa_code")) %>%
  left_join(deaths_lad_eng_asdr, by = c("gender", "lad_code")) %>%
  # Use MSOA rate if LSOA std rate is missing or has fewer than 10 deaths, and use LAD if msoa deaths are fewer than 20
  mutate(stdrate = ifelse(
    is.na(stdrate_lsoa) | deaths_lsoa < 10,   # if NA or fewer than 10 deaths at the lsoa level, use msoa
    ifelse(
      deaths_msoa > 20,             # if fewer than 20 deaths at the msoa level, use lad
      stdrate_msoa,
      stdrate_lad),
    stdrate_lsoa)) %>%
  group_by(gender) %>%
  mutate(stdrate_ave = sum(stdrate*population,na.rm = TRUE)/sum(population)) %>%
  mutate(RR = stdrate / stdrate_ave) %>%
  ungroup()

## Graph by index of multiple deprivation
# Note: for the whole of England, clear gradient on overall age and sex standardised rates and by gender. 

### Overall ASR

deaths_rate_imd_overall <- deaths_england_final %>%
  group_by(imd_decile) %>%
  summarise(
    # Only calculate the weighted mean if both rate_area_100000 and population are available
    ASR = sum(stdrate*population, na.rm=TRUE)/sum(population),
    .groups = "drop"
  )  %>%
  ungroup()

### Sex specific ASR

deaths_rate_imd_gender <- deaths_england_final %>%
  group_by(imd_decile, gender) %>%
  summarise(
    # Only calculate the weighted mean if both rate_area_100000 and population are available
    ASR = sum(stdrate*population, na.rm=TRUE)/sum(population),
    .groups = "drop"
  )  %>%
  ungroup()

### Marina, for comparison, see ASDR for the whole of england, by gender (for as is the average). We seems to ve underestimating. 
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/deathsregistrationsummarytables/2018#:~:text=In%202018%2C%20there%20were%20505%2C859,)%20and%20females%20(915.9).

## Join with mortality data

england_lifetable <- england_lifetable %>%
                    mutate(sex=as.character(sex)) %>%
                    mutate(sex=case_when(sex == "Male" ~ "Males", 
                                         sex == "Female" ~ "Females")) %>%
  select(age, sex, rate1000)

england_lifetable_lsoa <- deaths_england_final %>% rename (sex = gender) %>% 
  cross_join(england_lifetable) %>% 
  select(lsoa_code, lsoa_name, sex = sex.x, age, rate1000, imd_decile)

# BZD: the graphs need some work, the idea is to show differences in mortality rates across imd. A line for the 
# English average in each decile graph would be useful
# # Graphs
# p_england <- ggplot(england_lifetable_lsoa, aes(x = age, y = rate1000, col = lsoa_code)) +
#   geom_line(lwd = 1) +  # Draw lines with specified width
#   facet_grid(imd_decile ~ sex, scales = "free_y") +  # Facet by both imd_decile (rows) and sex (columns)
#   ylab("Mortality rate (per 1,000)") +  # Label for y-axis
#   xlab("Age (years)") +  # Label for x-axis
#   scale_color_discrete(guide = "none") +  # Disable legend for lsoa_code if too many colors
#   theme_minimal() +  # Use a clean theme
#   theme(legend.position = "none")  # Remove legend if not needed
# 
# # Print the plot
# print(p_england)


# Filter for Greater Manchester Only:

manchester_lifetable_lsoa <- england_lifetable_lsoa %>% 
  filter(
    str_starts(lsoa_name, "Bolton") |
      str_starts(lsoa_name, "Bury") |
      str_starts(lsoa_name, "Manchester") |
      str_starts(lsoa_name, "Oldham") |
      str_starts(lsoa_name, "Rochdale") |
      str_starts(lsoa_name, "Salford") |
      str_starts(lsoa_name, "Stockport") |
      str_starts(lsoa_name, "Tameside") |
      str_starts(lsoa_name, "Trafford") |
      str_starts(lsoa_name, "Wigan")) %>%
  mutate(prob=1-(exp(-rate1000/1000)))

saveRDS(manchester_lifetable_lsoa, "manchester/health/processed/mort_lsoa.RDS")

# Graphs
# p_manchester <- ggplot(manchester_lifetable_lsoa, aes(x = age, y = rate1000, col = lsoa_code)) +
#   geom_line(lwd = 1) +  # Draw lines with specified width
#   facet_grid(imd_decile ~ sex, scales = "free_y") +  # Facet by both imd_decile (rows) and sex (columns)
#   ylab("Mortality rate (per 1,000)") +  # Label for y-axis
#   xlab("Age (years)") +  # Label for x-axis
#   scale_color_discrete(guide = "none") +  # Disable legend for lsoa_code if too many colors
#   theme_minimal() +  # Use a clean theme
#   theme(legend.position = "none")  # Remove legend if not needed
# 
# # Print the plot
# print(p_manchester)
# 
# 
# # Plot: LSOA by Level of Deprivation
# 
# deprivation <- deprivation %>%
#   filter(`Indices of Deprivation` == "a. Index of Multiple Deprivation (IMD)" & Measurement == "Decile") # Score and Rank are also options
# 
# england_lsoa_deaths <- england_lsoa_deaths |>
#   left_join(deprivation |> select(deprivation_decile = Value, lsoa_code = FeatureCode), by = "lsoa_code") 
# 
# rr_plot <- ggplot(england_lsoa_deaths, aes(x = deprivation_decile, y = RR)) +
#   geom_line() +                    
#   geom_point() +                    
#   labs(
#     title = "Relative Mortality Risk for LSOA by Deprivation Decile, England (2016-2019)",
#     x = "Deprivation Decile (1 - Most Deprived, 10 - Least Deprived)",
#     y = "Relative Mortality Risk (RR)"
#   ) +
#   scale_x_continuous(breaks = 1:10) +
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank())
# 
# 
# rr_plot