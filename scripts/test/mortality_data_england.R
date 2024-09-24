# setup 
rm(list = ls())
packages <- c("tidyverse",
              "haven",
              "dplyr",
              "ggplot2",
              "data.table",
              "readxl")
lapply(packages, library, character.only = TRUE)

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

# Population weight for each age group within a lsoa
deaths_lsoa_eng  <- deaths_lsoa_eng  %>%
  group_by(lsoa_code) %>%
  mutate(total_population = sum(population),   
         population_weight = population/total_population) %>%
  ungroup()

# Age-standardized Rate
deaths_lsoa_eng  <- deaths_lsoa_eng  %>%
  group_by(lsoa_code, lsoa_name) %>%
  summarise(
    pop = sum(population),                       
    deaths = sum(deaths),
    stdrate = sum(age_specific_rate*population_weight)) %>%
  ungroup()


# BZD: given the many 0s we discussed replacing with MSOA when strrate is 0
# Marina: done, please verify 

##  Mortality by MSOA small areas in England (not by age and sex)

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
deaths_msoa_eng  <- deaths_msoa_eng  %>%
  mutate(age_specific_rate = deaths/population*1000)

# Instead of per 100,000 it is per 1000 similar to Victoria 

# Population weight for each age group
deaths_msoa_eng  <- deaths_msoa_eng  %>%
  group_by(msoa_code) %>%
  mutate(total_population = sum(population),   
         population_weight = population/total_population) %>%
  ungroup()

# Age-standardized Rate
deaths_msoa_eng  <- deaths_msoa_eng  %>%
  group_by(msoa_code, msoa_name) %>%
  summarise(
    pop = sum(population),                       
    deaths = sum(deaths),
    stdrate = sum(age_specific_rate*population_weight)) %>%
  ungroup()

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

# Age-specific (mortality) rates
deaths_lad_eng  <- deaths_lad_eng  %>%
  mutate(age_specific_rate = deaths/population*1000)

# Instead of per 100,000 it is per 1000 similar to Victoria 

# Population weight for each age group
deaths_lad_eng  <- deaths_lad_eng  %>%
  group_by(lad_code) %>%
  mutate(total_population = sum(population),   
         population_weight = population/total_population) %>%
  ungroup()

# Age-standardized Rate
deaths_lad_eng  <- deaths_lad_eng  %>%
  group_by(lad_code, lad_name) %>%
  summarise(
    pop = sum(population),                       
    deaths = sum(deaths),
    stdrate = sum(age_specific_rate*population_weight)) %>%
  ungroup()

# From Chris' code: A relative mortality rate is produced for each area,
# defined as the rate relative to the average mortality in the state.
# This average mortality is defined as a weighted average
# of the area-specific rates, weighted by the population of each area.

england_lsoa_deaths <- deaths_lsoa_eng |>
  left_join(msoa, join_by(lsoa_code == LSOA11CD)) |>
  select(lsoa_code, lsoa_name, msoa_code = "MSOA11CD", lad_code = "LAD20CD",pop, deaths, stdrate) |>
  distinct() |>
  mutate(stdrate_ave = with(.data, sum(stdrate*pop,na.rm = TRUE)/sum(pop))) |>
  left_join(deaths_msoa_eng |> select(msoa_code, stdrate_msoa = stdrate, deaths_msoa = deaths), by="msoa_code") |>
  left_join(deaths_lad_eng |> select(lad_code, stdrate_lad = stdrate), by="lad_code")|>
  # Use MSOA rate if LSOA std rate is missing or has fewer than 10 deaths, and use LAD if msoa deaths are fewer than 20
  mutate(stdrate = ifelse(
    is.na(stdrate) | deaths < 10,   # if NA or fewer than 10 deaths at the lsoa level, use msoa
    ifelse(
      deaths_msoa > 20,             # if fewer than 20 deaths at the msoa level, use lad
      stdrate_msoa,
      stdrate_lad),
    stdrate)) |>                    # if not NA and more than or equal to 10, use lsoa
  mutate(RR = stdrate / stdrate_ave)

# Filter for Greater Manchester Only:

manchester_lsoa_deaths <- england_lsoa_deaths %>% 
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
      str_starts(lsoa_name, "Wigan"))

# Plot: LSOA by Level of Deprivation

deprivation <- deprivation %>%
  filter(`Indices of Deprivation` == "a. Index of Multiple Deprivation (IMD)" & Measurement == "Decile") # Score and Rank are also options

england_lsoa_deaths <- england_lsoa_deaths |>
  left_join(deprivation |> select(deprivation_decile = Value, lsoa_code = FeatureCode), by = "lsoa_code") 

rr_plot <- ggplot(england_lsoa_deaths, aes(x = deprivation_decile, y = RR)) +
  geom_line() +                    
  geom_point() +                    
  labs(
    title = "Relative Mortality Risk for LSOA by Deprivation Decile, England (2016-2019)",
    x = "Deprivation Decile (1 - Most Deprived, 10 - Least Deprived)",
    y = "Relative Mortality Risk (RR)"
  ) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


rr_plot