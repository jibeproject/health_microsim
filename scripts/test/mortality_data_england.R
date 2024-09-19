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
# Mortality Data by LSOA by age and sex
# [Source]: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/1028deathregistrationsbysexfiveyearagegroupandlowerlayersuperoutputareaslsoa2011censusboundariesenglandandwales2001to2021
deaths_england <- fread('manchester/health/processed/ons/deaths_england.csv')
population_england <- fread('manchester/health/processed/ons/population_england.csv')

# [Source]: https://geoportal.statistics.gov.uk/datasets/e7c49b62898a417192a336aca17e3a3f/about
msoa <- fread('manchester/health/original/ons/lsoa_to_msoa.csv')

# Life Tables for England
# [Source]: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandreferencetables/current
eng <- fread('manchester/health/original/ons/life_tables_20172019.csv',skip = 5)

## Mortality by year of age and sex, for England (not by LSOAs)

# The mortality rate is defined as "the probability that a person aged x 
# exactly will die before reaching age (x+1)".
# It is also converted to a rate per 1000 people.


## BZD: the life table should have the rate not the probability, rate is mx
eng <- eng[ , -c(2,7,8,9)] ## BZD@ here we need to remove column 3 and 10 for qx and leave 2 and 9 for mx

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

# Source: https://www.statcan.gc.ca/en/dai/btd/asr

deaths_lsoa_eng <- merge(deaths_england, population_england, by = c("lsoa_code", "lsoa_name", "gender", "age"))

# Age-specific (mortality) rates
deaths_lsoa_eng  <- deaths_lsoa_eng  %>%
  mutate(age_specific_rate = deaths/population*1000)

# Instead of per 100,000 it is per 1000 similar to Victoria 

# Population weight for each age group
deaths_lsoa_eng  <- deaths_lsoa_eng  %>%
  group_by(lsoa_code) %>%
  mutate(total_population = sum(population),   
         population_weight = population/total_population)

# Age-standardized Rate
deaths_lsoa_eng  <- deaths_lsoa_eng  %>%
  group_by(lsoa_code, lsoa_name) %>%
  summarise(
    pop = sum(population),                       
    deaths = sum(deaths),
    stdrate = sum(age_specific_rate*population_weight))

# From Chris' code: A relative mortality rate is produced for each area,
# defined as the rate relative to the average mortality in the state.
# This average mortality is defined as a weighted average
# of the area-specific rates, weighted by the population of each area.

deaths_lsoa_eng <- deaths_lsoa_eng |>
  mutate(stdrate_ave = with(.data, sum(stdrate*pop,na.rm = TRUE)/sum(pop))) |>
  mutate(RR = stdrate / stdrate_ave)

# Filter for Greater Manchester Only:

deaths_lsoa_manchester <- deaths_lsoa_eng %>% 
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