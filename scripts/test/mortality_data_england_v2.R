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
# Mortality and Population Data by LSOA by age and sex (2018)
# [Source]: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/1028deathregistrationsbysexfiveyearagegroupandlowerlayersuperoutputareaslsoa2011censusboundariesenglandandwales2001to2021
deaths_england <- fread('manchester/health/processed/ons/deaths_england.csv')

deaths_england <- deaths_england %>%
  mutate(age = case_when(
    age %in% c("under 1", "01 to 04") ~ "00 to 04",
    TRUE ~ age
  )) %>%
  group_by(lsoa_code, lsoa_name, age, gender) %>%
  summarize(deaths = sum(deaths),.groups = "drop")
 
population_england <- fread('manchester/health/processed/ons/population_england.csv')

population_england <- population_england %>%
  mutate(age = case_when(
    age %in% c("under 1", "01 to 04") ~ "00 to 04",
    TRUE ~ age
  )) %>%
  group_by(lsoa_code, lsoa_name, age, gender) %>%
  summarize(population = sum(population),.groups = "drop")

# Geographical Hierarchy (as of 2021)
# [Source]: https://geoportal.statistics.gov.uk/datasets/e7c49b62898a417192a336aca17e3a3f/about
msoa <- fread('manchester/health/original/ons/lsoa_to_msoa.csv')

msoa <- msoa %>%
  select(2:7) %>%
  distinct()

# Life Tables for England (2017-2019)
# [Source]: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandreferencetables/current
lifetable_england  <- fread('manchester/health/original/ons/life_tables_20172019.csv',skip = 5)

# English Indices of Deprivation by LSOA (2019)
# [Source]: https://opendatacommunities.org/resource?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices
deprivation <- fread('manchester/health/original/ons/imd2019lsoa.csv') 

# European Standard Population 2013
# [Source]: https://publichealthscotland.scot/services/national-data-catalogue/national-datasets/search-the-datasets/european-standard-population/#:~:text=The%20European%20Standard%20Population%20(ESP,was%20originally%20introduced%20in%201976.
european_pop <- fread('manchester/health/original/ons/european_standard_population_by_sex.csv')

# Population Estimates in England 
# [Source]: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/analysisofpopulationestimatestoolforuk

## Mortality by year of age and sex, for England (not by LSOAs)

# The mortality rate `mx` is defined as "the number of deaths at age x 
# last birthday in the three year period (2017-2019) 
# divided by the average population at that age over the same period."

lifetable_england <- lifetable_england[, -c(3,7,8,10)] 

mf <- rep(c("male","female"),each=4)
names1 <- rep(c("rate","denom","deaths","le"), 2)
names(lifetable_england) <- c("age",paste(mf, names1, sep="_"))

lifetable_england <- lifetable_england %>% 
  mutate(male_personyears = round(male_denom - male_deaths),
         female_personyears = round(female_denom - female_deaths)) %>% 
  select(
    age, male_denom, male_rate, male_personyears, male_le,
    female_denom, female_rate, female_personyears, female_le)

england_lifetable <- lifetable_england |>
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

deaths_lsoa_eng <- deaths_lsoa_eng %>%
  filter(population != 0) %>%
  mutate(age_specific_rate = deaths/population*1000)

# Instead of per 100,000 it is per 1000 similar to Victoria 

### BZD: made changes here to change the reference population
# Population weight for each age group within all lsoas in England. England is our reference population, since the RRs will then be
# multiplied by death rates by age and sex for the whole of England. 

# Option 1: using LSOA-level mortality and population data

# # Calculate total population by gender
# population_gender_England <- deaths_lsoa_eng %>%
#   group_by(gender) %>%
#   summarise(gender_population = sum(population)) # Total population by gender
# 
# # Calculate age-specific population and join with gender totals
# population_weights_England <- deaths_lsoa_eng %>%
#   select(gender, age, population) %>%
#   group_by(gender, age) %>%
#   summarise(age_population = sum(population)) %>% 
#   ungroup() %>%
#   left_join(population_gender_England, by = "gender") %>% # Join with total population by gender
#   mutate(weight=age_population/gender_population)

# Option 2: using ONS reported statistics for England population estimates

age_groups <- c("00 to 04", "05 to 09", "10 to 14", "15 to 19", "20 to 24", 
                "25 to 29", "30 to 34", "35 to 39", "40 to 44", 
                "45 to 49", "50 to 54", "55 to 59", "60 to 64", 
                "65 to 69", "70 to 74", "75 to 79", "80 to 84", 
                "85 to 89", "90+")

# Age population for females
age_population_females <- c(1586736, 1671501, 1583573, 1519040, 1780978, 
                            1937844, 1971624, 1915382, 1737645, 
                            1948250, 1999121, 1812592, 1539980, 
                            1442042, 1410149, 994991, 780587, 
                            516960, 335578)

# Age population for males
age_population_males <- c(1670408, 1758059, 1668141, 1587797, 1779271, 
                          1847361, 1869280, 1851642, 1703652, 
                          1898976, 1943753, 1770400, 1488761, 
                          1355963, 1293965, 855145, 609495, 
                          335107, 152779)

population_england_ons <- data.frame(
  age = rep(age_groups, 2),  # Repeat age groups for both genders
  gender = rep(c("Females", "Males"), each = length(age_groups)),  # Gender labels
  age_population = c(age_population_females, age_population_males)  # Combine population data
)

population_england_ons <- population_england_ons %>%
  mutate(age = case_when(
    age %in% c("85 to 89", "90+") ~ "over 85",
    TRUE ~ age
  )) %>%
  group_by(age, gender) %>%
  summarize(age_population = sum(age_population), .groups = "drop")

# Calculate total population by gender
population_gender_England <- population_england_ons %>%
  group_by(gender) %>%
  summarise(gender_population = sum(age_population)) # Total population by gender

# Calculate age-specific population and join with gender totals
population_weights_England <- population_england_ons %>%
  select(gender, age, age_population) %>%
  group_by(gender, age) %>%
  summarise(age_population = sum(age_population)) %>% 
  ungroup() %>%
  left_join(population_gender_England, by = "gender") %>% # Join with total population by gender
  mutate(weight=age_population/gender_population)

# Now this is adding 
# Age-standardized Rate per lsoa
# Calculate the age-standardized rate per LSOA
deaths_lsoa_eng_asdr  <- deaths_lsoa_eng %>%
  left_join(population_weights_England, by = c("age", "gender"))  %>%
  group_by(lsoa_code, lsoa_name, gender) %>%
  summarise(
    stdrate = sum(age_specific_rate * weight, na.rm = TRUE),
    population_lsoa = sum(population),
    deaths_lsoa = sum(deaths)) %>% 
  ungroup()

##  Mortality by MSOA small areas in England (not by age)

# Population estimates by age and sex by MSOA 

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
deaths_msoa_eng_asdr  <- deaths_msoa_eng  %>%
  left_join(population_weights_England) %>% #added, the reference population is england
  filter(population != 0) %>% 
  group_by(msoa_code, msoa_name, gender) %>%
  summarise(
    stdrate = sum(age_specific_rate * weight, na.rm = TRUE),
    deaths_msoa = sum(deaths)) %>%
  ungroup()
# left_join(population_msoa_gender) %>%
# group_by(gender) %>%
# mutate(stdrate_ave = sum(stdrate*population,na.rm = TRUE)/sum(population)) %>%
# left_join(deaths_msoa_gender)


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

# Age-standardized Rate
deaths_lad_eng_asdr  <- deaths_lad_eng  %>%
  left_join(population_weights_England) %>%
  filter(population != 0) %>%
  group_by(lad_code, lad_name, gender) %>%
  summarise(
    pop = sum(population),                       
    deaths = sum(deaths),
    stdrate = sum(age_specific_rate*weight)) %>%
  ungroup()


# From Chris' code: A relative mortality rate is produced for each area,
# defined as the rate relative to the average mortality in the state.
# This average mortality is defined as a weighted average
# of the area-specific rates, weighted by the population of each area.

# All three data
# BZD: check the sensitivity of the results to the min deaths rules below. 

england_lsoa_deaths <- deaths_lsoa_eng_asdr |>
  left_join(msoa, join_by(lsoa_code == LSOA11CD)) |>
  select(lsoa_code, lsoa_name, msoa_code = "MSOA11CD", lad_code = "LAD20CD",gender,population_lsoa, deaths_lsoa, stdrate) |>
  distinct() |>
  left_join(deaths_msoa_eng_asdr |> select(msoa_code, stdrate_msoa = stdrate, deaths_msoa,gender), by= c("msoa_code","gender")) |>
  left_join(deaths_lad_eng_asdr |> select(lad_code, stdrate_lad = stdrate,gender), by=c("lad_code","gender")) %>% 
  group_by (gender) %>% 
  # Use MSOA rate if LSOA std rate is missing or has fewer than 10 deaths, and use LAD if msoa deaths are fewer than 20
  mutate(stdrate_ave = with(.data, sum(stdrate*population_lsoa,na.rm = TRUE)/sum(population_lsoa)),
         stdrate = ifelse(
           is.na(stdrate) | deaths_lsoa < 10,   # if NA or fewer than 10 deaths at the lsoa level, use msoa
           ifelse(
             deaths_msoa > 20,             # if fewer than 20 deaths at the msoa level, use lad
             stdrate_msoa,
             stdrate_lad),
           stdrate)) |>                    # if not NA and more than or equal to 10, use lsoa
  mutate(RR = stdrate / stdrate_ave)

## Graph by index of multiple deprivation
# Note: for the whole of England, clear gradient on overall age and sex standardised rates and by gender. 

# Merge with index of deprivation 

deprivation <- deprivation %>%
  filter(`Indices of Deprivation` == "a. Index of Multiple Deprivation (IMD)") %>%
  filter(Measurement == "Decile") %>%
  mutate(deprivation="IMD") %>%
  rename(lsoa_code=FeatureCode) %>%
  select(lsoa_code, imd_decile=Value, deprivation)

### Overall ASR

england_deaths_socio <- england_lsoa_deaths %>%
  left_join(deprivation, by = "lsoa_code")

mortality_socio_all <- england_deaths_socio %>% 
  group_by(imd_decile) %>%
  summarise(
    # Only calculate the weighted mean if both rate_area_100000 and population are available
    ASR = sum(stdrate*population_lsoa, na.rm=TRUE)/sum(population_lsoa),
    .groups = "drop"
  )  %>%
  ungroup()

### Sex specific ASR

mortality_socio_gender <- england_deaths_socio %>%
  group_by(imd_decile, gender) %>%
  summarise(
    # Only calculate the weighted mean if both rate_area_100000 and population are available
    pop = sum(population_lsoa),
    deaths = sum(deaths_lsoa),
    asr = mean(stdrate),
    .groups = "drop"
  )  %>%
  ungroup()

mortality_socio_gender <- england_deaths_socio %>%
  group_by(imd_decile, gender) %>%
  summarise(
    # Only calculate the weighted mean if both rate_area_100000 and population are available
    pop = sum(population_lsoa),
    deaths = sum(deaths_lsoa),
    asr = mean(stdrate),
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

england_lifetable_lsoa <- england_deaths_socio %>% 
  rename (sex = gender) %>% 
  cross_join(england_lifetable) %>% 
  select(lsoa_code, lsoa_name, sex = sex.x, age, rate1000, imd_decile)

# BZD: the graphs need some work, the idea is to show differences in mortality rates across imd. A line for the 
# English average in each decile graph would be useful
# # Graphs
ggplot(england_lifetable_lsoa, aes(x = age, y = rate1000, col = lsoa_code)) +
  geom_line(lwd = 1) +  # Draw lines with specified width
  facet_grid(imd_decile ~ sex, scales = "free_y") +  # Facet by both imd_decile (rows) and sex (columns)
  ylab("Mortality rate (per 1,000)") +  # Label for y-axis
  xlab("Age (years)") +  # Label for x-axis
  scale_color_discrete(guide = "none") +  # Disable legend for lsoa_code if too many colors
  theme_minimal() +  # Use a clean theme
  theme(legend.position = "none")  # Remove legend if not needed
# # 
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

saveRDS(manchester_lifetable_lsoa, "manchester/health/processed/manchester_mortality_lsoa.RDS")

# Graphs
# Assuming england_lifetable_lsoa contains columns: age, rate1000, and sex

plot1 <- ggplot(manchester_lifetable_lsoa, aes(x = age, y = rate1000)) + 
  geom_line(lwd=0.2) +   
  facet_grid(imd_decile ~ sex, scales = "free_y") + 
  # scale_color_manual(values = c("Males" = "#29B6A4", "Females" = "#E54060")) + 
  ylab("Mortality rate (per 1,000)") +  
  xlab("Age (years)") + 
  theme_minimal() +   
  theme(legend.position = "none")
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
# rr_plot

### European Standard Population 2013

# european_pop <- european_pop %>%
#   mutate(Sex = ifelse(Sex == "Male", "Males", "Females")) %>%
#   rename(
#     "age" = "AgeGroup",
#     "gender" = "Sex",
#     "population" = "EuropeanStandardPopulation"
#   ) %>%
#   mutate(age = case_when(
#     age == "90plus years" | age == "85-89 years" ~ "over 85",
#     TRUE ~ str_replace(age, "(\\d+)-(\\d+)", "\\1 to \\2")
#   )) %>%
#   mutate(age = str_replace_all(age, "\\b(\\d)\\b", "0\\1")) %>%
#   mutate(age = str_replace(age, " years", "")) %>%  # Remove 'years'
#   mutate(age = ifelse(age == "00 to 04", "01 to 04", age)) %>%  # Rename '00 to 04'
#   group_by(age, gender) %>%
#   summarise(population = sum(population), .groups = 'drop')
# 
# 
# # Calculate age-specific population and join with gender totals
# population_weights_europe <- european_pop %>%
#   select(gender, age, population) %>%
#   group_by(gender, age) %>%
#   summarise(age_population = sum(population)) %>%  # Total population by gender and age
#   ungroup() %>% 
#   mutate(weight=age_population/100000)
# 
# # lsoa
# deaths_lsoa_eng_eu  <- deaths_lsoa_eng %>%
#   left_join(population_weights_europe, by = c("age", "gender")) %>% 
#   group_by(lsoa_code, lsoa_name, gender) %>%
#   filter(population != 0) %>% 
#   summarise(
#     stdrate = sum(age_specific_rate * weight, na.rm = TRUE),
#     population_lsoa = sum(population),
#     deaths_lsoa = sum(deaths))
# 
# #msoa
# deaths_msoa_eng_eu  <- deaths_msoa_eng  %>%
#   left_join(population_weights_europe, by = c("age", "gender")) %>% 
#   filter(population != 0) %>% 
#   group_by(msoa_code, msoa_name, gender) %>%
#   summarise(
#     stdrate = sum(age_specific_rate * weight, na.rm = TRUE),
#     deaths_msoa = sum(deaths)) %>%
#   ungroup() 
# 
# #lad 
# deaths_lad_eng_eu  <- deaths_lad_eng  %>%
#   left_join(population_weights_europe, by = c("age", "gender")) %>%
#   filter(population != 0) %>% 
#   group_by(lad_code, lad_name, gender) %>%
#   summarise(
#     stdrate = sum(age_specific_rate*weight,  na.rm = TRUE)) %>%
#   ungroup()
# 
# england_lsoa_deaths_eu <- deaths_lsoa_eng_eu |>
#   left_join(msoa, join_by(lsoa_code == LSOA11CD)) |>
#   select(lsoa_code, lsoa_name, msoa_code = "MSOA11CD", lad_code = "LAD20CD",gender,population_lsoa, deaths_lsoa, stdrate) |>
#   distinct() |>
#   left_join(deaths_msoa_eng_eu |> select(msoa_code, stdrate_msoa = stdrate, deaths_msoa,gender), by= c("msoa_code","gender")) |>
#   left_join(deaths_lad_eng_eu |> select(lad_code, stdrate_lad = stdrate,gender), by=c("lad_code","gender")) %>% 
#   group_by (gender) %>% 
#   # Use MSOA rate if LSOA std rate is missing or has fewer than 10 deaths, and use LAD if msoa deaths are fewer than 20
#   mutate(stdrate_ave = with(.data, sum(stdrate*population_lsoa,na.rm = TRUE)/sum(population_lsoa)),
#          stdrate = ifelse(
#            is.na(stdrate) | deaths_lsoa < 10,   # if NA or fewer than 10 deaths at the lsoa level, use msoa
#            ifelse(
#              deaths_msoa > 20,             # if fewer than 20 deaths at the msoa level, use lad
#              stdrate_msoa,
#              stdrate_lad),
#            stdrate)) |>                    # if not NA and more than or equal to 10, use lsoa
#   mutate(RR = stdrate / stdrate_ave)
# 
# # Adding level of deprivation 
# 
# england_deaths_socio_eu <- england_lsoa_deaths_eu %>%
#   left_join(deprivation, by = "lsoa_code")
# 
# # For All
# mortality_socio_all_eu <- england_deaths_socio_eu %>% 
#   group_by(imd_decile) %>%
#   summarise(
#     # Only calculate the weighted mean if both rate_area_100000 and population are available
#     ASR = sum(stdrate*population_lsoa, na.rm=TRUE)/sum(population_lsoa),
#     .groups = "drop"
#   )  %>%
#   ungroup()
# 
# ### Sex specific ASR
# 
# mortality_socio_gender_eu <- england_deaths_socio_eu %>%
#   group_by(imd_decile, gender) %>%
#   summarise(
#     # Only calculate the weighted mean if both rate_area_100000 and population are available
#     ASR = sum(stdrate*population_lsoa, na.rm=TRUE)/sum(population_lsoa),
#     .groups = "drop"
#   )  %>%
#   ungroup()
# 
# # lad of Greater Manchester Only
# 
# deaths_lad_man_eu <- deaths_lad_eng_eu %>% 
#   filter(
#     str_starts(lad_name, "Bolton") |
#       str_starts(lad_name, "Bury") |
#       str_starts(lad_name, "Manchester") |
#       str_starts(lad_name, "Oldham") |
#       str_starts(lad_name, "Rochdale") |
#       str_starts(lad_name, "Salford") |
#       str_starts(lad_name, "Stockport") |
#       str_starts(lad_name, "Tameside") |
#       str_starts(lad_name, "Trafford") |
#       str_starts(lad_name, "Wigan"))
# 
# deaths_lad_man_eng <- deaths_lad_eng_asdr %>% 
#   filter(
#     str_starts(lad_name, "Bolton") |
#       str_starts(lad_name, "Bury") |
#       str_starts(lad_name, "Manchester") |
#       str_starts(lad_name, "Oldham") |
#       str_starts(lad_name, "Rochdale") |
#       str_starts(lad_name, "Salford") |
#       str_starts(lad_name, "Stockport") |
#       str_starts(lad_name, "Tameside") |
#       str_starts(lad_name, "Trafford") |
#       str_starts(lad_name, "Wigan"))