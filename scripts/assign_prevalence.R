#### Assign disease state for baseline population

library(tidyverse)  
library(here)
library(esquisse)


# Synthetic population baseline
synth_pop <- read.csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_rr_2021.csv")

# Zones data

zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv")) %>%
  rename(zone = oaID)

# Disease prevalence data
prevalence <- read_csv("manchester/health/processed/health_transitions_manchester_prevalence.csv") %>%
  mutate(prob = 1 - exp(-rate), # Convert rates to probabilities
         cause = str_replace_all(cause, fixed("parkinson’s_disease"), "parkinson"))
  
### Assign geographies

synth_pop <- synth_pop %>%
  left_join(zones, by = c("zone" = "zone")) 

### Join prevalence rates to synthetic population

# Assign prevalence to synthetic population TODO: ignore children. 
synth_pop_wprob <- synth_pop |> 
  rename(sex=gender) |>
  select(id, age, sex, ladcd, ladnm) |>
  rownames_to_column() |>
  left_join(
    prevalence |>
      pivot_wider(id_cols = c(age, sex, location_code), names_from = cause, values_from = prob),
    by = c("age", "sex", "ladcd" = "location_code")
  ) |> filter(age>18)

# Function to allocate disease statuses based on probability

set.seed(123)

allocate_disease <- function(df) {
  df %>%
    mutate(across(copd:stroke, ~ ifelse(runif(n()) < ., 1, 0), .names = "{.col}_status"))
}

# Apply function to assign diseases
synth_pop_prev <- allocate_disease(synth_pop_wprob) %>%
  select(id, age, sex, ladcd, ladnm, ends_with("_status"))  %>%# Ensure we only keep relevant columns
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    TRUE ~ as.character(sex)))

# Save

synth_pop_prev_long <- synth_pop_prev |> pivot_longer(
  cols = copd_status:stroke_status,
  names_to = "diseases",
  values_to = "values"
) |>
  mutate(diseases = str_remove(diseases, "_status")) |>
  select(id, diseases, values) |>
  filter(values !=0) |>
  select(!values) |>
  group_by(id) %>%
  summarise(diseases = paste(diseases, collapse = " "), .groups = "drop")

write_csv(synth_pop_prev_long, "Y:/HealthImpact/Data/Country/UK/JIBE_health_input_data/base_prevalence_id_clean_230725.csv")


# Compare assignment with GBD original data 

## Crude rates per age group, gender and by LAD per 100,000 people

gbd_original <- read.csv("manchester/health/processed/gbd_processed.csv") |>
                mutate(rate_100k = rate_1*100000) |>
                rename(total = val, 
                       ladnm = location) |>
                mutate(sex = case_when(sex == "Female" ~ "female", 
                                       sex == "Male" ~ "male")) |>
                select(sex, cause, agegroup, total, ladnm, rate_100k) |>
                mutate(type = "gbd")


population <- synth_pop_prev |> select(id, age, sex, ladcd, ladnm) |>
  mutate(agegroup = case_when(
    age < 5             ~ "<5",
    age >= 5 & age <= 9 ~ "5-9",
    age >= 10 & age <= 14 ~ "10-14",
    age >= 15 & age <= 19 ~ "15-19",
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    age >= 75 & age <= 79 ~ "75-79",
    age >= 80 & age <= 84 ~ "80-84",
    age >= 85 & age <= 89 ~ "85-89",
    age >= 90 & age <= 94 ~ "90-94",
    age >= 95            ~ "95+",
    TRUE ~ NA_character_
  )) |> group_by(agegroup, sex, ladcd, ladnm) |>
              summarize(pop = n_distinct(id))


prev_long <- synth_pop_prev |> 
  pivot_longer(
    cols = copd_status:stroke_status,
    values_to = "value"
  ) |>
  mutate(agegroup = case_when(
    age < 5             ~ "<5",
    age >= 5 & age <= 9 ~ "5-9",
    age >= 10 & age <= 14 ~ "10-14",
    age >= 15 & age <= 19 ~ "15-19",
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    age >= 75 & age <= 79 ~ "75-79",
    age >= 80 & age <= 84 ~ "80-84",
    age >= 85 & age <= 89 ~ "85-89",
    age >= 90 & age <= 94 ~ "90-94",
    age >= 95            ~ "95+",
    TRUE ~ NA_character_
  )) |>
  group_by(agegroup, sex, ladcd, ladnm, name) |>
  summarise(total = sum(value)) |>
  mutate(name = str_remove(name, "_status")) |>
  left_join(population) |>
  mutate(rate_100k = total/pop*100000) |> 
  mutate(type = "assigned") |>
  rename(cause = name) |>
  ungroup()|>
  select(sex, cause, agegroup, total, ladnm, rate_100k, type)


compare <- bind_rows(gbd_original, prev_long) |>
        filter(!agegroup %in% c("<5", "5-9", "10-14"))



# Generate plots for each location, sex, and cause
combinations <- compare %>% 
  select(ladnm, sex, cause) %>% 
  distinct()

for (i in seq_len(nrow(combinations))) {
  loc <- combinations$ladnm[i]
  sx <- combinations$sex[i]
  cs <- combinations$cause[i]
  
  df_filtered <- compare %>% 
    filter(ladnm == loc, sex == sx, cause == cs) 
  
  plot <- ggplot(df_filtered, aes(x = factor(agegroup), y = rate_100k, fill = type)) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
    scale_fill_manual(
      values = c("assigned" = "blue", "gbd" = "red"),
      labels = c("assigned" = "Values Assigned", "gbd" = "Val GBD")
    ) +
    labs(title = paste("Location:", loc, "| Sex:", sx, "| Cause:", cs),
         x = "Age Group",
         y = "Rate per 100k",
         fill = "Legend") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = paste0("images/manchester/prevalence/plot_", loc, "_", sx, "_", cs, ".png"),
         plot = plot, width = 8, height = 6)
}


