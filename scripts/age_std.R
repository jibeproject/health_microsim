# === Load libraries ===
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(DT)
  library(purrr)
  library(stringr)
  library(esquisse)
  library(ggplot2)
  library(plotly)
})


all_data <- arrow::open_dataset("temp/base_210725_w_exp") |> collect()

people <- all_data |> 
  group_by(agegroup_cycle, cycle, scen) |> 
  summarise(pop = n_distinct(id[!grepl("dead|null", value)])) |> 
  pivot_longer(cols = pop) |> 
  rename(pop = value ) |> 
  collect()

## Baseline pop reference weights. To use in ASR

ref_weights <- people |>
  filter(scen == "reference", cycle == 0) |>
  group_by(agegroup_cycle) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") |>
  ungroup() |>
  mutate(total_pop =  sum(pop),
         weight = pop / total_pop) |>
  dplyr::select(agegroup_cycle, weight, pop, total_pop)

inc_death <- all_data |> 
  filter(grepl("dead", value)) |>
  collect()

crude_rates_dead <- inc_death |>
  filter(cycle > 1) |>
  group_by(agegroup_cycle, cycle, scen) |>
  summarise(n = dplyr::n(), .groups = "drop") |>
  ungroup() |>
  left_join(people, by = c("agegroup_cycle", "scen", "cycle")) |>
  ungroup() |> 
  group_by(agegroup_cycle, cycle, scen) |> 
  reframe(n = sum(n), pop = sum(pop)) |> 
  mutate(crude_rate = if_else(pop > 0, n / pop * 100000, NA_real_)) |>
  filter(!is.na(crude_rate))


std_rates_dead <- crude_rates_dead |>
  dplyr::select(-dplyr::any_of("pop")) |>
  left_join(ref_weights |> dplyr::select(-dplyr::any_of("pop")) |> 
              group_by(agegroup_cycle) |> reframe(weight = sum(weight)), by = c("agegroup_cycle")) |>
  filter(!is.na(weight)) |>
  mutate(rate_w=crude_rate*weight)  |>
  group_by(cycle, scen) |>
  summarize(
    age_std_rate = sum(rate_w),
    .groups = "drop"
  ) |> ungroup()

ggplotly(ggplot(std_rates_dead) +
           aes(x = cycle, y = age_std_rate, colour = scen) +
           geom_smooth(se=FALSE) +
           labs(title = "Age standardised death rate using reference population") +
           scale_color_hue(direction = 1) +
           theme_minimal())


count_inc <- all_data |>
  filter(!grepl("dead|healthy|null", value)) |>
  group_by(id, scen, value) |>
  # to_duckdb() |>
  filter(cycle == min(cycle)) |>
  ungroup() |>
  filter(cycle > 0) |>
  group_by(agegroup_cycle, gender, cycle, scen, value) |>
  summarise(n = dplyr::n()) |>
  collect()

## Count 
count_inc_wp <- all_data |> 
  filter(!grepl("dead|healthy|null", value)) |> 
  group_by(id, scen, value) |> 
  # to_duckdb() |> 
  filter(cycle == min(cycle)) |> 
  ungroup() |> 
  filter(cycle > 0) |> 
  group_by(agegroup_cycle, cycle, scen, value) |> 
  summarise(n = dplyr::n()) |> 
  ungroup() |>
  left_join(people, by = c("agegroup_cycle", "scen", "cycle")) 

crude_rates_inc <- count_inc_wp |> 
  group_by(agegroup_cycle, cycle, value, scen) |> 
  reframe(n = sum(n), pop = sum(pop)) |> 
  mutate(crude_rate = if_else(pop > 0, n / pop * 100000, NA_real_)) |> 
  filter(!is.na(crude_rate))


std_rates_inc <- crude_rates_inc |>
  dplyr::select(-dplyr::any_of("pop")) |>
  left_join(ref_weights |> dplyr::select(-dplyr::any_of("pop")) |> 
              group_by(agegroup_cycle) |> reframe(weight = sum(weight)), by = c("agegroup_cycle")) |>
  filter(!is.na(weight)) |>
  mutate(rate_w=crude_rate*weight)  |>
  group_by(cycle, value, scen) |>
  summarize(
    age_std_rate = sum(rate_w),
    .groups = "drop"
  ) |> ungroup()

ggplotly(ggplot(std_rates_inc) +
           aes(x = cycle, y = age_std_rate, colour = value) +
           geom_smooth(se = FALSE) +
           scale_color_hue(direction = 1) +
           theme_minimal() +
           facet_wrap(vars(value), scales = "free_y")
)
