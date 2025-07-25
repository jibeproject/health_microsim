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

# 
# # # 20 percent baseline run WITHOUT disease interaction and prevalence ONLY being assigned to adults
# all_data <- arrow::open_dataset("temp/base_20p_wo_di.parquet") |> collect()
# 
# # Previous run of all scenarios WITHOUT exposure changes but WITH prevalnce assigned to kids
# # all_data <- arrow::open_dataset("temp/all_data.parquet/") |> filter(scen %in% c("reference",  "both")) |> collect()
# 
# # 100 percent baseline run WITHOUT disease interaction but exposure changes over time
# # all_data <- arrow::open_dataset("temp/base1.parquet") |> collect()
# 
# people <- all_data |> 
#   group_by(agegroup_cycle, cycle, scen) |> 
#   summarise(pop = n_distinct(id[!grepl("dead|null", value)])) |> 
#   pivot_longer(cols = pop) |> 
#   rename(pop = value ) |> 
#   collect()
# 
# ## Baseline pop reference weights. To use in ASR
# 
# ref_weights <- people |>
#   filter(scen == "reference", cycle == 0) |>
#   group_by(agegroup_cycle) |>
#   summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") |>
#   ungroup() |>
#   mutate(total_pop =  sum(pop),
#          weight = pop / total_pop) |>
#   dplyr::select(agegroup_cycle, weight, pop, total_pop)
# 
# inc_death <- all_data |> 
#   filter(grepl("dead", value)) |>
#   collect()
# 
# crude_rates_dead <- inc_death |>
#   filter(cycle > 1) |>
#   group_by(agegroup_cycle, cycle, scen) |>
#   summarise(n = dplyr::n(), .groups = "drop") |>
#   ungroup() |>
#   left_join(people, by = c("agegroup_cycle", "scen", "cycle")) |>
#   ungroup() |> 
#   group_by(agegroup_cycle, cycle, scen) |> 
#   mutate(n = sum(n, na.rm = T), pop = sum(pop, na.rm = T)) |> 
#   mutate(crude_rate = if_else(pop > 0, n / pop * 100000, NA_real_)) |>
#   filter(!is.na(crude_rate))
# 
# 
# std_rates_dead <- crude_rates_dead |>
#   dplyr::select(-dplyr::any_of("pop")) |>
#   left_join(ref_weights |> dplyr::select(-dplyr::any_of("pop")) |> 
#               group_by(agegroup_cycle) |> reframe(weight = sum(weight)), by = c("agegroup_cycle")) |>
#   filter(!is.na(weight)) |>
#   mutate(rate_w=crude_rate*weight)  |>
#   group_by(cycle, scen) |>
#   summarize(
#     age_std_rate = sum(rate_w),
#     .groups = "drop"
#   ) |> ungroup()
# 
# ggplot(std_rates_dead) +
#   aes(x = cycle, y = age_std_rate) +
#   geom_line() +
#   scale_color_hue(direction = 1) +
#   theme_minimal() #+
#   #facet_wrap(vars(ladcd))
# 
# 
# ggplotly(ggplot(std_rates_dead) +
#            aes(x = cycle, y = age_std_rate, colour = scen) +
#            geom_smooth(se=FALSE) +
#            labs(title = "Age standardised death rate using reference population") +
#            scale_color_hue(direction = 1) +
#            theme_minimal())
# 
# 
# # Count 
# count_inc_wp <- all_data |> 
#   filter(!grepl("dead|healthy|null", value)) |> 
#   group_by(id, scen, value) |> 
#   # to_duckdb() |> 
#   filter(cycle == min(cycle)) |> 
#   ungroup() |> 
#   filter(cycle > 0) |> 
#   group_by(ladcd, agegroup_cycle, cycle, scen, value) |> 
#   summarise(n = dplyr::n()) |> 
#   ungroup() |>
#   left_join(people, by = c("ladcd", "agegroup_cycle", "scen", "cycle")) 
# 
# crude_rates_inc <- count_inc_wp |> 
#   group_by(ladcd, agegroup_cycle, cycle, value, scen) |> 
#   reframe(n = sum(n), pop = sum(pop)) |> 
#   mutate(crude_rate = if_else(pop > 0, n / pop * 100000, NA_real_)) |> 
#   filter(!is.na(crude_rate))
# 
# 
# std_rates_inc <- crude_rates_inc |>
#   dplyr::select(-dplyr::any_of("pop")) |>
#   left_join(ref_weights |> dplyr::select(-dplyr::any_of("pop")) |> 
#               group_by(agegroup_cycle) |> reframe(weight = sum(weight)), by = c("agegroup_cycle")) |>
#   filter(!is.na(weight)) |>
#   mutate(rate_w=crude_rate*weight)  |>
#   group_by(ladcd,cycle, value, scen) |>
#   summarize(
#     age_std_rate = sum(rate_w),
#     .groups = "drop"
#   ) |> ungroup()
# 
# ggplotly(ggplot(std_rates_inc) +
#            aes(x = cycle, y = age_std_rate, colour = value, group =  scen) +
#            geom_smooth(se = FALSE) +
#            scale_color_hue(direction = 1) +
#            labs(title = "reference and both - WITHOUT exposure changes")+
#            theme_minimal() +
#            facet_wrap(vars(value), scales = "free_y")
# )



all_data <- arrow::open_dataset("temp/base_210725_w_exp") |> collect()

people <- all_data |> 
  group_by(agegroup_cycle, gender, cycle, scen) |> 
  summarise(pop = n_distinct(id[!grepl("dead|null", value)])) |> 
  pivot_longer(cols = pop) |> 
  rename(pop = value ) |> 
  collect()

## Baseline pop reference weights. To use in ASR

ref_weights <- people |>
  filter(scen == "reference", cycle == 0) |>
  group_by(gender) |>
  #mutate(pop = sum(pop, na.rm = TRUE), .groups = "drop") |>
  #ungroup() |>
  mutate(total_pop =  sum(pop),
         weight = pop / total_pop) |>
  dplyr::select(agegroup_cycle, gender, weight, pop, total_pop)

inc_death <- all_data |> 
  filter(grepl("dead", value)) |>
  collect()

crude_rates_dead <- inc_death |>
  filter(cycle > 1) |>
  group_by(agegroup_cycle, gender, cycle, scen) |>
  summarise(n = dplyr::n(), .groups = "drop") |>
  ungroup() |>
  left_join(people, by = c("agegroup_cycle", "gender",  "scen", "cycle")) |>
  ungroup() |> 
  group_by(agegroup_cycle, gender, cycle, scen) |> 
  reframe(n = sum(n), pop = sum(pop)) |> 
  mutate(crude_rate = if_else(pop > 0, n / pop * 100000, NA_real_)) |>
  filter(!is.na(crude_rate))


std_rates_dead <- crude_rates_dead |>
  dplyr::select(-dplyr::any_of("pop")) |>
  left_join(ref_weights |> dplyr::select(-dplyr::any_of("pop")), by = c("gender", "agegroup_cycle")) |>
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



count_inc_wp <- all_data |> 
  filter(!grepl("dead|healthy|null", value)) |> 
  group_by(id, scen, value) |> 
  # to_duckdb() |> 
  filter(cycle == min(cycle)) |> 
  ungroup() |> 
  filter(cycle > 0) |> 
  group_by(agegroup_cycle, gender, cycle, scen, value) |>
  summarise(n = dplyr::n()) |> 
  ungroup() |>
  left_join(people, by = c("agegroup_cycle", "gender",  "scen", "cycle"))


# crude_rates_dead <- inc_death |>
#   filter(cycle > 1) |>
#   group_by(agegroup_cycle, gender, cycle, scen) |>
#   summarise(n = dplyr::n(), .groups = "drop") |>
#   ungroup() |>
#   left_join(people, by = c("agegroup_cycle", "gender",  "scen", "cycle")) |>
#   ungroup() |> 
#   group_by(agegroup_cycle, gender, cycle, scen) |> 
#   reframe(n = sum(n), pop = sum(pop)) |> 
#   mutate(crude_rate = if_else(pop > 0, n / pop * 100000, NA_real_)) |>
#   filter(!is.na(crude_rate))


crude_rates_inc <- count_inc_wp |> 
  group_by(agegroup_cycle, gender, cycle, scen, value) |> 
  reframe(n = sum(n), pop = sum(pop)) |> 
  mutate(crude_rate = if_else(pop > 0, n / pop * 100000, NA_real_)) |> 
  filter(!is.na(crude_rate))


std_rates_inc <- crude_rates_inc |>
  dplyr::select(-dplyr::any_of("pop")) |>
  left_join(ref_weights |> dplyr::select(-dplyr::any_of("pop")), by = c("gender", "agegroup_cycle")) |>
  filter(!is.na(weight)) |>
  mutate(rate_w=crude_rate*weight)  |>
  group_by(cycle, value, scen) |>
  summarize(
    age_std_rate = sum(rate_w),
    .groups = "drop"
  ) |> ungroup()

ggplotly(ggplot(std_rates_inc) +
           aes(x = cycle, y = age_std_rate, colour = value, group =  scen) +
           geom_smooth(se = FALSE) +
           scale_color_hue(direction = 1) +
           labs(title = "reference (10 years)")+
           theme_minimal() +
           facet_wrap(vars(value), scales = "free_y")
)

