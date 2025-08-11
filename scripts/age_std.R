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



all_data <- arrow::open_dataset("temp/base_20p_10yrs_w_fixed_hd_250725.parquet") |> collect()

#all_data <- arrow::open_dataset("Y:/HealthImpact/Data/Country/UK/JIBE_health_output_data/base_20p_10yrs_w_fixed_hd_250725.parquet") |> collect()

get_ASR <- function(all_data, name = "rate", is_rem = F){
  
  #name = "rem 1 yr"
  #is_rem = T

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
  group_by(cycle, scen, gender) |> # and gender? (BELEN)
  summarize(
    age_std_rate = sum(rate_w),
    .groups = "drop"
  ) |> ungroup()

print(ggplot(std_rates_dead) +
           aes(x = cycle, y = age_std_rate, colour = scen) +
           geom_smooth(se=FALSE) +
           labs(title = paste("Age standardised death rate using reference population " , name)) +
           scale_color_hue(direction = 1) +
           facet_wrap(~ gender) +
           theme_minimal())


if (!is_rem){
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


}else{
  
  count_inc_wp <- all_data |> 
    filter(!grepl("dead|healthy|null|dep", value)) |> 
    group_by(id, scen, value) |> 
    # to_duckdb() |> 
    filter(cycle == min(cycle)) |> 
    ungroup() |> 
    filter(cycle > 0) |> 
    group_by(agegroup_cycle, gender, cycle, scen, value) |>
    summarise(n = dplyr::n()) |> 
    ungroup() |>
    left_join(people, by = c("agegroup_cycle", "gender",  "scen", "cycle"))
  
  
  gap_rows <- all_data |>
    filter(value == "depression") |> 
    group_by(id, value)|>
    filter(cycle > 0) |> 
    arrange(cycle) |>
    mutate(
      previous_cycle = lag(cycle),
      gap = if_else(is.na(previous_cycle), FALSE, cycle - previous_cycle != 1)
    ) |>
    filter(gap, value == "depression")
  
  # 2. Identify first (minimum) cycle with value = "depression" for each id:
  first_depression <- all_data |> 
    filter(value == "depression") |>
    filter(cycle > 0) |> 
    group_by(id, value) |> 
    filter(cycle == min(cycle))
  
  # 3. Combine both sets and remove duplicates:
  incidence_rows <- bind_rows(first_depression, gap_rows) |>
    distinct(id, value, cycle, .keep_all = TRUE)
    
    
  count_inc_wp <- 
    incidence_rows |> 
    group_by(agegroup_cycle, gender, cycle, scen, value) |>
    summarise(n = dplyr::n()) |> 
    ungroup() |>
    left_join(people, by = c("agegroup_cycle", "gender",  "scen", "cycle")) |> 
    bind_rows(count_inc_wp)
  
  
}

crude_rates_inc <- count_inc_wp |> 
  group_by(agegroup_cycle, gender, cycle, scen, value) |> # grouping by gender here so below too
  reframe(n = sum(n), pop = sum(pop)) |> 
  mutate(crude_rate = if_else(pop > 0, n / pop * 100000, NA_real_)) |> 
  filter(!is.na(crude_rate))




std_rates_inc <- crude_rates_inc |>
  dplyr::select(-dplyr::any_of("pop")) |>
  left_join(ref_weights |> dplyr::select(-dplyr::any_of("pop")), by = c("gender", "agegroup_cycle")) |>
  filter(!is.na(weight)) |>
  mutate(rate_w=crude_rate*weight)  |>
  group_by(cycle, value, scen, gender) |>
  summarize(
    age_std_rate = sum(rate_w),
    .groups = "drop"
  ) |> ungroup()

std_rates_inc$gender <- as.factor(std_rates_inc$gender)

  print(ggplot(std_rates_inc) +
           aes(x = cycle, y = age_std_rate, colour = gender) +
           geom_smooth(se = F) + labs(title = paste("Age standardised incidence rate " , name)) +
           scale_color_hue(direction = 1) +
           theme_minimal() +
           facet_wrap(vars(value), scales = "free_y"))
  
  raw_inc <- all_data |> 
    group_by(id, scen, value) |> 
    filter(cycle == min(cycle)) |> 
    ungroup() |> 
    filter(cycle > 0) |> 
    group_by(cycle, value) |>
    summarise(n = dplyr::n()) |> 
    left_join(people |> 
                group_by(cycle) |> 
                reframe(pop = sum(pop))) |> 
    mutate(prop = round(n/pop*100, 1)) |> 
    arrange(value, cycle)

return(list(std_rates_dead, std_rates_inc, raw_inc))

}



raw_inc <- all_data |> 
  group_by(id, scen, value) |> 
  filter(cycle == min(cycle)) |> 
  ungroup() |> 
  filter(cycle > 0) |> 
  group_by(cycle, value) |>
  summarise(n = dplyr::n()) |> 
  left_join(people |> 
              group_by(cycle) |> 
              reframe(pop = sum(pop))) |> 
  mutate(prop = round(n/pop*100, 1)) |> 
  arrange(value, cycle)


hd <- read_csv("/media/ali/Expansion/backup_tabea/manchester-main/input/health/health_transitions_manchester_250725.csv")

ggplot(hd |> filter(grepl("dep|coro", cause))) +
  aes(x = age, y = rate, colour = sex, group = sex) +
  geom_smooth(se = FALSE) +
  scale_color_gradient() +
  theme_minimal() +
  facet_wrap(vars(cause))
