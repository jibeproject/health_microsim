library(arrow)
library(tidyverse)
all_data <- arrow::open_dataset("temp/130526_stability_test.parquet/") |> to_duckdb()

dead_ids <- all_data |>
  group_by(id, cycle, scen) |>
  filter(any(grepl("dead", value))) |>
  ungroup() |> 
  collect()

dead_common_ids <- all_data |>
filter(grepl("dead", value)) |> # keep only rows with "dead"
distinct(id, scen) |> # which id×scen combinations have dead
group_by(id) |>
filter(n() == 2) |> # keep ids present in all scen levels
pull(id) |> # extract ids
unique()

dead_ind <- all_data |> collect() |> filter(id %in% dead_ids$id)

dead_ind_common <- all_data |> collect() |> filter(id %in% dead_common_ids)

dead_ind_count <- dead_ind |> 
  group_by(scen, value) |> 
  reframe(count = dplyr::n()) |> 
  mutate(diff_ref = count - count[scen == "reference"] ) |> 
  arrange(value, scen) 

ggplot(dead_ind, aes(x = cycle, y = scen, fill = scen)) + 
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value)) + scale_x_continuous(breaks = c(0:9))

dead_ind_common_count <- dead_ind_common |>
  group_by(scen, value) |>
  summarise(count = n(), .groups = "drop") |>
  # Get reference counts per value
  left_join(
    dead_ind_common |>
      filter(scen == "reference") |>
      group_by(value) |>
      summarise(ref_count = n(), .groups = "drop"),
    by = "value"
  ) |>
  mutate(
    diff = count - ifelse(is.na(ref_count), 0, ref_count)
  ) |> 
  arrange(value, scen) 

ggplot(dead_ind_common, aes(x = cycle, y = scen, fill = scen)) + 
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value)) + scale_x_continuous(breaks = c(0:9)) +
  labs(title = "Distribution of cause/disease by scenario across model years (cycle)",
       y = "")


max_age <- dead_ind_common |> 
  group_by(id, value, scen) |> 
  reframe(age_cycle = max(age_cycle)) |> 
  group_by(value, scen) |> reframe(median(age_cycle))

min_age <- dead_ind_common |> 
  group_by(id, value, scen) |> 
  reframe(age_cycle = min(age_cycle)) |> 
  group_by(value, scen) |> 
  reframe(min_age = median(age_cycle))




dead_ind_common_count <- dead_ind_common |> 
  filter(scen != "reference") |> 
  group_by(id, value, scen) |> 
  reframe(age_cycle = max(age_cycle)) |> 
  group_by(value, scen) |> 
  reframe(med_age = median(age_cycle)) |> 
  # Get reference counts per value
  left_join(
    dead_ind_common |> 
      filter(scen == "reference") |> 
      group_by(id, value, scen) |> 
      reframe(age_cycle = max(age_cycle)) |> 
      group_by(value, scen) |> 
      reframe(ref_med_age = median(age_cycle)) |> 
      dplyr::select(-scen),
    by = "value"
  ) |>
  mutate(
    diff = med_age - ifelse(is.na(ref_med_age), 0, ref_med_age)
  ) |> 
  arrange(value, scen) 


dead_ind_common_min_count <- dead_ind_common |> 
  filter(scen != "reference") |> 
  group_by(id, value, scen) |> 
  reframe(age_cycle = min(age_cycle)) |> 
  group_by(value, scen) |> 
  reframe(med_age = median(age_cycle)) |> 
  # Get reference counts per value
  left_join(
    dead_ind_common |> 
      filter(scen == "reference") |> 
      group_by(id, value, scen) |> 
      reframe(age_cycle = min(age_cycle)) |> 
      group_by(value, scen) |> 
      reframe(ref_med_age = median(age_cycle)) |> 
      dplyr::select(-scen),
    by = "value"
  ) |>
  mutate(
    diff = med_age - ifelse(is.na(ref_med_age), 0, ref_med_age)
  ) |> 
  arrange(value, scen) 

