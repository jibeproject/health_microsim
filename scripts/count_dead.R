library(arrow)
library(tidyverse)
all_data <- arrow::open_dataset("temp/120526_stability_test.parquet") |> to_duckdb()

dead_ids <- all_data |>
  group_by(id, cycle, scen) |>
  filter(any(grepl("dead", value))) |>
  ungroup() |> 
  collect()

dead_ind <- all_data |> collect() |> filter(id %in% dead_ids$id)

dead_ind_count <- dead_ind |> 
  group_by(scen, value) |> 
  reframe(count = dplyr::n()) |> 
  mutate(diff_ref = count - count[scen == "reference"] ) |> 
  arrange(value, scen) 

ggplot(dead_ind, aes(x = cycle, y = scen, fill = scen)) + 
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value))

ggplot(dead_ind, aes(x = cycle, y = scen, fill = scen)) + 
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value)) + scale_x_continuous(breaks = c(0:9))
