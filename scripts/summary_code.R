require(tidyverse)
require(arrow)
require(plotly)
all_data <- arrow::open_dataset("temp/all_data.parquet/") |> to_duckdb() #|> collect()

# Use collect() after each dplyr function

people <- all_data |> 
  group_by(agegroup_cycle, gender, cycle, scen) |> 
  summarise(np = n_distinct(id[!value %in% c("dead", "null")])) |> 
  pivot_longer(cols = np) |> collect()

count_inc <- all_data |> 
  group_by(id, value) |>  
  filter(cycle == min(cycle[cycle > 0])) |> 
  ungroup() |> 
  group_by(cycle, scen, value) |> 
  summarise(n = dplyr::n()) |> 
  collect()

backup <- count_inc

ggplot(count_inc) +
  aes(x = cycle, y = n, colour = scen) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value))

# NO need to call n_distinct(id), as it's a slower process. dplyr::n() is enough
# count_inc1 <- all_data |> 
#   group_by(id, value) |>  
#   filter(cycle == min(cycle[cycle > 0])) |> 
#   ungroup() |> 
#   group_by(cycle, scen, value) |> 
#   reframe(n = n_distinct(id))

count_inc <- count_inc |> 
  left_join(people |> filter(name == "np") |> 
              rename(pop = value)) |> 
  mutate(per_capita_count = n/pop, 
         per_100k = per_capita_count * 10^5)

count_inc <- count_inc |> group_by(cycle, value) |> mutate(
  ref_per_100k = per_100k[scen == "reference"],
  diff = per_100k - ref_per_100k
)

count_inc_overall <- count_inc |> group_by(scen, value) |> reframe(diff = sum(diff))

plotly::ggplotly(ggplot(count_inc_overall) +
  aes(x = value, y = diff, colour = scen) +
  geom_point() +
  scale_color_hue(direction = 1) +
  coord_flip() +
  theme_minimal()
)

base_pop <- all_data |> 
  filter(scen == "reference", 
         cycle == 1, 
         !value %in% c("dead", "null")) |> 
  group_by(agegroup_cycle, gender) |> 
  summarise(pop = n_distinct(id)) |> 
  ungroup() |> 
  mutate(w = pop/sum(pop), 
         total_pop = sum(pop)) |> 
  collect()

count_inc_age_sex <- all_data |> 
  group_by(id, value) |>  
  filter(cycle == min(cycle[cycle > 0])) |> 
  ungroup() |> 
  group_by(cycle, scen, value) |> 
  summarise(n = dplyr::n()) |> 
  collect()

t <- ad |> 
  filter(!value %in% c("dead", "null", "healthy")) |> 
  group_by(id, value, scen) |> 
  reframe(max_age = max(age_cycle))

people <- all_data |> 
  group_by(agegroup_cycle, gender, cycle, scen) |> 
  summarise(np = n_distinct(id[!value %in% c("dead", "null")])) |> 
  pivot_longer(cols = np) |> collect()

inc_age_gender <- all_data |>
  filter(!value %in% c("null", "dead", "healthy")) |> 
  group_by(id, value) |>
  filter(cycle == min(cycle[cycle > 0])) |>
  ungroup() |>
  group_by(agegroup_cycle, gender, cycle, scen, value) |>
  summarise(n = dplyr::n()) |> 
  collect()

inc_age_gender_uc <- inc_age_gender |> 
  left_join(people |> 
              rename(pop = value) |> dplyr::select(-name)) |> 
  mutate(per_100k = n/pop * 10^5)


inc_age_gender_w <- inc_age_gender_uc |> 
  left_join(base_pop |> rename(std_pop = pop)) |> 
  mutate(weighted_rate = per_100k * w) |> 
  group_by(cycle, scen, value) |>
  reframe(age_std_rate =
            sum(weighted_rate, na.rm = T)) |>
  ungroup()

# |> 
#   group_by(cycle, value) |> 
#   mutate(
#     ref_per_100k = weighted_count[scen == "reference"],
#     diff = weighted_count - ref_per_100k
#   )

plotly::ggplotly(inc_age_gender_w |> group_by(scen, value) |> summarise(weighted_count = sum(weighted_count)) |> 
                   #filter(!(scen %in% "reference")) |> 
                   ggplot() +
                   aes(x = value, y = weighted_count, colour = scen) +
                   geom_point() + scale_x_discrete(guide = guide_axis(angle = 90)) +
                   labs(title = "Age standardized rate per 100k population", y = "weighted_count", x = "") +
                   scale_color_hue(direction = 1) +
                   theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)))

inc_age_gender_w_wide <- 
  inc_age_gender_w |> 
  group_by(cycle, value) |> 
  mutate(diff = age_std_rate - age_std_rate[scen == "reference"])

inc_age_gender_w_wide %>%
  filter(!(scen %in% "reference")) %>%
  ggplot() +
  aes(x = diff, y = value, fill = scen) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(scen))

inc_age_gender_w_median <- inc_age_gender_w_wide |> group_by(scen, value) |> reframe(diff = median(diff)) |> 
       filter(!(scen %in% "reference"))

ggplotly(ggplot(inc_age_gender_w_median) +
  aes(x = value, y = diff, fill = scen) +
  geom_col(position = "dodge2") +
  scale_fill_hue(direction = 1) +
  labs(title = "Median difference from reference per 100K incidence over 30 years", x = "", y = "difference from reference") +
  coord_flip() +
  theme_minimal())

### Using age_cycle instead

base_pop <- all_data |> 
  filter(scen == "reference", 
         cycle == 1, 
         !value %in% c("dead", "null")) |> 
  group_by(age_cycle, gender) |> 
  summarise(pop = n_distinct(id)) |> 
  ungroup() |> 
  mutate(w = pop/sum(pop), 
         total_pop = sum(pop)) |> 
  collect()

people <- all_data |> 
  group_by(age_cycle, gender, cycle, scen) |> 
  summarise(np = n_distinct(id[!value %in% c("dead", "null")])) |> 
  pivot_longer(cols = np) |> 
  collect()

inc_but_death <- all_data |>
  filter(!value %in% c("null", "dead", "healthy")) |> 
  group_by(id, value, scen) |>
  filter(cycle == min(cycle[cycle > 1])) |>
  ungroup() |> 
  collect() 
  
inc_death <- all_data |> 
  group_by(id, scen) |> 
  mutate(age_cycle = max(age_cycle[value != "dead"], na.rm = T) + 1) |> 
  ungroup() |> 
  filter(value == "dead") |> 
  collect()

inc_but_death_grp <- 
  inc_but_death |>
  group_by(age_cycle, gender, cycle, scen, value) |>
  summarise(n = dplyr::n()) |> 
  collect() |> 
  left_join(people |> 
              rename(pop = value) |> 
              dplyr::select(-name)) |> 
  mutate(per_100k = n/pop * 10^5)


inc_age_gender_w <- inc_but_death_grp |> 
  left_join(base_pop |> rename(std_pop = pop)) |> 
  mutate(weighted_rate = per_100k * w) |> 
  group_by(cycle, scen, value) |>
  reframe(age_std_rate =
            sum(weighted_rate, na.rm = T)) |>
  ungroup()


