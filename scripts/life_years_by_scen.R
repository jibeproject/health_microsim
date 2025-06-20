reference_alive <- alive_data |> 
  filter(scen == "reference", !is.na(agegroup)) |> 
  group_by(cycle, agegroup,  scen) |> 
  summarise(alive_n_ref = sum(alive_n))

scen_alive <- alive_data |> 
  filter(scen != "reference", !is.na(agegroup)) |> 
  group_by(cycle, agegroup,  scen) |> 
  summarise(alive_n = sum(alive_n))


test <- scen_alive |> 
  left_join(reference_alive |> dplyr::select(-scen), by = c("cycle", "agegroup")) |> 
  mutate(count_change = alive_n - alive_n_ref,
         per_unit_change = count_change / alive_n)


# === European Standard Population (ESP2013) ===
esp2013 <- c(
  rep(4000, 1), rep(5500, 4),  # 0-4, 5-9, 10-14, 15-19, 20-24
  rep(5500, 2), rep(6000, 2),  # 25-29, 30-34, 35-39, 40-44
  rep(6000, 2), rep(5000, 2),  # 45-49, 50-54, 55-59, 60-64
  rep(4000, 2), rep(2500, 2),  # 65-69, 70-74, 75-79, 80-84
  rep(1500, 2)                 # 85-89, 90+
)


# summary_raw <- all_data$base
std_pop <- tibble(
  agegroup = c("0-24", "25-44", "45-64", "65-84", "85+"), # European standard population
  std_pop = c(
    sum(esp2013[1:5]),
    sum(esp2013[6:9]),
    sum(esp2013[10:13]),
    sum(esp2013[14:17]),
    sum(esp2013[18:19])
  )
) |> mutate(std_prop = std_pop / sum(std_pop))


test2 <- test |> 
  left_join(std_pop) |> 
  mutate(std_rate = per_unit_change * std_prop * 100000)


|> mutate(std_alive = alive_n/alive_n_ref * std_prop*100000)
alive_data |> 
  group_by(cycle, agegroup,  scen) |> summarise(alive_n = sum(alive_n))


alive_data |> filter(scen != "reference") |> group_by(cycle, scen) |> summarise(alive_n = sum(alive_n)) |> left_join(alive_data |> filter(scen == "reference") |> group_by(cycle, scen) |> summarise(alive_n_ref = sum(alive_n)) |> ungroup() |> dplyr::select(-c(scen))) |> mutate(diff = alive_n - alive_n_ref) |> View()
