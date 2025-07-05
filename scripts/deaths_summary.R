calculate_deaths <- function(dataset, scen_name, scaling_factor = 0.05){
  # dataset <- all_data$base
  # scen_name <- 'reference'
  # base_pop <- dataset |> filter(value != "dead", cycle == 1) |> group_by(cycle) |> summarise(n_distinct(id)) |>  pull()
  #base_pop <- dataset |> filter(value != "dead") |> group_by(cycle) |> summarise(n_distinct(id))
  deaths <- dataset |> 
    filter(value == "dead", !is.na(age_cycle)) |> #, !is.na(ladcd)) |> 
    group_by(cycle) |> 
    summarise(deaths = n_distinct(id)) |> 
    mutate(new_deaths = deaths - lag(deaths, 1))
  
  deaths$new_deaths <- 
    ifelse(is.na(deaths$new_deaths), 
           deaths$deaths, deaths$new_deaths)
  return(deaths |> mutate(total_deaths = new_deaths * 1 / scaling_factor, name = scen_name)) 
  # return(deaths |> left_join(base_pop) |> 
  #          mutate(unit_death = new_deaths/n_alive, 
  #                 total_deaths = round(unit_death * 2827285), 
  #                         name = scen_name))
  
  # return(deaths |> mutate(base_pop = base_pop,
  #                         unit_death = new_deaths/base_pop,
  #                         total_deaths = round(unit_death * 2827285), 
  #                         name = scen_name))
  
  
}

deaths <- rbind(calculate_deaths(all_data$base, "reference"), 
                calculate_deaths(all_data$green, "green"),
                calculate_deaths(all_data$safeStreet, "safeStreet"), 
                calculate_deaths(all_data$both, "both"))

deaths |> 
  group_by(name) |> 
  summarise(total_deaths = sum(total_deaths)) |> 
  mutate(averted_deaths = total_deaths[name == "reference"] - total_deaths) |> 
  print()



calculate_alive <- function(dataset, scen_name, scaling_factor = 0.05){
  return(dataset |> 
    filter(value != "dead", !is.na(value)) |> #, !is.na(ladcd)) |> 
    group_by(cycle) |> 
    summarise(n_alive = n_distinct(id)) |> 
    mutate(total_n_alive = n_alive * 1 / scaling_factor, name = scen_name)
    ) 
  
}

n_alive <- rbind(calculate_alive(all_data$base, "reference", scaling_factor = 1), 
                #calculate_alive(all_data$green, "green"),
                calculate_alive(all_data$safeStreet, "safeStreet", scaling_factor = 1))#, 
                #calculate_alive(all_data$both, "both"))

deaths |> 
  group_by(name) |> 
  summarise(total_deaths = sum(total_deaths)) |> 
  mutate(averted_deaths = total_deaths[name == "reference"] - total_deaths) |> 
  print()


annual_deaths <- deaths |>
  filter(name != "reference") |>
  group_by(cycle, name) |>
  summarise(total_deaths = total_deaths) |>
  left_join(deaths |>
              filter(name == "reference") |>
              group_by(cycle, name) |>
              summarise(total_deaths_ref = total_deaths) |>
              dplyr::select(-c(name))) |>
  mutate(diff = total_deaths - total_deaths_ref)

alive_data |>
  filter(scen != "reference") |>
  group_by(cycle, scen) |>
  summarise(alive_n = sum(alive_n) * 20) |>
  left_join(alive_data |>
              filter(scen == "reference") |>
              group_by(cycle, scen) |>
              summarise(alive_n_ref = sum(alive_n) * 20) |>
              ungroup() |>
              dplyr::select(-c(scen))) |>
  mutate(diff_alive = alive_n - alive_n_ref) |> 
  left_join(annual_deaths |> 
              rename(scen = name, 
                     diff_deaths = diff)) 


calculate_newborns_count <- function(dataset, scen_name, scaling_factor = 0.05) {
  newborns_df <- dataset |> 
    filter(age == 0) |> #, !is.na(ladcd)) |> 
    group_by(cycle) |> 
    summarise(newborns = n_distinct(id)) |> 
    mutate(new_newborns = newborns - lag(newborns, 1),
           new_newborns = if_else(is.na(new_newborns), newborns, new_newborns))
  return(newborns_df |> mutate(total_newborns = new_newborns * 1 / scaling_factor, name = scen_name)) 
}

nb <- rbind(calculate_newborns_count(all_data$base, "reference"), 
            calculate_newborns_count(all_data$green, "green"),
            calculate_newborns_count(all_data$safeStreet, "safeStreet"), 
            calculate_newborns_count(all_data$both, "both"))

deaths |> 
  group_by(name) |> 
  summarise(total_deaths = sum(total_deaths)) |> 
  mutate(averted_deaths = total_deaths[name == "reference"] - total_deaths) |> 
  print()



calculate_alive <- function(dataset, scen_name, scaling_factor = 0.05){
  
  base_pop <- dataset |> 
    filter(value != "dead", !is.na(ladcd), cycle == 1) |> 
    group_by(cycle) |> 
    summarise(n_distinct(id)) |>  pull()
  deaths <- dataset |> 
    filter(value != "dead") |> 
    group_by(cycle) |> 
    summarise(deaths = n_distinct(id)) |> 
    mutate(new_deaths = deaths - lag(deaths, 1))
  deaths$new_deaths <- ifelse(is.na(deaths$new_deaths), deaths$deaths, deaths$new_deaths)
  return(deaths |> mutate(base_pop = base_pop,
                          unit_death = new_deaths/base_pop,
                          total_deaths = round(unit_death * 2827285), 
                          name = scen_name))
  
  # dataset <- all_data$base
  # scen_name <- 'reference'
  # base_pop <- dataset |> filter(value != "dead", cycle == 1) |> group_by(cycle) |> summarise(n_distinct(id)) |>  pull()
  #base_pop <- dataset |> filter(value != "dead") |> group_by(cycle) |> summarise(n_distinct(id))
  # alive <- dataset |> 
  #   filter(value != "dead") |> 
  #   group_by(cycle) |> 
  #   summarise(alive = n_distinct(id)) |> 
  #   mutate(new_born = alive - lag(alive, 1))
  # 
  # alive$new_born <- 
  #   ifelse(is.na(alive$new_born), 
  #          alive$alive, alive$new_born)
  # return(alive |> mutate(total_alive = new_born * 1 / scaling_factor, name = scen_name)) 
  # return(deaths |> left_join(base_pop) |> 
  #          mutate(unit_death = new_deaths/n_alive, 
  #                 total_deaths = round(unit_death * 2827285), 
  #                         name = scen_name))
  
  # return(deaths |> mutate(base_pop = base_pop,
  #                         unit_death = new_deaths/base_pop,
  #                         total_deaths = round(unit_death * 2827285), 
  #                         name = scen_name))
  
  
}

alive <- rbind(calculate_alive(all_data$base, "reference"), 
               calculate_alive(all_data$green, "green"),
               calculate_alive(all_data$safeStreet, "safeStreet"), 
               calculate_alive(all_data$both, "both"))

alive |> 
  group_by(name) |> 
  summarise(total_deaths = sum(total_deaths)) |> 
  mutate(n_alive= total_deaths - total_deaths[name == "reference"]) |> 
  print()

all_data_scen <- bind_rows(all_data)


alive_diff_ref <- tot_pop |>
  filter(scen != "reference") |>
  group_by(cycle, scen) |>
  reframe(n_alive = n_alive) |>
  left_join(tot_pop |>
              filter(scen == "reference") |>
              group_by(cycle, scen) |>
              reframe(n_alive_ref = n_alive) |>
              dplyr::select(-c(scen))) |>
  mutate(diff_alive = n_alive - n_alive_ref)

alive_diff_ref |> group_by(scen) |> reframe(diff_alive = sum(diff_alive, na.rm = T))



dead_diff_ref <- tot_pop |>
  filter(scen != "reference") |>
  group_by(cycle, scen) |>
  reframe(new_deaths = new_deaths) |>
  left_join(tot_pop |>
              filter(scen == "reference") |>
              group_by(cycle, scen) |>
              reframe(new_deaths_ref = new_deaths) |>
              dplyr::select(-c(scen))) |>
  mutate(diff_dead = new_deaths - new_deaths_ref)

dead_diff_ref |> group_by(scen) |> reframe(diff_dead = sum(diff_dead, na.rm = T)) |> left_join(
  alive_diff_ref |> group_by(scen) |> reframe(diff_alive = sum(diff_alive, na.rm = T))
)



|>
  mutate(diff_pop = tot_pop - tot_pop_ref)





calculate_cause_count <- function(dataset, scen_name, scaling_factor = 0.05){
  # dataset <- all_data$base
  # scen_name <- 'reference'
  # scaling_factor = 0.05
  # base_pop <- dataset |> filter(value != "dead", cycle == 1) |> group_by(cycle) |> summarise(n_distinct(id)) |>  pull()
  #base_pop <- dataset |> filter(value != "dead") |> group_by(cycle) |> summarise(n_distinct(id))
  count_df <- dataset |> 
    filter(value != "dead") |> 
    group_by(cycle, value) |> 
    summarise(count = n_distinct(id)) |> 
    mutate(new_count = count - lag(count, 1),
           new_count = if_else(is.na(new_count), count, new_count),
           total_count = new_count * 1 / scaling_factor, name = scen_name)
           
  
  return(count_df)
  # return(deaths |> left_join(base_pop) |> 
  #          mutate(unit_death = new_deaths/n_alive, 
  #                 total_deaths = round(unit_death * 2827285), 
  #                         name = scen_name))
  
  # return(deaths |> mutate(base_pop = base_pop,
  #                         unit_death = new_deaths/base_pop,
  #                         total_deaths = round(unit_death * 2827285), 
  #                         name = scen_name))
  
  
}


count_df <- rbind(calculate_cause_count(all_data$base, "reference"), 
                  calculate_cause_count(all_data$green, "green"),
                  calculate_cause_count(all_data$safeStreet, "safeStreet"), 
                  calculate_cause_count(all_data$both, "both"))

summary_df <- 
  count_df |> 
  group_by(value, name) |> 
  summarise(total_count = sum(total_count)) |> 
  mutate(averted_n = total_count[name == "reference"] - total_count) 
  




calculate_n_alive <- function(dataset, scen_name, scaling_factor = 0.05){
  n_alive_df <- dataset |> 
    filter(value != "dead") |> 
    group_by(cycle) |> 
    summarise(n_alive = n_distinct(id)) |> 
    mutate(new_born = n_alive - lag(n_alive, 1))
  
  n_alive_df$new_born <- 
    ifelse(is.na(n_alive_df$new_born), 
           n_alive_df$deaths, n_alive_df$new_born)
  return(deaths |> mutate(total_deaths = new_deaths * 1 / scaling_factor, name = scen_name)) 
  
  
}

count_values <- function(dataset, scen_name, v = "healthy", scaling_factor = 0.05){
  return(dataset |> 
           filter(value == v) |> 
           group_by(cycle) |> 
           summarise(count = n_distinct(id)) |> 
           mutate(name = scen_name)
  )
}
    
count_df <- rbind(count_values(all_data$base, "reference"), 
                  count_values(all_data$green, "green"),
                  count_values(all_data$safeStreet, "safeStreet"), 
                  count_values(all_data$both, "both"))
 
