calculate_deaths <- function(dataset, scen_name){
  
  base_pop <- dataset |> filter(value != "dead", cycle == 1) |> group_by(cycle) |> summarise(n_distinct(id)) |>  pull()
  deaths <- dataset |> filter(value == "dead") |> group_by(cycle) |> summarise(deaths = n_distinct(id)) |> mutate(new_deaths = deaths - lag(deaths, 1))
  deaths$new_deaths <- ifelse(is.na(deaths$new_deaths), deaths$deaths, deaths$new_deaths)
  return(deaths |> mutate(unit_death = new_deaths/base_pop,
                          total_deaths = round(unit_death * 2827285), 
                          name = scen_name))
  
  
}

deaths <- rbind(calculate_deaths(all_data$base, "reference"), calculate_deaths(all_data$green, "green"),
                calculate_deaths(all_data$safeStreet, "safeStreet"), calculate_deaths(all_data$both, "both"))

