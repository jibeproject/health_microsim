require(tidyverse)
require(here)
require(arrow)

get_summary <- function(SCEN_NAME){
  
  m <- arrow::open_dataset(paste0("data/", SCEN_NAME, "_dis_inter_state_trans-n.c-10-n.i-28269-n.d-19.parquet/"))
  
  ## Synthetic population file with exposures and physical activity
  synth_pop <- read_csv(here(paste0("jibe health/", SCEN_NAME, "_pp_exposure_RR_2021.csv")))
  
  # Introduce agegroup
  synth_pop <- synth_pop |> 
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf),
                          right=FALSE, include.lowest = TRUE))

  m <- m |> collect() 
  
  m$id <- as.numeric(m$id)
  
  m <- m |> left_join(synth_pop |> select(id, age, agegroup, gender, ladcd, lsoa21cd))
  
  return(m |> pivot_longer(cols = starts_with("c")) |> 
           mutate(unpacked = str_split(value, " ")) |> 
           unnest() |> 
           mutate(value = str_trim(unpacked)) |> 
           dplyr::select(-unpacked) |> 
           mutate(value = str_replace_all(value, fixed("parkinson’s_disease"), "parkinson")) |> 
           group_by(ladcd, name, value) |> 
           summarise(count = dplyr::n()) |> 
           mutate(freq = round(count / sum(count) * 100, 1)) |> 
           arrange(parse_number(name)) |> 
           mutate(name = as.factor(name))
  )
  
  
  
}

dc_base <- get_summary("base") |> mutate(scen = "reference")
dc_green <- get_summary("green") |> mutate(scen = "green")
dc_safestreet <- get_summary("safestreet") |> mutate(scen = "safestreet")
dc_both <- get_summary("both") |> mutate(scen = "both")

dc <- plyr::rbind.fill(dc_base, dc_green, dc_safestreet, dc_both)

zones <- read_csv(here("jibe health/zoneSystem.csv"))

zones <- zones |> distinct(ladcd, ladnm)

# Filter the data for the "healthy" value and reference scenario
reference_df <- dc %>% filter(value == "healthy", scen == "reference")

# Join the reference data with the original data to calculate the change in count
df_change <- dc |> 
  filter(value == "healthy") |>
  left_join(reference_df, by = c("ladcd", "name"), suffix = c("", "_ref")) |>
  mutate(count_change = count - count_ref) |>
  select(ladcd, name, scen, count_change) |> 
  left_join(zones)

# Create a line plot showing the difference for the "healthy" value, faceted by ladcd
g <- ggplot(df_change, aes(x = factor(name, levels = paste0("c", 0:10)), y = count_change, color = scen, group = scen)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ ladnm) +
  labs(title = "Change in Count for Healthy Value Compared to Reference",
       x = "Year",
       y = "Change in Count") +
  theme_minimal()

plotly::ggplotly(g)
