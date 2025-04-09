require(tidyverse)
require(here)
require(arrow)

# Boolean variable for dir/file paths
FILE_PATH_BELEN <- FALSE
n.i <<- 2824#282727
n.c <<- 100

get_summary <- function(SCEN_NAME, group_vars = NULL){
  
  # SCEN_NAME <- "base"

  
  if (!FILE_PATH_BELEN){
    m <- arrow::open_dataset(here(paste0("data/", SCEN_NAME, "_dis_inter_state_trans-n.c-", n.c, "-n.i-", n.i, "-n.d-19.parquet")))
  }else{
    m <- arrow::open_dataset(paste0("manchester/health/processed/", SCEN_NAME, "_dis_inter_state_trans-n.c-10-n.i-28269-n.d-19.parquet/"))
  }
  
  
  ## Synthetic population file with exposures and physical activity
  if (!FILE_PATH_BELEN){
    synth_pop <- read_csv(here(paste0("jibe health/", SCEN_NAME, "_pp_exposure_RR_2021.csv")))
  }else{
    synth_pop <- read_csv(here(paste0("manchester/health/processed/", SCEN_NAME, "_pp_exposure_RR_2021.csv")))
  }
  
  # Introduce agegroup
  synth_pop <- synth_pop |> 
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf),
                          right=FALSE, include.lowest = TRUE))

  m <- m |> collect() 
  
  m$id <- as.numeric(m$id)
  
  m <- m |> left_join(synth_pop |> dplyr::select(id, age, agegroup, gender, ladcd, lsoa21cd))
  
  return(m |> pivot_longer(cols = starts_with("c")) |> 
           arrange(parse_number(name)) |> 
           mutate(cycle = as.factor(name))|> 
           mutate(unpacked = str_split(value, " ")) |> 
           unnest() |> 
           mutate(value = str_trim(unpacked)) |> 
           dplyr::select(-unpacked) |> 
           mutate(value = str_replace_all(value, fixed("parkinson’s_disease"), "parkinson")) |> 
           group_by(across(all_of(group_vars))) |> 
           summarise(count = dplyr::n()) |> 
           mutate(freq = round(count / sum(count) * 100, 1)) 
  )
  
  
  
}



############################################## Figures by LAD ######################################################
##### Over time by lad

dc_base <- get_summary("base", group_vars = c("ladcd", "cycle", "value")) |> mutate(scen = "reference")
dc_green <- get_summary("green", group_vars = c("ladcd", "cycle", "value")) |> mutate(scen = "green")
dc_safestreet <- get_summary("safestreet", group_vars = c("ladcd", "cycle", "value")) |> mutate(scen = "safestreet")
dc_both <- get_summary("both", group_vars = c("ladcd", "cycle", "value")) |> mutate(scen = "both")

dc <- plyr::rbind.fill(dc_base, dc_green, dc_safestreet, dc_both)

# dc <- plyr::rbind.fill(dc_base, dc_both)


if (!FILE_PATH_BELEN){
  zones <- read_csv(here("jibe health/zoneSystem.csv"))
}else{
  zones <- read_csv(here("manchester/health/processed/zoneSystem.csv"))
}

zones <- zones |> distinct(ladcd, ladnm)


####### Healthy people over time
########Difference with baseline
# Filter the data for the "healthy" value and reference scenario
reference_df <- dc %>% filter(value == "healthy", scen == "reference")

# Join the reference data with the original data to calculate the change in count
df_change <- dc |> 
  filter(value == "healthy") |>
  left_join(reference_df, by = c("ladcd", "cycle"), suffix = c("", "_ref")) |>
  mutate(count_change = count - count_ref) |>
  dplyr::select(ladcd, cycle, scen, count, count_ref, count_change) |> 
  left_join(zones)

# Create a line plot showing the difference for the "healthy" value, faceted by ladcd
g <- ggplot(df_change, aes(x = factor(cycle, levels = paste0("c", 0:n.c)), y = count_change, color = scen, group = scen)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ ladnm) +
  labs(title = "Change in Count for Healthy Value Compared to Reference",
       x = "Year",
       y = "Change in Count") +
  theme_minimal()

plotly::ggplotly(g)


###### Life years over time
# Filter the data for the "dead" value and reference scenario
reference_df <- dc %>% filter(value != "dead", scen == "reference") |> group_by(cycle, ladcd, scen) |> summarise(count = sum(count)) # futher grouping here as there are people in diff health states
# Join the reference data with the original data to calculate the change in count
df_change <- dc |>
  filter(value != "dead") |>
  group_by(cycle, ladcd, scen) |> summarise(count = sum(count)) |>
  ungroup() |>
  left_join(reference_df, by = c("ladcd", "cycle"), suffix = c("", "_ref")) |>
  mutate(count_change = count - count_ref) |>
  dplyr::select(ladcd, cycle, scen, count_change) |>
  left_join(zones)
# Create a line plot showing the difference for the "alive" value, faceted by ladcd
g <- ggplot(df_change, aes(x = factor(cycle, levels = paste0("c", 0:n.c)), y = count_change, color = scen, group = scen)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ ladnm) +
  labs(title = "Change in Count for Alive Individuals Compared to Reference",
       x = "Year",
       y = "Change in Count") +
  theme_minimal()
plotly::ggplotly(g)


###### Cumulative 

####### Cumulative by lad

dc_base <- get_summary("base", group_vars = c("ladcd", "value")) |> mutate(scen = "reference")
dc_green <- get_summary("green", group_vars = c("ladcd", "value")) |> mutate(scen = "green")
dc_safestreet <- get_summary("safestreet", group_vars = c("ladcd", "value")) |> mutate(scen = "safestreet")
dc_both <- get_summary("both", group_vars = c("ladcd", "value")) |> mutate(scen = "both")

dc <- plyr::rbind.fill(dc_base, dc_green, dc_safestreet, dc_both)

if (!FILE_PATH_BELEN){
  zones <- read_csv(here("jibe health/zoneSystem.csv"))
}else{
  zones <- read_csv(here("manchester/health/processed/zoneSystem.csv"))
}

zones <- zones |> distinct(ladcd, ladnm)

###### Change in number of healthy people

reference_df <- dc %>% filter(value == "healthy", scen == "reference")

df_change <- dc |> 
  filter(value == "healthy") |>
  left_join(reference_df, by = "ladcd", suffix = c("", "_ref")) |>
  mutate(count_change = count - count_ref) |>
  dplyr::select(ladcd, scen, count, count_ref, count_change) |> 
  left_join(zones)


# Filter out the 'reference' scenario
df_filtered <- df_change %>% filter(scen != "reference")

# Create the bar chart
g <- ggplot(df_filtered, aes(x = scen, y = count_change, fill = scen)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ ladnm, scales = "free") +
  labs(title = "Change in number of healthy people by Scenario and local district",
       x = "Scenario",
       y = "Count Change") +
  theme_minimal()


plotly::ggplotly(g)

###### Change in number of people alive

reference_df <- dc %>% filter(value != "dead", scen == "reference") |> group_by(ladcd, scen) |> summarise(count = sum(count)) # futher grouping here as there are people in diff health states
# Join the reference data with the original data to calculate the change in count
df_change <- dc |>
  filter(value != "dead") |>
  group_by(ladcd, scen) |> summarise(count = sum(count)) |>
  ungroup() |>
  left_join(reference_df, by = c("ladcd"), suffix = c("", "_ref")) |>
  mutate(count_change = count - count_ref) |>
  dplyr::select(ladcd, scen, count_change) |>
  left_join(zones)

# Create a line plot showing the difference for the "alive" value, faceted by ladcd

df_filtered <- df_change %>% filter(scen != "reference")


g <- ggplot(df_filtered, aes(x = scen, y = count_change, fill = scen)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ ladnm, scales = "free") +
  labs(title = "Change in number of life years by scenario and local district",
       x = "Scenario",
       y = "Count Change") +
  theme_minimal()


plotly::ggplotly(g)


##### Change incidence

### filter largest diseases as per pifs

reference_df <- dc %>% filter(!value %in% c("dead", "healthy"), scen == "reference")

# Join the reference data with the original data to calculate the change in count
df_change <- dc |>
  filter(!value %in% c("dead", "healthy")) |>
  left_join(reference_df, by = c("ladcd", "value"), suffix = c("", "_ref")) |>
  mutate(count_change = count - count_ref) |>
  dplyr::select(ladcd, scen, count_change, count, count_ref, value) |>
  left_join(zones)

# Create a line plot showing the difference for the "alive" value, faceted by ladcd

df_filtered <- df_change %>% filter(scen != "reference") %>% 
                filter(value %in% c("all_cause_dementia" , "coronary_heart_disease",
                                    "stroke", "diabetes", "depression"))


g <- ggplot(df_filtered, aes(x = scen, y = count_change, fill = value)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ ladnm, scales = "free") +
  labs(
    title = "Change in number of cases of diseases by scenario and local district",
    x = "Scenario",
    y = "Change in Number of Cases",
    fill = "Disease"
  ) +
  theme_minimal()



plotly::ggplotly(g)



####### Cumulative by sex

dc_base <- get_summary("base", group_vars = c("gender", "value")) |> mutate(scen = "reference")
dc_green <- get_summary("green", group_vars = c("gender", "value")) |> mutate(scen = "green")
dc_safestreet <- get_summary("safestreet", group_vars = c("gender", "value")) |> mutate(scen = "safestreet")
dc_both <- get_summary("both", group_vars = c("gender", "value")) |> mutate(scen = "both")

dc <- plyr::rbind.fill(dc_base, dc_green, dc_safestreet, dc_both)

if (!FILE_PATH_BELEN){
  zones <- read_csv(here("jibe health/zoneSystem.csv"))
}else{
  zones <- read_csv(here("manchester/health/processed/zoneSystem.csv"))
}

zones <- zones |> distinct(ladcd, ladnm)

reference_df <- dc %>% filter(value == "healthy", scen == "reference")

df_change <- dc |> 
  filter(value == "healthy") |>
  left_join(reference_df, by = "ladcd", suffix = c("", "_ref")) |>
  mutate(count_change = count - count_ref) |>
  dplyr::select(ladcd, scen, count, count_ref, count_change) |> 
  left_join(zones)


# Filter out the 'reference' scenario
df_filtered <- df_change %>% filter(scen != "reference")

# Create the bar chart
g <- ggplot(df_filtered, aes(x = scen, y = count_change, fill = scen)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ ladnm, scales = "free") +
  labs(title = "Change in number of healthy people by Scenario and local district",
       x = "Scenario",
       y = "Count Change") +
  theme_minimal()


plotly::ggplotly(g)


