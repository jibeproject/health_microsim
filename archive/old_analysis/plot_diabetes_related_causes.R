# Read libraries
library(here)
library(arrow)
library(plotly)
library(tidyverse)


# Boolean variable for dir/file paths
FILE_PATH_BELEN <- FALSE

n.i <- 282727#28269#2824#282727
n.c <- 30

get_summary <- function(SCEN_NAME, n.i, n.c){
  
  # SCEN_NAME <- 'base'
  
  if (!FILE_PATH_BELEN){
    m <- arrow::open_dataset(here(paste0("data/", SCEN_NAME, "_dis_inter_state_trans-n.c-", n.c, "-n.i-", n.i, "-n.d-19.parquet")))
  }else{
    m <- arrow::open_dataset(here(paste0("manchester/health/processed/", SCEN_NAME, "_dis_inter_state_trans-n.c-10-n.i-28269-n.d-19.parquet")))
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
  
  m <- m |> collect() |> mutate(id = as.numeric(id)) 
  
  #m$id <- as.numeric(m$id)
  
  m <- m |> left_join(synth_pop |> select(id, age, agegroup, gender, ladcd, lsoa21cd))
  
  return(m |> pivot_longer(cols = starts_with("c")) |> 
           mutate(unpacked = str_split(value, " ")) |> 
           unnest() |> 
           mutate(value = str_trim(unpacked)) |> 
           dplyr::select(-unpacked) |> 
           mutate(value = str_replace_all(value, fixed("parkinson’s_disease"), "parkinson")) |> 
           group_by(ladcd, name, agegroup, gender, value) |> 
           summarise(count = dplyr::n()) |> 
           mutate(freq = round(count / sum(count) * 100, 1)) |> 
           arrange(parse_number(name)) |> 
           mutate(name = as.factor(name))
  )
  
  
  
}

dc_base <- get_summary("base", n.i, n.c) |> mutate(scen = "reference")
dc_green <- get_summary("green", n.i, n.c) |> mutate(scen = "green")
dc_safestreet <- get_summary("safestreet", n.i, n.c) |> mutate(scen = "safestreet")
dc_both <- get_summary("both", n.i, n.c) |> mutate(scen = "both")

dc <- plyr::rbind.fill(dc_base, dc_green, dc_safestreet, dc_both)

df <- dc#read_csv("app/health_microsim_app/dc.csv")
zones <- read_csv(here("jibe health/zoneSystem.csv")) |> distinct(ladcd, ladnm)

df <- df |> left_join(zones) |> rename(cause = value)

# Summarize alive counts by ladnm and scen
alive_df <- df |>
  filter(cause != "dead") |>
  group_by(ladnm, scen) |>
  summarize(alive_count = sum(count), .groups = "drop")

disease_df <- df |>
  filter(cause %in% c("diabetes", "coronary_heart_disease", "stroke")) |>
  group_by(ladnm, scen, cause) |>
  summarize(disease_count = sum(count), .groups = "drop") |>
  left_join(alive_df, by = c("ladnm", "scen"), suffix = c("", "_alive")) |>
  mutate(prop = round(disease_count/alive_count * 100, 3)) |> 
  group_by(ladnm, cause) |>
  mutate(reference_prop = prop[scen == "reference"],
         prop_change = reference_prop - prop) |>
  ungroup() |>
  select(-reference_prop)


plotly::ggplotly(
  ggplot(disease_df |> filter(scen != "reference")) +
                   aes(x = prop_change, y = cause, fill = scen) +
                   geom_bar(stat = "summary", fun = "sum", position = "dodge2") +
                   scale_fill_hue(direction = 1) +
                   theme_minimal() +
                   facet_wrap(vars(ladnm)) + 
                   labs(title = "Reduction in incidence of disease (in proportion) when compared with the reference scenario", 
                        x = "Prop change (%)", y = "Causes/disease") + 
                   geom_text(aes(label = round(prop_change, 3)), 
                             size = 2.5,
                             position = position_dodge(width = 1)))
