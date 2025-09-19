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
  library(readr)
  library(rlang)
})

# pp_exposure_2021_base_140725.csv
# pp_exposure_2021_safeStreet_300725.csv
# pp_exposure_2021_green_310725.csv
# pp_exposure_2021_both_010825.csv

zones <- read_csv("/media/ali/Expansion/backup_tabea/manchester-main/input/zoneSystem.csv")

add_zones_and_scen <- function(df, zones, scen = "reference"){
  return(
    df |> left_join(zones |> 
                      dplyr::select(oaID, imd10) |> 
                      rename(zone = oaID)) |> 
      mutate(scen = scen)
  )
}

base <- read_csv("/media/ali/Expansion/backup_tabea/manchester-main/input/health/pp_exposure_2021_base_140725.csv")
green <- read_csv("/media/ali/Expansion/backup_tabea/manchester-main/input/health/pp_exposure_2021_green_310725.csv")
ss <- read_csv("/media/ali/Expansion/backup_tabea/manchester-main/input/health/pp_exposure_2021_safeStreet_300725.csv")
both <- read_csv("/media/ali/Expansion/backup_tabea/manchester-main/input/health/pp_exposure_2021_both_010825.csv")

base <- add_zones_and_scen(df = base, zones, scen = "reference")
green <- add_zones_and_scen(df = green, zones, scen = "green")
ss <- add_zones_and_scen(df = ss, zones, scen = "safeStreet")
both <- add_zones_and_scen(df = both, zones, scen = "both")

exp <- bind_rows(base, green, ss, both)

arrow::write_dataset(exp, partitioning = "scen", "temp/exp.parquet")

# exp <- arrow::open_dataset("temp/exp.parquet/")
# 
# exp |> group_by(imd10, scen) |> summarise(pa = median(mmetHr_cycle + mmetHr_otherSport + mmetHr_walk), 
#                                     across(starts_with("exposure"), ~median(.x, na.rm = TRUE), .names = "{.col}")
# ) |> head()
