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
  library(data.table)
})

# === Data loading function ===
get_summary <- function(SCEN_NAME, 
                        final_year = 2051, 
                        microdata_dir_name = "microdata", 
                        manchester_folder = "", 
                        group_vars = NULL, 
                        summarise = TRUE) {
  # final_year <- 2022
  # microdata_dir_name <- "microData"
  # 
  # SCEN_NAME <- "base"
  
  print(microdata_dir_name)
  
  #browser()
  
  file_path <- paste0(
    manchester_folder, "scenOutput/",
    SCEN_NAME, "/", microdata_dir_name, "/pp_healthDiseaseTracker_", final_year, ".csv"
  )
  
  zones <- read_csv(paste0(manchester_folder, "input/zoneSystem.csv"))
  
  
  # Read health tracker file
  if (grepl("\\.csv$", file_path)) {
    m <- read_csv(file_path)#arrow::open_csv_dataset(file_path) |> to_duckdb() |> collect()
  } else {
    m <- arrow::open_dataset(file_path)
  }
  
  m$id <- as.numeric(m$id)
  
  # Rename year columns to c0, c1, ...
  year_cols <- grep("^20", names(m), value = TRUE)
  new_names <- paste0("c", seq_along(year_cols) - 1)
  names(m)[match(year_cols, names(m))] <- new_names
  
  # Load population files
  pop_dir_path <- paste0(manchester_folder, "scenOutput/", SCEN_NAME, "/", microdata_dir_name)

  pp_csv_files <- list.files(path = pop_dir_path, pattern = "^pp_\\d{4}\\.csv$", full.names = TRUE)
  newborn_data <- lapply(pp_csv_files, function(file) {
    read_csv(file) |> filter(age == 0)
  }) |> bind_rows()
  
  dd_csv_files <- list.files(path = pop_dir_path, pattern = "^dd_\\d{4}\\.csv$", full.names = TRUE)
  dd_data <- lapply(dd_csv_files, read_csv) |> bind_rows()
  
  newborn_data <- newborn_data |> 
    left_join(dd_data |> select(hhID, zone) |> distinct(hhID, .keep_all = TRUE) |> rename(hhid = hhID))
  
  synth_pop <- newborn_data |>
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf), 
                          labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
                          right = FALSE, 
                          include.lowest = TRUE)) |> 
    dplyr::select(id, age, agegroup, gender, zone) |> 
    left_join(zones  |> rename(zone = oaID) |> dplyr::select(zone, ladcd, lsoa21cd)) |> 
    distinct()
  
  # Exposure population
  pop_path <- paste0(manchester_folder, "input/health/pp_exposure_2021_base_140725.csv")
  
  if (!file.exists(pop_path)) stop("Population exposure file does not exist: ", pop_path)
  
  synth_pop_2021 <- readr::read_csv(pop_path) |>
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf), 
                          labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
                          right = FALSE, 
                          include.lowest = TRUE))
  
  synth_pop_2021 <- synth_pop_2021 |> 
    dplyr::select(id, age, agegroup, gender, zone) |> 
    left_join(zones  |> 
                rename(zone = oaID) |> 
                dplyr::select(zone, ladcd, lsoa21cd))
  
  synth_pop <- bind_rows(synth_pop, synth_pop_2021)
  
  # # Filter out early dead and merge population info
  m <- m |> 
    mutate(across(
      everything(),
      ~ ifelse(
        str_detect(., "dead"),
        str_extract(., "\\bdead[^|,;]*"),
        .
      )
    )) |> 
    left_join(synth_pop |> dplyr::select(id, age, agegroup, gender, ladcd, lsoa21cd)) 
  #|> 
  #   mutate(across(starts_with("c"), ~ ifelse(str_detect(., "killed"), "dead", .))) # Ali to update
  
  # Convert to data.table (in place)
  setDT(m)
  
  # Melt wide to long on all columns starting with "c"
  long_data <- melt(m, id.vars = c("id", "age", "agegroup", "gender", "ladcd", "lsoa21cd"), 
                    measure.vars = patterns("^c"), 
                    variable.name = "name", value.name = "value")
  
  # Cycle from column name
  long_data[, cycle := as.numeric(str_remove(name, "^c"))]
  
  # Split pipe-separated values and unlist into rows
  long_data <- long_data[, .(value = unlist(strsplit(value, "\\|"))), 
                         by = .(id, age, agegroup, gender, ladcd, lsoa21cd, name, cycle)]
  
  # Trim whitespace and replace terms
  long_data[, value := str_trim(value)]
  long_data[, value := str_replace_all(value, fixed("parkinson’s_disease"), "parkinson")]
  
  # Filter out "null"
  long_data <- long_data[value != "null"]
  
  # Order by id and cycle
  setorder(long_data, id, cycle)
  
  # Compute cumulative dead count and age_cycle
  long_data[, dead_count := cumsum(grepl("dead", value)), by = id]
  min_cycle <- long_data[value != "null", .(min_cycle = min(cycle)), by = id]
  long_data <- merge(long_data, min_cycle, by = "id")
  long_data[, age_cycle := ifelse(value != "null", age + cycle - min_cycle, NA_real_)]
  
  # Remove rows with multiple "dead"
  long_data <- long_data[!(grepl("dead", value) & dead_count > 1)]
  
  # Remove helper columns
  long_data[, c("dead_count", "min_cycle") := NULL]
  
  # Create age groups
  breaks <- c(seq(0, 85, 5), Inf)
  labels <- c(paste(seq(0, 80, 5), seq(4, 84, 5), sep = "-"), "85+")
  long_data[, agegroup_cycle := cut(age_cycle, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)]
  
  # Remove duplicates
  long_data <- unique(long_data)
  
  if (!is.null(group_vars) && summarise && length(group_vars) > 0) {
  long_data <- long_data |>
    group_by(across(all_of(group_vars))) |>
    summarise(count = dplyr::n(), .groups = "drop") |>
    mutate(freq = round(count / sum(count) * 100, 1))
  }
  
  
  return(long_data)
}

## Do set three of the most important parameters
## they are: 1) final_year (the year when simulation ends)
## 2) microdata_dir_name (name of the microdata folder)
## 3) manchester_folder (path to the root of manchester input data folder)
## example call for the function is: base = get_summary("base", summarise = FALSE, 
## final_year = 2022, 
## microdata_dir_name = "microData", 
## manchester_folder = "/media/ali/Expansion/backup_tabea/manchester-main")
manchester_folder = "/run/user/1000/gvfs/smb-share:server=ifs-prod-1152-cifs.ifs.uis.private.cam.ac.uk,share=cedar-grp-drive/HealthImpact/Data/Country/UK/JIBE/manchester/"
manchester_folder <- "/media/ali/Expansion/backup_tabea/manchester-main/"
fyear <- 2051

## === Prepare general data long ===
all_data <- list(
  base = get_summary("100%/base", summarise = FALSE, final_year = fyear, manchester_folder = manchester_folder) |> mutate(scen = "reference"),
  #green = get_summary("green", summarise = FALSE, final_year = fyear, manchester_folder = manchester_folder) |> mutate(scen = "green"),
  safeStreet = get_summary("100%/safeStreet", summarise = FALSE, final_year = fyear, manchester_folder = manchester_folder) |> mutate(scen = "safeStreet"),
  #both = get_summary("both", summarise = FALSE, final_year = fyear, manchester_folder = manchester_folder) |> mutate(scen = "both")
)

