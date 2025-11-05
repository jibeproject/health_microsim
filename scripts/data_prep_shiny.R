# === Load libraries ===
pkgs <- c(
  "tidyverse","arrow","DT","purrr","stringr","esquisse",
  "ggplot2","plotly","readr","rlang","data.table","tcltk"
)

missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(missing)) install.packages(missing, dependencies = TRUE)

suppressPackageStartupMessages(
  invisible(lapply(pkgs, library, character.only = TRUE))
)

# === Data loading function ===
get_summary <- function(SCEN_NAME, 
                        final_year = 2051, 
                        zoneID = "oaID",
                        regionIDs = c(),
                        microdata_folder = "microdata", 
                        region_folder = "",
                        exposure_population = "", 
                        group_vars = NULL, 
                        summarise = TRUE) {
  # final_year <- 2022
  # microdata_dir_name <- "microData"
  # 
  # SCEN_NAME <- "base"
  if (exposure_population == "") {
    print("Argument for `exposure_population` within region_folder must be defined, for example: input/health/pp_exposure_2021_base_140725.csv")
    stop()
  } else {
    exposure_population <- paste0(region_folder, "/", exposure_population)
  }
  
  print(microdata_folder)
  
  #browser()
  output_dir <- paste0(region_folder, "/scenOutput/", SCEN_NAME)
  microdata_dir <- paste0(output_dir, "/", microdata_folder)
  healthDiseaseTracker_file_path <- paste0(microdata_dir, "/pp_healthDiseaseTracker_", final_year, ".csv")
  
  zones <- read_csv(paste0(region_folder, "/input/zoneSystem.csv"))
  zones <- zones %>% rename(zone = !!rlang::sym(zoneID))
  zones_sel <- zones %>% dplyr::select(dplyr::any_of(unique(c("zone", regionIDs))))
  
  
  # Read health tracker file
  if (grepl("\\.csv$", healthDiseaseTracker_file_path)) {
    m <- read_csv(healthDiseaseTracker_file_path)#arrow::open_csv_dataset(file_path) |> to_duckdb() |> collect()
  } else {
    m <- arrow::open_dataset(healthDiseaseTracker_file_path)
  }
  
  m$id <- as.numeric(m$id)
  
  # Rename year columns to c0, c1, ...
  year_cols <- grep("^20", names(m), value = TRUE)
  new_names <- paste0("c", seq_along(year_cols) - 1)
  names(m)[match(year_cols, names(m))] <- new_names
  
  # Load population files
  

  pp_csv_files <- list.files(path = microdata_dir, pattern = "^pp_\\d{4}\\.csv$", full.names = TRUE)
  newborn_data <- lapply(pp_csv_files, function(file) {
    read_csv(file) |> filter(age == 0)
  }) |> bind_rows()
  
  dd_csv_files <- list.files(path = microdata_dir, pattern = "^dd_\\d{4}\\.csv$", full.names = TRUE)
  dd_data <- lapply(dd_csv_files, read_csv) |> bind_rows()
  
  newborn_data <- newborn_data |> 
    left_join(dd_data |> select(hhID, zone) |> distinct(hhID, .keep_all = TRUE) |> rename(hhid = hhID))
  
  synth_pop <- newborn_data |>
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf), 
                          labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
                          right = FALSE, 
                          include.lowest = TRUE)) |> 
    dplyr::select(id, age, agegroup, gender, zone) |> 
    left_join(zones_sel) |> 
    distinct()
  
  # Exposure population  
  if (!file.exists(exposure_population)) stop("Population exposure file does not exist: ", exposure_population)
  
  synth_pop_base <- readr::read_csv(exposure_population) |>
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf), 
                          labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
                          right = FALSE, 
                          include.lowest = TRUE))
  
  synth_pop_base <- synth_pop_base |> 
    dplyr::select(id, age, agegroup, gender, zone) |> 
    left_join(zones_sel)
  
  synth_pop <- bind_rows(synth_pop, synth_pop_base)
  
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
    left_join(
      synth_pop %>% dplyr::select(
        dplyr::any_of(
          unique(c("id", "age", "agegroup", "gender", regionIDs)
          )
        )
      ),
      by = "id"
    )
  #|> 
  #   mutate(across(starts_with("c"), ~ ifelse(str_detect(., "killed"), "dead", .))) # Ali to update
  
  # Convert to data.table (in place)
  setDT(m)
  
  # Melt wide to long on all columns starting with "c"
  id_vars <- intersect(names(m), c("id", "age", "agegroup", "gender", regionIDs))

  long_data <- melt(
    m,
    id.vars = id_vars,
    measure.vars = patterns("^c"),
    variable.name = "name",
    value.name = "value"
  )
  
  # Cycle from column name
  long_data[, cycle := as.numeric(str_remove(name, "^c"))]
  
  # Split pipe-separated values and unlist into rows
  region_cols <- intersect(names(long_data), regionIDs)
  group_cols <- unique(c(intersect(names(long_data), c("id", "age", "agegroup", "gender")), region_cols, "name", "cycle"))

  long_data <- long_data[, .(value = unlist(strsplit(value, "\\|"))), by = group_cols]
  
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
region_folder = tcltk::tk_choose.dir(caption = "Select a study region folder with SILO outputs")

# Check if a folder was selected
if (!is.na(region_folder)) {
  cat("Selected folder path:", region_folder, "\n")
} else {
  cat("No folder selected.\n")
}

## === Prepare general data long ===

if (grepl("manchester", tolower(basename(region_folder)))) {
    exposure_population = "input/health/pp_exposure_2021_base_140725.csv"
    fyear <- 2051
    zoneID = "oaID"
    regionIDs <- c("ladcd","lsoa21cd")
    all_data <- list(
        base = get_summary("100%/base", 
                        zoneID = zoneID,
                        regionIDs = regionIDs,
                        exposure_population = exposure_population,
                        summarise = FALSE, 
                        final_year = fyear, 
                        region_folder = region_folder
                ) |> 
                mutate(scen = "reference"),
        #green = get_summary("green", summarise = FALSE, final_year = fyear, manchester_folder = manchester_folder) |> mutate(scen = "green"),
        safeStreet = get_summary("100%/safeStreet", 
                        zoneID = zoneID,
                        regionIDs = regionIDs,
                        exposure_population = exposure_population,
                        summarise = FALSE, 
                        final_year = fyear, 
                        region_folder = region_folder
                      ) |> mutate(scen = "safeStreet"),
        #both = get_summary("both", summarise = FALSE, final_year = fyear, manchester_folder = manchester_folder) |> mutate(scen = "both")
    )
}else if (grepl("melbourne", tolower(basename(region_folder)))) {
    exposure_population = "input/health/pp_exposure_2018_base.csv" # this file does not yet exist for melbourne
    fyear <- 2051
    regionIDs <- c("SA1_7DIG16","SA2_MAIN16")
    all_data <- list(
        base = get_summary("base", 
                zoneID = "SA1_7DIG16",
                regionIDs = regionIDs, 
                exposure_population = exposure_population,
                summarise = FALSE, 
                final_year = fyear, 
                region_folder = region_folder
              ) |> mutate(scen = "reference"),
        cycling = get_summary("cycling", 
                    zoneID = "SA1_7DIG16",
                    regionIDs = regionIDs, 
                    exposure_population = exposure_population,
                    summarise = FALSE, 
                    final_year = fyear, 
                    region_folder = region_folder
                  ) |> mutate(scen = "cycling"),
    )
    arrow::write_dataset(all_data, paste0(region_folder, "data/all_data.parquet", partitioning = c("scen", tail(regionIDs,n=1))))
  } else if (grepl("brunswick", tolower(basename(region_folder)))) {
    exposure_population = "input/health/pp_exposure_2018_base_2025-10-29_Brunswick.csv"
    fyear <- 2023 # this is just a single suburb test case with short run time for now; proof of concept
    regionIDs <- c("SA2_MAIN16")
    all_data <- list(
        base = get_summary("base", 
                zoneID = "SA1_7DIG16",
                regionIDs = regionIDs, 
                exposure_population = exposure_population,
                summarise = FALSE, 
                final_year = fyear, 
                region_folder = region_folder
              ) |> mutate(scen = "reference")
        # cycling = get_summary("cycling", 
        #             zoneID = "SA1_7DIG16",
        #             regionIDs = c("SA2_MAIN16"), 
        #             exposure_population = exposure_population,
        #             summarise = FALSE, 
        #             final_year = fyear, 
        #             region_folder = region_folder
        #           ) |> mutate(scen = "cycling"),
  )
  arrow::write_dataset(all_data, paste0(region_folder, "data/all_data.parquet", partitioning = c("scen", tail(regionIDs,n=1))))
}
