# === Load libraries ===
suppressPackageStartupMessages({
  library(tidyverse)
})

base_data_path <- "Z:/HealthImpact/Data/Country/UK/JIBE/manchester/"

inp_dir <- "Z:/HealthImpact/Data/Country/UK/JIBE/manchester/scenOutput/normalization_fix_052226/microData"

zones <- read_csv("Z:/HealthImpact/Data/Country/UK/JIBE/manchester/input/zoneSystem.csv")

add_zones_and_scen <- function(df, zones, scen = "reference"){
  return(
    df |> left_join(zones |> 
                      dplyr::select(oaID, imd10, ladnm) |> 
                      rename(zone = oaID)) |> 
      mutate(scen = scen)
  )
}

calc_quantiles_grouped <- function(data, group_var = NULL) {
  # data <- all_exposure
  # group_var <- c("scen", "gender")
  quantile_probs <- c(5, 25, 50, 75, 95)
  quantile_names <- c("5%", "25%", "50%", "75%", "95%")
  
  if (!is.null(group_var)) {
    group_vars_sym <- if (length(group_var) > 1) {
      rlang::syms(group_var)
    } else {
      rlang::sym(group_var)
    }
    grouped_df <- data |> group_by(!!!group_vars_sym)
  } else {
    grouped_df <- data
  }
  
  
  summarised <- grouped_df |>
    summarise(
      across(
        matches("^(exposure|total_PA)"),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          `5%` = ~ quantile(.x, 0.05, na.rm = TRUE),
          `25%` = ~ quantile(.x, 0.25, na.rm = TRUE),
          `50%` = ~ quantile(.x, 0.5, na.rm = TRUE),
          `75%` = ~ quantile(.x, 0.75, na.rm = TRUE),
          `95%` = ~ quantile(.x, 0.95, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = -all_of(group_var),
      names_to = c("variable", "stat"),
      names_pattern = "^(.*)_(.*)$"
    ) |>
    mutate(stat = factor(stat, levels = c("mean", quantile_names))) |>
    arrange(across(all_of(group_var)), variable, stat)
  
  return(summarised)
}

get_exp_summary <- function(inp_dir, zones, scen = "base") {
  #scen <- 'reference'
  ref_dir <- file.path(inp_dir, scen, "microData")
  
  # Years we want to read
  years <- c(2021, 2031, 2041, 2051)
  
  # Read only the target year files that exist
  file_map <- file.path(ref_dir, paste0("pp_exposure_", years, ".csv"))
  print(file_map)
  files <- file_map[file.exists(file_map)]
  
  if (length(files) == 0) {
    stop("No pp_exposure_YYYY.csv files found in: ", ref_dir)
  }
  
  read_with_year <- function(f) {
    yr <- as.integer(sub("^pp_exposure_([0-9]+)\\.csv$", "\\1", basename(f)))
    df <- readr::read_csv(f, show_col_types = FALSE)
    df$year <- yr
    df
  }
  
  all_exposure <- dplyr::bind_rows(lapply(files, read_with_year))
  
  # Replace default 2021 with scenario-specific pp_exp file
  scen_file <- dplyr::case_when(
    scen == "reference" ~ "pp_exposure_2021_base_220526.csv",
    scen == "green"     ~ "pp_exposure_2021_green_260526.csv",
    scen == "safeStreet"~ "pp_exposure_2021_safeStreet_260526.csv",
    scen == "goDutch"   ~ "pp_exposure_2021_goDutch_260526.csv",
    TRUE ~ NA_character_
  )
  
  if (is.na(scen_file)) {
    stop("Unknown scen value: ", scen)
  }
  inp_exp_dir <- "Z:/HealthImpact/Data/Country/UK/JIBE/manchester"
  
  scen_path <- file.path(inp_exp_dir, "input", "health", scen_file)
  if (!file.exists(scen_path)) {
    stop("Scenario 2021 file not found: ", scen_path)
  }
  
  pp_2021 <- readr::read_csv(scen_path, show_col_types = FALSE)
  pp_2021$year <- 2021
  
  # Remove any existing 2021 row data and replace with scenario-specific 2021
  all_exposure <- all_exposure |>
    dplyr::filter(year != 2021) |>
    dplyr::bind_rows(pp_2021) |>
    dplyr::arrange(year)
  
  all_exposure <- add_zones_and_scen(df = all_exposure, zones, scen = scen)
  
  all_exposure <- all_exposure |>
    dplyr::mutate(
      agegroup = cut(
        age,
        breaks = c(0, 25, 45, 65, 85, Inf),
        labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
        right = FALSE,
        include.lowest = TRUE
      ),
      total_PA = mmetHr_walk + mmetHr_cycle + mmetHr_otherSport,
      imd = (imd10 + 1) %/% 2
    )
  
  overall <- calc_quantiles_grouped(all_exposure, c("scen", "year"))
  by_gender <- calc_quantiles_grouped(all_exposure, c("scen", "gender", "year"))
  by_ladnm <- calc_quantiles_grouped(all_exposure, c("scen", "ladnm", "year"))
  by_imd <- calc_quantiles_grouped(all_exposure, c("scen", "imd", "year"))
  by_agegroup <- calc_quantiles_grouped(all_exposure, c("scen", "agegroup", "year"))
  
  overall <- overall |> dplyr::mutate(grouping = "Overall")
  by_gender <- by_gender |> dplyr::mutate(grouping = paste("Gender:", gender)) |> dplyr::select(-gender)
  by_ladnm <- by_ladnm |> dplyr::mutate(grouping = paste("LADNM:", ladnm)) |> dplyr::select(-ladnm)
  by_imd <- by_imd |> dplyr::mutate(grouping = paste("IMD:", imd)) |> dplyr::select(-imd)
  by_agegroup <- by_agegroup |> dplyr::mutate(grouping = paste("Agegroup:", agegroup)) |> dplyr::select(-agegroup)
  
  all_quantiles <- dplyr::bind_rows(overall, by_gender, by_ladnm, by_imd, by_agegroup)
  return(all_quantiles)
}

base_exp <- get_exp_summary(inp_dir, zones, scen = "reference")
green_exp <-  get_exp_summary(inp_dir, zones, scen = "green")
ss_exp <- get_exp_summary(inp_dir, zones, scen = "safeStreet")

exp <- bind_rows(base_exp, green_exp, ss_exp)
