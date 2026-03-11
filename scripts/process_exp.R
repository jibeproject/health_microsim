# === Load libraries ===
suppressPackageStartupMessages({
  library(tidyverse)
})

# pp_exposure_2021_base_140725.csv
# pp_exposure_2021_safeStreet_300725.csv
# pp_exposure_2021_green_310725.csv
# pp_exposure_2021_both_010825.csv

require(tidyverse)
inp_dir <- "/media/ali/Expansion/backup_tabea/Ali/manchester/scenOutput/base/microData"
zones <- read_csv("/media/ali/Expansion/backup_tabea/manchester-main/input/zoneSystem.csv")

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

get_exp_summary <- function(inp_dir, zones, scen = "base"){
  
  # get files like "pp_exposure_2021.csv", "pp_exposure_2031.csv", ...
  files <- list.files(
    path       = inp_dir,
    pattern    = "^pp_exposure_[0-9]+\\.csv$",
    full.names = TRUE
  )
  
  # helper to read one file and add year column
  read_with_year <- function(f) {
    # extract the integer after "pp_exposure_"
    yr <- sub("^pp_exposure_([0-9]+)\\.csv$", "\\1", basename(f))
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$year <- as.integer(yr)
    df
  }
  
  # read all and bind rows
  all_exposure <- do.call(rbind, lapply(files, read_with_year))
  
  all_exposure <- add_zones_and_scen(df = all_exposure, zones, scen = "reference")
  
  
  # Add agegroup column and calculate total_PA
  all_exposure <- all_exposure |> 
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf), 
                          labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
                          right = FALSE, 
                          include.lowest = TRUE),
           total_PA = mmetHr_walk + mmetHr_cycle + mmetHr_otherSport,
           imd = (imd10 + 1) %/% 2)
  
  # Function to calculate quantiles for columns starting with "exp" with optional grouping
  # Example usage with your dataframe `df`
  overall <- calc_quantiles_grouped(all_exposure, c("scen", "year"))
  by_gender <- calc_quantiles_grouped(all_exposure, c("scen", "gender", "year"))
  by_ladnm <- calc_quantiles_grouped(all_exposure, c("scen", "ladnm", "year"))
  by_imd <- calc_quantiles_grouped(all_exposure, c("scen", "imd", "year"))
  by_agegroup <- calc_quantiles_grouped(all_exposure, c("scen", "agegroup", "year"))
  # Add grouping column to differentiate
  overall <- overall |> mutate(grouping = "Overall")
  by_gender <- by_gender |> mutate(grouping = paste("Gender:", gender)) |> select(-gender)
  by_ladnm <- by_ladnm |> mutate(grouping = paste("LADNM:", ladnm)) |> select(-ladnm)
  by_imd <- by_imd |> mutate(grouping = paste("IMD:", imd)) |> select(-imd)
  by_agegroup <- by_agegroup |> mutate(grouping = paste("Agegroup:", agegroup)) |> select(-agegroup)
  # Bind all results into one dataframe
  all_quantiles <- bind_rows(overall, by_gender, by_ladnm, by_imd, by_agegroup) #|>
  
  return(all_quantiles)  
}

base_exp <- get_exp_summary(inp_dir, zones, scen = "reference")
