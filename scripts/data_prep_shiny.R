# === Load libraries ===
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(DT)
  library(purrr)
})

# === Global Settings ===
FILE_PATH_BELEN <- FALSE
FILE_PATH_JAVA <- TRUE
RUN_NAME <- "_5p_300625"

# Scaling factor for results

SCALING <- 20 # for a 5% sample and to be used to multiply results

#For saving files
data_path_Belen <- FALSE

# Set path based on condition
base_path <- if (data_path_Belen) {
  "manchester/health/processed/shiny_data/"
} else {
  "shiny_data/"
}

# === Load zone names ===
zones <- if (!FILE_PATH_BELEN) {
  read_csv("../../Tabea/manchester-main/input//zoneSystem.csv")
} else {
  read_csv("manchester/health/processed/zoneSystem.csv")
} 

#zones <- zones |> distinct(ladcd, ladnm)

# === European Standard Population (ESP2013) === Replace with baseline population 
esp2013 <- c(
  rep(4000, 1), rep(5500, 4),  # 0-4, 5-9, 10-14, 15-19, 20-24
  rep(5500, 2), rep(6000, 2),  # 25-29, 30-34, 35-39, 40-44
  rep(6000, 2), rep(5000, 2),  # 45-49, 50-54, 55-59, 60-64
  rep(4000, 2), rep(2500, 2),  # 65-69, 70-74, 75-79, 80-84
  rep(1500, 2)                 # 85-89, 90+
)


# === Data loading function ===
get_summary <- function(SCEN_NAME, group_vars = NULL, summarise = TRUE) {
  
  microdata_dir_name <- 'microData_5p_300625'
  #microdata_dir_name <- 'microData_5p_wo_interaction_010725'
  
  # SCEN_NAME <- 'base'
  # group_vars = NULL
  # summarise = TRUE
  
  file_path <- file_path <- if (exists("FILE_PATH_JAVA") && !FILE_PATH_BELEN) {
    paste0("~/Documents/Tabea/manchester-main/scenOutput/", SCEN_NAME, "/", microdata_dir_name, "/pp_healthDiseaseTracker_2051.csv")
  } else if (exists("FILE_PATH_JAVA") && FILE_PATH_BELEN && FILE_PATH_JAVA) {
    paste0("manchester/health/processed/health_model_outcomes/microData_", SCEN_NAME, RUN_NAME, "/microData/", "pp_healthDiseaseTracker_2051.csv")
  } else {
    paste0("manchester/health/processed/", SCEN_NAME, "_dis_inter_state_trans-n.c-noAPRISK-", n.c, "-n.i-", n.i, "-n.d-19.parquet")
  }
  
  ## Condition to open parquet and csv files
  if (grepl("\\.csv$", file_path)) {
    m <- readr::read_csv(file_path)
  } else {
    m <- arrow::open_dataset(file_path) |> collect()
  }
  m$id <- as.numeric(m$id)
  
  ## Condition if Java change years for cycles
  
  if (exists("FILE_PATH_JAVA") && FILE_PATH_JAVA) {
    year_cols <- grep("^20", names(m), value = TRUE)
    new_names <- paste0("c", seq_along(year_cols) - 1)
    names(m)[match(year_cols, names(m))] <- new_names
  }
  
  pop_dir_path <- if (!FILE_PATH_BELEN) {
    paste0("~/Documents/Tabea/manchester-main/scenOutput/", SCEN_NAME, "/", microdata_dir_name)
  } else {
    paste0("manchester/health/processed/health_model_outcomes/microData_", SCEN_NAME, RUN_NAME, "/microData/")
  }
  
  # List all CSV files starting with "pp_" in the specified directory
  pp_csv_files <- list.files(path = pop_dir_path, 
                             pattern = "^pp_\\d{4}\\.csv$",  # Matches pp_ followed by 4 digits and .csv
                             full.names = TRUE)
  
  # Read and filter each file, then combine into one data frame
  newborn_data <- lapply(pp_csv_files, function(file) {
    df <- read.csv(file)|> filter(age == 0)
  }) |> bind_rows()
  
  
  # List all CSV files starting with "pp_" in the specified directory (to get zones)
  dd_csv_files <- list.files(path = pop_dir_path, 
                             pattern = "^dd_\\d{4}\\.csv$",  # Matches pp_ followed by 4 digits and .csv
                             full.names = TRUE)
  
  # Read and filter each file, then combine into one data frame
  dd_data <- lapply(dd_csv_files, function(file) {
    df <- read.csv(file)
  }) |> bind_rows()
  
  newborn_data <- newborn_data |> 
    left_join(dd_data |> 
                dplyr::select(hhID, zone) |> 
                rename(hhid = hhID))
  
  
  synth_pop <- newborn_data |>
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf), 
                          labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
                          right = FALSE, 
                          include.lowest = TRUE)) |> 
    dplyr::select(id, age, agegroup, gender, zone) |> 
    left_join(zones  |> rename(zone = oaID) |> dplyr::select(zone, ladcd, lsoa21cd)) |> 
    distinct()
  
  
  pop_path <- if (!FILE_PATH_BELEN) {
    paste0("~/Documents/Tabea/manchester-main/scenOutput/", SCEN_NAME, "/", microdata_dir_name, "/pp_exposure_2021.csv")
  } else {
    paste0("manchester/health/processed/health_model_outcomes/microData_", SCEN_NAME, RUN_NAME, "/microData/pp_exposure_2021.csv")
  }
  if (!file.exists(pop_path)) stop("Population exposure file does not exist: ", pop_path)
  synth_pop_2021 <- readr::read_csv(pop_path) |>
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf), 
                          labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
                          right = FALSE, 
                          include.lowest = TRUE))
  
  synth_pop_2021 <- synth_pop_2021 |> dplyr::select(id, age, agegroup, gender, zone) |> left_join(zones  |> rename(zone = oaID) |> dplyr::select(zone, ladcd, lsoa21cd))
  
  synth_pop <- bind_rows(synth_pop, synth_pop_2021)
  
  m <- m |> left_join(synth_pop |> dplyr::select(id, age, agegroup, gender, ladcd, lsoa21cd)) |> 
    mutate(
      across(
        starts_with("c"),
        ~ ifelse(str_detect(., "killed"), "dead", .)
      )
    )
  
  long_data <- m |>
    pivot_longer(cols = starts_with("c")) |>
    arrange(id, parse_number(name)) |>
    mutate(
      cycle = as.numeric(str_remove(name, "^c")),
      unpacked = str_split(value, "\\|")
    ) |>
    unnest(unpacked) |>
    mutate(
      value = str_trim(unpacked),
      value = str_replace_all(value, fixed("parkinson’s_disease"), "parkinson"),
      value = na_if(value, "null")  # Convert "null" strings to proper NA
    ) |>
    select(-unpacked) |>
    arrange(id, cycle) |>
    
    # Remove rows before first non-NA value (i.e. pre-birth) 
    group_by(id) |>
    mutate(
      birth_cycle = suppressWarnings(min(cycle[!is.na(value)], na.rm = TRUE))
    ) |>
    filter(cycle >= birth_cycle) |>
    
    # Forward-fill 'dead' after first appearance
    mutate(
      #dead_seen = cumsum(coalesce(value == "dead", FALSE)) > 0,
      dead_seen = cumsum(coalesce(grepl("dead|kill", value), FALSE)) > 0,
      value = if_else(dead_seen, "dead", value)
    ) |>
    ungroup() |>
    
    # Create cycle age_cycle and agegroup_cycle
    
    mutate(
      age_cycle = age + cycle,
      agegroup_cycle = cut(age_cycle, c(0, 25, 45, 65, 85, Inf), 
                           labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
                           right = FALSE, include.lowest = TRUE)
    ) |>
    
    # Identify diseases
    mutate(
      is_disease = value %in% c(
        "diabetes", "stroke", "depression", "ischemic_heart_disease",
        "all_cause_dementia", "parkinson", "dead"
      )
    )
  
  
  if (!is.null(group_vars) && summarise && length(group_vars) > 0) {
    long_data <- long_data |>
      group_by(across(all_of(group_vars))) |>
      summarise(count = dplyr::n(), .groups = "drop") |>
      mutate(freq = round(count / sum(count) * 100, 1))
  }
  
  return(long_data)
}

# # === Load preprocessed data ===
all_data <- list(
  base = get_summary("base", summarise = FALSE) |> mutate(scen = "reference"),
  green = get_summary("green", summarise = FALSE) |> mutate(scen = "green"),
  safeStreet = get_summary("safeStreet", summarise = FALSE) |> mutate(scen = "safeStreet"),
  both = get_summary("both", summarise = FALSE) |> mutate(scen = "both")
)

## Save for checking

# Save each dataframe in the list as a separate parquet file
# walk2(all_data, names(all_data), ~ write_parquet(.x, paste0("manchester/health/processed/", .y, "_long_data.parquet")))

# # === Prepare time delay data ===

process_scenario <- function(scen_name) {
  # scen_name <- "base"
  df <- get_summary(scen_name) |>
    mutate(
      scen = scen_name,
      cycle_numeric = as.numeric(str_remove(cycle, "^c"))
    ) |>
    # filter(value != "healthy") |>
    dplyr::group_by(id, disease = value) |>
    dplyr::summarise(
      time_to_event = min(cycle_numeric),
      .groups = "drop"
    )
  return(df)
}


base_diag <- process_scenario("base") |> dplyr::rename(time_reference = time_to_event)
green_diag <- process_scenario("green") |> dplyr::rename(time_green = time_to_event)
safer_diag <- process_scenario("safeStreet") |> dplyr::rename(time_safeStreet = time_to_event)
both_diag <- process_scenario("both") |> dplyr::rename(time_both = time_to_event)


valid_data <- c("base_diag", "both_diag", "safer_diag") |> #, "both_diag") |>
  purrr::keep(exists) |>
  mget(envir = .GlobalEnv) |>
  purrr::keep(~ all(c("id", "disease") %in% names(.x))) |>
  purrr::iwalk(~ message(.y, ": ", paste(names(.x), collapse = ", ")))


result_wide <- base_diag |>
  full_join(green_diag, by = c("id", "disease")) |>
  full_join(safer_diag, by = c("id", "disease")) |>
  full_join(both_diag, by = c("id", "disease"))

age_sex_lad <- get_summary("base", summarise = FALSE) |>
  dplyr::select(id, agegroup, gender, ladcd) |>
  distinct(id, .keep_all = TRUE) |>
  left_join(zones |> distinct(ladcd, ladnm))

result_wide <- result_wide |>
  left_join(age_sex_lad, by = "id") |>
  mutate(
    time_reference = replace_na(time_reference, 0),
    diff_both = if_else(is.na(time_both), NA_real_, time_both - time_reference),#,
    diff_green = if_else(is.na(time_green), NA_real_, time_green - time_reference),
    diff_safeStreet = if_else(is.na(time_safeStreet), NA_real_, time_safeStreet - time_reference)
  )
## To fix, cannot be -1 when not happening because difference can be -1

# # === Alive Over Time Tab Data ===
alive_data <- bind_rows(all_data) |> # life years
  filter(value != "dead") |>
  dplyr::group_by(cycle, scen, ladcd, agegroup, gender) |>
  dplyr::summarise(alive_n = dplyr::n_distinct(id), .groups = "drop") |>
  left_join(zones |> distinct(ladcd, ladnm), by = "ladcd")

### Scale up here

alive_data_overall <- alive_data |>
  dplyr::group_by(cycle, scen) |>
  dplyr::summarise(alive=sum(alive_n)) |>
  dplyr::mutate(type = "overall",
                name = "overall")


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
  mutate(diff = alive_n - alive_n_ref) |>
  group_by(scen) |>
  summarise(sum(diff))

alive_data_sex <- alive_data |>
  dplyr::group_by(cycle, scen, gender) |>
  dplyr::summarise(alive=sum(alive_n)) |>
  dplyr::mutate(type = "sex") |>
  dplyr::rename(name=gender) |>
  dplyr::mutate(name = (case_when(name == 1 ~ "male",
                                  name == 2 ~ "female")))

alive_data_age <- alive_data |>
  dplyr::group_by(cycle, scen, agegroup) |>
  dplyr::summarise(alive=sum(alive_n)) |>
  dplyr::mutate(type = "age") |>
  dplyr::rename(name=agegroup)

alive_data_lad <- alive_data |>
  dplyr::group_by(cycle, scen, ladnm) |>
  dplyr::summarise(alive=sum(alive_n)) |>
  dplyr::mutate(type = "lad") |>
  dplyr::rename(name=ladnm)

alive_data_all <- bind_rows(alive_data_overall, alive_data_age, alive_data_lad, alive_data_sex)

## Change number files if you want to keep them for different test runs

saveRDS(alive_data_all, paste0(base_path, "/life_years_overtime_java_5p.RDS"))

# # === Alive Accumulated Tab Data ===

alive_acc_data <- bind_rows(all_data) |>
  filter(value != "dead") |>
  dplyr::group_by(cycle, scen, ladcd, agegroup, gender) |>
  dplyr::summarise(alive_n = n_distinct(id), .groups = "drop") |>
  dplyr::group_by(scen, ladcd, agegroup, gender) |>
  dplyr::summarise(acc_alive = sum(alive_n), .groups = "drop") |>
  left_join(zones |> distinct(ladcd, ladnm), by = "ladcd")


alive_acc_overall <- alive_acc_data |>
  dplyr::group_by(scen) |>
  dplyr::summarise(alive = sum(acc_alive), .groups = "drop") |>
  dplyr::mutate(type = "overall", name = "overall")


alive_acc_sex <- alive_acc_data |>
  dplyr::group_by(scen, gender) |>
  dplyr::summarise(alive = sum(acc_alive), .groups = "drop") |>
  dplyr::mutate(type = "sex") |>
  dplyr::rename(name = gender) |>
  dplyr::mutate(name = case_when(name == 1 ~ "male",
                                 name == 2 ~ "female"))


alive_acc_age <- alive_acc_data |>
  dplyr::group_by(scen, agegroup) |>
  dplyr::summarise(alive = sum(acc_alive), .groups = "drop") |>
  dplyr::mutate(type = "age") |>
  dplyr::rename(name = agegroup)


alive_acc_lad <- alive_acc_data |>
  dplyr::group_by(scen, ladnm) |>
  dplyr::summarise(alive = sum(acc_alive), .groups = "drop") |>
  dplyr::mutate(type = "lad") |>
  dplyr::rename(name = ladnm)


alive_acc_all <- bind_rows(alive_acc_overall, alive_acc_age, alive_acc_lad, alive_acc_sex)


saveRDS(alive_acc_all, paste0(base_path, "/accumulated_life_years_java_5p.RDS"))


# # === Avoided Disease/Death Tab Data ===
avoided_data <- result_wide |>
  dplyr::filter(time_reference > 0) |>
  pivot_longer(cols = starts_with("diff_"), names_to = "scenario", values_to = "time_event") |>
  dplyr::filter(scenario != "time_reference") |>
  dplyr::mutate(
    avoided = is.na(time_event)
  ) |>
  dplyr::group_by(scenario, disease, ladnm, agegroup, gender) |>
  dplyr::summarise(count = sum(avoided, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "avoided")

# Overall avoided
avoided_overall <- avoided_data |>
  dplyr::group_by(scenario, disease) |>
  dplyr::summarise(avoided_n = sum(count), .groups = "drop") |>
  dplyr::mutate(type = "overall", name = "overall")

# Avoided by sex
avoided_sex <- avoided_data |>
  dplyr::group_by(scenario, disease, gender) |>
  dplyr::summarise(avoided_n = sum(count), .groups = "drop") |>
  dplyr::mutate(type = "sex") |>
  dplyr::rename(name = gender) |>
  dplyr::mutate(name = case_when(name == 1 ~ "male",
                                 name == 2 ~ "female"))
# Avoided by age group
avoided_age <- avoided_data |>
  dplyr::group_by(scenario, disease, agegroup) |>
  dplyr::summarise(avoided_n = sum(count), .groups = "drop") |>
  dplyr::mutate(type = "age") |>
  dplyr::rename(name = agegroup)

# # Avoided by LAD
avoided_lad <- avoided_data |>
  dplyr::group_by(scenario, disease, ladnm) |>
  dplyr::summarise(avoided_n = sum(count), .groups = "drop") |>
  dplyr::mutate(type = "lad") |>
  dplyr::rename(name = ladnm)

# Combine all
avoided_all <- bind_rows(avoided_overall, avoided_sex, avoided_age, avoided_lad)

# Save
saveRDS(avoided_all, paste0(base_path, "/avoided_events_java_5p.RDS"))

# # === Disease Delay Tab Data ===
delay_data <- result_wide |>
  filter(time_reference > 0) |>
  pivot_longer(cols = c(diff_both, diff_safeStreet, diff_green), #diff_safeStreet, diff_both), # change to do in code checking if file exists
               names_to = "scenario", values_to = "delay_cycles") |>
  filter(delay_cycles >= 0) |>
  dplyr::group_by(scenario, disease, ladnm, agegroup, gender) |>
  dplyr::summarise(mean_delay = mean(delay_cycles * 365, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "delay")

# Overall delay
delay_overall <- delay_data |>
  dplyr::group_by(scenario, disease) |>
  dplyr::summarise(delay_days = mean(mean_delay, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "overall", name = "overall")

# Delay by sex
delay_sex <- delay_data |>
  dplyr::group_by(scenario, disease, gender) |>
  dplyr::summarise(delay_days = mean(mean_delay, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "sex") |>
  dplyr::rename(name = gender) |>
  dplyr::mutate(name = case_when(name == 1 ~ "male",
                                 name == 2 ~ "female"))

# # Delay by age group
delay_age <- delay_data |>
  dplyr::group_by(scenario, disease, agegroup) |>
  dplyr::summarise(delay_days = mean(mean_delay, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "age") |>
  dplyr::rename(name = agegroup)

# Delay by LAD
delay_lad <- delay_data |>
  dplyr::group_by(scenario, disease, ladnm) |>
  dplyr::summarise(delay_days = mean(mean_delay, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(type = "lad") |>
  dplyr::rename(name = ladnm)

# Combine all
delay_all <- bind_rows(delay_overall, delay_sex, delay_age, delay_lad)

# Save
saveRDS(delay_all, paste0(base_path, "/delay_days_java_5p.RDS"))

# # === Age standardized rates ====

run_age_standardised_rate_by_agegroup <- function(summary_raw, zones) {

  # summary_raw <- all_data$base
  std_pop <- tibble(
    agegroup = c("0-24", "25-44", "45-64", "65-84", "85+"), # European standard population
    std_pop = c(
      sum(esp2013[1:5]),
      sum(esp2013[6:9]),
      sum(esp2013[10:13]),
      sum(esp2013[14:17]),
      sum(esp2013[18:19])
    )
  ) |> mutate(std_prop = std_pop / sum(std_pop))

  target_cycles <- c(0, 10, 20)
  states <- c("healthy", "diabetes", "stroke", "coronary_heart_disease",
              "breast_cancer", "colon_cancer", "lung_cancer", "depression", "all_cause_dementia", "dead")

#Prevalence: Person has the disease in the current or any previous cycle.

# #Incidence: Person is diagnosed with the disease in this cycle only, but not in any previous cycle.
#
# Old code
#   df <- summary_raw |> ###  Should separate incidence and prevalence. Incidence new case in year and prevalence case in current and previous year.
#     dplyr::filter(value %in% states, cycle %in% target_cycles) |> # ASR for a given year
#     dplyr::mutate(agegroup = cut(age_cycle, c(0, 25, 45, 65, 85, Inf),
#                                  labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
#                                  right = FALSE, include.lowest = TRUE)) |>
#     dplyr::group_by(scen, ladcd, gender, agegroup, cycle) |>
#     dplyr::summarise(count = n_distinct(value), .groups = "drop",
#                      population = n_distinct(id)) |> # should be rate per 100,000
#     left_join(std_pop, by = "agegroup") |>
#     ungroup()

  # Filter to diseases of interest
  disease_long <- summary_raw |>
    # filter(value %in% states) |> # comment out for filtering to some diseases
    select(id, cycle, value, age_cycle, scen, ladcd, gender)

  # Disease history per person
  inc_prev <- disease_long |>
    arrange(id, value, cycle) |>
    group_by(id, value) |>
    mutate(
      first_diagnosis = min(cycle, na.rm = TRUE),  # first time disease appeared
      ever_had = cycle >= first_diagnosis,         # prevalence: ever had up to this cycle
      incident = cycle == first_diagnosis          # incidence: first time seen
    ) |>
    ungroup()

  # Add age group again
  inc_prev <- inc_prev |>
    mutate(agegroup = cut(
      age_cycle,
      breaks = c(0, 25, 45, 65, 85, Inf),
      labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
      right = FALSE,
      include.lowest = TRUE
    ))

  # Summarise counts
  summary_incprev <- inc_prev |>
    group_by(scen, ladcd, gender, agegroup, cycle, value) |>
    summarise(
      prevalence = n_distinct(id[ever_had]),
      incidence = n_distinct(id[incident]),
      .groups = "drop"
    )

  # Step 5 (optional): Add total population and rate calculations
  population_df <- summary_raw |>
    filter(cycle %in% target_cycles) |>
    mutate(agegroup = cut(
      age_cycle,
      c(0, 25, 45, 65, 85, Inf),
      labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
      right = FALSE,
      include.lowest = TRUE
    )) |>
    group_by(scen, ladcd, gender, agegroup, cycle) |>
    summarise(population = n_distinct(id), .groups = "drop")
  
  backup_sum <- summary_incprev

  ## Ali up to here for age standardised rates
  summary_incprev <- summary_incprev |>
    left_join(population_df, by = c("scen", "ladcd", "gender", "agegroup", "cycle")) |>
    mutate(
      incidence_rate = (incidence / population),
      prevalence_rate = (prevalence / population)
    ) |> filter(cycle %in%target_cycles) |>
    left_join(std_pop) |> 
    dplyr::mutate(std_rate_inc = incidence_rate*std_prop*100000, 
                  std_rate_prev = prevalence_rate*std_prop*100000)
  
  # browser()
    #
    #
  ### Data with groupings of results for age_std rates (below should be difference for incidence and prevalence)

  # overall
  df_overall <- summary_incprev |>
    dplyr::group_by(scen, value, cycle) |>
    dplyr::mutate(std_rate_inc = sum(std_rate_inc, na.rm = T),
                  std_rate_prev = sum(std_rate_prev, na.rm = T),
                  type="overall",
                  name = "overall") |> 
    dplyr::select(scen, value, std_rate_inc, std_rate_prev, type, name, cycle)
  # by lad

  df_lad <- summary_incprev  |>
    dplyr::group_by(scen, value, ladcd, cycle) |>
    dplyr::summarise(std_rate_inc = sum(std_rate_inc, na.rm = T),
                     std_rate_prev = sum(std_rate_prev, na.rm = T)) |>
    dplyr::mutate(type="lad") |>
    left_join(zones |> distinct(ladcd, ladnm)) |>
    dplyr::rename(name=ladnm) |> 
    dplyr::select(scen, value, std_rate_inc, std_rate_prev, ladcd, type, name, cycle)

  # by age

  df_age <- summary_incprev |>
    dplyr::group_by(scen, value, agegroup, cycle) |>
    dplyr::summarise(std_rate_inc = sum(std_rate_inc, na.rm = T),
                     std_rate_prev = sum(std_rate_prev, na.rm = T)) |>
    dplyr::mutate(type="age") |> 
    dplyr::rename(name=agegroup)  |> 
    dplyr::select(scen, value, std_rate_inc, std_rate_prev, type, name, cycle)

  # by sex

  df_sex <- summary_incprev  |>
    dplyr::group_by(scen, value, gender, cycle) |>
    dplyr::summarise(std_rate_inc = sum(std_rate_inc, na.rm = T),
                     std_rate_prev = sum(std_rate_prev, na.rm = T)) |>
    dplyr::mutate(type="sex") |> 
    dplyr::select(scen, value, std_rate_inc, std_rate_prev, type, cycle, gender) |> 
    dplyr::mutate(gender = (case_when(gender == 1 ~ "male",
                                      gender == 2 ~ "female"))) |>
    dplyr::rename(name=gender)

  df <- bind_rows(df_overall, df_lad, df_age, df_sex)

  return(df)
}

# === Load all data and compute standardised rates ===
all_data <- list(
  base = get_summary("base", summarise = FALSE) |> mutate(scen = "reference"),
  green = get_summary("green", summarise = FALSE) |> mutate(scen = "green"),
  safeStreet = get_summary("safeStreet", summarise = FALSE) |> mutate(scen = "safeStreet"),
  both = get_summary("both", summarise = FALSE) |> mutate(scen = "both")
)

std_rates_table <- map2_dfr(
  all_data,
  names(all_data),
  ~run_age_standardised_rate_by_agegroup(.x, zones)
)

saveRDS(std_rates_table, paste0(base_path, "/std_rates_tables_java_5p.RDS"))


calculate_deaths <- function(dataset, scen_name){
  
  base_pop <- dataset |> filter(value != "dead", cycle == 1) |> group_by(cycle) |> summarise(n_distinct(id)) |>  pull()
  deaths <- dataset |> filter(value == "dead") |> group_by(cycle) |> summarise(deaths = n_distinct(id)) |> mutate(new_deaths = deaths - lag(deaths, 1))
  deaths$new_deaths <- ifelse(is.na(deaths$new_deaths), deaths$deaths, deaths$new_deaths)
  return(deaths |> mutate(base_pop = base_pop,
                          unit_death = new_deaths/base_pop,
                          total_deaths = round(unit_death * 2827285), 
                          name = scen_name))
  
  
}

deaths <- rbind(calculate_deaths(all_data$base, "reference"), calculate_deaths(all_data$green, "green"),
                calculate_deaths(all_data$safeStreet, "safeStreet"), 
                calculate_deaths(all_data$both, "both"))
