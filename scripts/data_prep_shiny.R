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
})

# === Global Settings ===
FILE_PATH_BELEN <- TRUE # for location input files
DATA_PATH_BELEN <- TRUE # for location output files for shiny# Scaling factor for results

SCALING <- 20 # for a 5% sample and to be used to multiply results

# Set path based on condition
base_path <- if (DATA_PATH_BELEN) {
  "manchester/health/processed/shiny_data/"
} else {
  "shiny_data/"
}

# === Load zone names ===
zones <- if (!FILE_PATH_BELEN) {
  read_csv("/home/ali/IdeaProjects/manchester/input/zoneSystem.csv")
} else {
  read_csv("manchester/health/processed/zoneSystem.csv")
} 

# === Data loading function ===

get_summary <- function(SCEN_NAME, group_vars = NULL, summarise = TRUE) {
  microdata_dir_name <- "microData"
  
  # SCEN_NAME <- "base"
  
  # Select correct file path
  if (exists("FILE_PATH_BELEN") && isTRUE(FILE_PATH_BELEN)) {
    file_path <- paste0(
      "manchester/health/processed/health_model_outcomes/",
      "microData", SCEN_NAME, "/pp_healthDiseaseTracker_2051.csv"
    )
  } else {
    file_path <- paste0(
      "/media/ali/Expansion/backup_tabea/manchester-main/scenOutput/",
      SCEN_NAME, "/", microdata_dir_name, "/pp_healthDiseaseTracker_2051.csv"
    )
  }
  
  # Read health tracker file
  if (grepl("\\.csv$", file_path)) {
    m <- arrow::open_csv_dataset(file_path) |> to_duckdb() |> collect()
  } else {
    m <- arrow::open_dataset(file_path)
  }
  
  m$id <- as.numeric(m$id)
  
  # Rename year columns to c0, c1, ...
  year_cols <- grep("^20", names(m), value = TRUE)
  new_names <- paste0("c", seq_along(year_cols) - 1)
  names(m)[match(year_cols, names(m))] <- new_names
  
  # Load population files
  pop_dir_path <- if (!FILE_PATH_BELEN) {
    paste0("/media/ali/Expansion/backup_tabea/manchester-main/scenOutput/", SCEN_NAME, "/", microdata_dir_name)
  } else {
    paste0("manchester/health/processed/health_model_outcomes/microData", SCEN_NAME)
  }
  
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
  pop_path <- if (!FILE_PATH_BELEN) {
    paste0("/media/ali/Expansion/backup_tabea/manchester-main/scenOutput/", SCEN_NAME, "/", microdata_dir_name, "/pp_exposure_2021.csv")
  } else {
    paste0("manchester/health/processed/health_model_outcomes/microData", SCEN_NAME, "/pp_exposure_2021.csv")
  }
  
  if (!file.exists(pop_path)) stop("Population exposure file does not exist: ", pop_path)
  
  synth_pop_2021 <- readr::read_csv(pop_path) |>
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf), 
                          labels = c("0-24", "25-44", "45-64", "65-84", "85+"),
                          right = FALSE, 
                          include.lowest = TRUE))
  
  synth_pop_2021 <- synth_pop_2021 |> dplyr::select(id, age, agegroup, gender, zone) |> left_join(zones  |> rename(zone = oaID) |> dplyr::select(zone, ladcd, lsoa21cd))
  
  synth_pop <- bind_rows(synth_pop, synth_pop_2021)
  
  # Filter out early dead and merge population info
  m <- m |> 
    filter(c1 != "dead") |> 
    left_join(synth_pop |> select(id, age, agegroup, gender, ladcd, lsoa21cd)) |> 
    mutate(across(starts_with("c"), ~ ifelse(str_detect(., "killed"), "dead", .))) # Ali to update
  
  # Long format and event unpacking
  long_data <- m |> 
    pivot_longer(cols = starts_with("c")) |>
    arrange(id, parse_number(name)) |>
    mutate(
      cycle = as.numeric(str_remove(name, "^c")),
      unpacked = str_split(value, "\\|")
    ) |>
    unnest_longer(unpacked) |>
    mutate(
      value = str_trim(unpacked),
      value = str_replace_all(value, fixed("parkinson’s_disease"), "parkinson")
    ) |>
    dplyr::select(-unpacked) |>
    filter(value != "null") |>
    arrange(id, cycle) |>
    group_by(id) |>
    # Forward-fill 'age_cycle' after first appearance of non-null value
    mutate(
      dead_count = cumsum(value == "dead"),
      age_cycle = ifelse(!value %in% c("null", "dead"), age + cycle - min(cycle[!value %in% c("null", "dead")]), NA)
    ) |>
    ungroup() |> 
    filter(!(value == "dead" & dead_count > 1)) |> 
    dplyr::select(-dead_count) |> 
    mutate(agegroup_cycle = cut(
      age_cycle,
      breaks = c(seq(0, 85, by = 5), Inf),  # 0,5,10,...,85,Inf
      labels = c(
        paste(seq(0, 80, by = 5), seq(4, 84, by = 5), sep = "-"),
        "85+"
      ),
      right = FALSE,
      include.lowest = TRUE
    )) |> 
    distinct()
  
  if (!is.null(group_vars) && summarise && length(group_vars) > 0) {
    long_data <- long_data |>
      group_by(across(all_of(group_vars))) |>
      summarise(count = dplyr::n(), .groups = "drop") |>
      mutate(freq = round(count / sum(count) * 100, 1))
  }
  
  return(long_data)
}


## === Prepare general data long ===
all_data <- list(
  base = get_summary("Base", summarise = FALSE) |> mutate(scen = "reference"),
  # green = get_summary("green", summarise = FALSE) |> mutate(scen = "green"),
  safeStreet = get_summary("SafeStreet", summarise = FALSE) |> mutate(scen = "safeStreet"),
  both = get_summary("Both", summarise = FALSE) |> mutate(scen = "both")
)


### When working with 100% we need to add collect() at the end of each script below and also check how the data
### format is for above

# 5% sample with future exposures
all_data <- bind_rows(all_data) 

# 100% pop without future exposures
# all_data <- arrow::open_dataset("Y:/HealthImpact/Data/Country/UK/JIBE_health_output_data//all_data.parquet/") |> to_duckdb() #|> collect()

# ------ check ids initial pop (when using sample) ------

# initial_ref_IDs <- all_data |> filter(cycle == 0, scen == "reference") |> distinct(id) |> dplyr::select(id)
# 
# initial_both_IDs <- all_data |> filter(cycle == 0, scen == "both") |> distinct(id) |> dplyr::select(id)
# 
# anti_join(initial_both_IDs, initial_ref_IDs)|> View()
# 
# diff_ids <- all_data |> filter (id %in% (anti_join(initial_both_IDs, initial_ref_IDs) |> dplyr::select(id) |> pull()), scen != "safeStreet") |> distinct(id) |> nrow()

# ----- General data inputs -------

## People alive per cycle

people <- all_data |> 
  group_by(agegroup_cycle, gender, cycle, scen) |> 
  summarise(pop = n_distinct(id[!value %in% c("dead", "null")])) |> 
  pivot_longer(cols = pop) |> 
  rename(pop = value ) |> collect()

## Baseline pop reference weights. To use in ASR

ref_weights <- people |>
  filter(scen == "reference", cycle == 0) |>
  group_by(agegroup_cycle, gender) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop") |>
  ungroup() |>
  group_by(gender) |> 
  mutate(total_pop =  sum(pop),
        weight = pop / total_pop) |>
  select(agegroup_cycle, gender, weight, pop, total_pop)

ggplot(ref_weights) +
  aes(x = agegroup_cycle, y = weight) +
  geom_col(fill = "#112446") +
  labs(title = "Population weights ref population") +
  facet_wrap(~gender) +
  theme_minimal()


## European population weights
esp_5yr <- tibble::tibble(
  agegroup_cycle = factor(
    c(paste(seq(0, 80, by = 5), seq(4, 84, by = 5), sep = "-"), "85+"),
    levels = c(paste(seq(0, 80, by = 5), seq(4, 84, by = 5), sep = "-"), "85+"),
    ordered = FALSE
  ),
  weight = c(
    5000, 5500, 5500, 6000, 6000, 6500,
    7000, 7000, 6500, 6000, 5000, 4000,
    3000, 2000, 1000, 500, 250, 225
  )
)


esp_5yr <- esp_5yr %>%
  mutate(weight = weight / sum(weight))

esp_5yr_gendered <- esp_5yr %>%
  crossing(gender = c(1, 2))  # 1 = male, 2 = female

ggplot(esp_5yr_gendered) +
  aes(x = agegroup_cycle, y = weight) +
  geom_col(fill = "#112446") +
  labs(title = "Population weights ref population") +
  facet_wrap(~gender) +
  theme_minimal()

# ----- Dead incidence data ----

inc_death <- all_data |> 
  group_by(id, scen) |> 
  mutate(age_cycle = max(age_cycle[value != "dead"], na.rm = TRUE) + 1) |> 
  filter(value == "dead") |>
  mutate(agegroup_cycle = cut(
    age_cycle,
    breaks = c(seq(0, 85, by = 5), Inf),  # 0,5,10,...,85,Inf
    labels = c(
      paste(seq(0, 80, by = 5), seq(4, 84, by = 5), sep = "-"),
      "85+"
    ),
    right = FALSE,
    include.lowest = TRUE
  )) |> ungroup()


## 1) Average age at dead
mean_age_dead <- inc_death %>%
  count(age_cycle, name = "weight") %>%
  right_join(inc_death, by = "age_cycle") %>%
  group_by(scen) %>%
  summarize(
    mean_age_cycle = weighted.mean(age_cycle, weight, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ungroup()


## 2) Age standardized mortality rate per 100,000 people

# Calculate crude incidence rates per 100,000

crude_rates_dead <- inc_death %>%
  group_by(agegroup_cycle, gender, cycle, scen) |>
  filter(cycle > 1) |>
  summarise(n = n(), .groups = "drop") %>%
  ungroup() |>
  left_join(people, by = c("agegroup_cycle", "gender", "scen", "cycle")) |>
  mutate(crude_rate = if_else(pop > 0, n / pop * 100000, NA_real_)) %>%
  filter(!is.na(crude_rate))

ggplotly(ggplot(crude_rates_dead) +
           aes(x = agegroup_cycle, y = crude_rate, fill = gender) +
           geom_col() +
           scale_fill_gradient() +
           theme_minimal() +
           facet_wrap(vars(cycle))
)

# Functions to use either European or baseline population derived weights. 

calculate_std_rates <- function(crude_rates, ref_weights, use_esp = FALSE) {
  
  if (use_esp) {
    weights <- esp_5yr_gendered  # Fixed European standard weights
  } else {
    weights <- ref_weights  # baseline weights
  }
  
  # weights <- ref_weights
  
  std_rates_dead <- crude_rates |>
    select(-pop) |>
    left_join(weights |> select(-pop), by = c("agegroup_cycle", "gender")) |>
    filter(!is.na(weight)) |>
    mutate(rate_w=crude_rate*weight)  |>
    group_by(cycle, scen, gender) |>
    summarize(
      age_std_rate = sum(rate_w),
      .groups = "drop"
    ) |> ungroup() |>
    select(cycle, scen, age_std_rate, gender)
  
  return(std_rates_dead)
}


### Note belen@ not all age groups have ASR due to no dead observed (e.g. 5-9, 10-14, 25-29)
# Use baseline population weights
std_rates_ref <- calculate_std_rates(crude_rates_dead, ref_weights) 

# Use European standardised population
std_rates_esp <- calculate_std_rates(crude_rates_dead, ref_weights, use_esp = TRUE)

## Plot (change rates used with base population or european population)

ggplotly(ggplot(std_rates_ref) +
  aes(x = cycle, y = age_std_rate, colour = scen) +
  geom_smooth(se=FALSE) +
  scale_color_hue(direction = 1) +
    facet_wrap(~gender) +
  theme_minimal())

## 3) Difference number of deaths

dead_diff <- inc_death %>%
  group_by(agegroup_cycle, gender, cycle, scen) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(
    inc_death %>%
      filter(scen == "reference") %>%
      group_by(agegroup_cycle, gender, cycle) %>%
      summarise(n_reference = n(), .groups = "drop"),
    by = c("agegroup_cycle", "gender", "cycle")
  ) %>%
  mutate(
    n_reference = coalesce(n_reference, 0),   # replace NA with 0
    difference = n - n_reference
  )

## Metric overall (dead postpone)
dead_diff_acc <- dead_diff |>
                  group_by(scen) |>
                  summarise(sum(difference))

# ----- Healthy years ----

healthy_total_cycle <- all_data |>
  filter(value == "healthy") |>
  group_by(scen, cycle) |>
  summarise(healthy_years = n_distinct(id), .groups = "drop")


healthy_total_cycle_diff <- healthy_total_cycle %>%
  left_join(
    healthy_total_cycle %>%
      filter(scen == "reference") %>%
      rename(reference_healthy_years = healthy_years) %>%
      select(cycle, reference_healthy_years),
    by = "cycle"
  ) %>%
  mutate(
    healthy_years_difference = healthy_years - reference_healthy_years,
    percent_difference = 100 * healthy_years_difference / reference_healthy_years
  )

sum_healthy_years <- healthy_total_cycle_diff |> 
                  group_by(scen) |>
                  summarise(sum(healthy_years_difference))

# ----- Life years to do ------

life_years_cycle <- all_data |>
  filter(value != "dead") |>
  group_by(scen, cycle) |>
  summarise(life_years = n_distinct(id), .groups = "drop")


life_years_cycle_diff <- life_years_cycle %>%
  left_join(
    life_years_cycle %>%
      filter(scen == "reference") %>%
      rename(reference_life_years = life_years) %>%
      select(cycle, reference_life_years),
    by = "cycle"
  ) %>%
  mutate(
    life_years_difference = life_years - reference_life_years,
    percent_difference = 100 * life_years_difference / reference_life_years
  )

sum_life_years <- life_years_cycle_diff |> 
  group_by(scen) |>
  summarise(sum(life_years_difference))

#### test alternative with people
### Same result as calculation above
people_sum <- people |>
              group_by(scen) |>
              summarise(sum(pop))


# ----- Incidence diseases -----

## Count 
count_inc <- all_data |> 
  filter(value %in% c("healthy", "dead", "null")) |> 
  group_by(id, scen, value) |>  
  filter(cycle == min(cycle)) |> 
  ungroup() |> 
  filter(cycle > 0) |> 
  group_by(cycle, scen, value) |> 
  summarise(n = dplyr::n()) |> 
  collect()


## Count difference by scenario

# Summarise counts by scenario and value
count_inc_summary <- count_inc |>
  filter(cycle < 10) |> ## remove testing
  group_by(scen, value) |>
  summarise(total = sum(n), .groups = "drop")

# Extract reference totals
reference_totals <- count_inc_summary |>
  filter(scen == "reference") |>
  rename(ref_total = total) |>
  select(value, ref_total)

# Join and calculate difference
count_inc_diff <- count_inc_summary |>
  left_join(reference_totals, by = "value") |>
  mutate(
    total_diff = total - ref_total,
    percent_diff = 100 * total_diff / ref_total
  )


## Average age of onset

## tO DO 
## Age standardised rate (work in progress)

### ALI's incidence code
inc_age_gender <- all_data |>
  filter(!value %in% c("null", "dead", "healthy")) |> 
  group_by(id, value) |>
  filter(cycle == min(cycle[cycle > 0])) |>
  ungroup() |>
  group_by(agegroup_cycle, gender, cycle, scen, value) |>
  summarise(n = dplyr::n()) |> 
  collect()

inc_age_gender_uc <- inc_age_gender |> 
  left_join(people |> dplyr::select(-name)) |> 
  mutate(crude_rate = n/pop * 10^5)

inc_age_w <- inc_age_gender_uc |> 
  left_join(ref_weights, by = c("agegroup_cycle", "gender")) |> 
  mutate(weighted_rate = crude_rate * weight) |> 
  group_by(cycle, scen, value) |>
  reframe(age_std_rate = sum(weighted_rate, na.rm = TRUE)) |> 
  ungroup()

ggplotly((inc_age_w %>%
            filter(cycle >= 2L & cycle <= 30L) %>%
            filter(!(value %in% c("bladder_cancer", "endometrial_cancer", 
                                  "esophageal_cancer", "gastric_cardia_cancer", "liver_cancer", "myeloid_leukemia", "myeloma", "severely_injured_bike", 
                                  "severely_injured_car", "severely_injured_walk"))) %>%
            ggplot() +
            aes(x = cycle, y = age_std_rate, colour = scen) +
            geom_smooth(se=FALSE) +
            scale_color_hue(direction = 1) +
            theme_minimal() +
            facet_wrap(vars(value), scales = "free_y")
))




############################ TO HERE
 ####



calculate_std_rates_inc <- function(crude_rates, ref_weights, use_esp = FALSE) {
  
  if (use_esp) {
    weights <- esp_5yr_gendered  # Fixed European standard weights
  } else {
    weights <- ref_weights       # Baseline scenario weights
  }
  
  std_rates_inc <- crude_rates %>%
    left_join(weights, by = c("agegroup_cycle", "gender")) %>%
    filter(!is.na(weight)) %>%
    group_by(cycle, scen, value) %>%
    summarize(
      age_std_rate = sum(crude_rate * weight),
      .groups = "drop"
    )
  
  return(std_rates_inc)
}

# Using reference weights (baseline population)
std_rates_inc_ref <- calculate_std_rates_inc(crude_rates = inc_age_gender_uc, ref_weights = ref_weights, use_esp = FALSE)

# Using European standard population
std_rates_inc_esp <- calculate_std_rates_inc(crude_rates = inc_age_gender_uc, ref_weights = ref_weights, use_esp = TRUE)

## Graph

ggplotly(std_rates_inc_ref %>%
  filter(cycle >= 1L & cycle <= 30L) %>%
  filter(value %in% c("all_cause_dementia", 
                      "breast_cancer", "colon_cancer", "copd", "coronary_heart_disease", "depression", "diabetes", "lung_cancer", 
                      "parkinson", "stroke")) %>%
  ggplot() +
  aes(x = cycle, y = age_std_rate, colour = scen) +
  geom_smooth(se = FALSE) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(value), scales = "free_y"))


###

count_inc_ali <- count_inc |> 
  left_join(people |> filter(name == "np") |> 
              rename(pop = value)) |> 
  mutate(per_capita_count = n/pop, 
         per_100k = per_capita_count * 10^5)
## Average age of onset

## === Prepare time delay data ===

process_scenario <- function(scen_name) {
  # scen_name <- "base"
  df <- all_data[[scen_name]] |>
    dplyr::group_by(id, disease = value) |>
    dplyr::summarise(
      time_to_event = min(cycle),
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

## === Alive Over Time Tab Data ===

alive_data <- all_data |> # life years
  filter(!value %in% c("dead"), !is.na(age_cycle)) |>
  dplyr::group_by(cycle, scen, agegroup_cycle, gender, ladcd) |>
  dplyr::summarise(alive_n = dplyr::n_distinct(id), .groups = "drop") |> 
  left_join(zones |> distinct(ladcd, ladnm), by = "ladcd") 

alive_data_overall <- alive_data |>
  dplyr::group_by(cycle, scen) |>
  dplyr::summarise(alive=sum(alive_n)) |>
  dplyr::mutate(type = "overall",
                name = "overall")

## This is for checking, delete
alive_data_diff <- alive_data |>
  group_by(scen) |>
  summarise(alive_n = sum(alive_n) * 20) |> 
  mutate(diff = alive_n - alive_n[scen == "reference"])

alive_data_sex <- alive_data |>
  dplyr::group_by(cycle, scen, gender) |>
  dplyr::summarise(alive=sum(alive_n)) |>
  dplyr::mutate(type = "sex") |>
  dplyr::rename(name=gender) |>
  dplyr::mutate(name = (case_when(name == 1 ~ "male",
                                  name == 2 ~ "female")))

alive_data_age <- alive_data |>
  dplyr::group_by(cycle, scen, agegroup_cycle) |>
  dplyr::summarise(alive=sum(alive_n)) |>
  dplyr::mutate(type = "age") |>
  dplyr::rename(name=agegroup_cycle)

alive_data_lad <- alive_data |>
  dplyr::group_by(cycle, scen, ladnm) |>
  dplyr::summarise(alive=sum(alive_n)) |>
  dplyr::mutate(type = "lad") |>
  dplyr::rename(name=ladnm)

alive_data_all <- bind_rows(alive_data_overall, alive_data_age, alive_data_lad, alive_data_sex)

## Change number files if you want to keep them for different test runs

saveRDS(alive_data_all, paste0(base_path, "/life_years_overtime_java.RDS"))

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
  left_join(zones |> distinct(ladcd, ladnm), by = "ladcd") |> 
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

# Delay by age group
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

## == Population structure ===

population_df <- all_data |>
  filter(cycle %in% c(0, 10, 30), (!value %in% c("dead", "null")), !is.na(age_cycle)) |>
  group_by(scen, ladcd, gender, agegroup_cycle, cycle) |>
  summarise(population = n_distinct(id), .groups = "drop")



## Check population structure overall over time (time 0, 10, 20) by age and gender

population_check <- population_df |>
  group_by(scen, gender, agegroup_cycle, cycle) |>
  filter(!is.na(agegroup_cycle)) |>
  summarise(population_total = sum(population)) |>
  mutate(cycle=as.factor(cycle))

### to include in shinny, here for checking. 
plot_pop <- ggplot(population_check) +
  aes(x = cycle, y = population_total, fill = scen) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +  # or use another palette or scale_fill_manual()
  theme_minimal() +
  facet_wrap(~ agegroup_cycle)

ggplotly(plot_pop)

## === Age standardized rates ====

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
  
  #Prevalence: Person has the disease in the current or any previous cycle.
  
  #Incidence: Person is diagnosed with the disease in this cycle only, but not in any previous cycle.
  
  # Filter to diseases of interest
  disease_long <- summary_raw |>
    select(id, cycle, value, age_cycle, scen, ladcd, gender)
  
  # Disease history per person
  inc_prev <- disease_long |>
    arrange(id, value, cycle) |>
    group_by(id, value) |>
    mutate(
      first_diagnosis = min(cycle, na.rm = TRUE), #cycle > 0
      incident = cycle == first_diagnosis & cycle != 0,  # NOT incident if diagnosed at cycle 0
      ever_had = ifelse(cycle == 0, cycle == first_diagnosis, cycle != first_diagnosis))  |>
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
    mutate(
      prevalence_flag = case_when(ever_had == TRUE ~ 1, TRUE ~ 0),
      incidence_flag = case_when(incident == TRUE ~ 1, TRUE ~ 0)
    ) |>
    group_by(scen, ladcd, cycle, value) |> 
    summarise(
      prevalence = sum(prevalence_flag),
      incidence = sum(incidence_flag),
      .groups = "drop"
    ) 
  
  
  #  Add total population and rate calculations
  population_df <- inc_prev |>
    group_by(scen, ladcd, cycle) |>
    summarise(population = n_distinct(id), .groups = "drop")
  
  backup_sum <- summary_incprev
  
  ## Check population structure overall over time (time 0, 10, 20) by age and gender
  
  population_check <- population_df |>
    group_by(gender, agegroup, cycle) |>
    summarise(population_total = sum(population)) |>
    mutate(cycle=as.factor(cycle))
  
  ggplot(population_check) +
    aes(x = agegroup, y = population_total, fill = cycle) +
    geom_col(position = "dodge") +
    scale_fill_brewer(palette = "Set1") +  # or use another palette or scale_fill_manual()
    theme_minimal() +
    facet_wrap(vars(gender))
  
  
  
  
  # ## Calculate age standardized rates
  non_standardised_rate <- summary_incprev |>
    left_join(population_df, by = c("scen", "ladcd", "gender", "agegroup", "cycle")) |>
    mutate(
      incidence_rate = (incidence / population),
      prevalence_rate = (prevalence / population)
    ) |> filter(cycle %in% target_cycles) |>
    left_join(std_pop)
  
  # overall
  df_overall <- non_standardised_rate |>
    dplyr::group_by(scen, value, cycle) |>
    dplyr::mutate(std_rate_inc = incidence_rate*std_prop*100000,
                  std_rate_prev =  prevalence_rate*std_prop*100000,
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

### Additional for checking 

## Deaths scaled

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

## Dead by cycle and age group by scenario

calculate_deaths_agegroup <- function(dataset, scen_name){
  
  base_pop <- dataset |> filter(value != "dead", cycle == 1) |> group_by(cycle) |> summarise(n_distinct(id)) |>  pull()
  deaths <- dataset |> filter(value == "dead") |> group_by(cycle, agegroup_cycle) |> summarise(deaths = n_distinct(id)) |> mutate(new_deaths = deaths - lag(deaths, 1))
  deaths$new_deaths <- ifelse(is.na(deaths$new_deaths), deaths$deaths, deaths$new_deaths)
  return(deaths |> mutate(base_pop = base_pop,
                          unit_death = new_deaths/base_pop,
                          total_deaths = round(unit_death * 2827285), 
                          name = scen_name))
  
  
}

deaths_agegroup <- rbind(calculate_deaths_agegroup(all_data$base, "reference"), 
                         calculate_deaths_agegroup(all_data$green, "green"),
                         calculate_deaths_agegroup(all_data$safeStreet, "safeStreet"), 
                         calculate_deaths_agegroup(all_data$both, "both"))


library(ggplot2)
library(plotly)

plot <- ggplot(annual_deaths_agegroup) +
  aes(x = cycle, y = diff, colour = agegroup_cycle) +
  geom_point() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(name))

ggplotly(plot)

## Total difference over cycle by age group

calculate_deaths_agegroup <- function(dataset, scen_name){
  
  base_pop <- dataset |> filter(value != "dead", cycle == 1) |> group_by(cycle) |> summarise(n_distinct(id)) |>  pull()
  deaths <- dataset |> filter(value == "dead") |> group_by(cycle, agegroup_cycle) |> summarise(deaths = n_distinct(id)) |> mutate(new_deaths = deaths - lag(deaths, 1))
  deaths$new_deaths <- ifelse(is.na(deaths$new_deaths), deaths$deaths, deaths$new_deaths)
  return(deaths |> mutate(base_pop = base_pop,
                          unit_death = new_deaths/base_pop,
                          total_deaths = round(unit_death * 2827285), 
                          name = scen_name))
  
  
}

deaths_agegroup <- rbind(calculate_deaths_agegroup(all_data$base, "reference"), 
                         calculate_deaths_agegroup(all_data$green, "green"),
                         calculate_deaths_agegroup(all_data$safeStreet, "safeStreet"), 
                         calculate_deaths_agegroup(all_data$both, "both"))
