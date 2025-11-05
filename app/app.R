# ======================================================================
# Local Shiny: Population, Differences, Mean Ages (RAW only), ASR
# with simple on-disk caching in ./data to avoid recomputation
# ======================================================================

required_pkgs <- c(
  "shiny", "dplyr", "tidyr", "arrow", "readr", "ggplot2", "plotly",
  "scales", "forcats", "purrr", "here", "qs", "DT","tcltk"
)

# Install any missing packages from CRAN
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

# Load packages quietly
suppressPackageStartupMessages(
  lapply(required_pkgs, function(pkg) {
    library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  })
)

options(scipen = 999)

# ------------------- Paths (interactive study region data folder selection) -------------------
region_folder = tcltk::tk_choose.dir(caption = "Select data/[region] folder containing all_data.parquet")
region_name = basename(region_folder)
# Check if a folder was selected
if (!is.na(region_folder)) {
  cat("Selected folder path:", region_folder, "\n")
} else {
  cat("No folder selected.\n")
}

DATA_PATH  <- file.path(region_folder, "all_data.parquet")
ZONES_CSV  <- file.path(region_folder, "zoneSystem.csv")

if (grepl("manchester", tolower(basename(region_folder)))) {
    study_region <- "Manchester"
    regionID_code <- "ladcd"
    regionID_name <- "ladnm"
    regionID_type <- "LAD"
    SCEN_LABS_ORDER <- c("Safer Streets & Greening", "Greening", "Safer Streets")
} else if (grepl("melbourne", tolower(basename(region_folder)))) {
    study_region <- "Melbourne"
    regionID_code <- "SA2_MAIN16"
    regionID_name <- "SA2_MAIN16_NAME"
    regionID_type <- "SA2"
    SCEN_LABS_ORDER <- c("Reference", "Cycling")
} else if (grepl("brunswick", tolower(basename(region_folder)))) {
    study_region <- "Brunswick"
    regionID_code <- "SA2_MAIN16"
    regionID_name <- "SA2_NAME_2016"
    regionID_type <- "SA2"
    SCEN_LABS_ORDER <- c("Reference", "Cycling")
} else {
  stop("Unknown region folder selected: ", region_folder)
}

regionIDs <- c(regionID_code, regionID_name)


if (!dir.exists(DATA_PATH)) {
  stop("Data path does not exist: ", DATA_PATH, "\nPlease ensure data_prep_shiny.R has been successfully run prior to processing.")
}

if (!file.exists(ZONES_CSV)) {
  stop("Zones CSV does not exist: ", ZONES_CSV)
}

# Local cache folder (relative to app)
DATA_DIR <- "data"
if (!dir.exists(DATA_DIR))
  dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

USE_ALL_DATA_CACHE  <- TRUE   # cache the big parquet as an Arrow dataset dir
if (!exists("SCALING")) SCALING <- 5L

# ------------------- Load zones (tiny) ------------------------------
zones    <- readr::read_csv(ZONES_CSV, show_col_types = FALSE)
stopifnot(all(regionIDs %in% names(zones)))
regions <- zones |> distinct(!!rlang::sym(regionID_code), !!rlang::sym(regionID_name))

# ------------------- Load all_data with optional cache --------------
all_data_cache_path <- file.path(DATA_DIR, "all_data.parquet")

if (USE_ALL_DATA_CACHE && dir.exists(all_data_cache_path)) {
  message("Loading all_data from cache: ", all_data_cache_path)
  all_data <- arrow::open_dataset(all_data_cache_path) |> collect()
} else {
  message("Reading parquet from: ", DATA_PATH)
  all_data <- arrow::open_dataset(DATA_PATH) |> collect()
  if (USE_ALL_DATA_CACHE) {
    message("Saving all_data cache: ", all_data_cache_path)
    arrow::write_dataset(all_data, all_data_cache_path)
  }
}

MIN_CYCLE <- 1
MAX_CYCLE <- max(all_data$cycle, na.rm = TRUE)

# ------------------- Helpers ----------------------------------------
ageband <- function(x) cut(
  x, breaks = c(seq(0,100,5), Inf),
  labels = c(paste(seq(0,95,5), seq(4,99,5), sep = "-"), "100+"),
  right = FALSE, include.lowest = TRUE
)

add_agegroups <- function(df) {
  df |> mutate(agegroup_cycle = ageband(age_cycle))
}

theme_clean <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          strip.text = element_text(lineheight = 0.9))
}
add_zero_line <- function() geom_hline(yintercept = 0, linewidth = 0.3)

pop_share <- function(df, group_vars = c("cycle","scen")) {
  df |>
    group_by(across(all_of(c(group_vars, "agegroup_cycle")))) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop_last") |>
    mutate(share = pop / sum(pop, na.rm = TRUE)) |>
    ungroup()
}

diff_vs_reference <- function(df, by = character(0), value_col = "value") {
  ref <- df |>
    filter(scen == "reference") |>
    rename(value_ref = !!rlang::sym(value_col)) |>
    select(cycle, all_of(by), value_ref)
  df |>
    filter(scen != "reference") |>
    left_join(ref, by = c("cycle", by)) |>
    mutate(diff = .data[[value_col]] - value_ref) |>
    select(scen, cycle, all_of(by), diff)
}

get_age_levels <- function(x) if (is.factor(x)) levels(x) else sort(unique(x))
align_age_levels <- function(w, people_age) {
  lv <- get_age_levels(people_age)
  w |> mutate(agegroup_cycle = factor(as.character(agegroup_cycle), levels = lv))
}

# ------------------- Precompute (robust cache) -----------------------
precomp_path <- file.path(DATA_DIR, "precomputed_v1.qs")
death_values <- c("dead","dead_car","dead_bike","dead_walk")

# Set Sys.setenv(REBUILD_CACHE="1") before run to force rebuild
REBUILD_CACHE <- identical(Sys.getenv("REBUILD_CACHE"), "0")

# Only RAW mean ages (rename to mean_age)
REQUIRED_PC <- c(
  # population
  "people_overall","people_gender","people_region",
  # diffs
  "deaths_overall","deaths_gender","deaths_region",
  "diseases_overall","diseases_gender","diseases_region",
  "healthy_overall","healthy_gender","healthy_region",
  "lifey_overall","lifey_gender","lifey_region",
  # mean-age sources + RAW mean age tables (renamed columns)
  "incidence_src","inc_death_src",
  "mean_age_dead_by_scen_val",
  "mean_age_dead_by_scen_val_gender",
  "mean_age_dead_by_scen_val_region",
  "mean_age_onset_by_scen_val",
  "mean_age_onset_by_scen_val_gender",
  "mean_age_onset_by_scen_val_region",
  # ASR
  "asr_overall_all","asr_overall_avg_1_30",
  "asr_gender_all","asr_gender_all_avg_1_30",
  "asr_region_all_per_cycle","asr_region_all_avg_1_30",
  "asr_healthy_years_overall","asr_healthy_years_overall_avg_1_30"
)

cache_ok <- FALSE
if (file.exists(precomp_path) && !REBUILD_CACHE) {
  message("Loading precomputed tables from cache: ", precomp_path)
  pc <- tryCatch(qs::qread(precomp_path), error = function(e) NULL)
  if (!is.null(pc)) {
    list2env(pc, envir = environment())
    missing_names <- setdiff(REQUIRED_PC, names(pc))
    nulls <- names(Filter(is.null, pc))
    if (length(missing_names) == 0 && length(nulls) == 0) cache_ok <- TRUE
    if (!cache_ok) {
      message("Cache incomplete/stale; will rebuild (missing: ",
              paste(c(missing_names, nulls), collapse = ", "), ").")
    }
  }
}

if (!cache_ok) {
  message("Building precomputed tables …")
  
  # ---- Core event sets ----
  # First-onset for all non-death/healthy/null/depression causes
  incidence_all <- all_data |>
    filter(!grepl("dead|healthy|null|depression", value)) |>
    group_by(id, scen, value) |>
    slice_min(order_by = cycle, n = 1, with_ties = FALSE) |>
    ungroup()
  
  # New episodes of depression (first time within id,scen)
  incidence_depression <- all_data |>
    filter(value == "depression") |>
    arrange(id, scen, cycle) |>
    group_by(id, scen) |>
    mutate(is_new = is.na(lag(value)) | lag(value) != "depression") |>
    filter(is_new) |>
    ungroup()
  
  incidence_deaths <- all_data |> filter(value %in% death_values)
  
  incidence <- bind_rows(
    incidence_all,
    incidence_depression,
    incidence_deaths
  ) |>
    add_agegroups() |>
    left_join(regions, by = regionID_code)
  
  # ---- Population (at risk) ----
  people_raw <- all_data |>
    add_agegroups() |>
    # use dynamic grouping so a string column name (regionID_code) is handled safely
    group_by(across(all_of(c("agegroup_cycle", "gender", "cycle", "scen", regionID_code)))) |>
    summarise(pop = n_distinct(id[!grepl("dead|null", value)]), .groups = "drop") |>
    left_join(regions, by = regionID_code)
  
  people_overall <- people_raw |>
    group_by(agegroup_cycle, scen, cycle) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
  
  people_gender <- people_raw |>
    group_by(agegroup_cycle, scen, cycle, gender) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
  
  people_region <- people_raw |>
    # dynamic grouping: regionID_name is a string containing the actual column name (e.g. "regionID_name")
    group_by(across(all_of(c("agegroup_cycle", "scen", "cycle", regionID_name)))) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
  
  # ---- Healthy & Life years ----
  healthy_total_cycle <- all_data |>
    filter(value == "healthy") |>
    group_by(scen, cycle) |>
    summarise(value = n_distinct(id), .groups = "drop")
  
  life_years_cycle <- all_data |>
    filter(!grepl("dead", value)) |>
    group_by(scen, cycle) |>
    summarise(value = n_distinct(id), .groups = "drop")
  
  # ---- Death counts ----
  inc_death <- incidence |> filter(value %in% death_values)
  deaths_overall_raw <- inc_death |> group_by(scen, cycle) |>
    summarise(value = dplyr::n(), .groups = "drop")
  deaths_gender_raw  <- inc_death |> group_by(scen, cycle, gender) |>
    summarise(value = dplyr::n(), .groups = "drop")
  deaths_region_raw     <- inc_death |> 
    group_by(across(all_of(c("scen", "cycle", regionID_name)))) |>
    summarise(value = dplyr::n(), .groups = "drop")
  
  # ---- Disease counts (keep cause) ----
  diseases_all_cycle <- incidence |>
    filter(!grepl("dead|healthy|null", value)) |>
    rename(cause = value) |>
    group_by(cause, scen, cycle) |>
    summarise(value = dplyr::n(), .groups = "drop")
  
  # ---- Differences vs reference ----
  deaths_overall <- diff_vs_reference(deaths_overall_raw)
  deaths_gender  <- diff_vs_reference(deaths_gender_raw, by = "gender")
  deaths_region  <- diff_vs_reference(deaths_region_raw,   by = regionID_name)
  
  diseases_overall <- diff_vs_reference(diseases_all_cycle, by = "cause")
  diseases_gender  <- incidence |>
    filter(!value %in% c("dead","healthy","null")) |>
    rename(cause = value) |>
    group_by(cause, scen, cycle, gender) |>
    summarise(value = dplyr::n(), .groups = "drop") |>
    diff_vs_reference(by = c("gender","cause"))
  diseases_region     <- incidence |>
    filter(!value %in% c("dead","healthy","null")) |>
    rename(cause = value) |>
    group_by(across(all_of(c("cause", "scen", "cycle", regionID_name)))) |>
    summarise(value = dplyr::n(), .groups = "drop") |>
    diff_vs_reference(by = c(regionID_name,"cause"))
  
  healthy_overall  <- healthy_total_cycle |> diff_vs_reference()
  healthy_gender   <- all_data |> filter(value == "healthy") |>
    group_by(scen, cycle, gender) |> summarise(value = n_distinct(id), .groups = "drop") |>
    diff_vs_reference(by = "gender")
  healthy_region      <- all_data |> filter(value == "healthy") |>
    left_join(regions, by = regionID_code) |>
    group_by(across(all_of(c("scen", "cycle", regionID_name)))) |> summarise(value = n_distinct(id), .groups = "drop") |>
    diff_vs_reference(by = regionID_name)
  
  lifey_overall <- life_years_cycle |> diff_vs_reference()
  lifey_gender  <- all_data |> filter(!grepl("dead", value)) |>
    group_by(scen, cycle, gender) |> summarise(value = n_distinct(id), .groups = "drop") |>
    diff_vs_reference(by = "gender")
  lifey_region     <- all_data |> filter(!grepl("dead", value)) |>
    left_join(regions, by = regionID_code) |>
    group_by(across(all_of(c("scen", "cycle", regionID_name)))) |> summarise(value = n_distinct(id), .groups = "drop") |>
    diff_vs_reference(by = regionID_name)
  
  # ---- Mean age (RAW only; rename to mean_age) ----
  inc_death_src <- incidence |>
    filter(value %in% death_values) |>
    select(scen, value, age_cycle, gender, all_of(regionID_name))
  
  incidence_src <- incidence |>
    filter(!value %in% c("healthy","null") & !value %in% death_values) |>
    select(scen, value, age_cycle, gender, all_of(regionID_name))
  
  mean_age_dead_by_scen_val        <- inc_death_src |> group_by(scen, value) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  mean_age_dead_by_scen_val_gender <- inc_death_src |> group_by(scen, value, gender) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  mean_age_dead_by_scen_val_region    <- inc_death_src |> group_by(across(all_of(c("scen", "value", regionID_name)))) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  
  mean_age_onset_by_scen_val        <- incidence_src |> group_by(scen, value) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  mean_age_onset_by_scen_val_gender <- incidence_src |> group_by(scen, value, gender) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  mean_age_onset_by_scen_val_region    <- incidence_src |> group_by(across(all_of(c("scen", "value", regionID_name)))) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  
  # ---- ASR (age-standardised rates) ----
  ref_weights_overall <- people_raw |>
    filter(scen == "reference", cycle == 0) |>
    group_by(agegroup_cycle) |>
    summarise(pop = sum(pop), .groups = "drop") |>
    mutate(weight = pop / sum(pop)) |>
    select(agegroup_cycle, weight)
  
  ref_weights_gender <- people_raw |>
    filter(scen == "reference", cycle == 0) |>
    group_by(agegroup_cycle, gender) |>
    summarise(pop = sum(pop), .groups = "drop") |>
    group_by(gender) |> mutate(weight = pop / sum(pop)) |> ungroup() |>
    select(agegroup_cycle, gender, weight)
  
  W_OVERALL <- align_age_levels(ref_weights_overall, people_raw$agegroup_cycle)
  W_GENDER  <- ref_weights_gender |>
    mutate(agegroup_cycle = factor(as.character(agegroup_cycle),
                                   levels = get_age_levels(people_raw$agegroup_cycle)))
  
  calc_asr <- function(data_cases, causes, people, ref_weights,
                       group_vars = character(), avg_cycles = NULL, min_cycle = MIN_CYCLE) {
    base_groups    <- c("agegroup_cycle", "scen", "cycle")
    all_group_cols <- unique(c(base_groups, group_vars))
    join_vars      <- intersect(names(ref_weights), c("agegroup_cycle", "gender"))
    purrr::map_dfr(causes, function(cause) {
      crude_counts <- data_cases |>
        filter(value == cause, cycle >= min_cycle) |>
        group_by(across(all_of(all_group_cols))) |>
        summarise(cases = dplyr::n(), .groups = "drop")
      pop_counts <- people |>
        filter(cycle >= min_cycle) |>
        group_by(across(all_of(all_group_cols))) |>
        summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
      crude_rates <- crude_counts |>
        left_join(pop_counts, by = all_group_cols) |>
        mutate(crude_rate = if_else(pop > 0, cases / pop * 1e5, NA_real_)) |>
        filter(!is.na(crude_rate))
      std_rates <- crude_rates |>
        left_join(ref_weights, by = join_vars) |>
        filter(!is.na(weight)) |>
        mutate(rate_w = crude_rate * weight)
      sum_groups <- unique(c("scen", "cycle", group_vars))
      asr_per_cycle <- std_rates |>
        group_by(across(all_of(sum_groups))) |>
        summarise(age_std_rate = sum(rate_w), .groups = "drop") |>
        mutate(cause = cause, .before = 1)
      if (is.null(avg_cycles)) return(asr_per_cycle)
      asr_per_cycle |>
        filter(cycle %in% avg_cycles) |>
        group_by(across(all_of(setdiff(sum_groups, "cycle"))), cause) |>
        summarise(age_std_rate = mean(age_std_rate, na.rm = TRUE), .groups = "drop") |>
        mutate(cycle = paste0("avg_", paste(range(avg_cycles), collapse = "-")), .after = "scen")
    })
  }
  
  calc_asr_from_counts <- function(counts_df, people, ref_weights,
                                   group_vars = character(), scale = 1e5,
                                   avg_cycles = NULL, cause_name = "metric",
                                   min_cycle = MIN_CYCLE) {
    group_cols <- unique(c("agegroup_cycle", "scen", "cycle", group_vars))
    join_vars  <- intersect(names(ref_weights), c("agegroup_cycle", "gender"))
    counts_df <- counts_df |>
      filter(cycle >= min_cycle) |>
      group_by(across(all_of(group_cols))) |>
      summarise(num = sum(num, na.rm = TRUE), .groups = "drop")
    pop_counts <- people |>
      filter(cycle >= min_cycle) |>
      group_by(across(all_of(group_cols))) |>
      summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
    rates <- counts_df |>
      left_join(pop_counts, by = group_cols) |>
      mutate(rate = if_else(pop > 0, num / pop * scale, NA_real_)) |>
      filter(!is.na(rate))
    std <- rates |> left_join(ref_weights, by = join_vars) |>
      filter(!is.na(weight)) |> mutate(rate_w = rate * weight)
    sum_groups <- unique(c("scen", "cycle", group_vars))
    out <- std |>
      group_by(across(all_of(sum_groups))) |>
      summarise(age_std_rate = sum(rate_w), .groups = "drop") |>
      mutate(cause = cause_name, .before = 1)
    if (is.null(avg_cycles)) return(out)
    out |>
      filter(cycle %in% avg_cycles) |>
      group_by(across(all_of(setdiff(sum_groups, "cycle"))), cause) |>
      summarise(age_std_rate = mean(age_std_rate, na.rm = TRUE), .groups = "drop") |>
      mutate(cycle = paste0("avg_", paste(range(avg_cycles), collapse = "-")), .after = "scen")
  }
  
  healthy_age_counts <- all_data |>
    add_agegroups() |>
    filter(value == "healthy", cycle >= MIN_CYCLE) |>
    group_by(agegroup_cycle, scen, cycle) |>
    summarise(num = n_distinct(id), .groups = "drop")
  
  all_causes <- incidence |>
    distinct(value) |> pull(value) |> setdiff(c("healthy","null"))
  
  asr_overall_all <- calc_asr(incidence, all_causes, people_raw, W_OVERALL,
                              group_vars = character(), avg_cycles = NULL, min_cycle = MIN_CYCLE)
  asr_overall_avg_1_30 <- calc_asr(incidence, all_causes, people_raw, W_OVERALL,
                                   group_vars = character(), avg_cycles = MIN_CYCLE:MAX_CYCLE, min_cycle = MIN_CYCLE)
  asr_gender_all <- calc_asr(incidence, all_causes, people_raw, W_GENDER,
                             group_vars = "gender", avg_cycles = NULL, min_cycle = MIN_CYCLE)
  asr_gender_all_avg_1_30 <- calc_asr(incidence, all_causes, people_raw, W_GENDER,
                                      group_vars = "gender", avg_cycles = MIN_CYCLE:MAX_CYCLE, min_cycle = MIN_CYCLE)
  asr_region_all_per_cycle <- calc_asr(incidence, all_causes, people_raw, W_OVERALL,
                                    group_vars = regionID_code, avg_cycles = NULL, min_cycle = MIN_CYCLE) |>
    left_join(regions, by = regionID_code)
  asr_region_all_avg_1_30 <- calc_asr(incidence, all_causes, people_raw, W_OVERALL,
                                   group_vars = regionID_code, avg_cycles = MIN_CYCLE:MAX_CYCLE, min_cycle = MIN_CYCLE) |>
    left_join(regions, by = regionID_code)
  
  asr_healthy_years_overall <- calc_asr_from_counts(
    healthy_age_counts, people_raw, W_OVERALL,
    group_vars = character(), scale = 1e5, avg_cycles = NULL,
    cause_name = "healthy_years", min_cycle = MIN_CYCLE
  )
  asr_healthy_years_overall_avg_1_30 <- calc_asr_from_counts(
    healthy_age_counts, people_raw, W_OVERALL,
    group_vars = character(), scale = 1e5, avg_cycles = MIN_CYCLE:MAX_CYCLE,
    cause_name = "healthy_years", min_cycle = MIN_CYCLE
  )
  
  # ---- Save everything the app needs ----
  pc <- mget(REQUIRED_PC)
  message("Saving precomputed cache: ", precomp_path)
  qs::qsave(pc, precomp_path)
  list2env(pc, envir = environment())
}



# =================== SHINY APP ==========================
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(rlang)

# ---- Pretty scenario labels and fixed display order ----
scen_label <- function(x) dplyr::recode(
  x,
  safeStreet = "Safer Streets",
  green      = "Greening",
  both       = "Safer Streets & Greening",
  reference  = "Reference",
  base       = "Reference",
  cycling    = "Cycling",
  .default   = x
)

# ---- Causes to show on "Cases of major NCDs prevented" ----
CAUSE_KEEP <- c(
  "all_cause_dementia",
  "coronary_heart_disease",
  "stroke",
  "diabetes",
  "depression",
  "copd",
  "lung_cancer"
)

# ---- Layout & export constants (bigger for PPT) ----
PLOT_TOP_HEIGHT  <- 980   # tall top charts (esp. region facets)
PLOT_TIME_HEIGHT <- 580
PLOT_AVG_HEIGHT  <- 780
PLOT_POP_HEIGHT  <- 600
PLOT_RES_DPI     <- 144   # device DPI for non-plotly renderPlot (crisper on screen)
PNG_WIDTH_IN     <- 14    # PNG export size
PNG_HEIGHT_IN    <- 8.5
PNG_DPI          <- 300   # export DPI

# ---- Label helpers ----
number_lab0 <- function(x) scales::number(x, accuracy = 1)  # no decimals

# ---- PPT-friendly theme (builds on your theme_clean) ----
ppt_theme <- function() {
  theme_clean() +
    theme(
      text            = element_text(size = 18),
      plot.title      = element_text(size = 22, face = "bold", hjust = 0),
      axis.title.x    = element_text(size = 18),
      axis.title.y    = element_text(size = 18),
      axis.text.x     = element_text(size = 16),
      axis.text.y     = element_text(size = 16),
      strip.text      = element_text(size = 18, face = "bold"),
      legend.title    = element_text(size = 18),
      legend.text     = element_text(size = 16),
      plot.margin     = margin(10, 20, 10, 10)
    )
}

# ----------------------------------------------------------------------
# Expect these to exist in your environment:
# people_overall, people_gender, people_region,
# all_data, regions, incidence_src,
# asr_overall_all, asr_overall_avg_1_30,
# asr_gender_all, asr_gender_all_avg_1_30,
# asr_region_all_per_cycle, asr_region_all_avg_1_30,
# asr_healthy_years_overall, asr_healthy_years_overall_avg_1_30,
# pop_share(), add_agegroups(), theme_clean(), add_zero_line(),
# MIN_CYCLE, MAX_CYCLE, SCALING, diff_vs_reference()
# ----------------------------------------------------------------------

all_scenarios_raw <- sort(unique(people_overall$scen))
all_scenarios_lab <- scen_label(all_scenarios_raw)

pop_cycles    <- sort(unique(people_overall$cycle))
trend_cycles  <- sort(unique(asr_overall_all$cycle))
all_regions_nm   <- sort(unique(people_region[[regionID_name]]))
all_genders   <- sort(unique(people_gender$gender))
all_causes_asr <- sort(unique(c(unique(asr_overall_all$cause), "healthy_years")))

ui <- fluidPage(
  titlePanel("${study_region} Transport and Health model results"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxInput("use_plotly", "Interactive (Plotly)", value = TRUE),
      
      selectInput(
        "scen_sel", "Scenarios:",
        choices  = setNames(all_scenarios_raw, all_scenarios_lab),
        selected = all_scenarios_raw, multiple = TRUE
      ),
      
      selectInput("view_level", "View by:", choices = c("Overall","Gender", regionID_type),
                  selected = "Overall"),
      conditionalPanel(
        "input.view_level == '${regionID_type}'",
        selectizeInput("region_sel", "${regionID_type}(s):",
                       choices = all_regions_nm, multiple = TRUE,
                       options = list(placeholder = "Pick regions (optional)"))
      ),
      tags$hr(),
      
      selectInput(
        "metrics_picker", "Metrics:",
        choices = c(
          "Premature deaths avoided",
          "Life years gained",
          "Life years gained without major NCDs",
          "Cases of major NCDs prevented",
          "Average ages",
          "ASR",
          "Population"
        ),
        selected = "Premature deaths avoided"
      ),
      
      conditionalPanel(
        condition = "['Premature deaths avoided','Life years gained','Life years gained without major NCDs','Cases of major NCDs prevented'].indexOf(input.metrics_picker) >= 0 || ['Premature deaths avoided','Life years gained','Life years gained without major NCDs','Cases of major NCDs prevented'].indexOf(input.main_tabs) >= 0",
        h2("Differences"),
        sliderInput("diff_min_cycle", "Start cycle:",
                    min = min(trend_cycles), max = max(trend_cycles),
                    value = MIN_CYCLE, step = 1),
        checkboxInput("diff_cumulative", "Cumulative over cycles", value = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.metrics_picker == 'Average ages' || input.main_tabs == 'Average ages'",
        h2("Average ages"),
        selectInput("avg_kind", "Average age of:",
                    choices = c("Death" = "death", "Disease onset" = "onset")),
        uiOutput("avg_cause_ui")
      ),
      
      conditionalPanel(
        condition = "input.metrics_picker == 'ASR' || input.main_tabs == 'ASR'",
        h2("ASR"),
        selectInput("asr_mode", "ASR view:",
                    choices = c("Average 1-30 (bars)" = "avg",
                                "Over time (smoothed)" = "trend")),
        selectizeInput("asr_causes", "Causes:", choices = all_causes_asr,
                       selected = c("coronary_heart_disease","stroke","healthy_years"),
                       multiple = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.metrics_picker == 'Population' || input.main_tabs == 'Population'",
        h2("Population"),
        selectizeInput("pop_cycles", "Cycles to show (bars):",
                       choices = pop_cycles, selected = c(2,10,MAX_CYCLE), multiple = TRUE),
        radioButtons("pop_style", "Bar style:",
                     c("Stacked" = "stack", "Side-by-side" = "dodge"),
                     inline = TRUE),
        checkboxInput("pop_share", "Show shares (else counts)", value = TRUE)
      ),
      
      tags$hr(),
      downloadButton("download_csv", "Download current table (CSV)")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          "Premature deaths avoided",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_deaths_bar_ly", height = PLOT_TOP_HEIGHT)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_deaths_bar", height = PLOT_TOP_HEIGHT)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_deaths_bar_png", "Download bar (PNG)")),
          tags$hr(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_deaths_time_ly", height = PLOT_TIME_HEIGHT)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_deaths_time", height = PLOT_TIME_HEIGHT)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_deaths_time_png", "Download time (PNG)")),
          tags$hr(),
          h5("Summary (cumulative at latest cycle)"),
          DT::dataTableOutput("table_deaths_summary")
        ),
        
        tabPanel(
          "Life years gained",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_life_bar_ly", height = PLOT_TOP_HEIGHT)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_life_bar", height = PLOT_TOP_HEIGHT)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_life_bar_png", "Download bar (PNG)")),
          tags$hr(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_life_time_ly", height = PLOT_TIME_HEIGHT)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_life_time", height = PLOT_TIME_HEIGHT)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_life_time_png", "Download time (PNG)")),
          tags$hr(),
          h5("Summary (cumulative at latest cycle)"),
          DT::dataTableOutput("table_life_summary")
        ),
        
        tabPanel(
          "Life years gained without major NCDs",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_healthy_bar_ly", height = PLOT_TOP_HEIGHT)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_healthy_bar", height = PLOT_TOP_HEIGHT)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_healthy_bar_png", "Download bar (PNG)")),
          tags$hr(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_healthy_time_ly", height = PLOT_TIME_HEIGHT)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_healthy_time", height = PLOT_TIME_HEIGHT)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_healthy_time_png", "Download time (PNG)")),
          tags$hr(),
          h5("Summary (cumulative at latest cycle)"),
          DT::dataTableOutput("table_healthy_summary")
        ),
        
        tabPanel(
          "Cases of major NCDs prevented",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_diseases_bar_ly", height = PLOT_TOP_HEIGHT)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_diseases_bar", height = PLOT_TOP_HEIGHT)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_diseases_bar_png", "Download bar (PNG)")),
          tags$hr(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_diseases_time_ly", height = PLOT_TIME_HEIGHT)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_diseases_time", height = PLOT_TIME_HEIGHT)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_diseases_time_png", "Download time (PNG)")),
          tags$hr(),
          h5("Summary (cumulative at latest cycle)"),
          DT::dataTableOutput("table_diseases_summary")
        ),
        
        tabPanel(
          "Average ages",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_avg_ly", height = PLOT_AVG_HEIGHT)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_avg", height = PLOT_AVG_HEIGHT)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_avg_png", "Download chart (PNG)")),
          tags$hr(),
          tableOutput("table_avg")
        ),
        
        tabPanel(
          "ASR",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_asrly", height = 640)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_asr", height = 640)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_asr_png", "Download chart (PNG)"))
        ),
        
        tabPanel(
          "Population",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_poply", height = PLOT_POP_HEIGHT)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_pop", height = PLOT_POP_HEIGHT)),
          conditionalPanel("!input.use_plotly", downloadButton("dl_pop_png", "Download chart (PNG)"))
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  options(scipen = 999)
  
  # keep dropdown and tabs in sync
  observeEvent(input$main_tabs, {
    if (!identical(input$metrics_picker, input$main_tabs)) {
      updateSelectInput(session, "metrics_picker", selected = input$main_tabs)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$metrics_picker, {
    if (!identical(input$main_tabs, input$metrics_picker)) {
      updateTabsetPanel(session, "main_tabs", selected = input$metrics_picker)
    }
  }, ignoreInit = TRUE)
  
  # ---------- Avg ages cause picker (UI) ----------
  output$avg_cause_ui <- renderUI({
    if (input$avg_kind == "onset") {
      selectInput("avg_cause", "Disease (onset):",
                  choices = sort(unique(incidence_src$value)),
                  selected = "coronary_heart_disease")
    } else {
      pretty_death_choices <- c(
        "Average age deaths from all causes" = "dead",
        "Average age bike fatality"          = "dead_bike",
        "Average age car occupant fatality"  = "dead_car",
        "Average age pedestrian fatality"    = "dead_walk"
      )
      selectizeInput("avg_death_causes", "Death cause(s):",
                     choices  = pretty_death_choices,
                     selected = unname(pretty_death_choices),
                     multiple = TRUE)
    }
  })

  # ---------- Helper: apply region filter when viewing region ----------
  .filter_region <- function(df) {
    if (identical(input$view_level, regionID_type) &&
        regionID_name %in% names(df) &&
        length(input$region_sel)) {
      dplyr::filter(df, .data[[regionID_name]] %in% input$region_sel)
    } else {
      df
    }
  }
  
  # ---------- Population ----------
  pop_data <- reactive({
    req(input$pop_cycles, input$scen_sel)
    view <- input$view_level
    if (view == "Overall") {
      dat <- people_overall %>% dplyr::filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen")), y = "share", y_lab = "Share of pop.")
      } else list(data = dat, y = "pop", y_lab = "Population count")
    } else if (view == "Gender") {
      dat <- people_gender %>% dplyr::filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen","gender")), facet = "gender", y = "share", y_lab = "Share of pop.")
      } else list(data = dat, facet = "gender", y = "pop", y_lab = "Population count")
    } else {
      dat <- people_region %>% dplyr::filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
  if (length(input$region_sel)) dat <- dat %>% dplyr::filter(.data[[regionID_name]] %in% input$region_sel)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen",regionID_name)), facet = regionID_name, y = "share", y_lab = "Share of pop.")
      } else list(data = dat, facet = regionID_name, y = "pop", y_lab = "Population count")
    }
  })
  
  build_pop_plot <- reactive({
    pd <- pop_data(); d <- pd$data; req(nrow(d) > 0)
    d <- d %>%
      dplyr::mutate(scen_lab = factor(scen_label(scen),
                                      levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]))
    pos <- if (input$pop_style == "dodge") position_dodge(width = 0.8) else "stack"
    base <- ggplot(d, aes(x = agegroup_cycle, y = .data[[pd$y]], fill = scen_lab)) +
      geom_col(position = pos) +
      scale_y_continuous(labels = if (pd$y == "share") scales::percent else scales::label_comma()) +
      labs(title = "Population by age group", x = "Age group", y = pd$y_lab, fill = "Scenario") +
      ppt_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    if (!is.null(pd$facet)) {
      base + facet_grid(as.formula(paste(pd$facet, "~ cycle")), scales = "free_x")
    } else {
      base + facet_wrap(~ cycle, nrow = 1)
    }
  })
  output$plot_pop   <- renderPlot({ build_pop_plot() }, res = PLOT_RES_DPI)
  output$plot_poply <- renderPlotly({ ggplotly(build_pop_plot(), tooltip = c("x","y","fill")) })
  
  # ---------- Incidence helper ----------
  .incidence_first_onset <- function() {
    inc <- get0("incidence", ifnotfound = NULL, inherits = TRUE)
    if (!is.null(inc)) return(inc)
    incidence_all <- all_data %>%
      dplyr::filter(!grepl("dead|healthy|null|depression", value)) %>%
      dplyr::group_by(id, scen, value) %>%
      dplyr::slice_min(order_by = cycle, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
    incidence_depression <- all_data %>%
      dplyr::filter(value == "depression") %>%
      dplyr::arrange(id, scen, cycle) %>%
      dplyr::group_by(id, scen) %>%
      dplyr::mutate(is_new = is.na(dplyr::lag(value)) | dplyr::lag(value) != "depression") %>%
      dplyr::filter(is_new) %>%
      dplyr::ungroup()
    dplyr::bind_rows(incidence_all, incidence_depression) %>%
      add_agegroups() %>%
      dplyr::left_join(regions, by = regionID_code)
  }
  
  # ---------- Differences base ----------
  .compute_diff_base <- function(kind, view) {
    if (kind == "deaths") {
      src <- all_data %>% dplyr::filter(value %in% c("dead","dead_car","dead_bike","dead_walk"))
      if (view == "Overall") {
        counts <- src %>% dplyr::group_by(scen, cycle) %>% dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts)
      } else if (view == "Gender") {
        counts <- src %>% dplyr::group_by(scen, cycle, gender) %>% dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts, by = "gender")
      } else {
        counts <- src %>% dplyr::left_join(regions, by = regionID_code) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", "cycle", regionID_name)))) %>% dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts, by = regionID_name)
      }
    } else if (kind == "healthy") {
      src <- all_data %>% dplyr::filter(value == "healthy")
      if (view == "Overall") {
        counts <- src %>% dplyr::group_by(scen, cycle) %>% dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts)
      } else if (view == "Gender") {
        counts <- src %>% dplyr::group_by(scen, cycle, gender) %>% dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts, by = "gender")
      } else {
        counts <- src %>% dplyr::left_join(regions, by = regionID_code) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", "cycle", regionID_name)))) %>% dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts, by = regionID_name)
      }
    } else if (kind == "life") {
      src <- all_data %>% dplyr::filter(!grepl("dead", value))
      if (view == "Overall") {
        counts <- src %>% dplyr::group_by(scen, cycle) %>% dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts)
      } else if (view == "Gender") {
        counts <- src %>% dplyr::group_by(scen, cycle, gender) %>% dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts, by = "gender")
      } else {
        counts <- src %>% dplyr::left_join(regions, by = regionID_code) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", "cycle", regionID_name)))) %>% dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts, by = regionID_name)
      }
    } else {
      inc <- .incidence_first_onset()
      src <- inc %>%
        dplyr::filter(!value %in% c("healthy","null","dead","dead_car","dead_bike","dead_walk")) %>%
        dplyr::rename(cause = value)
      if (view == "Overall") {
        counts <- src %>% dplyr::group_by(cause, scen, cycle) %>% dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts, by = "cause")
      } else if (view == "Gender") {
        counts <- src %>% dplyr::group_by(cause, scen, cycle, gender) %>% dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts, by = c("gender","cause"))
      } else {
  counts <- src %>% dplyr::group_by(dplyr::across(dplyr::all_of(c("cause", "scen", "cycle", regionID_name)))) %>% dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts, by = c(regionID_name,"cause"))
      }
    }
  }
  
  make_diff_reactive <- function(kind) {
    reactive({
      req(input$view_level, input$diff_min_cycle)
      scen_keep <- setdiff(input$scen_sel, "reference")
      validate(need(length(scen_keep) > 0, "Select at least one non-reference scenario."))
      
      view <- input$view_level; minc <- input$diff_min_cycle; cumu <- isTRUE(input$diff_cumulative)
      base <- .compute_diff_base(kind, view)
      by_dims <- switch(view, Overall = character(0), Gender = "gender", Region = regionID_name)
      if (kind == "diseases") by_dims <- c("cause", by_dims)
      
      base %>%
        dplyr::filter(cycle >= minc, scen %in% scen_keep) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", by_dims, "cycle")))) %>%
        dplyr::summarise(diff = sum(diff, na.rm = TRUE), .groups = "drop") %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", by_dims)))) %>%
        dplyr::mutate(
          cumulative = cumsum(diff),
          y = if (cumu) cumulative else diff
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(metric = dplyr::case_when(
          kind == "deaths"   ~ "Premature deaths avoided",
          kind == "life"     ~ "Life years gained",
          kind == "healthy"  ~ "Life years gained without major NCDs",
          TRUE               ~ "Cases of major NCDs prevented"
        ))
    })
  }
  
  # base reactives
  diff_deaths   <- make_diff_reactive("deaths")
  diff_life     <- make_diff_reactive("life")
  diff_healthy  <- make_diff_reactive("healthy")
  diff_diseases <- make_diff_reactive("diseases")
  
  # Region-filtered wrappers used everywhere below
  diff_deaths_v   <- reactive({ .filter_region(diff_deaths()) })
  diff_life_v     <- reactive({ .filter_region(diff_life()) })
  diff_healthy_v  <- reactive({ .filter_region(diff_healthy()) })
  diff_diseases_v <- reactive({ .filter_region(diff_diseases()) })
  
  # Filter to requested NCD causes for diseases metric (after region filter)
  diff_diseases_sel <- reactive({
    d <- diff_diseases_v()
    if ("cause" %in% names(d)) d <- d %>% dplyr::filter(cause %in% CAUSE_KEEP)
    d
  })
  
  # ===== BAR CHARTS with labels NEAR ZERO (aligned) ===================
  .add_label_pos <- function(bars, value_col = "value_num", facet_vars = character(0)) {
    if (!length(facet_vars)) {
      maxv <- max(bars[[value_col]], na.rm = TRUE)
      pos  <- if (maxv > 0) 0.02 * maxv else 0.98 * maxv
      bars$label_pos <- pos
      bars
    } else {
      bars %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(facet_vars))) %>%
        dplyr::mutate(
          .mx = max(.data[[value_col]], na.rm = TRUE),
          label_pos = ifelse(.mx > 0, 0.02 * .mx, 0.98 * .mx)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.mx)
    }
  }
  
  build_bar_plot <- function(d) {
    req(nrow(d) > 0)
    d <- d %>% dplyr::mutate(scen_lab = scen_label(scen))
    if ("gender" %in% names(d)) {
      if (is.numeric(d$gender)) d <- d %>% dplyr::mutate(gender = factor(if_else(gender == 1, "Male", "Female")))
      else d <- d %>% dplyr::mutate(gender = as.factor(gender))
    }
    other <- intersect(c("cause","gender",regionID_name), names(d))
    
    bars <- d %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c("scen_lab", other)))) %>%
      dplyr::slice_max(order_by = cycle, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        scen_lab = factor(scen_lab, levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_lab]),
        value_num = y * SCALING,
        label_val = number_lab0(value_num)
      )
    
    facet_vars <- character(0)
    if (regionID_name %in% other && "cause" %in% other) facet_vars <- c(regionID_name,"cause")
    else if (regionID_name %in% other) facet_vars <- regionID_name
    else if ("cause" %in% other) facet_vars <- "cause"
    
    bars <- .add_label_pos(bars, value_col = "value_num", facet_vars = facet_vars)
    
    p <- ggplot(bars, aes(x = scen_lab, y = value_num, fill = scen_lab)) +
      geom_col(width = 0.8) +
      geom_text(aes(y = label_pos, label = label_val), hjust = 0, size = 5.5) +
      scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
      coord_flip() +
      labs(title = paste0(unique(d$metric)), x = NULL, y = "Cumulative difference", fill = "Scenario") +
      ppt_theme() + guides(fill = "none")
    
    if ("gender" %in% other && !"cause" %in% other && !regionID_name %in% other) {
      pos <- position_dodge(width = 0.75)
      p <- ggplot(bars, aes(x = scen_lab, y = value_num, fill = gender)) +
        geom_col(position = pos, width = 0.75) +
        geom_text(aes(y = label_pos, label = label_val), position = pos, hjust = 0, size = 5.5) +
        scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
        coord_flip() +
        labs(title = paste0(unique(d$metric)), x = NULL, y = "Cumulative difference", fill = "Gender") +
        ppt_theme()
    } else if ("cause" %in% other && !regionID_name %in% other && !"gender" %in% other) {
      p <- p + facet_wrap(~ cause, scales = "free_x")
    } else if ("cause" %in% other && "gender" %in% other && !regionID_name %in% other) {
      pos <- position_dodge(width = 0.75)
      p <- ggplot(bars, aes(x = scen_lab, y = value_num, fill = gender)) +
        geom_col(position = pos, width = 0.75) +
        geom_text(aes(y = label_pos, label = label_val), position = pos, hjust = 0, size = 5.5) +
        scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
        coord_flip() +
        facet_wrap(~ cause, scales = "free_x") +
        labs(title = paste0(unique(d$metric)), x = NULL, y = "Cumulative difference", fill = "Gender") +
        ppt_theme()
    } else if (regionID_name %in% other && !"cause" %in% other) {
      p <- p + facet_wrap(~ regionID_name, scales = "free_x", ncol = 2, nrow = 5)
    } else if (regionID_name %in% other && "cause" %in% other) {
      p <- p + facet_grid(regionID_name ~ cause, scales = "free_x")
    }
    p
  }
  
  build_time_plot <- function(d) {
    req(nrow(d) > 0)
    d <- d %>% dplyr::mutate(scen_lab = scen_label(scen))
    if ("gender" %in% names(d)) d <- d %>% dplyr::mutate(gender = as.factor(gender))
    ttl <- paste0(unique(d$metric), " — per cycle")
    
    base <- ggplot(d, aes(x = cycle, y = diff, colour = scen_lab))
    add <- function(p) p + add_zero_line() + ppt_theme() +
      labs(title = ttl, x = "Cycle (year)", y = "Difference vs reference", colour = "Scenario")
    
    if ("gender" %in% names(d) && !"cause" %in% names(d)) {
      add(base + aes(linetype = gender) + geom_smooth(se = FALSE))
    } else if (regionID_name %in% names(d) && !"cause" %in% names(d)) {
      add(base + geom_smooth(se = FALSE)) +
        facet_wrap(~ regionID_name, ncol = 2, nrow = 5, scales = "free_y")
    } else if ("cause" %in% names(d) && !regionID_name %in% names(d) && !"gender" %in% names(d)) {
      (add(base + geom_smooth(se = FALSE))) + facet_wrap(~ cause, scales = "free_y")
    } else if ("cause" %in% names(d) && "gender" %in% names(d)) {
      (add(base + aes(linetype = gender) + geom_smooth(se = FALSE))) + facet_wrap(~ cause, scales = "free_y")
    } else if ("cause" %in% names(d) && regionID_name %in% names(d)) {
      (add(base + geom_smooth(se = FALSE))) + facet_grid(regionID_name ~ cause, scales = "free_y")
    } else {
      add(base + geom_smooth(se = FALSE))
    }
  }
  
  # ---- Renders (non-plotly with higher DPI) ----
  # Deaths
  output$plot_deaths_bar     <- renderPlot({ build_bar_plot(diff_deaths_v()) }, res = PLOT_RES_DPI)
  output$plot_deaths_bar_ly  <- renderPlotly({ ggplotly(build_bar_plot(diff_deaths_v()),  tooltip = c("x","y","fill","label")) })
  output$plot_deaths_time    <- renderPlot({ build_time_plot(diff_deaths_v()) }, res = PLOT_RES_DPI)
  output$plot_deaths_time_ly <- renderPlotly({ ggplotly(build_time_plot(diff_deaths_v()), tooltip = c("x","y","colour","linetype")) })
  
  # Life
  output$plot_life_bar     <- renderPlot({ build_bar_plot(diff_life_v()) }, res = PLOT_RES_DPI)
  output$plot_life_bar_ly  <- renderPlotly({ ggplotly(build_bar_plot(diff_life_v()),  tooltip = c("x","y","fill","label")) })
  output$plot_life_time    <- renderPlot({ build_time_plot(diff_life_v()) }, res = PLOT_RES_DPI)
  output$plot_life_time_ly <- renderPlotly({ ggplotly(build_time_plot(diff_life_v()), tooltip = c("x","y","colour","linetype")) })
  
  # Healthy
  output$plot_healthy_bar     <- renderPlot({ build_bar_plot(diff_healthy_v()) }, res = PLOT_RES_DPI)
  output$plot_healthy_bar_ly  <- renderPlotly({ ggplotly(build_bar_plot(diff_healthy_v()),  tooltip = c("x","y","fill","label")) })
  output$plot_healthy_time    <- renderPlot({ build_time_plot(diff_healthy_v()) }, res = PLOT_RES_DPI)
  output$plot_healthy_time_ly <- renderPlotly({ ggplotly(build_time_plot(diff_healthy_v()), tooltip = c("x","y","colour","linetype")) })
  
  # Diseases (filtered by region + cause keep)
  output$plot_diseases_bar     <- renderPlot({ build_bar_plot(diff_diseases_sel()) }, res = PLOT_RES_DPI)
  output$plot_diseases_bar_ly  <- renderPlotly({ ggplotly(build_bar_plot(diff_diseases_sel()),  tooltip = c("x","y","fill","label")) })
  output$plot_diseases_time    <- renderPlot({ build_time_plot(diff_diseases_sel()) }, res = PLOT_RES_DPI)
  output$plot_diseases_time_ly <- renderPlotly({ ggplotly(build_time_plot(diff_diseases_sel()), tooltip = c("x","y","colour","linetype")) })
  
  # ----- PNG download helpers (non-plotly charts) -----
  save_png_plot <- function(plot, file, w = PNG_WIDTH_IN, h = PNG_HEIGHT_IN, dpi = PNG_DPI) {
    ggplot2::ggsave(filename = file, plot = plot, width = w, height = h, units = "in", dpi = dpi, bg = "white")
  }
  output$dl_deaths_bar_png <- downloadHandler(
    filename = function() "deaths_bar.png",
    content  = function(file) save_png_plot(build_bar_plot(diff_deaths_v()), file)
  )
  output$dl_deaths_time_png <- downloadHandler(
    filename = function() "deaths_time.png",
    content  = function(file) save_png_plot(build_time_plot(diff_deaths_v()), file)
  )
  output$dl_life_bar_png <- downloadHandler(
    filename = function() "life_bar.png",
    content  = function(file) save_png_plot(build_bar_plot(diff_life_v()), file)
  )
  output$dl_life_time_png <- downloadHandler(
    filename = function() "life_time.png",
    content  = function(file) save_png_plot(build_time_plot(diff_life_v()), file)
  )
  output$dl_healthy_bar_png <- downloadHandler(
    filename = function() "healthy_years_bar.png",
    content  = function(file) save_png_plot(build_bar_plot(diff_healthy_v()), file)
  )
  output$dl_healthy_time_png <- downloadHandler(
    filename = function() "healthy_years_time.png",
    content  = function(file) save_png_plot(build_time_plot(diff_healthy_v()), file)
  )
  output$dl_diseases_bar_png <- downloadHandler(
    filename = function() "ncd_cases_bar.png",
    content  = function(file) save_png_plot(build_bar_plot(diff_diseases_sel()), file)
  )
  output$dl_diseases_time_png <- downloadHandler(
    filename = function() "ncd_cases_time.png",
    content  = function(file) save_png_plot(build_time_plot(diff_diseases_sel()), file)
  )
  
  # =====================================================================
  # -------------------- Average ages -----------------------------------
  # =====================================================================
  death_value_lab <- function(v) dplyr::recode(
    v,
    dead      = "Average age deaths from all causes",
    dead_bike = "Average age bike fatality",
    dead_car  = "Average age car occupant fatality",
    dead_walk = "Average age pedestrian fatality",
    .default  = v
  )
  
  build_avg_plot <- reactive({
    req(input$avg_kind, input$view_level)
    view <- input$view_level
    
    if (input$avg_kind == "death") {
      causes <- input$avg_death_causes; req(causes)
      
      if (view == "Overall") {
        df <- mean_age_dead_by_scen_val %>%
          dplyr::filter(value %in% causes, scen %in% input$scen_sel) %>%
          dplyr::mutate(
            scen_lab  = factor(scen_label(scen), levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]),
            value_lab = death_value_lab(value)
          )
        req(nrow(df) > 0)
        
        lab_df <- df %>%
          group_by(value_lab) %>%
          summarise(mx = max(mean_age, na.rm = TRUE), .groups = "drop") %>%
          mutate(label_pos = ifelse(mx > 0, 0.02 * mx, 0.98 * mx))
        df <- df %>% left_join(lab_df, by = "value_lab")
        
        ggplot(df, aes(x = scen_lab, y = mean_age, fill = scen_lab)) +
          geom_col(width = 0.8) +
          geom_text(aes(y = label_pos, label = number_lab0(mean_age)), hjust = 0, size = 5.5) +
          scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
          coord_flip() +
          facet_wrap(~ value_lab, scales = "free_x") +
          labs(title = "", x = NULL, y = "Years") +
          ppt_theme() + guides(fill = "none")
        
      } else if (view == "Gender") {
        df <- mean_age_dead_by_scen_val_gender %>%
          dplyr::filter(value %in% causes, scen %in% input$scen_sel) %>%
          dplyr::mutate(
            scen_lab  = factor(scen_label(scen), levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]),
            gender    = as.factor(gender),
            value_lab = death_value_lab(value)
          )
        req(nrow(df) > 0)
        pos <- position_dodge(width = 0.75)
        
        lab_df <- df %>%
          group_by(value_lab) %>%
          summarise(mx = max(mean_age, na.rm = TRUE), .groups = "drop") %>%
          mutate(label_pos = ifelse(mx > 0, 0.02 * mx, 0.98 * mx))
        df <- df %>% left_join(lab_df, by = "value_lab")
        
        ggplot(df, aes(x = scen_lab, y = mean_age, fill = gender)) +
          geom_col(position = pos, width = 0.75) +
          geom_text(aes(y = label_pos, label = number_lab0(mean_age)), position = pos, hjust = 0, size = 5.5) +
          scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
          coord_flip() +
          facet_wrap(~ value_lab, scales = "free_x") +
          labs(title = "Mean age at death (by gender)", x = NULL, y = "Years", fill = "Gender") +
          ppt_theme()
        
      } else {
        df <- mean_age_dead_by_scen_val_region %>%
          dplyr::filter(value %in% causes, scen %in% input$scen_sel) %>%
          dplyr::mutate(
            scen_lab  = factor(scen_label(scen), levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]),
            value_lab = death_value_lab(value)
          )
  if (length(input$region_sel)) df <- df %>% dplyr::filter(.data[[regionID_name]] %in% input$region_sel)
        req(nrow(df) > 0)
        
        lab_df <- df %>%
          group_by(across(all_of(c(regionID_name, "value_lab")))) %>%
          summarise(mx = max(mean_age, na.rm = TRUE), .groups = "drop_last") %>%
          mutate(label_pos = ifelse(mx > 0, 0.02 * mx, 0.98 * mx))
        df <- df %>% left_join(lab_df, by = c(regionID_name,"value_lab"))
        
        ggplot(df, aes(x = scen_lab, y = mean_age, fill = scen_lab)) +
          geom_col(width = 0.8) +
          geom_text(aes(y = label_pos, label = number_lab0(mean_age)), hjust = 0, size = 5.5) +
          scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
          coord_flip() +
          facet_grid(regionID_name ~ value_lab, scales = "free_x") +
          labs(title = "Mean age at death (by ${regionID_type})", x = NULL, y = "Years") +
          ppt_theme() + guides(fill = "none")
      }
      
    } else { # onset
      cause <- input$avg_cause; req(cause)
      
      if (view == "Overall") {
        df <- mean_age_onset_by_scen_val %>%
          dplyr::filter(value == cause, scen %in% input$scen_sel) %>%
          dplyr::mutate(scen_lab = factor(scen_label(scen), levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]))
        req(nrow(df) > 0)
        
        mx  <- max(df$mean_age, na.rm = TRUE)
        pos <- if (mx > 0) 0.02 * mx else 0.98 * mx
        
        ggplot(df, aes(x = scen_lab, y = mean_age, fill = scen_lab)) +
          geom_col(width = 0.8) +
          geom_text(aes(y = pos, label = number_lab0(mean_age)), hjust = 0, size = 5.5) +
          scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
          coord_flip() +
          labs(title = paste0("Mean age at onset — ", cause), x = NULL, y = "Years") +
          ppt_theme() + guides(fill = "none")
        
      } else if (view == "Gender") {
        df <- mean_age_onset_by_scen_val_gender %>%
          dplyr::filter(value == cause, scen %in% input$scen_sel) %>%
          dplyr::mutate(scen_lab = factor(scen_label(scen), levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]),
                        gender = as.factor(gender))
        req(nrow(df) > 0)
        posd <- position_dodge(width = 0.75)
        mx   <- max(df$mean_age, na.rm = TRUE)
        pos  <- if (mx > 0) 0.02 * mx else 0.98 * mx
        
        ggplot(df, aes(x = scen_lab, y = mean_age, fill = gender)) +
          geom_col(position = posd, width = 0.75) +
          geom_text(aes(y = pos, label = number_lab0(mean_age)), position = posd, hjust = 0, size = 5.5) +
          scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
          coord_flip() +
          labs(title = paste0("Mean age at onset (by gender) — ", cause),
               x = NULL, y = "Years", fill = "Gender") +
          ppt_theme()
        
      } else {
        df <- mean_age_onset_by_scen_val_region %>%
          dplyr::filter(value == cause, scen %in% input$scen_sel) %>%
          dplyr::mutate(scen_lab = factor(scen_label(scen), levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]))
  if (length(input$region_sel)) df <- df %>% dplyr::filter(.data[[regionID_name]] %in% input$region_sel)
        req(nrow(df) > 0)
        
        lab_df <- df %>%
          group_by(across(all_of(c(regionID_name)))) %>%
          summarise(mx = max(mean_age, na.rm = TRUE), .groups = "drop") %>%
          mutate(label_pos = ifelse(mx > 0, 0.02 * mx, 0.98 * mx))
        df <- df %>% left_join(lab_df, by = regionID_name)
        
        ggplot(df, aes(x = scen_lab, y = mean_age, fill = scen_lab)) +
          geom_col(width = 0.8) +
          geom_text(aes(y = label_pos, label = number_lab0(mean_age)), hjust = 0, size = 5.5) +
          scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
          coord_flip() +
          facet_wrap(~ regionID_name, scales = "free_x", ncol = 2, nrow = 5) +
          labs(title = paste0("Mean age at onset (by ${regionID_type}) — ", cause), x = NULL, y = "Years") +
          ppt_theme() + guides(fill = "none")
      }
    }
  })
  output$plot_avg    <- renderPlot({ build_avg_plot() }, res = PLOT_RES_DPI)
  output$plot_avg_ly <- renderPlotly({ ggplotly(build_avg_plot(), tooltip = c("x","y","fill","label")) })
  output$dl_avg_png  <- downloadHandler(
    filename = function() "average_ages.png",
    content  = function(file) ggplot2::ggsave(file, build_avg_plot(), width = PNG_WIDTH_IN, height = PNG_HEIGHT_IN, units = "in", dpi = PNG_DPI, bg = "white")
  )
  
  output$table_avg   <- renderTable({
    view <- input$view_level
    if (input$avg_kind == "death") {
      causes <- input$avg_death_causes; req(causes)
      if (view == "Overall") {
        mean_age_dead_by_scen_val %>%
          dplyr::filter(value %in% causes, scen %in% input$scen_sel) %>%
          dplyr::mutate(scenario = scen_label(scen),
                        value = death_value_lab(value)) %>%
          dplyr::select(scenario, value, mean_age) %>%
          dplyr::arrange(scenario, value)
      } else if (view == "Gender") {
        mean_age_dead_by_scen_val_gender %>%
          dplyr::filter(value %in% causes, scen %in% input$scen_sel) %>%
          dplyr::mutate(scenario = scen_label(scen),
                        value = death_value_lab(value)) %>%
          dplyr::select(scenario, gender, value, mean_age) %>%
          dplyr::arrange(scenario, gender, value)
      } else {
        df <- mean_age_dead_by_scen_val_region %>%
          dplyr::filter(value %in% causes, scen %in% input$scen_sel) %>%
          dplyr::mutate(scenario = scen_label(scen),
                        value = death_value_lab(value))
  if (length(input$region_sel)) df <- df %>% dplyr::filter(.data[[regionID_name]] %in% input$region_sel)
        df %>% dplyr::select(scenario, all_of(regionID_name), value, mean_age) %>%
          dplyr::arrange(scenario, regionID_name, value)
      }
    } else {
      cause <- input$avg_cause; req(cause)
      if (view == "Overall") {
        mean_age_onset_by_scen_val %>%
          dplyr::filter(value == cause, scen %in% input$scen_sel) %>%
          dplyr::mutate(scenario = scen_label(scen)) %>%
          dplyr::select(scenario, value, mean_age) %>%
          dplyr::arrange(scenario)
      } else if (view == "Gender") {
        mean_age_onset_by_scen_val_gender %>%
          dplyr::filter(value == cause, scen %in% input$scen_sel) %>%
          dplyr::mutate(scenario = scen_label(scen)) %>%
          dplyr::select(scenario, gender, value, mean_age) %>%
          dplyr::arrange(scenario, gender)
      } else {
        df <- mean_age_onset_by_scen_val_region %>%
          dplyr::filter(value == cause, scen %in% input$scen_sel) %>%
          dplyr::mutate(scenario = scen_label(scen))
  if (length(input$region_sel)) df <- df %>% dplyr::filter(.data[[regionID_name]] %in% input$region_sel)
        df %>% dplyr::select(scenario, all_of(regionID_name), value, mean_age) %>%
          dplyr::arrange(scenario, regionID_name)
      }
    }
  })
  
  # ---------- ASR ----------
  to_chr_cause <- function(df) if ("cause" %in% names(df)) dplyr::mutate(df, cause = as.character(cause)) else df
  asr_overall_all                    <- to_chr_cause(asr_overall_all)
  asr_overall_avg_1_30               <- to_chr_cause(asr_overall_avg_1_30)
  asr_gender_all                     <- to_chr_cause(asr_gender_all)
  asr_gender_all_avg_1_30            <- to_chr_cause(asr_gender_all_avg_1_30)
  asr_region_all_per_cycle              <- to_chr_cause(asr_region_all_per_cycle)
  asr_region_all_avg_1_30               <- to_chr_cause(asr_region_all_avg_1_30)
  asr_healthy_years_overall          <- to_chr_cause(asr_healthy_years_overall)
  asr_healthy_years_overall_avg_1_30 <- to_chr_cause(asr_healthy_years_overall_avg_1_30)
  
  build_asr_plot <- reactive({
    req(input$asr_mode, input$asr_causes)
    causes <- input$asr_causes
    if (input$asr_mode == "avg") {
      if (input$view_level == "Overall") {
        df <- dplyr::bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) %>%
          dplyr::filter(cause %in% causes) %>%
          dplyr::mutate(scen_lab = factor(scen_label(scen),
                                          levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]))
        req(nrow(df) > 0)
        
        lab_df <- df %>%
          group_by(cause) %>%
          summarise(mx = max(age_std_rate, na.rm = TRUE), .groups = "drop") %>%
          mutate(label_pos = ifelse(mx > 0, 0.02 * mx, 0.98 * mx))
        df <- df %>% left_join(lab_df, by = "cause")
        
        ggplot(df, aes(x = scen_lab, y = age_std_rate, fill = scen_lab)) +
          geom_col(width = 0.8) +
          geom_text(aes(y = label_pos, label = number_lab0(age_std_rate)), hjust = 0, size = 5.5) +
          scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
          coord_flip() +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR (avg cycles 1–30)", x = NULL, y = "ASR per 100,000", fill = "Scenario") +
          ppt_theme() + guides(fill = "none")
        
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all_avg_1_30 %>%
          dplyr::filter(cause %in% causes) %>%
          dplyr::mutate(scen_lab = factor(scen_label(scen),
                                          levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]),
                        gender = as.factor(gender))
        req(nrow(df) > 0)
        pos <- position_dodge2(width = 0.75, padding = 0.05, preserve = "single")
        
        lab_df <- df %>%
          group_by(cause) %>%
          summarise(mx = max(age_std_rate, na.rm = TRUE), .groups = "drop") %>%
          mutate(label_pos = ifelse(mx > 0, 0.02 * mx, 0.98 * mx))
        df <- df %>% left_join(lab_df, by = "cause")
        
        ggplot(df, aes(x = scen_lab, y = age_std_rate, fill = gender)) +
          geom_col(position = pos, width = 0.75) +
          geom_text(aes(y = label_pos, label = number_lab0(age_std_rate)), position = pos, hjust = 0, size = 5.5) +
          scale_y_continuous(expand = expansion(mult = c(0.08, 0.05))) +
          coord_flip() +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR by gender (avg cycles 1–30)", x = NULL, y = "ASR per 100,000", fill = "Gender") +
          ppt_theme()
        
      } else {
        df <- asr_region_all_avg_1_30 %>%
          dplyr::filter(cause %in% causes) %>%
          dplyr::mutate(scen_lab = factor(scen_label(scen),
                                          levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]))
  if (length(input$region_sel)) df <- df %>% dplyr::filter(.data[[regionID_name]] %in% input$region_sel)
        req(nrow(df) > 0)
        pos <- position_dodge2(width = 0.8, padding = 0.08, preserve = "single")
        
        mx   <- max(df$age_std_rate, na.rm = TRUE)
        lpos <- if (mx > 0) 0.02 * mx else 0.98 * mx
        
        ggplot(df, aes(x = reorder(regionID_name, age_std_rate), y = age_std_rate, fill = scen_lab)) +
          geom_col(position = pos, width = 0.8) +
          geom_text(aes(y = lpos, label = number_lab0(age_std_rate)), position = pos, hjust = 0, size = 5.0) +
          scale_y_continuous(expand = expansion(mult = c(0.10, 0.06))) +
          coord_flip() +
          labs(title = "ASR by ${regionID_type} (avg cycles 1–30)", x = NULL, y = "ASR per 100,000", fill = "Scenario") +
          ppt_theme()
      }
      
    } else {
      if (input$view_level == "Overall") {
        df <- dplyr::bind_rows(asr_overall_all, asr_healthy_years_overall) %>%
          dplyr::filter(cause %in% causes, cycle >= MIN_CYCLE) %>%
          dplyr::mutate(scen_lab = factor(scen_label(scen),
                                          levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]))
        req(nrow(df) > 0)
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen_lab, group = scen_lab)) +
          geom_smooth(se = FALSE) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle (smoothed, cycles 1–30)", x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          ppt_theme()
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all %>%
          dplyr::filter(cause %in% causes, cycle >= MIN_CYCLE) %>%
          dplyr::mutate(scen_lab = factor(scen_label(scen),
                                          levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]),
                        gender = as.factor(gender))
        req(nrow(df) > 0)
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen_lab, linetype = gender)) +
          geom_smooth(se = FALSE) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle by gender (smoothed, cycles 1–30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario", linetype = "Gender") +
          ppt_theme()
      } else {
        dat <- asr_region_all_per_cycle %>%
          dplyr::filter(cause %in% causes, cycle >= MIN_CYCLE) %>%
          dplyr::mutate(scen_lab = factor(scen_label(scen),
                                          levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_label(scen)]))
        req(nrow(dat) > 0)
  if (length(input$region_sel)) dat <- dat %>% dplyr::filter(.data[[regionID_name]] %in% input$region_sel)
        req(nrow(dat) > 0)
        ggplot(dat, aes(x = cycle, y = age_std_rate, colour = scen_lab)) +
          geom_smooth(se = FALSE) +
          facet_grid(regionID_name ~ cause, scales = "free_y") +
          labs(title = "ASR per cycle by ${regionID_type} (smoothed, cycles 1–30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          ppt_theme()
      }
    }
  })
  output$plot_asr   <- renderPlot({ build_asr_plot() }, res = PLOT_RES_DPI)
  output$plot_asrly <- renderPlotly({ ggplotly(build_asr_plot(), tooltip = c("x","y","colour","fill","linetype")) })
  output$dl_asr_png <- downloadHandler(
    filename = function() "asr.png",
    content  = function(file) ggplot2::ggsave(file, build_asr_plot(), width = PNG_WIDTH_IN, height = PNG_HEIGHT_IN, units = "in", dpi = PNG_DPI, bg = "white")
  )
  
  # ---------- CSV download ----------
  current_table <- reactive({
    tab <- input$main_tabs
    if (tab == "Population") {
      pd <- pop_data(); pd$data %>% dplyr::mutate(across(where(is.numeric), ~ round(., 6)))
    } else if (tab %in% c(
      "Premature deaths avoided",
      "Life years gained",
      "Life years gained without major NCDs",
      "Cases of major NCDs prevented"
    )) {
      d <- switch(tab,
                  "Premature deaths avoided"              = diff_deaths_v(),
                  "Life years gained"                      = diff_life_v(),
                  "Life years gained without major NCDs"   = diff_healthy_v(),
                  "Cases of major NCDs prevented"          = diff_diseases_sel()
      )
      req(nrow(d) > 0)
      other <- intersect(c("cause","gender",regionID_name), names(d))
      d %>%
        dplyr::mutate(scen_lab = scen_label(scen)) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c("scen_lab", other)))) %>%
        dplyr::slice_max(order_by = cycle, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
          scenario = factor(scen_lab, levels = SCEN_LABS_ORDER[SCEN_LABS_ORDER %in% scen_lab]),
          !!!(if (length(other)) rlang::syms(other) else NULL),
          final_cycle = cycle,
          cumulative_value = y,
          cumulative_value_scaled = y * SCALING
        )
    } else if (tab == "Average ages") {
      output$table_avg %>% req(); isolate({ output$table_avg() })
    } else if (tab == "ASR") {
      NULL
    }
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0(
      "export_",
      gsub("[[:space:]]+", "_", tolower(input$main_tabs)),
      "_", as.character(Sys.Date()), ".csv"
    ),
    content  = function(file) readr::write_csv(current_table(), file, na = "")
  )
  
  # ----- PNG download for population -----
  output$dl_pop_png <- downloadHandler(
    filename = function() "population.png",
    content  = function(file) ggplot2::ggsave(file, build_pop_plot(), width = PNG_WIDTH_IN, height = PNG_HEIGHT_IN, units = "in", dpi = PNG_DPI, bg = "white")
  )
}



shinyApp(ui, server)
# ======================================================================
