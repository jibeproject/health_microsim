# ======================================================================
# Local Shiny: Population, Differences, Mean Ages (death/onset), ASR
# + Disease picker for Δ diseases
# + Robust gender fix (handles 1/2, M/F strings)
# + NEW: Impact summary tab (deaths prevented, life years, healthy) by scenario
# ======================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(readr)
  library(ggplot2)
  library(plotly)
  library(scales)
  library(forcats)
  library(purrr)
  library(here)
  library(qs)
  library(DT)
})

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
    regionID_code <- "regioncd"
    regionID_name <- regionID_name
    regionID_type <- "region"
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

# ------------------- Constants ----------------------------------------

MIN_CYCLE <- 1
MAX_CYCLE <- 30
SCALING   <- 1L
death_values <- c("dead","dead_car","dead_bike","dead_walk")

# ------------------- Helpers -------------------------------------------
theme_clean_base <- function(base_size = 18) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold"),
      strip.text       = element_text(lineheight = 0.9, face = "bold"),
      axis.title       = element_text(face = "bold"),
      plot.margin      = margin(10, 16, 10, 10)
    )
}
add_zero_line <- function() geom_hline(yintercept = 0, linewidth = 0.4, colour = "#555555")

# Scenario pretty labels
scen_label <- function(x) dplyr::recode(
  x,
  safeStreets = "Safer Streets",
  safeStreet  = "Safer Streets",
  green       = "Greening",
  both        = "Safer Streets & Greening",
  .default = x
)

# ---- Gender normalisation (handles 1/2 numeric and text) ----
norm_gender <- function(x) {
  lx <- tolower(trimws(as.character(x)))
  out <- dplyr::case_when(
    lx %in% c("2","f","female","woman","women") ~ "Female",
    lx %in% c("1","m","male","man","men")       ~ "Male",
    TRUE                                        ~ NA_character_
  )
  factor(out, levels = c("Female","Male"))
}

# unify any gender-like column to a standard "gender" factor and drop NAs
fix_gender_cols <- function(df) {
  gcol <- intersect(names(df), c("gender","sex","sex_group"))
  if (length(gcol) == 0) return(df)
  df |>
    mutate(gender = norm_gender(.data[[gcol[1]]])) |>
    filter(!is.na(gender))
}

# Apply to known gender tables if present
for (nm in c(
  "people_gender",
  "deaths_gender","diseases_gender","healthy_gender","lifey_gender",
  "asr_gender_all","asr_gender_all_avg_1_30",
  "mean_age_dead_raw_by_scen_val_gender",
  "mean_age_dead_weight_by_scen_val_gender",
  "mean_age_onset_raw_by_scen_val_gender",
  "mean_age_onset_weight_by_scen_val_gender"
)) {
  if (exists(nm, inherits = FALSE)) {
    obj <- get(nm, envir = environment())
    if (is.data.frame(obj)) assign(nm, fix_gender_cols(obj), envir = environment())
  }
}

# Gender colours / scales
pal_gender <- c(Female = "#0072B2", Male = "#D55E00")
scale_gender_fill     <- function() scale_fill_manual(values = pal_gender, drop = FALSE)
scale_gender_colour   <- function() scale_colour_manual(values = pal_gender, drop = FALSE)
scale_gender_linetype <- function() scale_linetype_manual(values = c(Female = "solid", Male = "dashed"), drop = FALSE)

# Metric palette (Impact summary)
pal_metric <- c(
  "Premature deaths avoided"                 = "#E69F00",
  "Life years gained"                        = "#56B4E9",
  "Life years without major NCDs"            = "#009E73"
)

# Age helpers for Population shares
ageband <- function(x) cut(
  x, breaks = c(seq(0,100,5), Inf),
  labels = c(paste(seq(0,95,5), seq(4,99,5), sep = "-"), "100+"),
  right = FALSE, include.lowest = TRUE
)
add_agegroups <- function(df) df |> mutate(agegroup_cycle = ageband(age_cycle))

pop_share <- function(df, group_vars = c("cycle","scen")) {
  df |>
    group_by(across(all_of(c(group_vars, "agegroup_cycle")))) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop_last") |>
    mutate(share = pop / sum(pop, na.rm = TRUE)) |>
    ungroup()
}

# Label placement for horizontal bars
compute_bar_labels <- function(df, value_col, facet_cols = character(), inside_frac = 0.08) {
  vcol <- rlang::sym(value_col)
  df |>
    group_by(across(all_of(facet_cols))) |>
    mutate(
      .v      = !!vcol,
      .vmax   = max(.v, na.rm = TRUE),
      .inside = .v >= inside_frac * .vmax,
      label_hjust = ifelse(.inside, 0.98, -0.10)
    ) |>
    ungroup()
}

# ------------------- UI -------------------------------------------------
all_scenarios <- sort(unique(people_overall$scen))
all_scenarios_lab <- stats::setNames(all_scenarios, scen_label(all_scenarios))
pop_cycles    <- sort(unique(people_overall$cycle))
trend_cycles  <- sort(unique(asr_overall_all$cycle))
all_regions_nm   <- sort(unique(zones[[regionID_name]]))
all_causes_asr <- sort(unique(c(unique(asr_overall_all$cause), "healthy_years")))
all_disease_causes <- sort(unique(as.character(diseases_overall$cause)))

ui <- fluidPage(
  titlePanel("Local Health Explorer — Population, Differences, Mean Ages & ASR"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxInput("use_plotly", "Interactive (Plotly)", value = TRUE),
      selectInput("scen_sel", "Scenarios:", choices = all_scenarios_lab,
                  selected = all_scenarios, multiple = TRUE),
      selectInput("view_level", "View by:", choices = c("Overall","Gender", regionID_type),
                  selected = "Overall"),
      conditionalPanel(
        paste0("input.view_level == '", regionID_type, "'"),
        selectizeInput("region_sel", paste0(regionID_type, "(s):"),
                       choices = all_regions_nm, multiple = TRUE,
                       options = list(placeholder = paste0("Pick ", regionID_type, "(s) (optional)")))
      ),
      tags$hr(),
      tabsetPanel(
        id = "control_tabs", type = "pills",
        # --- Population controls ---
        conditionalPanel(
          condition = "input.main_tabs == 'Population'",
          h2("Population"),
          selectizeInput("pop_cycles", "Cycles to show (bars):",
                         choices = pop_cycles, selected = c(2,10,MAX_CYCLE), multiple = TRUE),
          radioButtons("pop_style", "Bar style:", c("Stacked"="stack","Side-by-side"="dodge"),
                       inline = TRUE),
          checkboxInput("pop_share", "Show shares (else counts)", value = TRUE)
        ),
        # --- Differences controls ---
        conditionalPanel(
          condition = "input.main_tabs == 'Differences vs reference'",
          h2("Differences"),
          selectInput("metric_kind", "Metric:", choices = c(
            "Diseases postponed (Δ diseases)"      = "diseases",
            "Premature deaths avoided"             = "deaths",
            "Life years gained without major NCDs" = "healthy",
            "Life years gained"                    = "life"
          )),
          conditionalPanel(
            condition = "input.metric_kind == 'diseases'",
            selectizeInput(
              "diff_diseases",
              "Select diseases to show:",
              choices  = all_disease_causes,
              selected = all_disease_causes,
              multiple = TRUE,
              options  = list(plugins = list("remove_button"), placeholder = "Choose diseases...")
            )
          ),
          sliderInput("diff_min_cycle", "Start cycle:",
                      min = min(trend_cycles), max = max(trend_cycles),
                      value = MIN_CYCLE, step = 1),
          checkboxInput("diff_cumulative", "Cumulative over cycles", value = TRUE)
        ),
        # --- Average ages controls ---
        conditionalPanel(
          condition = "input.main_tabs == 'Average ages'",
          h2("Average ages"),
          selectInput("avg_kind", "Average age of:", choices = c("Death"="death","Disease onset"="onset")),
          uiOutput("avg_cause_ui")
        ),
        # --- ASR controls ---
        conditionalPanel(
          condition = "input.main_tabs == 'ASR'",
          h2("ASR"),
          selectInput("asr_mode", "ASR view:",
                      choices = c("Average 1-30 (bars)"="avg","Over time (smoothed)"="trend")),
          selectizeInput("asr_causes", "Causes:", choices = all_causes_asr,
                         selected = c("coronary_heart_disease","stroke","healthy_years"),
                         multiple = TRUE)
        ),
        # --- Impact summary controls (NEW) ---
        conditionalPanel(
          condition = "input.main_tabs == 'Impact summary'",
          h2("Impact summary"),
          sliderInput("impact_min_cycle", "Start cycle:",
                      min = min(trend_cycles), max = max(trend_cycles),
                      value = MIN_CYCLE, step = 1),
          checkboxInput("impact_cumulative", "Cumulative over cycles", value = TRUE),
          helpText("Shows Premature deaths avoided, Life years gained, and Life years without major NCDs by scenario.")
        )
      ),
      tags$hr(),
      downloadButton("download_csv", "Download current table (CSV)")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        # ----------------- Differences -----------------
        tabPanel(
          "Differences vs reference",
          fluidRow(
            column(6, strong("Download summary bars: "),
                   downloadButton("dl_diffbar_png", "PNG"),
                   downloadButton("dl_diffbar_svg", "SVG")),
            column(6, strong("Download trend lines: "),
                   downloadButton("dl_diff_png", "PNG"),
                   downloadButton("dl_diff_svg", "SVG"))
          ),
          br(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_diffbarly", height = 300)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_diffbar", height = 300)),
          tags$hr(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_diffly", height = 560)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_diff", height = 560)),
          tags$hr(),
          h5("Summary (cumulative at latest cycle, or sum if non-cumulative)"),
          DT::dataTableOutput("table_diff_summary")
        ),
        
        # ----------------- Average ages -----------------
        tabPanel("Average ages", tableOutput("table_avg")),
        
        # ----------------- ASR -----------------
        tabPanel(
          "ASR",
          fluidRow(
            column(12, strong("Download ASR chart: "),
                   downloadButton("dl_asr_png", "PNG"),
                   downloadButton("dl_asr_svg", "SVG"))
          ),
          br(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_asrly", height = 680)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_asr", height = 680))
        ),
        
        # ----------------- Population -----------------
        tabPanel(
          "Population",
          fluidRow(
            column(12, strong("Download population chart: "),
                   downloadButton("dl_pop_png", "PNG"),
                   downloadButton("dl_pop_svg", "SVG"))
          ),
          br(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_poply", height = 560)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_pop", height = 560))
        ),
        
        # ----------------- Impact summary (NEW) -----------------
        tabPanel(
          "Impact summary",
          fluidRow(
            column(12, strong("Download impact chart: "),
                   downloadButton("dl_impact_png", "PNG"),
                   downloadButton("dl_impact_svg", "SVG"))
          ),
          br(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_impactly", height = 560)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_impact", height = 560)),
          tags$hr(),
          DT::dataTableOutput("table_impact")
        )
      )
    )
  )
)

# ------------------- SERVER --------------------------------------------
server <- function(input, output, session) {
  
  # ---------- Avg ages cause picker ----------
  output$avg_cause_ui <- renderUI({
    if (input$avg_kind == "onset") {
      selectInput("avg_cause", "Disease (onset):",
                  choices = sort(unique(incidence_src$value)),
                  selected = "coronary_heart_disease")
    } else {
      selectizeInput("avg_death_causes", "Death cause(s):",
                     choices = c("dead","dead_car","dead_bike","dead_walk"),
                     selected = c("dead","dead_car","dead_bike","dead_walk"),
                     multiple = TRUE)
    }
  })
  
  # ---------- Population ----------
  pop_data <- reactive({
    req(input$pop_cycles, input$scen_sel)
    view <- input$view_level
    
    if (view == "Overall") {
      dat <- people_overall |>
        filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles) |>
        mutate(scen_lab = scen_label(scen))
      if (isTRUE(input$pop_share)) {
        tmp <- pop_share(dat, c("cycle","scen")) |> mutate(scen_lab = scen_label(scen))
        list(data = tmp, y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, y = "pop", y_lab = "Population count")
      }
      
    } else if (view == "Gender") {
      dat <- people_gender |>
        filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles) |>
        mutate(scen_lab = scen_label(scen))
      if (!"agegroup_cycle" %in% names(dat) && "age_cycle" %in% names(dat)) dat <- add_agegroups(dat)
      if (isTRUE(input$pop_share)) {
        tmp <- pop_share(dat, c("cycle","scen","gender")) |> mutate(scen_lab = scen_label(scen))
        list(data = tmp, facet = "gender", y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, facet = "gender", y = "pop", y_lab = "Population count")
      }
      
    } else { # Region
      dat <- people_region |>
        filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (length(input$region_sel)) dat <- dplyr::filter(dat, regionID_name %in% input$region_sel)
      dat <- mutate(dat, scen_lab = scen_label(scen))
      if (isTRUE(input$pop_share)) {
        tmp <- pop_share(dat, c("cycle","scen",regionID_name)) |> mutate(scen_lab = scen_label(scen))
        list(data = tmp, facet = regionID_name, y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, facet = regionID_name, y = "pop", y_lab = "Population count")
      }
    }
  })
  
  build_pop_plot <- reactive({
    pd <- pop_data(); d <- pd$data; req(nrow(d) > 0)
    pos <- if (input$pop_style == "dodge") position_dodge(width = 0.8) else "stack"
    base <- ggplot(d, aes(x = agegroup_cycle, y = .data[[pd$y]], fill = scen_lab)) +
      geom_col(position = pos) +
      scale_y_continuous(labels = if (pd$y == "share") percent else label_number(big.mark=",")) +
      labs(title = "Population by age group", x = "Age group", y = pd$y_lab, fill = "Scenario") +
      theme_clean_base()
    if (!is.null(pd$facet)) {
      base + facet_grid(as.formula(paste(pd$facet, "~ cycle")), scales = "free_x")
    } else {
      base + facet_wrap(~ cycle, nrow = 1)
    }
  })
  output$plot_pop   <- renderPlot({ build_pop_plot() })
  output$plot_poply <- renderPlotly({ ggplotly(build_pop_plot(), tooltip = c("x","y","fill")) })
  
  # ---------- Differences vs reference ----------
  diff_long <- reactive({
    req(input$metric_kind, input$view_level, input$diff_min_cycle)
    scen_keep <- setdiff(input$scen_sel, "reference")
    validate(need(length(scen_keep) > 0, "Select at least one non-reference scenario."))
    minc <- input$diff_min_cycle; view <- input$view_level; cumu <- isTRUE(input$diff_cumulative)
    
    dl  <- deaths_region
    dil <- diseases_region
    hl  <- healthy_region
    ll  <- lifey_region
    if (length(input$region_sel)){
      dl  <- dplyr::filter(dl,  .data[[regionID_name]] %in% input$region_sel)
      dil <- dplyr::filter(dil, .data[[regionID_name]] %in% input$region_sel)
      hl  <- dplyr::filter(hl,  .data[[regionID_name]] %in% input$region_sel)
      ll  <- dplyr::filter(ll,  .data[[regionID_name]] %in% input$region_sel)
    }
    
    pick <- switch(input$metric_kind,
                   deaths   = list(Overall=deaths_overall,  Gender=deaths_gender,  region=dl,  label="Premature deaths avoided"),
                   diseases = list(Overall=diseases_overall,Gender=diseases_gender,region=dil, label="Δ Diseases"),
                   healthy  = list(Overall=healthy_overall, Gender=healthy_gender, region=hl,  label="Life years without major NCDs"),
                   life     = list(Overall=lifey_overall,   Gender=lifey_gender,   region=ll,  label="Life years gained")
    )
    base <- pick[[view]]
    
    # Choose grouping strategy
    if (input$metric_kind == "diseases") {
      by <- switch(view, Overall="cause", Gender=c("cause","gender"), region=c("cause",regionID_name))
    } else {
      by <- switch(view, Overall=character(0), Gender="gender", region=regionID_name)
    }
    
    df <- base |>
      filter(cycle >= minc, scen %in% scen_keep)
    
    # Apply disease filter only when metric is diseases
    if (input$metric_kind == "diseases" && length(input$diff_diseases)) {
      df <- df |> filter(as.character(cause) %in% input$diff_diseases)
    }
    
    df |>
      group_by(across(all_of(c("scen", by, "cycle")))) |>
      summarise(diff = sum(diff, na.rm = TRUE), .groups = "drop") |>
      group_by(across(all_of(c("scen", by)))) |>
      mutate(y = if (cumu) cumsum(diff) else diff) |>
      ungroup() |>
      mutate(metric = pick$label, scen_lab = scen_label(scen))
  })
  
  # ---- Summary for bars ----
  diff_summary <- reactive({
    d <- diff_long(); req(nrow(d) > 0)
    by <- c()
    if ("cause"  %in% names(d)) by <- c(by, "cause")
    if ("gender" %in% names(d)) by <- c(by, "gender")
    if (regionID_name  %in% names(d)) by <- c(by, regionID_name)
    
    metric_lab <- unique(d$metric)[1]
    
    out <- if (isTRUE(input$diff_cumulative)) {
      d |>
        group_by(across(all_of(c("scen", by)))) |>
        slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
        ungroup() |>
        transmute(
          metric = metric_lab, scen,
          !!!(if (length(by)) rlang::syms(by) else NULL),
          final_cycle = cycle,
          cumulative_value = y
        )
    } else {
      d |>
        group_by(across(all_of(c("scen", by)))) |>
        summarise(
          final_cycle = max(cycle, na.rm = TRUE),
          cumulative_value = sum(diff, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(metric = metric_lab, .before = 1)
    }
    
    mutate(out, scen_lab = scen_label(scen))
  })
  
  # ---- Summary bars (gender gets its own colours) ----
  build_diffbar_plot <- reactive({
    ds <- diff_summary(); req(nrow(ds) > 0)
    
    has_cause  <- "cause"  %in% names(ds)
    has_gender <- "gender" %in% names(ds)
    has_region    <- regionID_name  %in% names(ds)
    
    facet_cols <- character(0)
    if (has_cause)  facet_cols <- c(facet_cols, "cause")
    if (has_region)    facet_cols <- c(facet_cols, regionID_name)
    if (has_gender) facet_cols <- c(facet_cols, "gender")
    
    ds <- compute_bar_labels(ds, value_col = "cumulative_value", facet_cols = facet_cols)
    
    lev <- ds |>
      group_by(scen_lab) |>
      summarise(total = sum(cumulative_value, na.rm = TRUE), .groups = "drop") |>
      arrange(total) |>
      pull(scen_lab)
    ds$scen_lab <- factor(ds$scen_lab, levels = lev)
    
    if (has_gender) {
      pos <- position_dodge2(width = 0.75, padding = 0.05, preserve = "single")
      p <- ggplot(ds, aes(x = scen_lab, y = cumulative_value, fill = gender)) +
        geom_col(position = pos, width = 0.75) +
        geom_text(
          aes(label = number(cumulative_value, accuracy = 1), hjust = label_hjust),
          position = pos, vjust = 0.5, colour = "black", fontface = "bold", size = 4.2,
          show.legend = FALSE
        ) +
        scale_gender_fill()
    } else {
      p <- ggplot(ds, aes(x = scen_lab, y = cumulative_value, fill = scen_lab)) +
        geom_col(width = 0.75) +
        geom_text(
          aes(label = number(cumulative_value, accuracy = 1), hjust = label_hjust),
          vjust = 0.5, colour = "black", fontface = "bold", size = 4.2, show.legend = FALSE
        ) +
        guides(fill = "none")
    }
    
    if (has_region && has_cause) {
      p <- p + facet_grid(regionnm ~ cause, scales = "free_y")
    } else if (has_region) {
      p <- p + facet_wrap(~ regionnm, nrow = 2, scales = "free_y")
    } else if (has_cause) {
      p <- p + facet_wrap(~ cause, ncol = 4, scales = "free_y")
    }
    
    ylab <- if (isTRUE(input$diff_cumulative)) "Cumulative difference" else "Sum Δ vs reference"
    
    p +
      coord_flip(clip = "off") +
      labs(title = unique(ds$metric)[1], x = NULL, y = ylab) +
      scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                         expand = expansion(mult = c(0.05, 0.05))) +
      theme_clean_base()
  })
  output$plot_diffbar   <- renderPlot({ build_diffbar_plot() })
  output$plot_diffbarly <- renderPlotly({ ggplotly(build_diffbar_plot(), tooltip = c("x","y","fill")) })
  
  # ---- Trends (lines) ----
  build_diff_plot <- reactive({
    d <- diff_long(); req(nrow(d) > 0)
    ylab <- if (isTRUE(input$diff_cumulative)) "Cumulative Δ vs reference" else "Δ vs reference"
    ttl  <- d$metric[1]
    
    if ("gender" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen_lab, linetype = gender)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        scale_gender_linetype() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario", linetype = "Gender") +
        theme_clean_base()
    } else if (regionID_name %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen_lab)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        facet_wrap(~ regionnm, nrow = 2, scales = "free_y") +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean_base()
    } else {
      ggplot(d, aes(x = cycle, y = y, colour = scen_lab)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean_base()
    }
  })
  output$plot_diff   <- renderPlot({ build_diff_plot() })
  output$plot_diffly <- renderPlotly({ ggplotly(build_diff_plot(), tooltip = c("x","y","colour","linetype")) })
  
  # ---------- Diff summary table ----------
  output$table_diff_summary <- DT::renderDT({
    d <- diff_long(); req(nrow(d) > 0)
    
    by <- if ("gender" %in% names(d)) {
      if ("cause" %in% names(d)) c("cause","gender") else "gender"
    } else if (regionID_name %in% names(d)) {
      if ("cause" %in% names(d)) c("cause",regionID_name) else regionID_name
    } else {
      if ("cause" %in% names(d)) "cause" else character(0)
    }
    
    metric_lab <- unique(d$metric)[1]
    
    if (isTRUE(input$diff_cumulative)) {
      d |>
        group_by(across(all_of(c("scen", by)))) |>
        slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
        ungroup() |>
        transmute(
          metric = metric_lab, scen,
          !!!(if (length(by)) rlang::syms(by) else NULL),
          final_cycle = cycle,
          cumulative_value = y,
          cumulative_value_scaled = y * SCALING
        ) |>
        mutate(scen = scen_label(scen)) |>
        arrange(scen, across(all_of(by))) |>
        DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
    } else {
      d |>
        group_by(across(all_of(c("scen", by)))) |>
        summarise(final_cycle = max(cycle, na.rm = TRUE),
                  cumulative_value = sum(diff, na.rm = TRUE),
                  .groups = "drop") |>
        mutate(metric = metric_lab, cumulative_value_scaled = cumulative_value * SCALING, .before = 1) |>
        mutate(scen = scen_label(scen)) |>
        arrange(scen, across(all_of(by))) |>
        DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
    }
  })
  
  # ---------- Average ages (death / onset) ----------
  output$table_avg <- renderTable({
    view <- input$view_level
    if (input$avg_kind == "death") {
      causes <- input$avg_death_causes; req(causes)
      if (view == "Overall") {
        mean_age_dead_raw_by_scen_val |>
          filter(value %in% causes) |>
          left_join(mean_age_dead_weight_by_scen_val |> filter(value %in% causes),
                    by = c("scen","value")) |>
          arrange(scen, value) |>
          mutate(scen = scen_label(scen)) |>
          rename(mean_age_raw_years = mean_age_raw)
      } else if (view == "Gender") {
        mean_age_dead_raw_by_scen_val_gender |>
          filter(value %in% causes) |>
          left_join(mean_age_dead_weight_by_scen_val_gender |> filter(value %in% causes),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender, value) |>
          mutate(scen = scen_label(scen)) |>
          rename(mean_age_raw_years = mean_age_raw)
      } else {
        tmp <- mean_age_dead_raw_by_scen_val_region
        if (length(input$region_sel) > 0) tmp <- filter(tmp, regionnm %in% input$region_sel)
        tmp |>
          filter(value %in% causes) |>
          arrange(scen, regionnm, value) |>
          mutate(scen = scen_label(scen)) |>
          rename(mean_age_raw_years = mean_age_raw)
      }
    } else {
      cause <- input$avg_cause; req(cause)
      if (view == "Overall") {
        mean_age_onset_raw_by_scen_val |>
          filter(value == cause) |>
          left_join(mean_age_onset_weight_by_scen_val |> filter(value == cause),
                    by = c("scen","value")) |>
          arrange(scen) |>
          mutate(scen = scen_label(scen)) |>
          select(scen, value, mean_age_raw_years = mean_age_raw)
      } else if (view == "Gender") {
        mean_age_onset_raw_by_scen_val_gender |>
          filter(value == cause) |>
          left_join(mean_age_onset_weight_by_scen_val_gender |> filter(value == cause),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender) |>
          mutate(scen = scen_label(scen)) |>
          select(scen, gender, value, mean_age_raw_years = mean_age_raw)
      } else {
        tmp <- mean_age_onset_raw_by_scen_val_region
        if (length(input$region_sel) > 0) tmp <- filter(tmp, regionnm %in% input$region_sel)
        tmp |>
          filter(value == cause) |>
          arrange(scen, regionnm) |>
          mutate(scen = scen_label(scen)) |>
          rename(mean_age_raw_years = mean_age_raw)
      }
    }
  })
  
  # ---------- ASR (respect scenario selection) ----------
  to_chr_cause <- function(df) if ("cause" %in% names(df)) dplyr::mutate(df, cause = as.character(cause)) else df
  asr_overall_all                   <<- to_chr_cause(asr_overall_all)
  asr_overall_avg_1_30              <<- to_chr_cause(asr_overall_avg_1_30)
  asr_gender_all                    <<- to_chr_cause(asr_gender_all)
  asr_gender_all_avg_1_30           <<- to_chr_cause(asr_gender_all_avg_1_30)
  asr_region_all_per_cycle             <<- to_chr_cause(asr_region_all_per_cycle)
  asr_region_all_avg_1_30              <<- to_chr_cause(asr_region_all_avg_1_30)
  asr_healthy_years_overall         <<- to_chr_cause(asr_healthy_years_overall)
  asr_healthy_years_overall_avg_1_30<<- to_chr_cause(asr_healthy_years_overall_avg_1_30)
  
  build_asr_plot <- reactive({
    req(input$asr_mode, input$asr_causes, input$scen_sel)
    causes    <- input$asr_causes
    scen_keep <- input$scen_sel
    
    if (input$asr_mode == "avg") {
      
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
          filter(cause %in% causes, scen %in% scen_keep) |>
          mutate(scen_lab = scen_label(scen)) |>
          compute_bar_labels(value_col = "age_std_rate", facet_cols = "cause")
        lev <- df |>
          group_by(scen_lab) |>
          summarise(t = sum(age_std_rate, na.rm = TRUE), .groups = "drop") |>
          arrange(t) |>
          pull(scen_lab)
        df$scen_lab <- factor(df$scen_lab, levels = lev)
        
        ggplot(df, aes(x = scen_lab, y = age_std_rate, fill = scen_lab)) +
          geom_col(width = 0.8) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1), hjust = label_hjust),
                    vjust = 0.5, colour = "black", fontface = "bold", size = 4.2, show.legend = FALSE) +
          guides(fill = "none") +
          coord_flip(clip = "off") +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR (avg cycles 1–30)", x = NULL, y = "ASR per 100,000") +
          scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                             expand = expansion(mult = c(0.05, 0.05))) +
          theme_clean_base()
        
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all_avg_1_30 |>
          filter(cause %in% causes, scen %in% scen_keep) |>
          mutate(scen_lab = scen_label(scen)) |>
          compute_bar_labels(value_col = "age_std_rate", facet_cols = c("cause","gender"))
        lev <- df |>
          group_by(scen_lab) |>
          summarise(t = sum(age_std_rate, na.rm = TRUE), .groups = "drop") |>
          arrange(t) |>
          pull(scen_lab)
        df$scen_lab <- factor(df$scen_lab, levels = lev)
        
        pos <- position_dodge2(width = 0.75, padding = 0.05, preserve = "single")
        ggplot(df, aes(x = scen_lab, y = age_std_rate, fill = gender)) +
          geom_col(position = pos, width = 0.75) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1), hjust = label_hjust),
                    position = pos, vjust = 0.5, colour = "black", fontface = "bold", size = 4.0, show.legend = FALSE) +
          scale_gender_fill() +
          coord_flip(clip = "off") +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR by gender (avg cycles 1–30)", x = NULL, y = "ASR per 100,000") +
          scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                             expand = expansion(mult = c(0.05, 0.05))) +
          theme_clean_base()
        
      } else { # region
        df <- asr_region_all_avg_1_30 |>
          filter(cause %in% causes, scen %in% scen_keep)
        if (length(input$region_sel)) df <- dplyr::filter(df, .data[[regionID_name]] %in% input$region_sel)
        df <- df |>
          mutate(scen_lab = scen_label(scen)) |>
          compute_bar_labels(value_col = "age_std_rate", facet_cols = regionID_name)
        
        pos <- position_dodge2(width = 0.8, padding = 0.08, preserve = "single")
        ggplot(df, aes(x = reorder(regionnm, age_std_rate), y = age_std_rate, fill = scen_lab)) +
          geom_col(position = pos, width = 0.8) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1), hjust = label_hjust),
                    position = pos, vjust = 0.5, colour = "black", fontface = "bold", size = 3.6, show.legend = FALSE) +
          coord_flip(clip = "off") +
          labs(title = paste0("ASR by region (avg cycles 1–30) — ", causes[1]),
               x = NULL, y = "ASR per 100,000", fill = "Scenario") +
          scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                             expand = expansion(mult = c(0.05, 0.05))) +
          theme_clean_base()
      }
      
    } else { # trend
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_all, asr_healthy_years_overall) |>
          filter(cause %in% causes, cycle >= MIN_CYCLE, scen %in% scen_keep) |>
          mutate(scen_lab = scen_label(scen))
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen_lab, group = scen_lab)) +
          geom_smooth(se = FALSE) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle (smoothed, cycles 1–30)", x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean_base()
        
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all |>
          filter(cause %in% causes, cycle >= MIN_CYCLE, scen %in% scen_keep) |>
          mutate(scen_lab = scen_label(scen))
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen_lab, linetype = gender)) +
          geom_smooth(se = FALSE) +
          scale_gender_linetype() +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle by gender (smoothed, cycles 1–30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario", linetype = "Gender") +
          theme_clean_base()
        
      } else {
        dat <- asr_region_all_per_cycle |>
          filter(cause %in% causes, cycle >= MIN_CYCLE, scen %in% scen_keep)
        if (length(input$region_sel)) dat <- dplyr::filter(dat, regionnm %in% input$region_sel)
        dat <- mutate(dat, scen_lab = scen_label(scen))
        ggplot(dat, aes(x = cycle, y = age_std_rate, colour = scen_lab)) +
          geom_smooth(se = FALSE) +
          facet_grid(regionnm ~ cause, scales = "free_y") +
          labs(title = "ASR per cycle by region (smoothed, cycles 1–30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean_base()
      }
    }
  })
  output$plot_asr   <- renderPlot({ build_asr_plot() })
  output$plot_asrly <- renderPlotly({ ggplotly(build_asr_plot(), tooltip = c("x","y","colour","fill","linetype")) })
  
  # ---------- IMPACT SUMMARY (NEW) ----------
  # Combine deaths / life years / healthy into one bar chart
  impact_summary <- reactive({
    req(input$impact_min_cycle, input$scen_sel)
    scen_keep <- setdiff(input$scen_sel, "reference")
    validate(need(length(scen_keep) > 0, "Select at least one non-reference scenario."))
    minc <- input$impact_min_cycle; view <- input$view_level; cumu <- isTRUE(input$impact_cumulative)
    
    # Choose base tables by view
    tbls <- switch(view,
                   "Overall" = list(
                     deaths = deaths_overall,
                     lifey  = lifey_overall,
                     healthy= healthy_overall
                   ),
                   "Gender" = list(
                     deaths = deaths_gender,
                     lifey  = lifey_gender,
                     healthy= healthy_gender
                   ),
                   "region" = list(
                     deaths = deaths_region,
                     lifey  = lifey_region,
                     healthy= healthy_region
                   )
    )
    
    # Optional region filter
    if (view == "region" && length(input$region_sel)) {
      tbls <- lapply(tbls, \(d) d |> filter(regionnm %in% input$region_sel))
    }
    
    # Stack three metrics with their labels
    lab_map <- c(
      deaths = "Premature deaths avoided",
      lifey  = "Life years gained",
      healthy= "Life years without major NCDs"
    )
    
    long <- bind_rows(
      tbls$deaths |> mutate(metric = lab_map["deaths"]),
      tbls$lifey  |> mutate(metric = lab_map["lifey"]),
      tbls$healthy|> mutate(metric = lab_map["healthy"])
    ) |>
      filter(cycle >= minc, scen %in% scen_keep)
    
    # Grouping columns by view
    by <- switch(view,
                 "Overall" = c(),
                 "Gender"  = c("gender"),
                 "region"     = c(regionID_name)
    )
    
    long |>
      group_by(scen, across(all_of(by)), metric, cycle) |>
      summarise(value = sum(diff, na.rm = TRUE), .groups = "drop") |>
      group_by(scen, across(all_of(by)), metric) |>
      summarise(
        y = if (cumu) cumsum(value) else value,
        cycle = cycle,
        .groups = "keep"
      ) |>
      slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
      ungroup() |>
      mutate(scen_lab = scen_label(scen))
  })
  
  build_impact_plot <- reactive({
    df <- impact_summary(); req(nrow(df) > 0)
    view <- input$view_level
    
    # Order scenarios by total across metrics
    lev <- df |>
      group_by(scen_lab) |>
      summarise(t = sum(y, na.rm = TRUE), .groups = "drop") |>
      arrange(t) |>
      pull(scen_lab)
    df$scen_lab <- factor(df$scen_lab, levels = lev)
    
    base <- ggplot(df, aes(x = scen_lab, y = y, fill = metric)) +
      geom_col(position = position_dodge2(width = 0.78, padding = 0.05, preserve = "single"), width = 0.78) +
      scale_fill_manual(values = pal_metric) +
      coord_flip() +
      scale_y_continuous(labels = label_number(big.mark = ",")) +
      labs(
        title = "Impact summary by scenario",
        x = NULL,
        y = if (isTRUE(input$impact_cumulative)) "Cumulative effect since start cycle" else "Effect in selected range",
        fill = NULL
      ) +
      theme_clean_base()
    
    if (view == "Gender") {
      base + facet_wrap(~ gender, nrow = 1, drop = TRUE)
    } else if (view == "region") {
      base + facet_wrap(~ regionnm, nrow = 2, scales = "free_y")
    } else base
  })
  
  output$plot_impact   <- renderPlot({ build_impact_plot() })
  output$plot_impactly <- renderPlotly({ ggplotly(build_impact_plot(), tooltip = c("x","y","fill")) })
  
  output$table_impact <- DT::renderDT({
    df <- impact_summary(); req(nrow(df) > 0)
    df |>
      transmute(
        Scenario = scen_lab,
        Group    = dplyr::coalesce(as.character(get0("gender", ifnotfound = NULL)), as.character(get0(regionID_name, ifnotfound = NULL))),
        Metric   = metric,
        Value    = round(y, 2),
        Final_Cycle = cycle
      ) |>
      DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # ---------- CSV download ----------
  current_table <- reactive({
    tab <- input$main_tabs
    if (tab == "Population") {
      pd <- pop_data()
      pd$data |> mutate(across(where(is.numeric), ~ round(., 6)))
    } else if (tab == "Differences vs reference") {
      d <- diff_long()
      if (isTRUE(input$diff_cumulative)) {
        by <- if ("gender" %in% names(d)) "gender" else if (regionID_name %in% names(d)) regionID_name else character(0)
        d |>
          group_by(across(all_of(c("scen", by)))) |>
          slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
          ungroup() |>
          transmute(scen = scen_label(scen), across(all_of(by)),
                    final_cycle = cycle,
                    cumulative_value = y,
                    cumulative_value_scaled = y * SCALING)
      } else {
        d |> mutate(scen = scen_label(scen))
      }
    } else if (tab == "Average ages") {
      out <- output$table_avg |> req(); isolate(out()) |> mutate(scen = scen_label(scen))
    } else if (tab == "ASR") {
      if (input$asr_mode == "avg") {
        if (input$view_level == "Overall") {
          bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
            filter(cause %in% input$asr_causes, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        } else if (input$view_level == "Gender") {
          asr_gender_all_avg_1_30 |>
            filter(cause %in% input$asr_causes, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        } else {
          asr_region_all_avg_1_30 |>
            filter(cause %in% input$asr_causes, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        }
      } else {
        if (input$view_level == "Overall") {
          bind_rows(asr_overall_all, asr_healthy_years_overall) |>
            filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        } else if (input$view_level == "Gender") {
          asr_gender_all |>
            filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        } else {
          asr_region_all_per_cycle |>
            filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        }
      }
    } else if (tab == "Impact summary") {
      impact_summary() |>
        mutate(scen = scen_lab) |>
        select(scen, gender = any_of("gender"), regionnm = any_of(regionID_name),
               metric, value = y, final_cycle = cycle)
    }
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("export_", gsub("\\s+","_", tolower(input$main_tabs)), "_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(current_table(), file, na = "")
  )
  
  # ---------- Downloads (PNG/SVG) ----------
  save_png <- function(p, file, width_in = 11, height_in = 6.5, dpi = 300) {
    ggplot2::ggsave(file, p, width = width_in, height = height_in, dpi = dpi, units = "in", device = "png")
  }
  save_svg <- function(p, file, width_in = 11, height_in = 6.5) {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      stop("To export SVG, please install the 'svglite' package: install.packages('svglite')")
    }
    svglite::svglite(file, width = width_in, height = height_in)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(p)
  }
  
  output$dl_diffbar_png <- downloadHandler(
    filename = function() paste0("differences_summary_", Sys.Date(), ".png"),
    content = function(file) { save_png(build_diffbar_plot(), file) }
  )
  output$dl_diffbar_svg <- downloadHandler(
    filename = function() paste0("differences_summary_", Sys.Date(), ".svg"),
    content = function(file) { save_svg(build_diffbar_plot(), file) }
  )
  output$dl_diff_png <- downloadHandler(
    filename = function() paste0("differences_trends_", Sys.Date(), ".png"),
    content = function(file) { save_png(build_diff_plot(), file) }
  )
  output$dl_diff_svg <- downloadHandler(
    filename = function() paste0("differences_trends_", Sys.Date(), ".svg"),
    content = function(file) { save_svg(build_diff_plot(), file) }
  )
  output$dl_asr_png <- downloadHandler(
    filename = function() paste0("asr_", Sys.Date(), ".png"),
    content = function(file) { save_png(build_asr_plot(), file) }
  )
  output$dl_asr_svg <- downloadHandler(
    filename = function() paste0("asr_", Sys.Date(), ".svg"),
    content = function(file) { save_svg(build_asr_plot(), file) }
  )
  output$dl_pop_png <- downloadHandler(
    filename = function() paste0("population_", Sys.Date(), ".png"),
    content = function(file) { save_png(build_pop_plot(), file) }
  )
  output$dl_pop_svg <- downloadHandler(
    filename = function() paste0("population_", Sys.Date(), ".svg"),
    content = function(file) { save_svg(build_pop_plot(), file) }
  )
  output$dl_impact_png <- downloadHandler(
    filename = function() paste0("impact_summary_", Sys.Date(), ".png"),
    content = function(file) { save_png(build_impact_plot(), file) }
  )
  output$dl_impact_svg <- downloadHandler(
    filename = function() paste0("impact_summary_", Sys.Date(), ".svg"),
    content = function(file) { save_svg(build_impact_plot(), file) }
  )
}

shinyApp(ui, server)
