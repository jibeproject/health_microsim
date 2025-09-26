# ======================================================================
# Local Shiny: Population, Differences, Mean Ages (RAW only), ASR
# with simple on-disk caching in ./data to avoid recomputation
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
# ------------------- Paths (edit these if needed) -------------------
DATA_PATH  <- "data/all_data.parquet"
ZONES_CSV  <- "data/zoneSystem.csv"

# Local cache folder (relative to app)
DATA_DIR <- "data"
if (!dir.exists(DATA_DIR))
  dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

USE_ALL_DATA_CACHE  <- TRUE   # cache the big parquet as an Arrow dataset dir
if (!exists("SCALING")) SCALING <- 5L

# ------------------- Load zones (tiny) ------------------------------
zones    <- readr::read_csv(ZONES_CSV, show_col_types = FALSE)
stopifnot(all(c("ladcd","ladnm") %in% names(zones)))
lads <- zones |> distinct(ladcd, ladnm)

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
  "people_overall","people_gender","people_lad",
  # diffs
  "deaths_overall","deaths_gender","deaths_lad",
  "diseases_overall","diseases_gender","diseases_lad",
  "healthy_overall","healthy_gender","healthy_lad",
  "lifey_overall","lifey_gender","lifey_lad",
  # mean-age sources + RAW mean age tables (renamed columns)
  "incidence_src","inc_death_src",
  "mean_age_dead_by_scen_val",
  "mean_age_dead_by_scen_val_gender",
  "mean_age_dead_by_scen_val_lad",
  "mean_age_onset_by_scen_val",
  "mean_age_onset_by_scen_val_gender",
  "mean_age_onset_by_scen_val_lad",
  # ASR
  "asr_overall_all","asr_overall_avg_1_30",
  "asr_gender_all","asr_gender_all_avg_1_30",
  "asr_lad_all_per_cycle","asr_lad_all_avg_1_30",
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
    left_join(lads, by = "ladcd")
  
  # ---- Population (at risk) ----
  people_raw <- all_data |>
    add_agegroups() |>
    group_by(agegroup_cycle, gender, cycle, scen, ladcd) |>
    summarise(pop = n_distinct(id[!grepl("dead|null", value)]), .groups = "drop") |>
    left_join(lads, by = "ladcd")
  
  people_overall <- people_raw |>
    group_by(agegroup_cycle, scen, cycle) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
  
  people_gender <- people_raw |>
    group_by(agegroup_cycle, scen, cycle, gender) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
  
  people_lad <- people_raw |>
    group_by(agegroup_cycle, scen, cycle, ladnm) |>
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
  deaths_lad_raw     <- inc_death |> group_by(scen, cycle, ladnm) |>
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
  deaths_lad     <- diff_vs_reference(deaths_lad_raw,   by = "ladnm")
  
  diseases_overall <- diff_vs_reference(diseases_all_cycle, by = "cause")
  diseases_gender  <- incidence |>
    filter(!value %in% c("dead","healthy","null")) |>
    rename(cause = value) |>
    group_by(cause, scen, cycle, gender) |>
    summarise(value = dplyr::n(), .groups = "drop") |>
    diff_vs_reference(by = c("gender","cause"))
  diseases_lad     <- incidence |>
    filter(!value %in% c("dead","healthy","null")) |>
    rename(cause = value) |>
    group_by(cause, scen, cycle, ladnm) |>
    summarise(value = dplyr::n(), .groups = "drop") |>
    diff_vs_reference(by = c("ladnm","cause"))
  
  healthy_overall  <- healthy_total_cycle |> diff_vs_reference()
  healthy_gender   <- all_data |> filter(value == "healthy") |>
    group_by(scen, cycle, gender) |> summarise(value = n_distinct(id), .groups = "drop") |>
    diff_vs_reference(by = "gender")
  healthy_lad      <- all_data |> filter(value == "healthy") |>
    left_join(lads, by = "ladcd") |>
    group_by(scen, cycle, ladnm) |> summarise(value = n_distinct(id), .groups = "drop") |>
    diff_vs_reference(by = "ladnm")
  
  lifey_overall <- life_years_cycle |> diff_vs_reference()
  lifey_gender  <- all_data |> filter(!grepl("dead", value)) |>
    group_by(scen, cycle, gender) |> summarise(value = n_distinct(id), .groups = "drop") |>
    diff_vs_reference(by = "gender")
  lifey_lad     <- all_data |> filter(!grepl("dead", value)) |>
    left_join(lads, by = "ladcd") |>
    group_by(scen, cycle, ladnm) |> summarise(value = n_distinct(id), .groups = "drop") |>
    diff_vs_reference(by = "ladnm")
  
  # ---- Mean age (RAW only; rename to mean_age) ----
  inc_death_src <- incidence |>
    filter(value %in% death_values) |>
    select(scen, value, age_cycle, gender, ladnm)
  
  incidence_src <- incidence |>
    filter(!value %in% c("healthy","null") & !value %in% death_values) |>
    select(scen, value, age_cycle, gender, ladnm)
  
  mean_age_dead_by_scen_val        <- inc_death_src |> group_by(scen, value) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  mean_age_dead_by_scen_val_gender <- inc_death_src |> group_by(scen, value, gender) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  mean_age_dead_by_scen_val_lad    <- inc_death_src |> group_by(scen, value, ladnm) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  
  mean_age_onset_by_scen_val        <- incidence_src |> group_by(scen, value) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  mean_age_onset_by_scen_val_gender <- incidence_src |> group_by(scen, value, gender) |>
    summarise(mean_age = mean(age_cycle), .groups="drop")
  mean_age_onset_by_scen_val_lad    <- incidence_src |> group_by(scen, value, ladnm) |>
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
  asr_lad_all_per_cycle <- calc_asr(incidence, all_causes, people_raw, W_OVERALL,
                                    group_vars = "ladcd", avg_cycles = NULL, min_cycle = MIN_CYCLE) |>
    left_join(lads, by = "ladcd")
  asr_lad_all_avg_1_30 <- calc_asr(incidence, all_causes, people_raw, W_OVERALL,
                                   group_vars = "ladcd", avg_cycles = MIN_CYCLE:MAX_CYCLE, min_cycle = MIN_CYCLE) |>
    left_join(lads, by = "ladcd")
  
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


# ------------------- UI -------------------------------------------------
# ------------------- UI -------------------------------------------------
all_scenarios <- sort(unique(people_overall$scen))
pop_cycles    <- sort(unique(people_overall$cycle))
trend_cycles  <- sort(unique(asr_overall_all$cycle))
all_lads_nm   <- sort(unique(people_lad$ladnm))
all_genders   <- sort(unique(people_gender$gender))
all_causes_asr <- sort(unique(c(unique(asr_overall_all$cause), "healthy_years")))

ui <- fluidPage(
  titlePanel("Manchester Transport and Health model results"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxInput("use_plotly", "Interactive (Plotly)", value = TRUE),
      selectInput("scen_sel", "Scenarios:", choices = all_scenarios,
                  selected = all_scenarios, multiple = TRUE),
      selectInput("view_level", "View by:", choices = c("Overall","Gender","LAD"),
                  selected = "Overall"),
      conditionalPanel(
        "input.view_level == 'LAD'",
        selectizeInput("lad_sel", "LAD(s):",
                       choices = all_lads_nm, multiple = TRUE,
                       options = list(placeholder = "Pick LADs (optional)"))
      ),
      tags$hr(),
      
      ## --- Exact-tab selector (dropdown mirrors tabs) ---
      selectInput(
        "metrics_picker", "Metrics:",
        choices = c(
          "Premature deaths avoided",
          "Life years",
          "Years without modelled diseases and injuries",
          "Diseases postponed",
          "Average ages",
          "ASR",
          "Population"
        ),
        selected = "Premature deaths avoided"
      ),
      
      ## Differences controls (visible for any of the 4 Differences tabs)
      conditionalPanel(
        condition =
          "['Premature deaths avoided','Life years','Years without modelled diseases and injuries','Diseases postponed'].indexOf(input.metrics_picker) >= 0 ||" %+%
          "['Premature deaths avoided','Life years','Years without modelled diseases and injuries','Diseases postponed'].indexOf(input.main_tabs) >= 0",
        h2("Differences"),
        sliderInput("diff_min_cycle", "Start cycle:",
                    min = min(trend_cycles), max = max(trend_cycles),
                    value = MIN_CYCLE, step = 1),
        checkboxInput("diff_cumulative", "Cumulative over cycles", value = TRUE)
      ),
      
      ## Average ages controls
      conditionalPanel(
        condition = "input.metrics_picker == 'Average ages' || input.main_tabs == 'Average ages'",
        h2("Average ages"),
        selectInput("avg_kind", "Average age of:",
                    choices = c("Death" = "death", "Disease onset" = "onset")),
        uiOutput("avg_cause_ui")
      ),
      
      ## ASR controls
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
      
      ## Population controls
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
        
        # ---- Differences (4 metric tabs) ----
        tabPanel(
          "Premature deaths avoided",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_deaths_bar_ly", height = 420)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_deaths_bar", height = 420)),
          tags$hr(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_deaths_time_ly", height = 520)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_deaths_time", height = 520)),
          tags$hr(),
          h5("Summary (cumulative at latest cycle; scaled)"),
          DT::dataTableOutput("table_deaths_summary")
        ),
        
        tabPanel(
          "Life years gained",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_life_bar_ly", height = 420)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_life_bar", height = 420)),
          tags$hr(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_life_time_ly", height = 520)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_life_time", height = 520)),
          tags$hr(),
          h5("Summary (cumulative at latest cycle; scaled)"),
          DT::dataTableOutput("table_life_summary")
        ),
        
        tabPanel(
          "Life years gained without modelled diseases and injuries",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_healthy_bar_ly", height = 420)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_healthy_bar", height = 420)),
          tags$hr(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_healthy_time_ly", height = 520)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_healthy_time", height = 520)),
          tags$hr(),
          h5("Summary (cumulative at latest cycle; scaled)"),
          DT::dataTableOutput("table_healthy_summary")
        ),
        
        tabPanel(
          "Cases of diseases prevented",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_diseases_bar_ly", height = 420)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_diseases_bar", height = 420)),
          tags$hr(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_diseases_time_ly", height = 520)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_diseases_time", height = 520)),
          tags$hr(),
          h5("Summary (cumulative at latest cycle; scaled)"),
          DT::dataTableOutput("table_diseases_summary")
        ),
        
        # ---- Average ages ----
        tabPanel(
          "Average ages",
          h5("Mean age (bar charts)"),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_avg_ly", height = 420)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_avg", height = 420)),
          tags$hr(),
          tableOutput("table_avg")
        ),
        
        # ---- ASR ----
        tabPanel(
          "ASR",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_asrly", height = 640)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_asr", height = 640))
        ),
        
        # ---- Population LAST ----
        tabPanel(
          "Population",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_poply", height = 520)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_pop", height = 520))
        )
      )
    )
  )
)

# ------------------- SERVER --------------------------------------------
server <- function(input, output, session) {
  ## Avoid scientific notation globally
  options(scipen = 999)
  
  # Keep dropdown and tabs in sync (one-to-one mapping)
  diff_tabs <- c(
    "Premature deaths avoided",
    "Life years",
    "Years without modelled diseases and injuries",
    "Diseases postponed"
  )
  
  # When a tab is clicked, update the dropdown to that exact tab
  observeEvent(input$main_tabs, {
    if (!identical(input$metrics_picker, input$main_tabs)) {
      updateSelectInput(session, "metrics_picker", selected = input$main_tabs)
    }
  }, ignoreInit = TRUE)
  
  # When the dropdown changes, jump to that exact tab
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
      selectizeInput("avg_death_causes", "Death cause(s):",
                     choices  = c("dead","dead_car","dead_bike","dead_walk"),
                     selected = c("dead","dead_car","dead_bike","dead_walk"),
                     multiple = TRUE)
    }
  })
  
  # ---------- Population ----------
  pop_data <- reactive({
    req(input$pop_cycles, input$scen_sel)
    view <- input$view_level
    if (view == "Overall") {
      dat <- people_overall |> dplyr::filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen")), y = "share", y_lab = "Share of pop.")
      } else list(data = dat, y = "pop", y_lab = "Population count")
    } else if (view == "Gender") {
      dat <- people_gender |> dplyr::filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen","gender")), facet = "gender", y = "share", y_lab = "Share of pop.")
      } else list(data = dat, facet = "gender", y = "pop", y_lab = "Population count")
    } else {
      dat <- people_lad |> dplyr::filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (length(input$lad_sel)) dat <- dat |> dplyr::filter(ladnm %in% input$lad_sel)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen","ladnm")), facet = "ladnm", y = "share", y_lab = "Share of pop.")
      } else list(data = dat, facet = "ladnm", y = "pop", y_lab = "Population count")
    }
  })
  
  build_pop_plot <- reactive({
    pd <- pop_data(); d <- pd$data; req(nrow(d) > 0)
    pos <- if (input$pop_style == "dodge") position_dodge(width = 0.8) else "stack"
    base <- ggplot(d, aes(x = agegroup_cycle, y = .data[[pd$y]], fill = scen)) +
      geom_col(position = pos) +
      scale_y_continuous(labels = if (pd$y == "share") scales::percent else scales::label_comma()) +
      labs(title = "Population by age group", x = "Age group", y = pd$y_lab, fill = "Scenario") +
      theme_clean() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    if (!is.null(pd$facet)) {
      base + facet_grid(as.formula(paste(pd$facet, "~ cycle")), scales = "free_x")
    } else {
      base + facet_wrap(~ cycle, nrow = 1)
    }
  })
  output$plot_pop   <- renderPlot({ build_pop_plot() })
  output$plot_poply <- renderPlotly({ ggplotly(build_pop_plot(), tooltip = c("x","y","fill")) })
  
  # =====================================================================
  # ---------- Δ vs reference: build from all_data on the fly -----------
  # =====================================================================
  
  .incidence_first_onset <- function() {
    inc <- get0("incidence", ifnotfound = NULL, inherits = TRUE)
    if (!is.null(inc)) return(inc)
    incidence_all <- all_data |>
      dplyr::filter(!grepl("dead|healthy|null|depression", value)) |>
      dplyr::group_by(id, scen, value) |>
      dplyr::slice_min(order_by = cycle, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
    incidence_depression <- all_data |>
      dplyr::filter(value == "depression") |>
      dplyr::arrange(id, scen, cycle) |>
      dplyr::group_by(id, scen) |>
      dplyr::mutate(is_new = is.na(dplyr::lag(value)) | dplyr::lag(value) != "depression") |>
      dplyr::filter(is_new) |>
      dplyr::ungroup()
    dplyr::bind_rows(incidence_all, incidence_depression) |>
      add_agegroups() |>
      dplyr::left_join(lads, by = "ladcd")
  }
  
  diff_vs_reference <- get0("diff_vs_reference", inherits = TRUE)
  
  .compute_diff_base <- function(kind, view) {
    if (kind == "deaths") {
      src <- all_data |> dplyr::filter(value %in% c("dead","dead_car","dead_bike","dead_walk"))
      if (view == "Overall") {
        counts <- src |> dplyr::group_by(scen, cycle) |> dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts)
      } else if (view == "Gender") {
        counts <- src |> dplyr::group_by(scen, cycle, gender) |> dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts, by = "gender")
      } else {
        counts <- src |> dplyr::left_join(lads, by = "ladcd") |>
          dplyr::group_by(scen, cycle, ladnm) |> dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts, by = "ladnm")
      }
    } else if (kind == "healthy") {
      src <- all_data |> dplyr::filter(value == "healthy")
      if (view == "Overall") {
        counts <- src |> dplyr::group_by(scen, cycle) |> dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts)
      } else if (view == "Gender") {
        counts <- src |> dplyr::group_by(scen, cycle, gender) |> dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts, by = "gender")
      } else {
        counts <- src |> dplyr::left_join(lads, by = "ladcd") |>
          dplyr::group_by(scen, cycle, ladnm) |> dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts, by = "ladnm")
      }
    } else if (kind == "life") {
      src <- all_data |> dplyr::filter(!grepl("dead", value))
      if (view == "Overall") {
        counts <- src |> dplyr::group_by(scen, cycle) |> dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts)
      } else if (view == "Gender") {
        counts <- src |> dplyr::group_by(scen, cycle, gender) |> dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts, by = "gender")
      } else {
        counts <- src |> dplyr::left_join(lads, by = "ladcd") |>
          dplyr::group_by(scen, cycle, ladnm) |> dplyr::summarise(value = dplyr::n_distinct(id), .groups = "drop")
        diff_vs_reference(counts, by = "ladnm")
      }
    } else {
      inc <- .incidence_first_onset()
      src <- inc |>
        dplyr::filter(!value %in% c("healthy","null","dead","dead_car","dead_bike","dead_walk")) |>
        dplyr::rename(cause = value)
      if (view == "Overall") {
        counts <- src |> dplyr::group_by(cause, scen, cycle) |> dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts, by = "cause")
      } else if (view == "Gender") {
        counts <- src |> dplyr::group_by(cause, scen, cycle, gender) |> dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts, by = c("gender","cause"))
      } else {
        counts <- src |> dplyr::group_by(cause, scen, cycle, ladnm) |> dplyr::summarise(value = dplyr::n(), .groups = "drop")
        diff_vs_reference(counts, by = c("ladnm","cause"))
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
      by_dims <- switch(view, Overall = character(0), Gender = "gender", LAD = "ladnm")
      if (kind == "diseases") by_dims <- c("cause", by_dims)
      
      base |>
        dplyr::filter(cycle >= minc, scen %in% scen_keep) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", by_dims, "cycle")))) |>
        dplyr::summarise(diff = sum(diff, na.rm = TRUE), .groups = "drop") |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", by_dims)))) |>
        dplyr::mutate(
          cumulative = cumsum(diff),
          y = if (cumu) cumulative else diff
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(metric = dplyr::case_when(
          kind == "deaths"   ~ "Premature deaths avoided",
          kind == "life"     ~ "Life years gained",
          kind == "healthy"  ~ "Life years gained without modelled diseases and injuries",
          TRUE               ~ "Cases of disease prevented"
        ))
    })
  }
  
  diff_deaths   <- make_diff_reactive("deaths")
  diff_life     <- make_diff_reactive("life")
  diff_healthy  <- make_diff_reactive("healthy")
  diff_diseases <- make_diff_reactive("diseases")
  
  build_bar_plot <- function(d) {
    req(nrow(d) > 0)
    if ("gender" %in% names(d)) d <- d |> 
        dplyr::mutate(gender = as.factor(case_when(gender == 1 ~ "Male", 
                                                   gender == 2 ~ "Female")))
    other <- intersect(c("cause","gender","ladnm"), names(d))
    bars <- d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", other)))) |>
      dplyr::slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::mutate(cumulative_value_scaled = y * SCALING)
    
    p <- ggplot(bars, aes(x = scen, y = cumulative_value_scaled, fill = scen)) +
      geom_col(width = 0.8) + coord_flip() +
      labs(title = paste0(unique(d$metric)), x = NULL, y = "Cumulative Δ over simulation period") +
      theme_clean() + guides(fill = "none")
    
    if ("gender" %in% other && !"cause" %in% other && !"ladnm" %in% other) {
      p <- ggplot(bars, aes(x = scen, y = cumulative_value_scaled, fill = gender)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.75) + coord_flip() +
        labs(title = paste0(unique(d$metric)), x = NULL, y = "Cumulative Δ (scaled)", fill = "Gender") +
        theme_clean()
    } else if ("cause" %in% other && !"ladnm" %in% other && !"gender" %in% other) {
      p <- p + facet_wrap(~ cause, scales = "free_x")
    } else if ("cause" %in% other && "gender" %in% other && !"ladnm" %in% other) {
      p <- ggplot(bars, aes(x = scen, y = cumulative_value_scaled, fill = gender)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.75) + coord_flip() +
        facet_wrap(~ cause, scales = "free_x") +
        labs(title = paste0(unique(d$metric)), x = NULL, y = "Cumulative Δ (scaled)", fill = "Gender") +
        theme_clean()
    } else if ("ladnm" %in% other && !"cause" %in% other) {
      p <- p + facet_wrap(~ ladnm, scales = "free_x")
    } else if ("ladnm" %in% other && "cause" %in% other) {
      p <- p + facet_grid(ladnm ~ cause, scales = "free_x")
    }
    p
  }
  
  build_time_plot <- function(d) {
    req(nrow(d) > 0)
    if ("gender" %in% names(d)) d <- d |> dplyr::mutate(gender = as.factor(gender))
    ttl <- paste0(unique(d$metric), " — per cycle")
    if ("gender" %in% names(d) && !"cause" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = diff, colour = scen, linetype = gender)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = "Δ vs reference", colour = "Scenario", linetype = "Gender") +
        theme_clean()
    } else if ("ladnm" %in% names(d) && !"cause" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = diff, colour = scen)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        facet_wrap(~ ladnm, nrow = 2, scales = "free_y") +
        labs(title = ttl, x = "Cycle (year)", y = "Δ vs reference", colour = "Scenario") +
        theme_clean()
    } else if ("cause" %in% names(d) && !"ladnm" %in% names(d) && !"gender" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = diff, colour = scen)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        facet_wrap(~ cause, scales = "free_y") +
        labs(title = ttl, x = "Cycle (year)", y = "Δ vs reference", colour = "Scenario") +
        theme_clean()
    } else if ("cause" %in% names(d) && "gender" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = diff, colour = scen, linetype = gender)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        facet_wrap(~ cause, scales = "free_y") +
        labs(title = ttl, x = "Cycle (year)", y = "Δ vs reference", colour = "Scenario", linetype = "Gender") +
        theme_clean()
    } else if ("cause" %in% names(d) && "ladnm" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = diff, colour = scen)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        facet_grid(ladnm ~ cause, scales = "free_y") +
        labs(title = ttl, x = "Cycle (year)", y = "Δ vs reference", colour = "Scenario") +
        theme_clean()
    } else {
      ggplot(d, aes(x = cycle, y = diff, colour = scen)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = "Δ vs reference", colour = "Scenario") +
        theme_clean()
    }
  }
  
  # Deaths
  output$plot_deaths_bar     <- renderPlot({ build_bar_plot(diff_deaths()) })
  output$plot_deaths_bar_ly  <- renderPlotly({ ggplotly(build_bar_plot(diff_deaths()),  tooltip = c("x","y","fill")) })
  output$plot_deaths_time    <- renderPlot({ build_time_plot(diff_deaths()) })
  output$plot_deaths_time_ly <- renderPlotly({ ggplotly(build_time_plot(diff_deaths()), tooltip = c("x","y","colour","linetype")) })
  output$table_deaths_summary <- DT::renderDT({
    d <- diff_deaths(); req(nrow(d) > 0)
    other <- intersect(c("cause","gender","ladnm"), names(d))
    d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", other)))) |>
      dplyr::slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        metric = unique(d$metric)[1], scen,
        !!!(if (length(other)) rlang::syms(other) else NULL),
        final_cycle = cycle,
        cumulative_value = y,
        cumulative_value_scaled = y * SCALING
      ) |>
      dplyr::arrange(scen, dplyr::across(dplyr::all_of(other))) |>
      DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Life
  output$plot_life_bar     <- renderPlot({ build_bar_plot(diff_life()) })
  output$plot_life_bar_ly  <- renderPlotly({ ggplotly(build_bar_plot(diff_life()),  tooltip = c("x","y","fill")) })
  output$plot_life_time    <- renderPlot({ build_time_plot(diff_life()) })
  output$plot_life_time_ly <- renderPlotly({ ggplotly(build_time_plot(diff_life()), tooltip = c("x","y","colour","linetype")) })
  output$table_life_summary <- DT::renderDT({
    d <- diff_life(); req(nrow(d) > 0)
    other <- intersect(c("cause","gender","ladnm"), names(d))
    d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", other)))) |>
      dplyr::slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        metric = unique(d$metric)[1], scen,
        !!!(if (length(other)) rlang::syms(other) else NULL),
        final_cycle = cycle,
        cumulative_value = y,
        cumulative_value_scaled = y * SCALING
      ) |>
      dplyr::arrange(scen, dplyr::across(dplyr::all_of(other))) |>
      DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Years without modelled diseases and injuries
  output$plot_healthy_bar     <- renderPlot({ build_bar_plot(diff_healthy()) })
  output$plot_healthy_bar_ly  <- renderPlotly({ ggplotly(build_bar_plot(diff_healthy()),  tooltip = c("x","y","fill")) })
  output$plot_healthy_time    <- renderPlot({ build_time_plot(diff_healthy()) })
  output$plot_healthy_time_ly <- renderPlotly({ ggplotly(build_time_plot(diff_healthy()), tooltip = c("x","y","colour","linetype")) })
  output$table_healthy_summary <- DT::renderDT({
    d <- diff_healthy(); req(nrow(d) > 0)
    other <- intersect(c("cause","gender","ladnm"), names(d))
    d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", other)))) |>
      dplyr::slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        metric = unique(d$metric)[1], scen,
        !!!(if (length(other)) rlang::syms(other) else NULL),
        final_cycle = cycle,
        cumulative_value = y,
        cumulative_value_scaled = y * SCALING
      ) |>
      dplyr::arrange(scen, dplyr::across(dplyr::all_of(other))) |>
      DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # Diseases
  output$plot_diseases_bar     <- renderPlot({ build_bar_plot(diff_diseases()) })
  output$plot_diseases_bar_ly  <- renderPlotly({ ggplotly(build_bar_plot(diff_diseases()),  tooltip = c("x","y","fill")) })
  output$plot_diseases_time    <- renderPlot({ build_time_plot(diff_diseases()) })
  output$plot_diseases_time_ly <- renderPlotly({ ggplotly(build_time_plot(diff_diseases()), tooltip = c("x","y","colour","linetype")) })
  output$table_diseases_summary <- DT::renderDT({
    d <- diff_diseases(); req(nrow(d) > 0)
    other <- intersect(c("cause","gender","ladnm"), names(d))
    d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", other)))) |>
      dplyr::slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        metric = unique(d$metric)[1], scen,
        !!!(if (length(other)) rlang::syms(other) else NULL),
        final_cycle = cycle,
        cumulative_value = y,
        cumulative_value_scaled = y * SCALING
      ) |>
      dplyr::arrange(scen, dplyr::across(dplyr::all_of(other))) |>
      DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # =====================================================================
  # ---------- Average ages: RAW ONLY (mean_age_*) ----------------------
  # =====================================================================
  build_avg_plot <- reactive({
    req(input$avg_kind, input$view_level)
    view <- input$view_level
    if (input$avg_kind == "death") {
      causes <- input$avg_death_causes; req(causes)
      if (view == "Overall") {
        df <- mean_age_dead_by_scen_val |>
          dplyr::filter(value %in% causes, scen %in% input$scen_sel)
        req(nrow(df) > 0)
        ggplot(df, aes(x = scen, y = mean_age, fill = scen)) +
          geom_col(width = 0.8) + coord_flip() +
          facet_wrap(~ value, scales = "free_x") +
          labs(title = "Mean age at death", x = NULL, y = "Years") +
          theme_clean() + guides(fill = "none")
      } else if (view == "Gender") {
        df <- mean_age_dead_by_scen_val_gender |>
          dplyr::filter(value %in% causes, scen %in% input$scen_sel) |>
          dplyr::mutate(gender = as.factor(gender))
        req(nrow(df) > 0)
        ggplot(df, aes(x = scen, y = mean_age, fill = gender)) +
          geom_col(position = position_dodge(width = 0.75), width = 0.75) +
          coord_flip() +
          facet_wrap(~ value, scales = "free_x") +
          labs(title = "Mean age at death (by gender)", x = NULL, y = "Years", fill = "Gender") +
          theme_clean()
      } else {
        df <- mean_age_dead_by_scen_val_lad |>
          dplyr::filter(value %in% causes, scen %in% input$scen_sel)
        if (length(input$lad_sel)) df <- df |> dplyr::filter(ladnm %in% input$lad_sel)
        req(nrow(df) > 0)
        ggplot(df, aes(x = scen, y = mean_age, fill = scen)) +
          geom_col(width = 0.8) + coord_flip() +
          facet_grid(ladnm ~ value, scales = "free_x") +
          labs(title = "Mean age at death (by LAD)", x = NULL, y = "Years") +
          theme_clean() + guides(fill = "none")
      }
    } else { # onset
      cause <- input$avg_cause; req(cause)
      if (view == "Overall") {
        df <- mean_age_onset_by_scen_val |>
          dplyr::filter(value == cause, scen %in% input$scen_sel)
        req(nrow(df) > 0)
        ggplot(df, aes(x = scen, y = mean_age, fill = scen)) +
          geom_col(width = 0.8) + coord_flip() +
          labs(title = paste0("Mean age at onset — ", cause), x = NULL, y = "Years") +
          theme_clean() + guides(fill = "none")
      } else if (view == "Gender") {
        df <- mean_age_onset_by_scen_val_gender |>
          dplyr::filter(value == cause, scen %in% input$scen_sel) |>
          dplyr::mutate(gender = as.factor(gender))
        req(nrow(df) > 0)
        ggplot(df, aes(x = scen, y = mean_age, fill = gender)) +
          geom_col(position = position_dodge(width = 0.75), width = 0.75) +
          coord_flip() +
          labs(title = paste0("Mean age at onset (by gender) — ", cause),
               x = NULL, y = "Years", fill = "Gender") +
          theme_clean()
      } else {
        df <- mean_age_onset_by_scen_val_lad |>
          dplyr::filter(value == cause, scen %in% input$scen_sel)
        if (length(input$lad_sel)) df <- df |> dplyr::filter(ladnm %in% input$lad_sel)
        req(nrow(df) > 0)
        ggplot(df, aes(x = scen, y = mean_age, fill = scen)) +
          geom_col(width = 0.8) + coord_flip() +
          facet_wrap(~ ladnm, scales = "free_x") +
          labs(title = paste0("Mean age at onset (by LAD) — ", cause), x = NULL, y = "Years") +
          theme_clean() + guides(fill = "none")
      }
    }
  })
  output$plot_avg     <- renderPlot({ build_avg_plot() })
  output$plot_avg_ly  <- renderPlotly({ ggplotly(build_avg_plot(), tooltip = c("x","y","fill")) })
  output$table_avg    <- renderTable({
    view <- input$view_level
    if (input$avg_kind == "death") {
      causes <- input$avg_death_causes; req(causes)
      if (view == "Overall") {
        mean_age_dead_by_scen_val |>
          dplyr::filter(value %in% causes, scen %in% input$scen_sel) |>
          dplyr::arrange(scen, value)
      } else if (view == "Gender") {
        mean_age_dead_by_scen_val_gender |>
          dplyr::filter(value %in% causes, scen %in% input$scen_sel) |>
          dplyr::arrange(scen, gender, value)
      } else {
        df <- mean_age_dead_by_scen_val_lad |>
          dplyr::filter(value %in% causes, scen %in% input$scen_sel)
        if (length(input$lad_sel)) df <- df |> dplyr::filter(ladnm %in% input$lad_sel)
        df |> dplyr::arrange(scen, ladnm, value)
      }
    } else {
      cause <- input$avg_cause; req(cause)
      if (view == "Overall") {
        mean_age_onset_by_scen_val |>
          dplyr::filter(value == cause, scen %in% input$scen_sel) |>
          dplyr::arrange(scen)
      } else if (view == "Gender") {
        mean_age_onset_by_scen_val_gender |>
          dplyr::filter(value == cause, scen %in% input$scen_sel) |>
          dplyr::arrange(scen, gender)
      } else {
        df <- mean_age_onset_by_scen_val_lad |>
          dplyr::filter(value == cause, scen %in% input$scen_sel)
        if (length(input$lad_sel)) df <- df |> dplyr::filter(ladnm %in% input$lad_sel)
        df |> dplyr::arrange(scen, ladnm)
      }
    }
  })
  
  # ---------- ASR ----------
  to_chr_cause <- function(df) if ("cause" %in% names(df)) dplyr::mutate(df, cause = as.character(cause)) else df
  asr_overall_all                 <- to_chr_cause(asr_overall_all)
  asr_overall_avg_1_30            <- to_chr_cause(asr_overall_avg_1_30)
  asr_gender_all                  <- to_chr_cause(asr_gender_all)
  asr_gender_all_avg_1_30         <- to_chr_cause(asr_gender_all_avg_1_30)
  asr_lad_all_per_cycle           <- to_chr_cause(asr_lad_all_per_cycle)
  asr_lad_all_avg_1_30            <- to_chr_cause(asr_lad_all_avg_1_30)
  asr_healthy_years_overall       <- to_chr_cause(asr_healthy_years_overall)
  asr_healthy_years_overall_avg_1_30 <- to_chr_cause(asr_healthy_years_overall_avg_1_30)
  
  build_asr_plot <- reactive({
    req(input$asr_mode, input$asr_causes)
    causes <- input$asr_causes
    if (input$asr_mode == "avg") {
      if (input$view_level == "Overall") {
        df <- dplyr::bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
          dplyr::filter(cause %in% causes)
        req(nrow(df) > 0)
        ggplot(df, aes(x = scen, y = age_std_rate, fill = scen)) +
          geom_col(width = 0.8) +
          geom_text(aes(label = scales::number(age_std_rate, accuracy = 0.1)), hjust = -0.12, size = 3) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
          coord_flip(clip = "off") +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR (avg cycles 1–30)", x = NULL, y = "ASR per 100,000") +
          theme_clean() + guides(fill = "none") +
          theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all_avg_1_30 |>
          dplyr::filter(cause %in% causes) |>
          dplyr::mutate(gender = as.factor(gender))
        req(nrow(df) > 0)
        pos <- position_dodge2(width = 0.75, padding = 0.05, preserve = "single")
        ggplot(df, aes(x = scen, y = age_std_rate, fill = gender)) +
          geom_col(position = pos, width = 0.75) +
          geom_text(aes(label = scales::number(age_std_rate, accuracy = 0.1)), position = pos, hjust = -0.12, size = 3) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
          coord_flip(clip = "off") +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR by gender (avg cycles 1–30)", x = NULL, y = "ASR per 100,000", fill = "Gender") +
          theme_clean() + theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
      } else {
        df <- asr_lad_all_avg_1_30 |> dplyr::filter(cause %in% causes)
        if (length(input$lad_sel)) df <- df |> dplyr::filter(ladnm %in% input$lad_sel)
        req(nrow(df) > 0)
        pos <- position_dodge2(width = 0.8, padding = 0.08, preserve = "single")
        ggplot(df, aes(x = reorder(ladnm, age_std_rate), y = age_std_rate, fill = scen)) +
          geom_col(position = pos, width = 0.8) +
          geom_text(aes(label = scales::number(age_std_rate, accuracy = 0.1)), position = pos, hjust = -0.10, size = 2.6) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
          coord_flip(clip = "off") +
          labs(title = "ASR by LAD (avg cycles 1–30)", x = NULL, y = "ASR per 100,000", fill = "Scenario") +
          theme_clean() + theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
      }
    } else {
      if (input$view_level == "Overall") {
        df <- dplyr::bind_rows(asr_overall_all, asr_healthy_years_overall) |>
          dplyr::filter(cause %in% causes, cycle >= MIN_CYCLE)
        req(nrow(df) > 0)
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen, group = scen)) +
          geom_smooth(se = FALSE) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle (smoothed, cycles 1–30)", x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean()
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all |>
          dplyr::filter(cause %in% causes, cycle >= MIN_CYCLE) |>
          dplyr::mutate(gender = as.factor(gender))
        req(nrow(df) > 0)
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen, linetype = gender)) +
          geom_smooth(se = FALSE) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle by gender (smoothed, cycles 1-30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario", linetype = "Gender") +
          theme_clean()
      } else {
        dat <- asr_lad_all_per_cycle |> dplyr::filter(cause %in% causes, cycle >= MIN_CYCLE)
        req(nrow(dat) > 0)
        if (length(input$lad_sel)) dat <- dat |> dplyr::filter(ladnm %in% input$lad_sel)
        req(nrow(dat) > 0)
        ggplot(dat, aes(x = cycle, y = age_std_rate, colour = scen)) +
          geom_smooth(se = FALSE) +
          facet_grid(ladnm ~ cause, scales = "free_y") +
          labs(title = "ASR per cycle by LAD (smoothed, cycles 1-30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean()
      }
    }
  })
  output$plot_asr   <- renderPlot({ build_asr_plot() })
  output$plot_asrly <- renderPlotly({ ggplotly(build_asr_plot(), tooltip = c("x","y","colour","fill","linetype")) })
  
  # ---------- CSV download ----------
  current_table <- reactive({
    tab <- input$main_tabs
    if (tab == "Population") {
      pd <- pop_data(); pd$data |> dplyr::mutate(across(where(is.numeric), ~ round(., 6)))
    } else if (tab %in% c("Premature deaths avoided","Life years","Years without modelled diseases and injuries","Diseases postponed")) {
      d <- switch(tab,
                  "Premature deaths avoided"                      = diff_deaths(),
                  "Life years"                                     = diff_life(),
                  "Years without modelled diseases and injuries"   = diff_healthy(),
                  "Diseases postponed"                              = diff_diseases())
      req(nrow(d) > 0)
      other <- intersect(c("cause","gender","ladnm"), names(d))
      d |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c("scen", other)))) |>
        dplyr::slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::transmute(
          scen,
          !!!(if (length(other)) rlang::syms(other) else NULL),
          final_cycle = cycle,
          cumulative_value = y,
          cumulative_value_scaled = y * SCALING
        )
    } else if (tab == "Average ages") {
      output$table_avg |> req(); isolate({ output$table_avg() })
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
}

# Helper for JS string concatenation inside R if needed
`%+%` <- function(a,b) paste0(a,b)



shinyApp(ui, server)
