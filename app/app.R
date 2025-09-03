# ======================================================================
# Local Shiny: Population, Differences, Mean Ages (death/onset), ASR
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
})

# ------------------- Paths (edit these if needed) -------------------
DATA_PATH  <- "../temp/all_data_200825.parquet" #all_data_230825.parquet"
ZONES_CSV  <- "/media/ali/Expansion/backup_tabea/manchester-main/input/zoneSystem.csv"

# Local cache folder (relative to app)
DATA_DIR <- "data"
if (!dir.exists(DATA_DIR)) 
  dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)

# Toggle caches:
# - Set once in R: Sys.setenv(REBUILD_CACHE="1")   # forces rebuild on next run
# - Or just delete files in ./data
REBUILD_CACHE       <- identical(Sys.getenv("REBUILD_CACHE"), "1") # 0 if no change in data
USE_ALL_DATA_CACHE  <- TRUE   # cache the big parquet as RDS for faster reloads

# Scaling for 20% samples (used in summary tables only)
if (!exists("SCALING")) SCALING <- 5L

# ------------------- Load zones (tiny; no need to cache) --------------
zones    <- readr::read_csv(ZONES_CSV, show_col_types = FALSE)
stopifnot(all(c("ladcd","ladnm") %in% names(zones)))
lads <- zones |> distinct(ladcd, ladnm)

# ------------------- Load all_data with optional cache -----------------
all_data_cache_path <- file.path(DATA_DIR, "all_data.parquet")

if (USE_ALL_DATA_CACHE && file.exists(all_data_cache_path) && !REBUILD_CACHE) {
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
MAX_CYCLE <- max(all_data$cycle)
  

# ------------------- Helpers -------------------------------------------
ageband <- function(x) cut(
  x, breaks = c(seq(0,100,5), Inf),
  labels = c(paste(seq(0,95,5), seq(4,99,5), sep = "-"), "100+"),
  right = FALSE, include.lowest = TRUE
)

add_agegroups <- function(df) {
  df |>
    mutate(agegroup_cycle = ageband(age_cycle))
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

# ------------------- Precompute (with cache) ---------------------------
precomp_path <- file.path(DATA_DIR, "precomputed_v1.qs")
death_values <- c("dead","dead_car","dead_bike","dead_walk")

if (file.exists(precomp_path) && !REBUILD_CACHE) {
  message("Loading precomputed tables from cache: ", precomp_path)
  pc <- qs::qread(precomp_path)
  list2env(pc, envir = environment())
} else {
  message("Building precomputed tables …")
  
  # ---- Core event sets ----
  incidence_all <- all_data |>
    filter(!grepl("dead|healthy|null|depression", value)) |>
    group_by(id, scen, value) |>
    to_duckdb() |> 
    filter(cycle == min(cycle)) |>
    ungroup()# |> 
    #collect()
  
  incidence_depression <- all_data |>
    filter(value == "depression") |>
    arrange(id, scen, cycle) |>
    group_by(id, scen) |>
    to_duckdb() |> 
    mutate(is_new = is.na(lag(value)) | lag(value) != "depression") |>
    filter(is_new) |>
    ungroup() #|> 
    #collect()
  
  incidence_deaths <- all_data |> filter(value %in% death_values) #|> collect()
  
  incidence <- bind_rows(incidence_all |> 
                           collect(), 
                         incidence_depression |> collect(), 
                         incidence_deaths |> collect()) |>
    add_agegroups() |>
    left_join(lads, by = "ladcd")
  
  # ---- Population (at risk) ----
  people_raw <- all_data |>
    #to_duckdb() |> 
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
    summarise(value = n(), .groups = "drop")
  deaths_gender_raw  <- inc_death |> group_by(scen, cycle, gender) |>
    summarise(value = n(), .groups = "drop")
  deaths_lad_raw     <- inc_death |> group_by(scen, cycle, ladnm) |>
    summarise(value = n(), .groups = "drop")
  
  # ---- Disease counts (all causes combined) ----
  diseases_all_cycle <- incidence |>
    filter(!value %in% c("dead","healthy","null")) |>
    group_by(scen, cycle) |>
    summarise(value = n(), .groups = "drop")
  
  # ---- Differences vs reference ----
  deaths_overall <- diff_vs_reference(deaths_overall_raw)
  deaths_gender  <- diff_vs_reference(deaths_gender_raw, by = "gender")
  deaths_lad     <- diff_vs_reference(deaths_lad_raw,   by = "ladnm")
  
  diseases_overall <- diff_vs_reference(diseases_all_cycle)
  diseases_gender  <- incidence |>
    filter(!value %in% c("dead","healthy","null")) |>
    group_by(scen, cycle, gender) |> summarise(value = n(), .groups = "drop") |>
    diff_vs_reference(by = "gender")
  diseases_lad     <- incidence |>
    filter(!value %in% c("dead","healthy","null")) |>
    group_by(scen, cycle, ladnm) |> summarise(value = n(), .groups = "drop") |>
    diff_vs_reference(by = "ladnm")
  
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
  
  # ---- Mean age (death & onset) ----
  inc_death_src <- incidence |>
    filter(value %in% death_values) |>
    select(scen, value, age_cycle, gender, ladnm)
  
  incidence_src <- incidence |>
    filter(!value %in% c("healthy","null") & !value %in% death_values) |>
    select(scen, value, age_cycle, gender, ladnm)
  
  weighted_mean_by <- function(df, group_keys) {
    w <- df |> count(across(all_of(c(group_keys, "age_cycle"))), name = "w")
    w |> group_by(across(all_of(group_keys))) |>
      summarise(mean_age_weighted = weighted.mean(age_cycle, w), .groups = "drop")
  }
  
  mean_age_dead_raw_by_scen_val           <- inc_death_src |> group_by(scen, value) |> summarise(mean_age_raw = mean(age_cycle), .groups="drop")
  mean_age_dead_weight_by_scen_val        <- weighted_mean_by(inc_death_src, c("scen","value"))
  mean_age_dead_raw_by_scen_val_gender    <- inc_death_src |> group_by(scen, value, gender) |> summarise(mean_age_raw = mean(age_cycle), .groups="drop")
  mean_age_dead_weight_by_scen_val_gender <- weighted_mean_by(inc_death_src, c("scen","value","gender"))
  mean_age_dead_raw_by_scen_val_lad       <- inc_death_src |> group_by(scen, value, ladnm) |> summarise(mean_age_raw = mean(age_cycle), .groups="drop")
  
  mean_age_onset_raw_by_scen_val           <- incidence_src |> group_by(scen, value) |> summarise(mean_age_raw = mean(age_cycle), .groups="drop")
  mean_age_onset_weight_by_scen_val        <- weighted_mean_by(incidence_src, c("scen","value"))
  mean_age_onset_raw_by_scen_val_gender    <- incidence_src |> group_by(scen, value, gender) |> summarise(mean_age_raw = mean(age_cycle), .groups="drop")
  mean_age_onset_weight_by_scen_val_gender <- weighted_mean_by(incidence_src, c("scen","value","gender"))
  mean_age_onset_raw_by_scen_val_lad       <- incidence_src |> group_by(scen, value, ladnm) |> summarise(mean_age_raw = mean(age_cycle), .groups="drop")
  
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
        summarise(cases = n(), .groups = "drop")
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
  healthy_age_counts_gender <- all_data |>
    add_agegroups() |>
    filter(value == "healthy", cycle >= MIN_CYCLE) |>
    group_by(agegroup_cycle, scen, cycle, gender) |>
    summarise(num = n_distinct(id), .groups = "drop")
  healthy_age_counts_lad <- all_data |>
    add_agegroups() |>
    filter(value == "healthy", cycle >= MIN_CYCLE) |>
    group_by(agegroup_cycle, scen, cycle, ladcd) |>
    summarise(num = n_distinct(id), .groups = "drop") |>
    left_join(lads, by = "ladcd")
  
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
  pc <- mget(c(
    # population
    "people_overall","people_gender","people_lad",
    # diffs
    "deaths_overall","deaths_gender","deaths_lad",
    "diseases_overall","diseases_gender","diseases_lad",
    "healthy_overall","healthy_gender","healthy_lad",
    "lifey_overall","lifey_gender","lifey_lad",
    # mean-age sources
    "incidence_src","inc_death_src",
    # mean-age tables
    "mean_age_dead_raw_by_scen_val",
    "mean_age_dead_weight_by_scen_val",
    "mean_age_dead_raw_by_scen_val_gender",
    "mean_age_dead_weight_by_scen_val_gender",
    "mean_age_dead_raw_by_scen_val_lad",
    "mean_age_onset_raw_by_scen_val",
    "mean_age_onset_weight_by_scen_val",
    "mean_age_onset_raw_by_scen_val_gender",
    "mean_age_onset_weight_by_scen_val_gender",
    "mean_age_onset_raw_by_scen_val_lad",
    # ASR
    "asr_overall_all","asr_overall_avg_1_30",
    "asr_gender_all","asr_gender_all_avg_1_30",
    "asr_lad_all_per_cycle","asr_lad_all_avg_1_30",
    "asr_healthy_years_overall","asr_healthy_years_overall_avg_1_30"
  ))
  message("Saving precomputed cache: ", precomp_path)
  #saveRDS(pc, precomp_path, compress = "xz")
  qs::qsave(pc, precomp_path)
  # Expose to the current environment
  list2env(pc, envir = environment())
}

# ------------------- UI -------------------------------------------------
all_scenarios <- sort(unique(people_overall$scen))
pop_cycles    <- sort(unique(people_overall$cycle))
trend_cycles  <- sort(unique(asr_overall_all$cycle))
all_lads_nm   <- sort(unique(people_lad$ladnm))
all_genders   <- sort(unique(people_gender$gender))
all_causes_asr <- sort(unique(c(
  unique(asr_overall_all$cause),
  "healthy_years"
)))

ui <- fluidPage(
  titlePanel("Local Health Explorer — Population, Differences, Mean Ages & ASR"),
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
      tabsetPanel(
        id = "control_tabs", type = "pills",
        tabPanel("Population",
                 selectizeInput("pop_cycles", "Cycles to show (bars):",
                                choices = pop_cycles, selected = c(2,10,MAX_CYCLE), multiple = TRUE),
                 radioButtons("pop_style", "Bar style:", c("Stacked"="stack","Side-by-side"="dodge"),
                              inline = TRUE),
                 checkboxInput("pop_share", "Show shares (else counts)", value = TRUE)
        ),
        tabPanel("Differences",
                 selectInput("metric_kind", "Metric:", choices = c(
                   "Deaths postponed (Δ deaths)"     = "deaths",
                   "Diseases postponed (Δ diseases)" = "diseases",
                   "Δ Healthy years"                 = "healthy",
                   "Δ Life years"                    = "life"
                 )),
                 sliderInput("diff_min_cycle", "Start cycle:",
                             min = min(trend_cycles), max = max(trend_cycles),
                             value = MIN_CYCLE, step = 1),
                 checkboxInput("diff_cumulative", "Cumulative over cycles", value = TRUE)
        ),
        tabPanel("Average ages",
                 selectInput("avg_kind", "Average age of:", choices = c("Death"="death","Disease onset"="onset")),
                 uiOutput("avg_cause_ui")
        ),
        tabPanel("ASR",
                 selectInput("asr_mode", "ASR view:",
                             choices = c("Average 1-30 (bars)"="avg","Over time (smoothed)"="trend")),
                 selectizeInput("asr_causes", "Causes:", choices = all_causes_asr,
                                selected = c("coronary_heart_disease","stroke","healthy_years"),
                                multiple = TRUE),
                 conditionalPanel(
                   "input.view_level == 'LAD' && input.asr_mode == 'avg'",
                   numericInput("lad_topn", "Top N LADs:", min = 5, max = 50, value = 25, step = 1)
                 )
        )
      ),
      tags$hr(),
      downloadButton("download_csv", "Download current table (CSV)")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Population",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_poply", height = 520)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_pop", height = 520))
        ),
        tabPanel(
          "Differences vs reference",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_diffly", height = 520)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_diff", height = 520)),
          tags$hr(),
          h5("Summary (cumulative at latest cycle, or sum if non-cumulative)"),
          tableOutput("table_diff_summary")
        ),
        tabPanel("Average ages", tableOutput("table_avg")),
        tabPanel(
          "ASR",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_asrly", height = 640)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_asr", height = 640))
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
      dat <- people_overall |> filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen")), y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, y = "pop", y_lab = "Population count")
      }
    } else if (view == "Gender") {
      dat <- people_gender |> filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen","gender")), facet = "gender", y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, facet = "gender", y = "pop", y_lab = "Population count")
      }
    } else {
      dat <- people_lad |> filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (length(input$lad_sel)) dat <- dat |> filter(ladnm %in% input$lad_sel)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen","ladnm")), facet = "ladnm", y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, facet = "ladnm", y = "pop", y_lab = "Population count")
      }
    }
  })
  
  build_pop_plot <- reactive({
    pd <- pop_data(); d <- pd$data; req(nrow(d) > 0)
    pos <- if (input$pop_style == "dodge") position_dodge(width = 0.8) else "stack"
    base <- ggplot(d, aes(x = agegroup_cycle, y = .data[[pd$y]], fill = scen)) +
      geom_col(position = pos) +
      scale_y_continuous(labels = if (pd$y == "share") percent else label_comma()) +
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
  
  # ---------- Differences vs reference ----------
  diff_long <- reactive({
    req(input$metric_kind, input$view_level, input$diff_min_cycle)
    scen_keep <- setdiff(input$scen_sel, "reference")
    validate(need(length(scen_keep) > 0, "Select at least one non-reference scenario."))
    minc <- input$diff_min_cycle; view <- input$view_level; cumu <- isTRUE(input$diff_cumulative)
    pick <- switch(input$metric_kind,
                   deaths   = list(Overall=deaths_overall,  Gender=deaths_gender,  LAD=deaths_lad,  label="Δ Deaths"),
                   diseases = list(Overall=diseases_overall,Gender=diseases_gender,LAD=diseases_lad,label="Δ Diseases"),
                   healthy  = list(Overall=healthy_overall, Gender=healthy_gender, LAD=healthy_lad,label="Δ Healthy years"),
                   life     = list(Overall=lifey_overall,   Gender=lifey_gender,   LAD=lifey_lad,  label="Δ Life years"))
    base <- pick[[view]]; by <- switch(view, Overall=character(0), Gender="gender", LAD="ladnm")
    df <- base |> filter(cycle >= minc, scen %in% scen_keep); grp <- c("scen", by)
    df |> group_by(across(all_of(c(grp, "cycle")))) |>
      summarise(diff = sum(diff, na.rm = TRUE), .groups = "drop") |>
      group_by(across(all_of(grp))) |>
      mutate(y = if (cumu) cumsum(diff) else diff) |>
      ungroup() |>
      mutate(metric = pick$label)
  })
  
  build_diff_plot <- reactive({
    d <- diff_long(); req(nrow(d) > 0)
    ylab <- if (isTRUE(input$diff_cumulative)) "Cumulative Δ vs reference" else "Δ vs reference"
    ttl  <- d$metric[1]
    if ("gender" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen, linetype = gender)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario", linetype = "Gender") +
        theme_clean()
    } else if ("ladnm" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        facet_wrap(~ ladnm, nrow = 2, scales = "free_y") +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean()
    } else {
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean()
    }
  })
  output$plot_diff   <- renderPlot({ build_diff_plot() })
  output$plot_diffly <- renderPlotly({ ggplotly(build_diff_plot(), tooltip = c("x","y","colour","linetype")) })
  
  output$table_diff_summary <- renderTable({
    d <- diff_long(); req(nrow(d) > 0)
    by <- if ("gender" %in% names(d)) "gender" else if ("ladnm" %in% names(d)) "ladnm" else character(0)
    metric_lab <- unique(d$metric)[1]
    if (isTRUE(input$diff_cumulative)) {
      d |> group_by(across(all_of(c("scen", by)))) |>
        slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
        ungroup() |>
        transmute(
          metric = metric_lab, scen,
          !!!(if (length(by)) rlang::syms(by) else NULL),
          final_cycle = cycle,
          cumulative_value = y,
          cumulative_value_scaled = y * SCALING
        ) |> arrange(scen, across(all_of(by)))
    } else {
      d |> group_by(across(all_of(c("scen", by)))) |>
        summarise(final_cycle = max(cycle, na.rm = TRUE),
                  cumulative_value = sum(diff, na.rm = TRUE), .groups = "drop") |>
        mutate(metric = metric_lab, cumulative_value_scaled = cumulative_value * SCALING, .before = 1) |>
        arrange(scen, across(all_of(by)))
    }
  })
  
  # ---------- Average ages (death / onset) ----------
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
          rename(mean_age_raw_years = mean_age_raw,
                 mean_age_weighted_years = mean_age_weighted)
      } else if (view == "Gender") {
        mean_age_dead_raw_by_scen_val_gender |>
          filter(value %in% causes) |>
          left_join(mean_age_dead_weight_by_scen_val_gender |> filter(value %in% causes),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender, value) |>
          rename(mean_age_raw_years = mean_age_raw,
                 mean_age_weighted_years = mean_age_weighted)
      } else {
        mean_age_dead_raw_by_scen_val_lad |>
          filter(value %in% causes) |>
          arrange(scen, ladnm, value) |>
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
          select(scen, value,
                 mean_age_raw_years = mean_age_raw,
                 mean_age_weighted_years = mean_age_weighted)
      } else if (view == "Gender") {
        mean_age_onset_raw_by_scen_val_gender |>
          filter(value == cause) |>
          left_join(mean_age_onset_weight_by_scen_val_gender |> filter(value == cause),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender) |>
          select(scen, gender, value,
                 mean_age_raw_years = mean_age_raw,
                 mean_age_weighted_years = mean_age_weighted)
      } else {
        mean_age_onset_raw_by_scen_val_lad |>
          filter(value == cause) |>
          arrange(scen, ladnm) |>
          rename(mean_age_raw_years = mean_age_raw)
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
        df <- bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
          filter(cause %in% causes)
        req(nrow(df) > 0)
        ggplot(df, aes(x = scen, y = age_std_rate, fill = scen)) +
          geom_col(width = 0.8) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1)), hjust = -0.12, size = 3) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
          coord_flip(clip = "off") +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR (avg cycles 1–30)", x = NULL, y = "ASR per 100,000") +
          theme_clean() + guides(fill = "none") +
          theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all_avg_1_30 |> filter(cause %in% causes)
        req(nrow(df) > 0)
        pos <- position_dodge2(width = 0.75, padding = 0.05, preserve = "single")
        ggplot(df, aes(x = scen, y = age_std_rate, fill = gender)) +
          geom_col(position = pos, width = 0.75) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1)), position = pos, hjust = -0.12, size = 3) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
          coord_flip(clip = "off") +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR by gender (avg cycles 1–30)", x = NULL, y = "ASR per 100,000", fill = "Gender") +
          theme_clean() + theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
      } else {
        df <- asr_lad_all_avg_1_30 |> filter(cause %in% causes)
        req(nrow(df) > 0)
        top_ids <- df |>
          filter(cause == causes[1], scen == "reference") |>
          slice_max(order_by = age_std_rate, n = input$lad_topn, with_ties = FALSE) |>
          distinct(ladnm)
        dplot <- df |> filter(ladnm %in% top_ids$ladnm)
        req(nrow(dplot) > 0)
        pos <- position_dodge2(width = 0.8, padding = 0.08, preserve = "single")
        ggplot(dplot, aes(x = reorder(ladnm, age_std_rate), y = age_std_rate, fill = scen)) +
          geom_col(position = pos, width = 0.8) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1)), position = pos, hjust = -0.10, size = 2.6) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
          coord_flip(clip = "off") +
          labs(title = paste0("ASR by LAD (avg cycles 1–30) — ", causes[1]),
               x = NULL, y = "ASR per 100,000", fill = "Scenario") +
          theme_clean() + theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
      }
    } else {
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_all, asr_healthy_years_overall) |>
          filter(cause %in% causes, cycle >= MIN_CYCLE)
        req(nrow(df) > 0)
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen, group = scen)) +
          geom_smooth(se = FALSE) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle (smoothed, cycles 1–30)", x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean()
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all |> filter(cause %in% causes, cycle >= MIN_CYCLE)
        req(nrow(df) > 0)
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen, linetype = gender)) +
          geom_smooth(se = FALSE) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle by gender (smoothed, cycles 1-30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario", linetype = "Gender") +
          theme_clean()
      } else {
        dat <- asr_lad_all_per_cycle |> filter(cause %in% causes, cycle >= MIN_CYCLE)
        req(nrow(dat) > 0)
        if (length(input$lad_sel)) {
          dat <- dat |> filter(ladnm %in% input$lad_sel)
        } else {
          top_ids <- dat |> filter(cause == causes[1], scen == "reference") |>
            group_by(ladnm) |> summarise(m = mean(age_std_rate, na.rm = TRUE), .groups = "drop") |>
            slice_max(order_by = m, n = min(9, dplyr::n())) |> pull(ladnm)
          dat <- dat |> filter(ladnm %in% top_ids)
        }
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
      pd <- pop_data(); pd$data |> mutate(across(where(is.numeric), ~ round(., 6)))
    } else if (tab == "Differences vs reference") {
      d <- diff_long()
      if (isTRUE(input$diff_cumulative)) {
        by <- if ("gender" %in% names(d)) "gender" else if ("ladnm" %in% names(d)) "ladnm" else character(0)
        d |> group_by(across(all_of(c("scen", by)))) |> slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
          ungroup() |> transmute(scen, across(all_of(by)), final_cycle = cycle, cumulative_value = y, cumulative_value_scaled = y * SCALING)
      } else d
    } else if (tab == "Average ages") {
      output$table_avg |> req(); isolate({ output$table_avg() })
    } else if (tab == "ASR") {
      if (input$asr_mode == "avg") {
        if (input$view_level == "Overall") {
          bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
            filter(cause %in% input$asr_causes)
        } else if (input$view_level == "Gender") {
          asr_gender_all_avg_1_30 |> filter(cause %in% input$asr_causes)
        } else {
          asr_lad_all_avg_1_30 |> filter(cause %in% input$asr_causes)
        }
      } else {
        if (input$view_level == "Overall") {
          bind_rows(asr_overall_all, asr_healthy_years_overall) |>
            filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE)
        } else if (input$view_level == "Gender") {
          asr_gender_all |> filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE)
        } else {
          asr_lad_all_per_cycle |> filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE)
        }
      }
    }
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("export_", gsub("\\s+","_", tolower(input$main_tabs)), "_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(current_table(), file, na = "")
  )
}

shinyApp(ui, server)
