library(dtplyr)
library(arrow)
library(data.table)
library(qs2)
library(dplyr)   
library(DBI)
library(duckdb)

# ZONES_CSV  <- "/media/ali/Expansion/backup_tabea/manchester-main/input/zoneSystem.csv"
ZONES_CSV <- "manchester/health/processed/zoneSystem.csv"
zones    <- readr::read_csv(ZONES_CSV, show_col_types = FALSE)
# ---- Geography and deprivation groupings ---------------------------------
# NOTE ON COLUMN NAMES: the columns below keep their original names `ladnm`
# and `imd10`, but their CONTENTS have changed:
#   ladnm : now one of 4 AREA groups, not an individual district name
#   imd10 : now an IMD QUINTILE (1-5), not a decile (1-10)


# Deciles -> quintiles: 1,2 -> 1;  3,4 -> 2;  5,6 -> 3;  7,8 -> 4;  9,10 -> 5
to_quintile <- function(d) ceiling(d / 2)

AREA_MAP <- c(
  "Manchester" = "City core",
  "Salford"    = "City core",
  "Oldham"     = "East",
  "Rochdale"   = "East",
  "Tameside"   = "East",
  "Stockport"  = "South",
  "Trafford"   = "South",
  "Bolton"     = "West/North-west",
  "Wigan"      = "West/North-west",
  "Bury"       = "West/North-west"
)

lads_raw <- zones |> distinct(ladcd, ladnm)

#  a silent NA here would quietly drop a whole district from every LAD-level output.
unmapped <- setdiff(lads_raw$ladnm, names(AREA_MAP))
if (length(unmapped))
  stop("District(s) not in AREA_MAP: ", paste(unmapped, collapse = ", "),
       "\nCheck the exact spelling in zoneSystem.csv$ladnm.")

# ladcd stays unique (still 10 rows); ladnm now carries the area label, so
# every existing group_by(ladnm) collapses to 4 groups automatically.
lads <- lads_raw |> mutate(ladnm = unname(AREA_MAP[ladnm]))

# ---- Paths ---------------------------------------------------------------
# Local copy of the partitioned dataset (scen=*/ladcd=*/part-0.parquet) in local drive (to avoind network issues)
PARQUET  <- "C:/jibe_data/all_data_220726.parquet"

SRC_DIR  <- "X:/HealthImpact/Data/Country/UK/JIBE/manchester/scenOutput/normalization_fix_052226/processed"
SRC_FILE <- "all_data_220726.parquet"

# ---- One-off copy from the network share ---------------------------------
# set to true if results changed
COPY_FROM_SOURCE <- FALSE

if (COPY_FROM_SOURCE) {
  src_path <- file.path(SRC_DIR, SRC_FILE)
  
  # The source is a DIRECTORY of part files, not a single file. file.info()
  # reports size 0 for a directory (NA if truly missing) -- hence the isdir
  # check, and hence /E below rather than a filename pattern.
  stopifnot(isTRUE(suppressWarnings(file.info(src_path)$isdir)))
  
  f <- suppressWarnings(list.files(src_path, recursive = TRUE, full.names = TRUE))
  src_bytes <- sum(suppressWarnings(file.info(f)$size))
  message(sprintf("Source: %d part files, %.1f GB", length(f), src_bytes / 1e9))
  
  dir.create(PARQUET, recursive = TRUE, showWarnings = FALSE)
  rc <- system2("robocopy", c(shQuote(src_path), shQuote(PARQUET),
                              "/E", "/J", "/R:5", "/W:10", "/NP"))
  stopifnot(rc < 8)   # robocopy: 0-7 are success codes, 8+ are failures
  
  dest_bytes <- sum(file.info(
    list.files(PARQUET, recursive = TRUE, full.names = TRUE))$size)
  stopifnot(dest_bytes == src_bytes)   # guard against a truncated copy
  message(sprintf("Copied %.1f GB, verified.", dest_bytes / 1e9))
}

# ---- Connect -------------------------------------------------------------
# DuckDB's native Parquet reader, not arrow::open_dataset() |> to_duckdb():

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, sprintf(
  "CREATE VIEW all_data_v AS SELECT * FROM read_parquet('%s/**/*.parquet', hive_partitioning = true)",
  PARQUET))

all_data <- dplyr::tbl(con, "all_data_v") |> filter(cycle != 31)

zones_db <- dplyr::copy_to(
  con,
  zones |> dplyr::select(lsoa21cd, imd10) |> distinct() |>
    mutate(imd10 = to_quintile(imd10)),   # NB: now a QUINTILE, see note above
  name = "zones_db", temporary = TRUE, overwrite = TRUE)

lads_db <- dplyr::copy_to(
  con, lads,
  name = "lads_db", temporary = TRUE, overwrite = TRUE)

MIN_CYCLE <- 1
MAX_CYCLE <- 30

#------------------- Helpers -------------------------------------------
# Define age break function compatible with Arrow (without cut)
add_agegroups <- function(df) {
  df |> 
    mutate(
      agegroup_cycle = case_when(
        age_cycle >= 0   & age_cycle < 5   ~ "0-4",
        age_cycle >= 5   & age_cycle < 10  ~ "5-9",
        age_cycle >= 10  & age_cycle < 15  ~ "10-14",
        age_cycle >= 15  & age_cycle < 20  ~ "15-19",
        age_cycle >= 20  & age_cycle < 25  ~ "20-24",
        age_cycle >= 25  & age_cycle < 30  ~ "25-29",
        age_cycle >= 30  & age_cycle < 35  ~ "30-34",
        age_cycle >= 35  & age_cycle < 40  ~ "35-39",
        age_cycle >= 40  & age_cycle < 45  ~ "40-44",
        age_cycle >= 45  & age_cycle < 50  ~ "45-49",
        age_cycle >= 50  & age_cycle < 55  ~ "50-54",
        age_cycle >= 55  & age_cycle < 60  ~ "55-59",
        age_cycle >= 60  & age_cycle < 65  ~ "60-64",
        age_cycle >= 65  & age_cycle < 70  ~ "65-69",
        age_cycle >= 70  & age_cycle < 75  ~ "70-74",
        age_cycle >= 75  & age_cycle < 80  ~ "75-79",
        age_cycle >= 80  & age_cycle < 85  ~ "80-84",
        age_cycle >= 85  & age_cycle < 90  ~ "85-89",
        # ESP2013 is top-coded at 90+. Keeping 90-94 / 95-99 / 100+ separate
        # would leave those bands without a standard weight, and calc_asr()
        # drops unweighted rows -- silently losing all cases aged 90+.
        age_cycle >= 90                    ~ "90+",
        TRUE                            ~ NA_character_
      )
    )
}

# Canonical age-band order (ESP2013 bands). Use for factor levels / plotting.
AGE_LEVELS <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                "75-79","80-84","85-89","90+")




diff_vs_reference <- function(df, by = character(0), value_col = "value") {
  ref <- df |>
    filter(scen == "reference") |>
    rename(value_ref = !!rlang::sym(value_col)) |>
    select(cycle, all_of(by), value_ref)
  df |>
    filter(scen != "reference") |>
    left_join(ref, by = c("cycle", by)) |>
    mutate(diff = .data[[value_col]] - value_ref) |>
    select(scen, cycle, all_of(by), diff, value_ref, .data[[value_col]] )
}



# ---- Core event sets ----
incidence_all <- all_data |>
  filter(!stringr::str_detect(value, "dead|healthy|null|depression")) |>
  group_by(id, scen, value) |>
  #arrow::to_duckdb() |>
  slice_min(order_by = cycle, n = 1, with_ties = FALSE) |>
  ungroup() |>
  collect()

incidence_depression <- all_data |>
  filter(value == "depression") |>
  arrange(id, scen, cycle) |>
  group_by(id, scen) |>
  # arrow::to_duckdb() |>
  mutate(is_new = is.na(lag(value)) | lag(value) != "depression") |>
  filter(is_new) |>
  ungroup() |> 
  collect()

incidence_deaths <- all_data |> filter(grepl("dead", value)) |> collect()

incidence <- bind_rows(incidence_all,
                       incidence_depression,
                       incidence_deaths) |>
  add_agegroups() |>
  left_join(lads, by = "ladcd")

incidence <- left_join(incidence,
                       zones |>
                         dplyr::select(lsoa21cd, imd10) |>
                         distinct() |>
                         mutate(imd10 = to_quintile(imd10)),
                       by = "lsoa21cd")

# ---- Population (at risk) ----

# all_data should be an Arrow Dataset, e.g.
# all_data <- open_dataset("s3://mybucket/data/")

people_raw <- all_data |> 
  filter(!grepl("dead|null", value)) |> 
  add_agegroups() |> 
  left_join(zones_db, by = "lsoa21cd") |> 
  group_by(agegroup_cycle, imd10, gender, cycle, scen, ladcd) |> 
  summarise(pop = n_distinct(id[!grepl("dead|null", value)]), .groups = "drop") |> 
  left_join(lads_db, by = "ladcd") |> 
  collect()

people_overall <- people_raw |>
  group_by(agegroup_cycle, scen, cycle) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

people_gender <- people_raw |>
  group_by(agegroup_cycle, scen, cycle, gender) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

people_lad <- people_raw |>
  group_by(agegroup_cycle, scen, cycle, ladnm) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

people_imd <- people_raw |>
  group_by(agegroup_cycle, scen, cycle, imd10) |>
  summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

# ---- Healthy & Life years ----

life_years_cycle <- all_data |>
  filter(!grepl("dead", value)) |>
  group_by(scen, cycle) |>
  summarise(value = n_distinct(id), .groups = "drop") |> 
  collect()

# ---- Death counts ----
inc_death <- incidence |> filter(grepl("dead", value))
deaths_overall_raw <- inc_death |> group_by(scen, cycle) |>
  summarise(value = dplyr::n(), .groups = "drop")
deaths_gender_raw  <- inc_death |> group_by(scen, cycle, gender) |>
  summarise(value = dplyr::n(), .groups = "drop")
deaths_lad_raw     <- inc_death |> group_by(scen, cycle, ladnm) |>
  summarise(value = dplyr::n(), .groups = "drop")
deaths_imd_raw     <- inc_death |> group_by(scen, cycle, imd10) |>
  summarise(value = dplyr::n(), .groups = "drop")
deaths_agegroup_cycle_raw     <- inc_death |> group_by(scen, cycle, agegroup_cycle) |>
  summarise(value = dplyr::n(), .groups = "drop")


# ---- Disease counts (all causes combined) ----
diseases_all_cycle <- incidence |>
  filter(!grepl("dead|healthy|null", value)) |> 
  rename(cause = value) |> 
  group_by(cause, scen, cycle) |>
  to_duckdb() |> 
  summarise(value = dplyr::n(), .groups = "drop") |> 
  collect()

# ---- Differences vs reference ----
deaths_overall <- diff_vs_reference(deaths_overall_raw)
deaths_gender  <- diff_vs_reference(deaths_gender_raw, by = "gender")
deaths_lad     <- diff_vs_reference(deaths_lad_raw,   by = "ladnm")
deaths_imd     <- diff_vs_reference(deaths_imd_raw,   by = "imd10")
deaths_agegroup_cycle  <- diff_vs_reference(deaths_agegroup_cycle_raw,   by = "agegroup_cycle")


diseases_overall <- diff_vs_reference(diseases_all_cycle, by = "cause")
diseases_gender  <- incidence |>
  filter(!value %in% c("dead","healthy","null")) |>
  rename(cause = value) |> 
  group_by(cause, scen, cycle, gender) |> 
  summarise(value = dplyr::n(), .groups = "drop") |>
  diff_vs_reference(by = c("gender", "cause"))
diseases_lad     <- incidence |>
  filter(!value %in% c("dead","healthy","null")) |>
  rename(cause = value) |> 
  group_by(cause, scen, cycle, ladnm) |> summarise(value = dplyr::n(), .groups = "drop") |>
  diff_vs_reference(by = c("ladnm", "cause"))
diseases_agegroup_cycle     <- incidence |>
  filter(!value %in% c("dead","healthy","null")) |>
  rename(cause = value) |> 
  group_by(cause, scen, cycle, agegroup_cycle) |> summarise(value = dplyr::n(), .groups = "drop") |>
  diff_vs_reference(by = c("agegroup_cycle", "cause"))
diseases_imd     <- incidence |>
  filter(!value %in% c("dead","healthy","null")) |>
  rename(cause = value) |> 
  group_by(cause, scen, cycle, imd10) |> summarise(value = dplyr::n(), .groups = "drop") |>
  diff_vs_reference(by = c("imd10", "cause"))


# ---- healthy cube: ONE scan serving all healthy_* outputs ----------------
# n() is used deliberately in place of n_distinct(id). Verified 2026-07-31:
#   all_data |> count(scen, id, cycle, value) |> filter(n > 1) |> count(scen)
# returns zero rows for ALL FOUR scenarios, so within a single `value` the
# grain is one row per person-cycle and rows == people.
# This holds ONLY for single-value filters. The multi-value filters used by
# life_years_cycle and people_raw (!grepl("dead|null", value)) must keep
# n_distinct(id): people there average ~1.06 rows each, so n() would
# overcount by ~5.8%.
healthy_cube <- all_data |>
  filter(value == "healthy") |>
  add_agegroups() |>
  left_join(zones_db, by = "lsoa21cd") |>
  left_join(lads_db,  by = "ladcd") |>
  group_by(scen, cycle, agegroup_cycle, gender, imd10, ladnm) |>
  summarise(n = n(), .groups = "drop") |>
  collect()

roll <- function(cube, by) {
  cube |>
    group_by(across(all_of(c("scen", "cycle", by)))) |>
    summarise(value = sum(n), .groups = "drop")
}

healthy_total_cycle <- roll(healthy_cube, character(0))
healthy_overall     <- healthy_total_cycle |> diff_vs_reference()

healthy_gender          <- roll(healthy_cube, "gender")         |> diff_vs_reference(by = "gender")
healthy_lad             <- roll(healthy_cube, "ladnm")          |> diff_vs_reference(by = "ladnm")
healthy_agegroup_cycle  <- roll(healthy_cube, "agegroup_cycle") |> diff_vs_reference(by = "agegroup_cycle")
healthy_imd             <- roll(healthy_cube, "imd10")          |> diff_vs_reference(by = "imd10")


lifey_overall <- life_years_cycle |> 
  diff_vs_reference()

lifey_gender  <- all_data |> 
  filter(!grepl("dead", value)) |>
  group_by(scen, cycle, gender) |> 
  summarise(value = n_distinct(id), .groups = "drop") |>
  diff_vs_reference(by = "gender") |> 
  collect()

lifey_agegroup_cycle  <- all_data |> 
  filter(!grepl("dead", value)) |>
  group_by(scen, cycle, agegroup_cycle) |> 
  summarise(value = n_distinct(id), .groups = "drop") |>
  diff_vs_reference(by = "agegroup_cycle") |> 
  collect()

lifey_imd  <- all_data |> 
  filter(!grepl("dead", value)) |>
  left_join(zones_db, by = "lsoa21cd") |> 
  group_by(scen, cycle, imd10) |> 
  summarise(value = n_distinct(id), .groups = "drop") |>
  diff_vs_reference(by = "imd10") |> 
  collect()

lifey_lad <- all_data |>
  filter(!grepl("dead", value)) |>
  left_join(lads_db, by = "ladcd") |>
  group_by(scen, cycle, ladnm) |>
  summarise(value = n_distinct(id), .groups = "drop") |>
  diff_vs_reference(by = "ladnm") |>
  collect()
# ---- Mean age (death & onset) ----
inc_death_src <- incidence |>
  filter(grepl("dead", value)) |> 
  select(scen, value, age_cycle, gender, ladnm, imd10, agegroup_cycle)

incidence_src <- incidence |>
  filter(!value %in% c("healthy","null") & !grepl("dead", value)) |>
  select(scen, value, age_cycle, gender, ladnm, imd10, agegroup_cycle)

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

mean_age_onset_raw_by_scen_val_imd    <- incidence_src |> group_by(scen, value, imd10) |> summarise(mean_age_raw = mean(age_cycle), .groups="drop")
mean_age_onset_weight_by_scen_val_imd <- weighted_mean_by(incidence_src, c("scen","value","imd10"))


mean_age_onset_raw_by_scen_val_lad       <- incidence_src |> group_by(scen, value, ladnm) |> summarise(mean_age_raw = mean(age_cycle), .groups="drop")

# ---- ASR (age-standardised rates) ----
# Standard population: European Standard Population 2013 (ESP2013), Eurostat /
# ONS revision. Counts are per 100,000. ESP2013 reports 0 and 1-4 separately
# (1000 + 4000); they are merged here to 5000 to match the 0-4 band above.
# The final band is 90+.
esp2013 <- tibble::tribble(
  ~agegroup_cycle, ~esp_pop,
  "0-4",    5000,
  "5-9",    5500,
  "10-14",  5500,
  "15-19",  5500,
  "20-24",  6000,
  "25-29",  6000,
  "30-34",  6500,
  "35-39",  7000,
  "40-44",  7000,
  "45-49",  7000,
  "50-54",  7000,
  "55-59",  6500,
  "60-64",  6000,
  "65-69",  5500,
  "70-74",  5000,
  "75-79",  4000,
  "80-84",  2500,
  "85-89",  1500,
  "90+",    1000
)
stopifnot(sum(esp2013$esp_pop) == 100000)

ref_weights_overall <- esp2013 |>
  mutate(weight = esp_pop / sum(esp_pop)) |>
  select(agegroup_cycle, weight)

# Both sexes are standardised to the SAME external population, so male and

ref_weights_gender <- tidyr::expand_grid(
  gender = unique(people_raw$gender),
  ref_weights_overall
) |>
  select(agegroup_cycle, gender, weight)

# Keep agegroup_cycle as character: the data-side columns are character, and a
# factor/character join silently coerces (and previously took its levels from
# whatever bands happened to appear in the data).
W_OVERALL <- ref_weights_overall |> mutate(agegroup_cycle = as.character(agegroup_cycle))
W_GENDER  <- ref_weights_gender  |> mutate(agegroup_cycle = as.character(agegroup_cycle))

# Fail if any observed age band has no ESP weight -- those rows are
# dropped by calc_asr() and would quietly bias every rate downwards.
check_weight_coverage <- function(df, ref_weights, label) {
  observed <- setdiff(unique(as.character(df$agegroup_cycle)), NA)
  missing  <- setdiff(observed, unique(as.character(ref_weights$agegroup_cycle)))
  if (length(missing)) {
    stop("[", label, "] age bands with no standard-population weight: ",
         paste(sort(missing), collapse = ", "))
  }
  invisible(TRUE)
}
check_weight_coverage(people_raw, W_OVERALL, "people_raw")
check_weight_coverage(incidence,  W_OVERALL, "incidence")

calc_asr <- function(data_cases, causes, people, ref_weights,
                     group_vars = character(), avg_cycles = NULL, min_cycle = MIN_CYCLE) {
  base_groups    <- c("agegroup_cycle", "scen", "cycle")
  all_group_cols <- unique(c(base_groups, group_vars))
  join_vars      <- intersect(names(ref_weights), c("agegroup_cycle", "gender"))
  # pop_counts does not depend on `cause` -- compute once, not once per cause.
  pop_counts <- people |>
    filter(cycle >= min_cycle) |>
    group_by(across(all_of(all_group_cols))) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
  purrr::map_dfr(causes, function(cause) {
    crude_counts <- data_cases |>
      filter(value == cause, cycle >= min_cycle) |>
      group_by(across(all_of(all_group_cols))) |>
      summarise(cases = dplyr::n(), .groups = "drop")
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

healthy_age_counts <- healthy_cube |>
  filter(cycle >= MIN_CYCLE) |>
  group_by(agegroup_cycle, scen, cycle) |>
  summarise(num = sum(n), .groups = "drop")

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

asr_imd_all <- calc_asr(incidence, all_causes, people_raw, W_OVERALL,
                        group_vars = "imd10", avg_cycles = NULL, min_cycle = MIN_CYCLE)
asr_imd_all_avg_1_30 <- calc_asr(incidence, all_causes, people_raw, W_OVERALL,
                                 group_vars = "imd10", avg_cycles = MIN_CYCLE:MAX_CYCLE, min_cycle = MIN_CYCLE)

# ---- Age-specific rates (deliberately NOT standardised) -----------------
# A breakdown BY age band cannot also be age-standardised: direct
# standardisation exists to remove the age effect, so within a single band the
# weighted sum collapses to rate * w_band -- the rate arbitrarily rescaled by
# that band's ESP share (0.01 for 90+, 0.07 for 40-44). Report crude
# age-specific rates instead; these are already free of age confounding by
# construction, and are what standardisation is built from.
calc_age_specific <- function(data_cases, causes, people, group_vars = character(),
                              scale = 1e5, avg_cycles = NULL, min_cycle = MIN_CYCLE) {
  group_cols <- unique(c("agegroup_cycle", "scen", "cycle", group_vars))
  # cause-invariant: compute once outside the loop
  pop_counts <- people |>
    filter(cycle >= min_cycle) |>
    group_by(across(all_of(group_cols))) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
  purrr::map_dfr(causes, function(cause) {
    case_counts <- data_cases |>
      filter(value == cause, cycle >= min_cycle) |>
      group_by(across(all_of(group_cols))) |>
      summarise(cases = dplyr::n(), .groups = "drop")
    # Drive the join from the population side so bands with a denominator but
    # no cases return a rate of 0 rather than vanishing from the output. (In
    # calc_asr() the missing rows were harmless -- they contribute 0 to the
    # sum -- but here they would appear as gaps in the by-age series.)
    per_cycle <- pop_counts |>
      left_join(case_counts, by = group_cols) |>
      mutate(cases = dplyr::coalesce(cases, 0L),
             rate_per_100k = if_else(pop > 0, cases / pop * scale, NA_real_)) |>
      filter(!is.na(rate_per_100k)) |>
      mutate(cause = cause, .before = 1)
    if (is.null(avg_cycles)) return(per_cycle)
    avg_groups <- unique(c("cause", "agegroup_cycle", "scen", group_vars))
    per_cycle |>
      filter(cycle %in% avg_cycles) |>
      group_by(across(all_of(avg_groups))) |>
      summarise(cases = sum(cases, na.rm = TRUE),
                pop   = sum(pop, na.rm = TRUE),
                rate_per_100k = mean(rate_per_100k, na.rm = TRUE),
                .groups = "drop") |>
      mutate(cycle = paste0("avg_", paste(range(avg_cycles), collapse = "-")),
             .after = "scen")
  })
}

rate_agegroup_all <- calc_age_specific(incidence, all_causes, people_raw,
                                       avg_cycles = NULL, min_cycle = MIN_CYCLE)
rate_agegroup_avg_1_30 <- calc_age_specific(incidence, all_causes, people_raw,
                                            avg_cycles = MIN_CYCLE:MAX_CYCLE,
                                            min_cycle = MIN_CYCLE)

# Grouped by ladnm (= area) rather than ladcd: the ASR must be standardised
# WITHIN each area. Standardising by district and averaging afterwards would
# weight a small district equally with a large one.
asr_lad_all_per_cycle <- calc_asr(incidence, all_causes, people_raw, W_OVERALL,
                                  group_vars = "ladnm", avg_cycles = NULL, min_cycle = MIN_CYCLE)
asr_lad_all_avg_1_30 <- calc_asr(incidence, all_causes, people_raw, W_OVERALL,
                                 group_vars = "ladnm", avg_cycles = MIN_CYCLE:MAX_CYCLE, min_cycle = MIN_CYCLE)

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
  # canonical age-band order for plotting
  "AGE_LEVELS",
  # population
  "people_overall","people_gender","people_lad", "people_imd",
  # diffs
  "deaths_overall","deaths_gender","deaths_lad", "deaths_imd", "deaths_agegroup_cycle",
  "diseases_overall","diseases_gender","diseases_lad", "diseases_imd", "diseases_agegroup_cycle",
  "healthy_overall","healthy_gender","healthy_lad", "healthy_imd", "healthy_agegroup_cycle",
  "lifey_overall","lifey_gender","lifey_lad", "lifey_imd", "lifey_agegroup_cycle",
  # mean-age sources
  #"incidence_src","inc_death_src",
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
  "mean_age_onset_raw_by_scen_val_imd",
  "mean_age_onset_weight_by_scen_val_imd",
  "mean_age_onset_raw_by_scen_val_lad",
  # ASR
  "asr_overall_all","asr_overall_avg_1_30",
  "asr_gender_all","asr_gender_all_avg_1_30",
  "asr_imd_all","asr_imd_all_avg_1_30",
  # age-specific (crude) rates -- replaces the former asr_agegroup_cycle_*,
  # which applied standardisation weights within single age bands
  "rate_agegroup_all","rate_agegroup_avg_1_30",
  "asr_lad_all_per_cycle","asr_lad_all_avg_1_30",
  "asr_healthy_years_overall","asr_healthy_years_overall_avg_1_30"
))
precomp_path <- "app/data/precomputed_100%V6.qs2"
message("Saving precomputed cache: ", precomp_path)
#saveRDS(pc, precomp_path, compress = "xz")
qs2::qs_save(pc, precomp_path)

DBI::dbDisconnect(con, shutdown = TRUE)