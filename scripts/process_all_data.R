library(dtplyr)
library(arrow)
library(data.table)
library(dplyr)  # still needed for verbs but they dispatch to data.table

ZONES_CSV  <- "/media/ali/Expansion/backup_tabea/manchester-main/input/zoneSystem.csv"
zones    <- readr::read_csv(ZONES_CSV, show_col_types = FALSE)
lads <- zones |> distinct(ladcd, ladnm)
all_data <- arrow::open_dataset("temp/081025/all_data.parquet/")# |> filter(cycle < 21) |> to_duckdb() |> collect()

#all_data <- all_data |> filter(ladcd == "E08000003")

MIN_CYCLE <- 1
MAX_CYCLE <- 30#max(all_data$cycle)

#------------------- Helpers -------------------------------------------
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
      select(scen, cycle, all_of(by), diff, value_ref, .data[[value_col]] )
  }
  
  get_age_levels <- function(x) if (is.factor(x)) levels(x) else sort(unique(x))
  align_age_levels <- function(w, people_age) {
    lv <- get_age_levels(people_age)
    w |> mutate(agegroup_cycle = factor(as.character(agegroup_cycle), levels = lv))
  }
  

# ---- Core event sets ----
# incidence_all <- all_data |>
#   filter(!grepl("dead|healthy|null|depression", value)) |>
#   group_by(id, scen, value) |>
#   filter(cycle == min(cycle)) |>
#   ungroup()# |> 
# #collect()
  
  incidence_all <- all_data |>
    filter(!stringr::str_detect(value, "dead|healthy|null|depression")) |>
    group_by(id, scen, value) |>
    arrow::to_duckdb() |>
    slice_min(order_by = cycle, n = 1, with_ties = FALSE) |>
    ungroup() |>
    collect()
  
incidence_depression <- all_data |>
  filter(value == "depression") |>
  arrange(id, scen, cycle) |>
  group_by(id, scen) |>
  arrow::to_duckdb() |>
  mutate(is_new = is.na(lag(value)) | lag(value) != "depression") |>
  filter(is_new) |>
  ungroup() |> 
  collect()

incidence_deaths <- all_data |> filter(grepl("dead", value)) |> collect() # %in% death_values) #|> collect()

incidence <- bind_rows(incidence_all |> 
                         collect(), 
                       incidence_depression |> collect(), 
                       incidence_deaths |> collect()) |>
  add_agegroups() |>
  left_join(lads, by = "ladcd")

# ---- Population (at risk) ----
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
        age_cycle >= 90  & age_cycle < 95  ~ "90-94",
        age_cycle >= 95  & age_cycle < 100 ~ "95-99",
        age_cycle >= 100                 ~ "100+",
        TRUE                            ~ NA_character_
      )
    )
}

# all_data should be an Arrow Dataset, e.g.
# all_data <- open_dataset("s3://mybucket/data/")

people_raw <- all_data |> 
  add_agegroups() |> 
  group_by(agegroup_cycle, gender, cycle, scen, ladcd) |> 
  collect() |> 
  summarise(pop = n_distinct(id[!grepl("dead|null", value)]), .groups = "drop") |> 
  left_join(lads, by = "ladcd")


# people_raw <- all_data |>
#   #to_duckdb() |> 
#   add_agegroups() |>
#   group_by(agegroup_cycle, gender, cycle, scen, ladcd) |>
#   summarise(pop = n_distinct(id[!grepl("dead|null", value)]), .groups = "drop") |>
#   left_join(lads, by = "ladcd")

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
  to_duckdb() |> 
  summarise(value = n_distinct(id), .groups = "drop") |> 
  collect()

life_years_cycle <- all_data |>
  filter(!grepl("dead", value)) |>
  group_by(scen, cycle) |>
  to_duckdb() |> 
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

healthy_overall  <- healthy_total_cycle |> diff_vs_reference()
healthy_gender   <- all_data |> 
  filter(value == "healthy") |>
  group_by(scen, cycle, gender) |> 
  to_duckdb() |> 
  summarise(value = n_distinct(id), .groups = "drop") |>
  diff_vs_reference(by = "gender") |> 
  collect()

healthy_lad      <- all_data |> 
  filter(value == "healthy") |>
  left_join(lads, by = "ladcd") |>
  group_by(scen, cycle, ladnm) |> 
  to_duckdb() |> 
  summarise(value = n_distinct(id), .groups = "drop") |>
  diff_vs_reference(by = "ladnm") |> 
  collect()

lifey_overall <- life_years_cycle |> 
  diff_vs_reference()

lifey_gender  <- all_data |> 
  filter(!grepl("dead", value)) |>
  group_by(scen, cycle, gender) |> 
  to_duckdb() |> 
  summarise(value = n_distinct(id), .groups = "drop") |>
  diff_vs_reference(by = "gender") |> 
  collect()
# 
# lifey_lad     <- all_data |> 
#   filter(!grepl("dead", value)) |>
#   left_join(lads, by = "ladcd") |>
#   group_by(scen, cycle, ladnm) |> 
#   to_duckdb() |> 
#   summarise(value = n_distinct(id), .groups = "drop") |>
#   diff_vs_reference(by = "ladnm") |> 
#   collect()

lifey_lad <- all_data |>
  filter(!grepl("dead", value)) |>
  to_duckdb() |>  # convert to DuckDB relation early
  left_join(lads, by = "ladcd") |>
  group_by(scen, cycle, ladnm) |>
  summarise(value = n_distinct(id), .groups = "drop") |>
  diff_vs_reference(by = "ladnm") |>
  collect()  # bring results into memory only now

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