# ======================================================================
# SCRIPT: Life years, Healthy years, Death diffs, Incidence,
# and Age-Standardised Rates (ASR) + Exports & Plots
# ======================================================================

# ---- Libraries --------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(arrow)
  library(purrr)
  library(ggplot2)
  library(plotly)
  library(readr)
  library(fs)
  library(scales)
  library(forcats)
})

# ---- Config -----------------------------------------------------------
DATA_PATH  <- "Y:/HealthImpact/Data/Country/UK/JIBE_health_output_data/190825/all_data_190825.parquet"
OUTPUT_DIR <- "Y:/HealthImpact/Outputs/190825"
zones <- read_csv("Y:/HealthImpact/Data/Country/UK/JIBE_health_output_data/190825/zoneSystem.csv")
dir_create(OUTPUT_DIR, recurse = TRUE)

if (!exists("SCALING")) SCALING <- 5  # default for 20% sample

# zones must contain ladcd, ladnm
stopifnot(all(c("ladcd","ladnm") %in% names(zones)))
lads <- zones |> distinct(ladcd, ladnm)

# ---- Load & collect base data (Arrow -> R) ----------------------------
all_data <- arrow::open_dataset(DATA_PATH) |>
  collect()

# ---- Helpers ----------------------------------------------------------
add_agegroups <- function(df) {
  df %>%
    mutate(
      value = if_else(grepl("dead", value), "dead", value),
      agegroup_cycle = cut(
        age_cycle,
        breaks = c(seq(0, 100, by = 5), Inf),
        labels = c(paste(seq(0, 95, by = 5), seq(4, 99, by = 5), sep = "-"), "100+"),
        right = FALSE, include.lowest = TRUE
      )
    )
}

write_csv_safe <- function(x, path) {
  readr::write_csv(x, path, na = "")
  message("Saved: ", path)
}

theme_clean <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 0, hjust = 0.5))
}
add_zero_line <- function() geom_hline(yintercept = 0, linewidth = 0.3)
plot_diff_lines <- function(df_long, y_var, title, y_lab) {
  ggplot(df_long, aes(x = cycle, y = .data[[y_var]], colour = scenario)) +
    geom_smooth(se = FALSE) +
    add_zero_line() +
    scale_y_continuous(labels = label_comma()) +
    labs(title = title, x = "Cycle (year)", y = y_lab, colour = "Scenario") +
    theme_clean()
}

# ---- People (population at risk) -------------------------------------
people <- all_data %>%
  add_agegroups() %>%
  group_by(agegroup_cycle, gender, cycle, scen, ladcd) %>%
  summarise(pop = n_distinct(id[!grepl("dead|null", value)]), .groups = "drop") %>%
  left_join(lads, by = "ladcd")

# ---- Healthy life years ----------------------------------------------
healthy_total_cycle <- all_data %>%
  filter(value == "healthy") %>%
  group_by(scen, cycle) %>%
  summarise(healthy_years = n_distinct(id), .groups = "drop")

healthy_total_cycle_diff <- healthy_total_cycle %>%
  left_join(
    healthy_total_cycle %>%
      filter(scen == "reference") %>%
      select(cycle, reference_healthy_years = healthy_years),
    by = "cycle"
  ) %>%
  mutate(
    healthy_years_difference = healthy_years - reference_healthy_years,
    percent_difference = 100 * healthy_years_difference / reference_healthy_years
  )

sum_healthy_years <- healthy_total_cycle_diff %>%
  group_by(scen) %>%
  summarise(difference = sum(healthy_years_difference), .groups = "drop") %>%
  mutate(diff_scaled = difference * SCALING)

# ---- Life years (alive) ----------------------------------------------
life_years_cycle <- all_data %>%
  filter(!grepl("dead", value)) %>%
  group_by(scen, cycle) %>%
  summarise(life_years = n_distinct(id), .groups = "drop")

life_years_cycle_diff <- life_years_cycle %>%
  left_join(
    life_years_cycle %>%
      filter(scen == "reference") %>%
      select(cycle, reference_life_years = life_years),
    by = "cycle"
  ) %>%
  mutate(
    life_years_difference = life_years - reference_life_years,
    percent_difference = 100 * life_years_difference / reference_life_years
  )

sum_life_years <- life_years_cycle_diff %>%
  group_by(scen) %>%
  summarise(difference = sum(life_years_difference), .groups = "drop") %>%
  mutate(diff_scaled = difference * SCALING)

# ---- Deaths & differences --------------------------------------------
death_values <- c("dead","dead_car","dead_bike","dead_walk")

inc_death <- all_data %>%
  filter(value %in% death_values) %>%
  left_join(lads, by = "ladcd")

# Weighted mean age at death
mean_age_dead <- inc_death %>%
  count(age_cycle, name = "weight") %>%
  right_join(inc_death, by = "age_cycle") %>%
  group_by(scen) %>%
  summarise(mean_age_cycle = weighted.mean(age_cycle, weight, na.rm = TRUE),
            .groups = "drop")

# Differences vs reference by age group × gender × cycle
dead_diff <- inc_death %>%
  add_agegroups() %>%
  group_by(agegroup_cycle, gender, cycle, scen) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(
    inc_death %>%
      add_agegroups() %>%
      filter(scen == "reference") %>%
      group_by(agegroup_cycle, gender, cycle) %>%
      summarise(n_reference = n(), .groups = "drop"),
    by = c("agegroup_cycle","gender","cycle")
  ) %>%
  mutate(n_reference = coalesce(n_reference, 0L),
         difference  = n - n_reference)

dead_diff_acc <- dead_diff %>%
  group_by(scen) %>%
  summarise(difference = sum(difference), .groups = "drop") %>%
  mutate(diff_scaled = difference * SCALING)

# ---- Incidence (first events + depression-first + deaths) --------------
incidence_all <- all_data %>%
  filter(!grepl("dead|healthy|null|depression", value)) %>%
  group_by(id, scen, value) %>%
  filter(cycle == min(cycle)) %>%
  ungroup()

incidence_depression <- all_data %>%
  filter(value == "depression") %>%
  arrange(id, scen, cycle) %>%
  group_by(id, scen) %>%
  mutate(prev_value = lag(value),
         is_new_depression = if_else(is.na(prev_value) | prev_value != "depression", TRUE, FALSE)) %>%
  filter(is_new_depression) %>%
  ungroup()

incidence <- bind_rows(
  incidence_all,
  incidence_depression,
  all_data %>% filter(value %in% death_values)
) %>%
  add_agegroups() %>%
  left_join(lads, by = "ladcd")

mean_age_onset <- incidence |> 
  group_by(scen, value) %>%
  summarise(mean_age_cycle = mean(age_cycle, na.rm = TRUE),
            .groups = "drop")

# Per-cycle counts (for diagnostics/plots)
count_inc <- incidence %>%
  filter(cycle > 0) %>%
  group_by(cycle, scen, value, agegroup_cycle, age_cycle) %>%
  summarise(n = n(), .groups = "drop")

# ---- Cross-check tables (wide) ----------------------------------------
healthy_total_cycle_wider <- healthy_total_cycle %>%
  pivot_wider(names_from = scen, values_from = healthy_years) %>%
  mutate(
    diff_h_safe  = safeStreet - reference,
    diff_h_green = green - reference,
    diff_h_both  = both - reference
  ) %>%
  select(cycle, diff_h_safe, diff_h_green, diff_h_both)

inc_diff_wider <- count_inc %>%
  group_by(cycle, scen) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = scen, values_from = n) %>%
  mutate(
    diff_dis_safe  = safeStreet - reference,
    diff_dis_green = green - reference,
    diff_dis_both  = both - reference
  ) %>%
  select(cycle, diff_dis_safe, diff_dis_both, diff_dis_green)

life_years_wider <- life_years_cycle %>%
  pivot_wider(names_from = scen, values_from = life_years) %>%
  mutate(
    ly_diff_safe  = safeStreet - reference,
    ly_diff_green = green - reference,
    ly_diff_both  = both - reference
  ) %>%
  select(cycle, ly_diff_safe, ly_diff_green, ly_diff_both)

dead_diff_wider <- inc_death %>%
  group_by(cycle, scen) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = scen, values_from = n) %>%
  mutate(
    dead_diff_safe  = safeStreet - reference,
    dead_diff_green = green - reference,
    dead_diff_both  = both - reference
  ) %>%
  select(cycle, dead_diff_safe, dead_diff_green, dead_diff_both)

diff_metrics_cycle <- healthy_total_cycle_wider %>%
  left_join(life_years_wider, by = "cycle") %>%
  left_join(inc_diff_wider,  by = "cycle") %>%
  left_join(dead_diff_wider, by = "cycle")


# Column totals across all cycles (cross check with totals for healthy years, life years, dead, disease
# already calculated and also for direct comparison)
diff_metrics_totals <- diff_metrics_cycle |>
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))


## Summary table of all differences
diff_metrics_totals_long <- diff_metrics_cycle |>
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
  pivot_longer(
    cols = everything(),                          # exclude 'cycle' by not carrying it forward
    names_to = c("p1","p2","scen"),
    names_pattern = "^([^_]+)_([^_]+)_([^_]+)$",  # robust: exactly three parts
    values_to = "value"
  ) |>
  mutate(metric = paste(p1, p2, sep = "_")) |>
  select(metric, scen, value) |>
  arrange(metric, scen) |>
  mutate(scaled = value *SCALING)


# ---- Diseases developed for those kept alive in scenarios --- (also add to see what is happening with safe large gain in health years)

# ---------------------------
# 0) Setup / helpers
# ---------------------------
death_values <- c("dead","dead_car","dead_bike","dead_walk")

ageband <- function(x) cut(
  x, breaks = c(seq(0,100,5), Inf),
  labels = c(paste(seq(0,95,5), seq(4,99,5), sep = "-"), "100+"),
  right = FALSE, include.lowest = TRUE
)

# Optional LAD names
lads <- tryCatch(zones %>% distinct(ladcd, ladnm), error = function(e) NULL)

# ---------------------------
# 1) Single-source-of-truth tables
# ---------------------------

# Earliest death per id × scenario
death_cycles_long <- all_data %>%
  filter(value %in% death_values) %>%
  group_by(id, scen) %>%
  summarise(death_cycle = min(cycle), .groups = "drop")

# Wide for easy ref/scenario comparisons (columns: death_reference, death_safeStreet, death_green, death_both, ...)
death_cycles_wide <- death_cycles_long %>%
  pivot_wider(names_from = scen, values_from = death_cycle, names_glue = "death_{scen}")

# Earliest onset per id × scenario × disease (exclude deaths/healthy/null)
onsets <- all_data %>%
  filter(!(value %in% c(death_values, "healthy","null"))) %>%
  group_by(id, scen, value) %>%
  summarise(onset_cycle = min(cycle), .groups = "drop")

# ---------------------------
# 2) Diseases after REF death (measured in a scenario)
#    “They die in reference; are alive in scen at that time (or die later);
#     then develop disease in scen after ref death and before scen death.”
# ---------------------------
diseases_after_ref_death_in <- function(scen_name, same_day = FALSE) {
  
  col_scen <- paste0("death_", scen_name)
  
  dat <- onsets %>%
    filter(scen == scen_name) %>%
    left_join(death_cycles_wide, by = "id") %>%
    # must have a reference death
    filter(!is.na(death_reference)) %>%
    # scenario must not kill them earlier than (or equal to, if same_day=FALSE) ref death
    {
      if (same_day) {
        filter(., is.na(.data[[col_scen]]) | .data[[col_scen]] >= death_reference)
      } else {
        filter(., is.na(.data[[col_scen]]) | .data[[col_scen]] >  death_reference)
      }
    } %>%
    # onset must be after ref death
    {
      if (same_day) filter(., onset_cycle >= death_reference)
      else          filter(., onset_cycle >  death_reference)
    } %>%
    # if they die in scenario, onset must be before scen death
    {
      if (same_day) filter(., is.na(.data[[col_scen]]) | onset_cycle <= .data[[col_scen]])
      else          filter(., is.na(.data[[col_scen]]) | onset_cycle <  .data[[col_scen]])
    }
  
  # attach onset attributes (age/sex/lad) in scenario timeline
  onset_info <- all_data %>%
    filter(scen == scen_name) %>%
    select(id, cycle, age_cycle, gender, ladcd) %>%
    distinct()
  
  out <- dat %>%
    left_join(onset_info, by = c("id","onset_cycle" = "cycle")) %>%
    mutate(
      agegroup_cycle = ageband(age_cycle),
      scen = scen_name, .before = 1
    ) %>%
    select(scen, id, value, onset_cycle, death_reference, !!col_scen, age_cycle, agegroup_cycle, gender, ladcd)
  
  if (!is.null(lads)) out <- out %>% left_join(lads, by = "ladcd")
  out
}

# ---------------------------
# 3) REF onsets after SCEN death (mirror)
#    “They die in scen; either never die in ref or die later there;
#     then develop disease in REF after scen death and before ref death (if any).”
# ---------------------------
ref_onsets_after_scen_death <- function(scen_name, same_day = FALSE) {
  
  col_scen <- paste0("death_", scen_name)
  
  dat <- onsets %>%
    filter(scen == "reference") %>%
    left_join(death_cycles_wide, by = "id") %>%
    # must die in scenario
    filter(!is.na(.data[[col_scen]])) %>%
    # reference either never dies or dies later than (or equal to, if same_day) scen death
    {
      if (same_day) {
        filter(., is.na(death_reference) | death_reference >= .data[[col_scen]])
      } else {
        filter(., is.na(death_reference) | death_reference >  .data[[col_scen]])
      }
    } %>%
    # onset in REF after scen death
    {
      if (same_day) filter(., onset_cycle >= .data[[col_scen]])
      else          filter(., onset_cycle >  .data[[col_scen]])
    } %>%
    # if they die in ref, onset must be before ref death
    {
      if (same_day) filter(., is.na(death_reference) | onset_cycle <= death_reference)
      else          filter(., is.na(death_reference) | onset_cycle <  death_reference)
    }
  
  # attach onset attributes (age/sex/lad) in reference timeline
  onset_info_ref <- all_data %>%
    filter(scen == "reference") %>%
    select(id, cycle, age_cycle, gender, ladcd) %>%
    distinct()
  
  out <- dat %>%
    left_join(onset_info_ref, by = c("id","onset_cycle" = "cycle")) %>%
    mutate(
      agegroup_cycle = ageband(age_cycle),
      scen = scen_name, .before = 1
    ) %>%
    select(scen, id, value, onset_cycle,
           death_scen = !!sym(col_scen), death_reference,
           age_cycle, agegroup_cycle, gender, ladcd)
  
  if (!is.null(lads)) out <- out %>% left_join(lads, by = "ladcd")
  out
}

# ---------------------------
# 4) Run both analyses
# ---------------------------

# A) After REF death, in each non-reference scenario (e.g., safeStreet, green, both)
scenarios_nonref <- setdiff(sort(unique(all_data$scen)), "reference")
after_ref_all <- map_dfr(scenarios_nonref, ~ diseases_after_ref_death_in(.x, same_day = FALSE))

# B) Mirror: After SCEN death, onsets in REFERENCE (e.g., safeStreet only)
ref_after_scen_safe <- ref_onsets_after_scen_death("safeStreet", same_day = FALSE)

# ---------------------------
# 5) Summaries + your cancer bucket
# ---------------------------
cancers_other_set <- c(
  "endometrial_cancer", "endometrial_cnacer",
  "myeloma",
  "bladder_cancer", "bladder_cnacer",
  "esophageal_cancer",
  "head_neck_cancer", "head_nech_cancer",
  "gastric_cardia_cancer",
  "liver_cancer",
  "myeloid_leukemia"
)

summarise_with_groups <- function(df) {
  df %>%
    mutate(
      disease_group = case_when(value %in% cancers_other_set ~ "cancers_others",
                                TRUE ~ value)
    ) %>%
    list(
      by_sex = group_by(., scen, disease_group, gender) %>%
        summarise(n_people = n_distinct(id), .groups = "drop") %>%
        arrange(scen, disease_group, desc(n_people)),
      by_age = group_by(., scen, disease_group, agegroup_cycle) %>%
        summarise(n_people = n_distinct(id), .groups = "drop") %>%
        arrange(scen, disease_group, agegroup_cycle),
      totals = group_by(., scen, disease_group) %>%
        summarise(n_people = n_distinct(id), .groups = "drop") %>%
        arrange(scen, desc(n_people))
    )
}

sum_after_ref_all   <- summarise_with_groups(after_ref_all)
sum_ref_after_scen  <- summarise_with_groups(ref_after_scen_safe)


# ======================================================================
# ASR MODULE — Baseline weights only; exclude cycles 0 & 1 from ALL ASRs
# ======================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr)
  library(ggplot2); library(scales)
})

# --------- Global toggle / constants ----------------------------------
USE_BASELINE <- TRUE      # keep TRUE (baseline weights only)
MIN_CYCLE    <- 2         # EXCLUDE cycles 0 and 1 from all ASR calcs

# --------- Helpers -----------------------------------------------------
if (!exists("add_agegroups")) {
  add_agegroups <- function(df) {
    df %>%
      mutate(
        value = if_else(grepl("dead", value), "dead", value),
        agegroup_cycle = cut(
          age_cycle,
          breaks = c(seq(0, 100, by = 5), Inf),
          labels = c(paste(seq(0, 95, by = 5), seq(4, 99, by = 5), sep = "-"), "100+"),
          right = FALSE, include.lowest = TRUE
        )
      )
  }
}

get_age_levels <- function(x) if (is.factor(x)) levels(x) else sort(unique(x))
align_age_levels <- function(w, people_age) {
  lv <- get_age_levels(people_age)
  w %>% mutate(agegroup_cycle = factor(as.character(agegroup_cycle), levels = lv))
}

# --------- Build BASELINE weights (cycle 0 of reference) ---------------
stopifnot(exists("people"))

ref_weights_overall <- people %>%
  filter(scen == "reference", cycle == 0) %>%
  group_by(agegroup_cycle) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  mutate(weight = pop / sum(pop)) %>%
  select(agegroup_cycle, weight)

ref_weights_gender <- people %>%
  filter(scen == "reference", cycle == 0) %>%
  group_by(agegroup_cycle, gender) %>%
  summarise(pop = sum(pop), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(weight = pop / sum(pop)) %>%
  ungroup() %>%
  select(agegroup_cycle, gender, weight)

# Use baseline weights (requested)
W_OVERALL <- align_age_levels(ref_weights_overall, people$agegroup_cycle)
W_GENDER  <- if ("gender" %in% names(ref_weights_gender)) {
  ref_weights_gender %>%
    mutate(agegroup_cycle = factor(as.character(agegroup_cycle),
                                   levels = get_age_levels(people$agegroup_cycle)))
} else {
  align_age_levels(ref_weights_gender, people$agegroup_cycle)
}

# --------- ASR functions (min_cycle applied everywhere) ----------------
calc_asr <- function(data_cases, causes, people, ref_weights,
                     group_vars = character(),
                     avg_cycles = NULL,
                     min_cycle = 2) {
  
  base_groups    <- c("agegroup_cycle", "scen", "cycle")
  all_group_cols <- unique(c(base_groups, group_vars))
  join_vars      <- intersect(names(ref_weights), c("agegroup_cycle", "gender"))
  
  map_dfr(causes, function(cause) {
    crude_counts <- data_cases %>%
      filter(value == cause, cycle >= min_cycle) %>%
      group_by(across(all_of(all_group_cols))) %>%
      summarise(cases = n(), .groups = "drop")
    
    pop_counts <- people %>%
      filter(cycle >= min_cycle) %>%
      group_by(across(all_of(all_group_cols))) %>%
      summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
    
    crude_rates <- crude_counts %>%
      left_join(pop_counts, by = all_group_cols) %>%
      mutate(crude_rate = if_else(pop > 0, cases / pop * 1e5, NA_real_)) %>%
      filter(!is.na(crude_rate))
    
    std_rates <- crude_rates %>%
      left_join(ref_weights, by = join_vars) %>%
      filter(!is.na(weight)) %>%
      mutate(rate_w = crude_rate * weight)
    
    sum_groups   <- unique(c("scen", "cycle", group_vars))
    asr_per_cycle <- std_rates %>%
      group_by(across(all_of(sum_groups))) %>%
      summarise(age_std_rate = sum(rate_w), .groups = "drop") %>%
      mutate(cause = cause, .before = 1)
    
    if (is.null(avg_cycles)) return(asr_per_cycle)
    
    asr_per_cycle %>%
      filter(cycle %in% avg_cycles) %>%
      group_by(across(all_of(setdiff(sum_groups, "cycle"))), cause) %>%
      summarise(age_std_rate = mean(age_std_rate, na.rm = TRUE), .groups = "drop") %>%
      mutate(cycle = paste0("avg_", paste(range(avg_cycles), collapse = "-")), .after = "scen")
  })
}

calc_asr_from_counts <- function(counts_df, people, ref_weights,
                                 group_vars = character(),
                                 scale = 1e5,
                                 avg_cycles = NULL,
                                 cause_name = "metric",
                                 min_cycle = 2) {
  group_cols <- unique(c("agegroup_cycle", "scen", "cycle", group_vars))
  join_vars  <- intersect(names(ref_weights), c("agegroup_cycle", "gender"))
  
  counts_df <- counts_df %>%
    filter(cycle >= min_cycle) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(num = sum(num, na.rm = TRUE), .groups = "drop")
  
  pop_counts <- people %>%
    filter(cycle >= min_cycle) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
  
  rates <- counts_df %>%
    left_join(pop_counts, by = group_cols) %>%
    mutate(rate = if_else(pop > 0, num / pop * scale, NA_real_)) %>%
    filter(!is.na(rate))
  
  std <- rates %>%
    left_join(ref_weights, by = join_vars) %>%
    filter(!is.na(weight)) %>%
    mutate(rate_w = rate * weight)
  
  sum_groups <- unique(c("scen", "cycle", group_vars))
  out <- std %>%
    group_by(across(all_of(sum_groups))) %>%
    summarise(age_std_rate = sum(rate_w), .groups = "drop") %>%
    mutate(cause = cause_name, .before = 1)
  
  if (is.null(avg_cycles)) return(out)
  
  out %>%
    filter(cycle %in% avg_cycles) %>%
    group_by(across(all_of(setdiff(sum_groups, "cycle"))), cause) %>%
    summarise(age_std_rate = mean(age_std_rate, na.rm = TRUE), .groups = "drop") %>%
    mutate(cycle = paste0("avg_", paste(range(avg_cycles), collapse = "-")), .after = "scen")
}

# --------- Build inputs for healthy-years ASR (pre-filter cycles) ------
stopifnot(exists("all_data"))
healthy_age_counts <- all_data %>%
  add_agegroups() %>%
  filter(value == "healthy", cycle >= MIN_CYCLE) %>%
  group_by(agegroup_cycle, scen, cycle) %>%
  summarise(num = n_distinct(id), .groups = "drop")

healthy_age_counts_gender <- all_data %>%
  add_agegroups() %>%
  filter(value == "healthy", cycle >= MIN_CYCLE) %>%
  group_by(agegroup_cycle, scen, cycle, gender) %>%
  summarise(num = n_distinct(id), .groups = "drop")

healthy_age_counts_lad <- all_data %>%
  add_agegroups() %>%
  filter(value == "healthy", cycle >= MIN_CYCLE) %>%
  group_by(agegroup_cycle, scen, cycle, ladcd) %>%
  summarise(num = n_distinct(id), .groups = "drop")

healthy_age_counts_lad_gender <- all_data %>%
  add_agegroups() %>%
  filter(value == "healthy", cycle >= MIN_CYCLE) %>%
  group_by(agegroup_cycle, scen, cycle, ladcd, gender) %>%
  summarise(num = n_distinct(id), .groups = "drop")

# --------- Causes & DISEASE ASRs (cycles ≥ 2 only) ---------------------
stopifnot(exists("incidence"))
all_causes <- incidence %>%
  distinct(value) %>% pull(value) %>% setdiff(c("healthy","null"))

# Overall (per-cycle and average)
asr_overall_all <- calc_asr(incidence, all_causes, people, W_OVERALL,
                            group_vars = character(),
                            avg_cycles = NULL,
                            min_cycle = MIN_CYCLE)

asr_overall_avg_2_30 <- calc_asr(incidence, all_causes, people, W_OVERALL,
                                 group_vars = character(),
                                 avg_cycles = MIN_CYCLE:30,
                                 min_cycle = MIN_CYCLE)

# Gender
asr_gender_all <- calc_asr(incidence, all_causes, people, W_GENDER,
                           group_vars = "gender",
                           avg_cycles = NULL,
                           min_cycle = MIN_CYCLE)

asr_gender_all_avg_2_30 <- calc_asr(incidence, all_causes, people, W_GENDER,
                                    group_vars = "gender",
                                    avg_cycles = MIN_CYCLE:30,
                                    min_cycle = MIN_CYCLE)

# LAD
stopifnot(exists("lads"))
asr_lad_all_per_cycle <- calc_asr(incidence, all_causes, people, W_OVERALL,
                                  group_vars = "ladcd",
                                  avg_cycles = NULL,
                                  min_cycle = MIN_CYCLE) %>%
  left_join(lads, by = "ladcd")

asr_lad_all_avg_2_30 <- calc_asr(incidence, all_causes, people, W_OVERALL,
                                 group_vars = "ladcd",
                                 avg_cycles = MIN_CYCLE:30,
                                 min_cycle = MIN_CYCLE) %>%
  left_join(lads, by = "ladcd")

# LAD × gender
asr_lad_gender_per_cycle <- calc_asr(incidence, all_causes, people, W_GENDER,
                                     group_vars = c("ladcd","gender"),
                                     avg_cycles = NULL,
                                     min_cycle = MIN_CYCLE) %>%
  left_join(lads, by = "ladcd")

asr_lad_gender_avg_2_30 <- calc_asr(incidence, all_causes, people, W_GENDER,
                                    group_vars = c("ladcd","gender"),
                                    avg_cycles = MIN_CYCLE:30,
                                    min_cycle = MIN_CYCLE) %>%
  left_join(lads, by = "ladcd")

# --------- HEALTHY-YEARS ASRs (cycles ≥ 2 only) ------------------------
asr_healthy_years_overall <- calc_asr_from_counts(
  healthy_age_counts, people, W_OVERALL,
  group_vars = character(), scale = 1e5,
  avg_cycles = NULL, cause_name = "healthy_years", min_cycle = MIN_CYCLE
)

asr_healthy_years_overall_avg_2_30 <- calc_asr_from_counts(
  healthy_age_counts, people, W_OVERALL,
  group_vars = character(), scale = 1e5,
  avg_cycles = MIN_CYCLE:30, cause_name = "healthy_years", min_cycle = MIN_CYCLE
)

asr_healthy_years_gender <- calc_asr_from_counts(
  healthy_age_counts_gender, people, W_GENDER,
  group_vars = "gender", scale = 1e5,
  avg_cycles = NULL, cause_name = "healthy_years", min_cycle = MIN_CYCLE
)

asr_healthy_years_gender_avg_2_30 <- calc_asr_from_counts(
  healthy_age_counts_gender, people, W_GENDER,
  group_vars = "gender", scale = 1e5,
  avg_cycles = MIN_CYCLE:30, cause_name = "healthy_years", min_cycle = MIN_CYCLE
)

asr_healthy_years_lad_per_cycle <- calc_asr_from_counts(
  healthy_age_counts_lad, people, W_OVERALL,
  group_vars = "ladcd", scale = 1e5,
  avg_cycles = NULL, cause_name = "healthy_years", min_cycle = MIN_CYCLE
) %>% left_join(lads, by = "ladcd")

asr_healthy_years_lad_avg_2_30 <- calc_asr_from_counts(
  healthy_age_counts_lad, people, W_OVERALL,
  group_vars = "ladcd", scale = 1e5,
  avg_cycles = MIN_CYCLE:30, cause_name = "healthy_years", min_cycle = MIN_CYCLE
) %>% left_join(lads, by = "ladcd")

asr_healthy_years_lad_gender_per_cycle <- calc_asr_from_counts(
  healthy_age_counts_lad_gender, people, W_GENDER,
  group_vars = c("ladcd","gender"), scale = 1e5,
  avg_cycles = NULL, cause_name = "healthy_years", min_cycle = MIN_CYCLE
) %>% left_join(lads, by = "ladcd")

asr_healthy_years_lad_gender_avg_2_30 <- calc_asr_from_counts(
  healthy_age_counts_lad_gender, people, W_GENDER,
  group_vars = c("ladcd","gender"), scale = 1e5,
  avg_cycles = MIN_CYCLE:30, cause_name = "healthy_years", min_cycle = MIN_CYCLE
) %>% left_join(lads, by = "ladcd")

# ======================================================================
# PLOTTING HELPERS (single standard; works for all causes)
# ======================================================================

theme_clean <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          strip.text = element_text(lineheight = 0.9))
}

# label_pos: "outside" | "inside" | "none"
plot_asr_overall_bars <- function(df_overall_avg, causes = NULL,
                                  label_pos = "outside", accuracy = 0.1,
                                  wrap = 18, ncol = 4) {
  dat <- if (is.null(causes)) df_overall_avg else dplyr::filter(df_overall_avg, cause %in% causes)
  
  p <- ggplot(dat, aes(x = scen, y = age_std_rate, fill = scen)) +
    geom_col(width = 0.8, colour = NA)
  
  if (label_pos == "inside") {
    p <- p + geom_text(aes(label = number(age_std_rate, accuracy = accuracy)),
                       hjust = 1.05, size = 3, colour = "white")
  } else if (label_pos == "outside") {
    p <- p + geom_text(aes(label = number(age_std_rate, accuracy = accuracy)),
                       hjust = -0.12, size = 3, colour = "black") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.14)))
  }
  
  p +
    coord_flip(clip = "off") +
    facet_wrap(vars(cause), scales = "free_x",
               labeller = labeller(cause = label_wrap_gen(width = wrap)),
               ncol = ncol) +
    labs(title = "Age-standardised rates (avg cycles 2–30)",
         x = NULL, y = "ASR per 100,000") +
    theme_clean() +
    guides(fill = "none") +
    theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
}

# Single-cause trend (per-cycle). Starts at min_cycle (default = MIN_CYCLE).
# Uses a smoothed curve; no SE ribbon.
plot_asr_overall_trend <- function(df_overall_cycle, cause_to_plot, min_cycle = MIN_CYCLE) {
  df_overall_cycle %>%
    dplyr::filter(cause == cause_to_plot, cycle >= min_cycle) %>%
    ggplot2::ggplot(ggplot2::aes(x = cycle, y = age_std_rate, colour = scen, group = scen)) +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::labs(title = paste0("ASR per cycle – ", cause_to_plot),
                  x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
    theme_clean()
}

# Multi-cause trends (small multiples). Smoothed; no SE ribbon.
plot_asr_overall_trends_all <- function(df_overall_cycle, causes = NULL,
                                        min_cycle = MIN_CYCLE, ncol = 4, wrap = 18) {
  dat <- df_overall_cycle %>% dplyr::filter(cycle >= min_cycle)
  if (!is.null(causes)) dat <- dat %>% dplyr::filter(cause %in% causes)
  
  ggplot2::ggplot(dat, ggplot2::aes(x = cycle, y = age_std_rate, colour = scen, group = scen)) +
    ggplot2::geom_smooth(se = FALSE) +
    ggplot2::facet_wrap(ggplot2::vars(cause),
                        labeller = ggplot2::labeller(cause = label_wrap_gen(width = wrap)),
                        ncol = ncol, scales = "free_y") +
    ggplot2::labs(title = "ASR per cycle (cycles 2–30)",
                  x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
    theme_clean()
}

# Gender bars (avg 2–30)
plot_asr_gender_bars <- function(df_gender_avg, causes = NULL,
                                 label_pos = "outside", accuracy = 0.1,
                                 wrap = 18, ncol = 4) {
  dat <- if (is.null(causes)) df_gender_avg else dplyr::filter(df_gender_avg, cause %in% causes)
  pos <- position_dodge2(width = 0.75, padding = 0.05, preserve = "single")
  
  p <- ggplot(dat, aes(x = scen, y = age_std_rate, fill = gender)) +
    geom_col(position = pos, width = 0.75)
  
  if (label_pos == "inside") {
    p <- p + geom_text(aes(label = number(age_std_rate, accuracy = accuracy)),
                       position = pos, hjust = 1.05, size = 3, colour = "white")
  } else if (label_pos == "outside") {
    p <- p + geom_text(aes(label = number(age_std_rate, accuracy = accuracy)),
                       position = pos, hjust = -0.12, size = 3, colour = "black") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.14)))
  }
  
  p +
    coord_flip(clip = "off") +
    facet_wrap(vars(cause),
               labeller = labeller(cause = label_wrap_gen(width = wrap)),
               ncol = ncol, scales = "free_x") +
    labs(title = "ASR by Gender (avg cycles 2–30)",
         x = NULL, y = "ASR per 100,000", fill = "Gender") +
    theme_clean() +
    theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
}

# LAD bars (avg 2–30), Top-N LADs by a focus scenario
plot_asr_lad_topN <- function(df_lad_avg, cause_to_plot,
                              scen_focus = "reference", top_n = 20,
                              label_pos = "outside", accuracy = 0.1) {
  
  top_ids <- df_lad_avg %>%
    filter(cause == cause_to_plot, scen == scen_focus) %>%
    slice_max(order_by = age_std_rate, n = top_n, with_ties = FALSE) %>%
    distinct(ladcd)
  
  dat <- df_lad_avg %>%
    filter(cause == cause_to_plot, ladcd %in% top_ids$ladcd) %>%
    mutate(ladnm_ord = reorder(ladnm, age_std_rate))
  
  pos <- position_dodge2(width = 0.8, padding = 0.08, preserve = "single")
  
  p <- ggplot(dat, aes(x = ladnm_ord, y = age_std_rate, fill = scen)) +
    geom_col(position = pos, width = 0.8, colour = NA)
  
  if (label_pos == "inside") {
    p <- p + geom_text(aes(label = number(age_std_rate, accuracy = accuracy)),
                       position = pos, hjust = 1.02, size = 2.6, colour = "white")
  } else if (label_pos == "outside") {
    p <- p + geom_text(aes(label = number(age_std_rate, accuracy = accuracy)),
                       position = pos, hjust = -0.10, size = 2.6, colour = "black") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.16)))
  }
  
  p +
    coord_flip(clip = "off") +
    labs(title = paste0("ASR by LAD (avg cycles 2–30) – ", cause_to_plot),
         x = NULL, y = "ASR per 100,000", fill = "Scenario") +
    theme_clean() +
    theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
}

# ======================================================================
# EXAMPLE CALLS (baseline, cycles 2–30)
# ======================================================================
# ============================================================
# CLEAN EXAMPLES — averages (2–30) + smoothed over-time
# ============================================================

# --- Inputs expected from earlier steps ---
# asr_overall_avg_2_30, asr_healthy_years_overall_avg_2_30
# asr_overall_all,      asr_healthy_years_overall
# asr_gender_all_avg_2_30, asr_healthy_years_gender_avg_2_30, asr_gender_all
# asr_lad_all_avg_2_30,    asr_healthy_years_lad_avg_2_30,    asr_lad_all_per_cycle
# MIN_CYCLE, plot_* functions already defined (bars + trends)

# ---------- AVERAGE BARS (cycles 2–30) ----------
# Combine diseases + healthy
asr_overall_status_avg_2_30 <- dplyr::bind_rows(
  asr_overall_avg_2_30,
  asr_healthy_years_overall_avg_2_30
)

# Selected + all causes
selected_causes <- c(
  "all_cause_dementia","breast_cancer","colon_cancer","copd",
  "coronary_heart_disease","dead","depression","diabetes",
  "lung_cancer","parkinson","stroke","healthy_years"
)
all_causes_for_plot <- sort(unique(asr_overall_status_avg_2_30$cause))

# Overall bars (selected & all)
p_overall_selected <- plot_asr_overall_bars(
  asr_overall_status_avg_2_30,
  causes   = selected_causes,
  label_pos= "outside"
)
p_overall_all <- plot_asr_overall_bars(
  asr_overall_status_avg_2_30,
  causes   = all_causes_for_plot,
  label_pos= "outside",
  ncol     = 5
)

# Gender bars (avg 2–30): combine disease + healthy
asr_gender_status_avg_2_30 <- dplyr::bind_rows(
  asr_gender_all_avg_2_30,
  asr_healthy_years_gender_avg_2_30
)
p_gender_selected <- plot_asr_gender_bars(
  asr_gender_status_avg_2_30,
  causes   = selected_causes,
  label_pos= "outside"
)

# LAD bars (avg 2–30), Top-N for two example outcomes
p_lad_chd_top25 <- plot_asr_lad_topN(
  asr_lad_all_avg_2_30, "coronary_heart_disease",
  scen_focus = "reference", top_n = 25, label_pos = "outside"
)
p_lad_healthy_top25 <- plot_asr_lad_topN(
  asr_healthy_years_lad_avg_2_30, "healthy_years",
  scen_focus = "reference", top_n = 25, label_pos = "outside"
)

# ---------- OVER-TIME (smoothed) ----------
# 1) Single-cause smoothed trends — build a named list (no repetition)
causes_demo <- c("coronary_heart_disease","stroke","diabetes",
                 "lung_cancer","depression","dead","healthy_years")

plots_trend_selected <- setNames(
  lapply(causes_demo, function(cx) {
    # healthy_years lives in its own table; others in asr_overall_all
    if (cx == "healthy_years") {
      plot_asr_overall_trend(asr_healthy_years_overall, cx, min_cycle = MIN_CYCLE)
    } else {
      plot_asr_overall_trend(asr_overall_all,        cx, min_cycle = MIN_CYCLE)
    }
  }),
  causes_demo
)
# Example: print one
# plots_trend_selected[["stroke"]]

# 2) Small multiples (smoothed) for any set of causes (incl. healthy_years)
asr_over_time_both <- dplyr::bind_rows(asr_overall_all, asr_healthy_years_overall)
p_trends_demo <- plot_asr_overall_trends_all(
  asr_over_time_both,
  causes    = causes_demo,   # or `all_causes_for_plot` for everything
  ncol      = 4,
  min_cycle = MIN_CYCLE
)

# 3) Gender-stratified trend for a chosen cause (inline; smoothed)
p_trend_chd_gender <- asr_gender_all %>%
  dplyr::filter(cause == "coronary_heart_disease", cycle >= MIN_CYCLE) %>%
  ggplot2::ggplot(ggplot2::aes(x = cycle, y = age_std_rate, colour = scen, linetype = gender)) +
  ggplot2::geom_smooth(se = FALSE) +
  ggplot2::labs(title = "ASR per cycle — CHD by gender",
                x = "Cycle (year)", y = "ASR per 100,000",
                colour = "Scenario", linetype = "Gender") +
  theme_clean()

# 4) LAD over-time trends for top 6 LADs in reference (smoothed)
#    (Example for CHD; duplicate the block for another cause if needed)
cause_for_lad_trend <- "coronary_heart_disease"
top_lads_for_trend <- asr_lad_all_per_cycle %>%
  dplyr::filter(cause == cause_for_lad_trend, scen == "reference", cycle >= MIN_CYCLE) %>%
  dplyr::group_by(ladcd, ladnm) %>%
  dplyr::summarise(mean_rate = mean(age_std_rate, na.rm = TRUE), .groups = "drop") %>%
  dplyr::slice_max(order_by = mean_rate, n = 6) %>%
  dplyr::pull(ladcd)

p_trend_lad_chd <- asr_lad_all_per_cycle %>%
  dplyr::filter(cause == cause_for_lad_trend, cycle >= MIN_CYCLE, ladcd %in% top_lads_for_trend) %>%
  ggplot2::ggplot(ggplot2::aes(x = cycle, y = age_std_rate, colour = scen)) +
  ggplot2::geom_smooth(se = FALSE) +
  ggplot2::facet_wrap(~ ladnm, ncol = 3, scales = "free_y") +
  ggplot2::labs(title = "ASR per cycle — CHD for top LADs",
                x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
  theme_clean()

