#### Assign disease state for baseline population

library(tidyverse)  
library(here)
library(esquisse)

# ============================================================================
# File picker to select pp_rr_2018.csv and extract scenario name and region
# ============================================================================

select_synth_pop_file <- function() {
  # Open file picker dialog
  file_choice <- file.choose()
  
  if (is.na(file_choice) || file_choice == "") {
    stop("No file selected. Aborting process.")
  }
  
  # Normalize path separators to forward slashes for consistent parsing
  normalized_path <- normalizePath(file_choice, winslash = "/")
  
  # Extract scenario name from path: look for pattern scenOutput/{scenario}/microData/
  # Use regex to capture the scenario directory between scenOutput and microData
  scenario_match <- stringr::str_extract(
    normalized_path,
    "(?<=scenOutput/)([^/]+)(?=/microData/)"
  )
  
  if (is.na(scenario_match) || scenario_match == "") {
    warning(
      "Could not determine scenario name from file path.\n",
      "Expected path structure: .../{region}/scenOutput/{scenario}/microData/pp_rr_2018.csv\n",
      "Got: ", normalized_path, "\n",
      "Aborting process."
    )
    stop("Scenario extraction failed.")
  }
  
  # Extract study region from path: the folder immediately before scenOutput
  # Pattern: .../{region}/scenOutput/...
  region_match <- stringr::str_extract(
    normalized_path,
    "(?<=/)[^/]+(?=/scenOutput/)"
  )
  
  if (is.na(region_match) || region_match == "") {
    warning(
      "Could not determine study region from file path.\n",
      "Expected path structure: .../{region}/scenOutput/{scenario}/microData/pp_rr_2018.csv\n",
      "Got: ", normalized_path, "\n",
      "Aborting process."
    )
    stop("Region extraction failed.")
  }
  
  cat("\n✓ File selected:", file_choice, "\n")
  cat("✓ Study region detected:", region_match, "\n")
  cat("✓ Scenario detected:", scenario_match, "\n\n")
  
  list(
    file_path = file_choice,
    region = region_match,
    scenario = scenario_match
  )
}

# Run file picker
file_info <- select_synth_pop_file()
input_file <- file_info$file_path
study_region <- file_info$region
scenario_name <- file_info$scenario

# Synthetic population baseline
synth_pop <- read.csv(input_file)

# Zones data
cat("Loading zones data...\n")
zones <- read_csv(
    here(paste0("../", study_region, "/input/zoneSystem.csv")),
    show_col_types = FALSE
  ) %>%
  rename(zone = SA1_7DIG16)
cat("✓ Zones data loaded. Rows:", nrow(zones), "\n")

# Disease prevalence data
cat("Loading prevalence data...\n")
prevalence <- read_csv(
  here(
    paste0(
      "../",
      study_region,
      "/input/health/health_transitions_melbourne.csv"
    )
  ),
  show_col_types = FALSE
)
cat("✓ Prevalence data loaded. Rows:", nrow(prevalence), "\n")

## The following is commented out as the prevalence data already contains probabilities
# %>%
#   mutate(prob = 1 - exp(-rate), # Convert rates to probabilities
#          cause = str_replace_all(cause, fixed("parkinson’s_disease"), "parkinson"))
  
### Assign geographies
cat("Assigning geographies...\n")
synth_pop <- synth_pop %>%
  left_join(zones, by = c("zone" = "zone"))
cat("✓ Geographies assigned. Rows:", nrow(synth_pop), "\n")

### Join prevalence rates to synthetic population

# Assign prevalence to synthetic population TODO: ignore children.
cat("Joining prevalence data to synthetic population...\n")
synth_pop_wprob <- synth_pop |> 
  rename(sex=gender) |>
  select(id, age, sex, SA2_MAIN16) |>
  rownames_to_column() |>
  left_join(
    prevalence |>
      pivot_wider(id_cols = c(age, sex, SA2_MAIN16), names_from = cause, values_from = prob),
    by = c("age", "sex", "SA2_MAIN16")
  ) |>
  filter(age > 18)
cat("✓ Prevalence joined. Rows:", nrow(synth_pop_wprob), "\n")

# Function to allocate disease statuses based on probability

set.seed(123)

allocate_disease <- function(df) {
  df %>%
    mutate(
      across(
        copd:stroke,
        ~ ifelse(runif(n()) < ., 1, 0),
        .names = "{.col}_status"
      )
    )
}

# Apply function to assign diseases
cat("Allocating disease statuses...\n")
synth_pop_prev <- allocate_disease(synth_pop_wprob) %>%
  select(id, age, sex, SA2_MAIN16, ends_with("_status"))  %>%
  # Ensure we only keep relevant columns
  mutate(
    sex = case_when(
      sex == 1 ~ "male",
      sex == 2 ~ "female",
      TRUE ~ as.character(sex)
    )
  )
cat("✓ Disease statuses allocated. Rows:", nrow(synth_pop_prev), "\n")

# Save
cat("Pivoting data to long format...\n")
synth_pop_prev_long <- synth_pop_prev |> pivot_longer(
  cols = copd_status:stroke_status,
  names_to = "diseases",
  values_to = "values"
) |>
  mutate(diseases = str_remove(diseases, "_status")) |>
  select(id, diseases, values) |>
  filter(values !=0) |>
  select(!values) |>
  group_by(id) %>%
  summarise(diseases = paste(diseases, collapse = " "), .groups = "drop")
cat("✓ Data pivoted to long format. Rows:", nrow(synth_pop_prev_long), "\n")

date_today_yyyy_mm_dd <- format(Sys.Date(), "%Y-%m-%d")

cat("Preparing to save output file...\n")
output_path <- paste0(
  "../",
  study_region,
  "/input/health/",
  scenario_name,
  "_prevalence_id_clean_",
  date_today_yyyy_mm_dd,
  "_",
  study_region,
  ".csv"
)
cat("Output path:", output_path, "\n")

tryCatch(
  {
    write_csv(synth_pop_prev_long, output_path)
    cat("✓ File saved successfully!\n")
    cat("  Location:", normalizePath(output_path), "\n")
  },
  error = function(e) {
    cat("✗ ERROR saving file:\n")
    cat("  Message:", conditionMessage(e), "\n")
    cat("  Path:", output_path, "\n")
    stop(e)
  }
)


# # Compare assignment with GBD original data

# ## Crude rates per age group, gender and by LAD per 100,000 people

# gbd_original <- read.csv(paste0(
#   "../", study_region, "/health/processed/gbd_processed.csv"
# )) |>
#   mutate(rate_100k = rate_1*100000) |>
#   rename(total = val, 
#           SA2_MAIN16 = location) |>
#   mutate(sex = case_when(sex == "Female" ~ "female", 
#                           sex == "Male" ~ "male")) |>
#   select(sex, cause, agegroup, total, SA2_MAIN16, rate_100k) |>
#   mutate(type = "gbd")


# population <- synth_pop_prev |> select(id, age, sex, ladcd, ladnm) |>
#   mutate(agegroup = case_when(
#     age < 5             ~ "<5",
#     age >= 5 & age <= 9 ~ "5-9",
#     age >= 10 & age <= 14 ~ "10-14",
#     age >= 15 & age <= 19 ~ "15-19",
#     age >= 20 & age <= 24 ~ "20-24",
#     age >= 25 & age <= 29 ~ "25-29",
#     age >= 30 & age <= 34 ~ "30-34",
#     age >= 35 & age <= 39 ~ "35-39",
#     age >= 40 & age <= 44 ~ "40-44",
#     age >= 45 & age <= 49 ~ "45-49",
#     age >= 50 & age <= 54 ~ "50-54",
#     age >= 55 & age <= 59 ~ "55-59",
#     age >= 60 & age <= 64 ~ "60-64",
#     age >= 65 & age <= 69 ~ "65-69",
#     age >= 70 & age <= 74 ~ "70-74",
#     age >= 75 & age <= 79 ~ "75-79",
#     age >= 80 & age <= 84 ~ "80-84",
#     age >= 85 & age <= 89 ~ "85-89",
#     age >= 90 & age <= 94 ~ "90-94",
#     age >= 95            ~ "95+",
#     TRUE ~ NA_character_
#   )) |> group_by(agegroup, sex, SA2_MAIN16) |>
#               summarize(pop = n_distinct(id))


# prev_long <- synth_pop_prev |> 
#   pivot_longer(
#     cols = copd_status:stroke_status,
#     values_to = "value"
#   ) |>
#   mutate(agegroup = case_when(
#     age < 5             ~ "<5",
#     age >= 5 & age <= 9 ~ "5-9",
#     age >= 10 & age <= 14 ~ "10-14",
#     age >= 15 & age <= 19 ~ "15-19",
#     age >= 20 & age <= 24 ~ "20-24",
#     age >= 25 & age <= 29 ~ "25-29",
#     age >= 30 & age <= 34 ~ "30-34",
#     age >= 35 & age <= 39 ~ "35-39",
#     age >= 40 & age <= 44 ~ "40-44",
#     age >= 45 & age <= 49 ~ "45-49",
#     age >= 50 & age <= 54 ~ "50-54",
#     age >= 55 & age <= 59 ~ "55-59",
#     age >= 60 & age <= 64 ~ "60-64",
#     age >= 65 & age <= 69 ~ "65-69",
#     age >= 70 & age <= 74 ~ "70-74",
#     age >= 75 & age <= 79 ~ "75-79",
#     age >= 80 & age <= 84 ~ "80-84",
#     age >= 85 & age <= 89 ~ "85-89",
#     age >= 90 & age <= 94 ~ "90-94",
#     age >= 95            ~ "95+",
#     TRUE ~ NA_character_
#   )) |>
#   group_by(agegroup, sex, ladcd, ladnm, name) |>
#   summarise(total = sum(value)) |>
#   mutate(name = str_remove(name, "_status")) |>
#   left_join(population) |>
#   mutate(rate_100k = total/pop*100000) |> 
#   mutate(type = "assigned") |>
#   rename(cause = name) |>
#   ungroup()|>
#   select(sex, cause, agegroup, total, ladnm, rate_100k, type)


# compare <- bind_rows(gbd_original, prev_long) |>
#         filter(!agegroup %in% c("<5", "5-9", "10-14"))



# # Generate plots for each location, sex, and cause
# combinations <- compare %>% 
#   select(SA2_MAIN16, sex, cause) %>% 
#   distinct()

# for (i in seq_len(nrow(combinations))) {
#   loc <- combinations$SA2_MAIN16[i]
#   sx <- combinations$sex[i]
#   cs <- combinations$cause[i]

#   df_filtered <- compare %>%
#     filter(SA2_MAIN16 == loc, sex == sx, cause == cs)
  
#   plot <- ggplot(df_filtered, aes(x = factor(agegroup), y = rate_100k, fill = type)) +
#     geom_col(position = position_dodge(width = 0.8), alpha = 0.8) +
#     scale_fill_manual(
#       values = c("assigned" = "blue", "gbd" = "red"),
#       labels = c("assigned" = "Values Assigned", "gbd" = "Val GBD")
#     ) +
#     labs(title = paste("Location:", loc, "| Sex:", sx, "| Cause:", cs),
#          x = "Age Group",
#          y = "Rate per 100k",
#          fill = "Legend") +
#     theme_classic() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))

#   ggsave(filename = paste0("../",study_region,"/images/prevalence/plot_", loc, "_", sx, "_", cs, ".png"),
#          plot = plot, width = 8, height = 6)
# }


