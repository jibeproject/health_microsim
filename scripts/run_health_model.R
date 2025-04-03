##### This script runs the health model for all scenarios

# Load libraries
library(tidyverse) # For parallel processing
library(future.apply) # For directory/file structure
library(here) 
# install.packages("remotes")
# remotes::install_github("meta-analyses/drpa")# For DR PA 
library(drpa)
library(arrow) # For fast reading/processing
library(tictoc)
library(data.table)  # For faster data operations
library(stringi)     # For faster string operations

source("functions/micro_sim_function.R")


# Closed cohort
# mmets and exposures constant over time for individual

# For reproducibility set seed
set.seed(2)
options(future.globals.maxSize = +Inf)

# Set sample_pro to be greater than zero
sample_prop <- 0.1

# Number of cycles/years the simulation works
n.c <- 30

# Define DISEASE RISK
DISEASE_RISK <- TRUE

# Data ----

possible_paths <- c(
  "manchester/health/processed/",
  "manchester/synPop/sp_2021/",
  "health/"
)
# Find the first path that exists
base_path <- possible_paths[file.exists(possible_paths)][1]

if (is.na(base_path)) stop("Base directory not found!")

hd <- read_csv(file.path(base_path, "health_transitions_manchester.csv"))
hd[hd$cause == "head_neck_cancer",]$cause <- "head_and_neck_cancer"

prev <- read_csv(file.path(base_path, "prevalence_id.csv"))
zones <- read_csv(file.path(base_path, "zoneSystem.csv"))
disease_risks <- read_csv(file.path("health/mod_disease_risks.csv"))


# Scenario specific runs
# Take sample representative of age, gender, lad

### Reference

synth_pop_ref <- read_csv(file.path(base_path, "ref_pp_exposure_RR_2021.csv")) |> 
  mutate(age_group = sprintf("[%d,%d)", 5 * floor(age/5), 5 * (floor(age/5) + 1)))


if (sample_prop > 0){
  synth_pop_ref <- synth_pop_ref  |> 
    group_by(age_group, gender, ladcd) |> 
    sample_frac(sample_prop)
}

results_ref <- run_microsim_model(synth_pop_ref, hd, prev, zones, diseases_risks) 


arrow::write_dataset(results_ref|> as.data.frame(), paste0("manchester/health/processed/ref_exp_dis_inter_off_state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".parquet"))


### Safer

synth_pop_safer <- read_csv(file.path(base_path, "safer_pp_exposure_RR_2021.csv")) |> 
  mutate(age_group = sprintf("[%d,%d)", 5 * floor(age/5), 5 * (floor(age/5) + 1)))


if (sample_prop > 0){
  synth_pop_safer <- synth_pop_safer  |> 
    group_by(age_group, gender, ladcd) |> 
    sample_frac(sample_prop)
}

results_safer <- run_microsim_model(synth_pop_ref, hd, prev, zones, diseases_risks) 


arrow::write_dataset(results_safer|> as.data.frame(), paste0("manchester/health/processed/safer_exp_dis_inter_off_state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".parquet"))


### Green

synth_pop_green <- read_csv(file.path(base_path, "green_pp_exposure_RR_2021.csv")) |> 
  mutate(age_group = sprintf("[%d,%d)", 5 * floor(age/5), 5 * (floor(age/5) + 1)))


if (sample_prop > 0){
  synth_pop_green <- synth_pop_green  |> 
    group_by(age_group, gender, ladcd) |> 
    sample_frac(sample_prop)
}

results_green <- run_microsim_model(synth_pop_green, hd, prev, zones, diseases_risks) 


arrow::write_dataset(results_green|> as.data.frame(), paste0("manchester/health/processed/green_exp_dis_inter_off_state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".parquet"))


### Both

synth_pop_both <- read_csv(file.path(base_path, "both_pp_exposure_RR_2021.csv")) |> 
  mutate(age_group = sprintf("[%d,%d)", 5 * floor(age/5), 5 * (floor(age/5) + 1)))


if (sample_prop > 0){
  synth_pop_both <- synth_pop_both  |> 
    group_by(age_group, gender, ladcd) |> 
    sample_frac(sample_prop)
}

results_green <- run_microsim_model(synth_pop_both, hd, prev, zones, diseases_risks) 


arrow::write_dataset(results_green|> as.data.frame(), paste0("manchester/health/processed/both_exp_dis_inter_off_state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".parquet"))







#### Some plots

## some plots to visualise results

# Create individual states, while ignoring the all_cause_mortality state as dead state already captures it
l <- data.frame(states = c('dead', diseases |> str_subset(pattern = "all_cause_mortality", negate = TRUE)), freq = 0, c = 0)
for (ind in 1:n.c){
  df <- unlist(strsplit(m[, ind], " ")) |> 
    as.data.frame()
  names(df) <- 'states'
  tbl <- df |> 
    group_by(states) |> 
    summarise(cn = dplyr::n()) |> 
    mutate(freq = round(cn / sum(cn) * 100, 1), c = ind) |> 
    dplyr::select(-cn)
  l <- plyr::rbind.fill(l, tbl)
}

l$c <- as.factor(l$c)

l <- l |> filter(!is.na(states))

# Generate historic state transitions of all diseases + dead
ggplot(l |> filter(freq > 0)) +
  aes(x = c, y = freq, fill = states) +
  geom_col() +
  labs(x = "Years", y = "Frequency (%)", title = "State transitions over the years") +
  theme_minimal()

plotly::ggplotly(ggplot(l |> filter(states != "healthy"), aes(x = c, y = freq, color = states, group = states)) + geom_line() + geom_point() +
                   labs(x = "Years", y = "Frequency (%)", title = "State transitions over the years") ) 


m |> as.data.frame() |> 
  rownames_to_column("id") |> 
  pivot_longer(cols = -c(id)) |> 
  mutate(unpacked = str_split(value, " ")) |> 
  unnest() |> 
  mutate(value = str_trim(unpacked)) |> 
  dplyr::select(-unpacked) |> 
  group_by(name, value)|> 
  summarise(nv = dplyr::n(), 
            freq = round(100 * nv / nrow(m), 1)) |>  
  filter(freq > 0) |> 
  pivot_wider(id_cols = value, 
              names_from = name, values_from = freq) |> print()

prep_trans_df <- function(m, measure = "freq"){
  
  # measure <- "freq"
  df <- m |> as.data.frame() |> 
    rownames_to_column("id") |> 
    pivot_longer(cols = -c(id)) |> 
    mutate(unpacked = str_split(value, " ")) |> 
    unnest() |> 
    mutate(value = str_trim(unpacked)) |> 
    dplyr::select(-unpacked) |> 
    group_by(name, value)|> 
    summarise(nv = dplyr::n(), 
              freq = round(100 * nv / nrow(m), 1)) |>  
    filter(freq > 0) |> 
    {\(.) if(measure == "freq"){
      pivot_wider(., id_cols = value, names_from = name, values_from = freq)
    }
      else{
        pivot_wider(., id_cols = value, names_from = name, values_from = nv)
      }
    }()  |> 
    rename(cause = value) |> 
    pivot_longer(cols = -cause) |> 
    as.data.frame() |> 
    filter(cause != "healthy")
  
  
  df$name <- gsub("c","",as.character(df$name))
  df$name <- as.numeric(df$name)
  
  df <- df |> arrange(cause, name)
  
  return(df)
  
}

plotly::ggplotly(ggplot(prep_trans_df(m, measure = "freq"), 
                        aes(x = name, y = value, color = cause, group = cause) ) + 
                   geom_point() + 
                   geom_line() +
                   labs(title = "Disease freq over time", x = "years", y = "freq (%) "))


plotly::ggplotly(ggplot(prep_trans_df(m, measure = "nv"), 
                        aes(x = name, y = value, color = cause, group = cause) ) + 
                   geom_point() + 
                   geom_line() +
                   labs(title = "Disease count over time", x = "years", y = "count (n) "))


# # # Save the diagram
# ggsave(paste0("diagrams/state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".png"), height = 5, width = 10, units = "in", dpi = 600, scale = 1)
# # 
# # # Also save state transitions as a CSV
# arrow::write_dataset(m |> as.data.frame(), paste0("data/exp_dis_inter_off_state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".parquet"))

