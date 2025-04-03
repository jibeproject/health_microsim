# Load libraries
library(tidyverse)
# For parallel processing
library(future.apply)
# For directory/file structure
library(here)
# For DR PA 
library(drpa)
# For fast reading/processing
library(arrow)
library(tictoc)
library(data.table)  # For faster data operations
library(stringi)     # For faster string operations

# For reproducibility set seed
set.seed(2)
options(future.globals.maxSize = +Inf)

# Set sample_pro to be greater than zero
sample_prop <- 0.001

# Number of cycles/years the simulation works
n.c <- 50

# Define DISEASE RISK to incorporate disease interaction
DISEASE_RISK <- TRUE

# Data ----

## Synthetic population file with exposures and physical activity

# synth_pop <- read_csv(here("jibe health/ref_pp_exposure_RR_2021.csv"))

synth_pop <- read_csv("manchester/health/processed/ref_pp_exposure_RR_2021.csv")

## Health transitions
# hd <- read_csv(here("jibe health/health_transitions_manchester.csv")) # |> filter(cause != "head_neck_cancer")

hd <- read_csv("manchester/health/processed/health_transitions_manchester.csv")

hd[hd$cause == "head_neck_cancer",]$cause <- "head_and_neck_cancer"

# Read prevalence dataset
# prev <- read_csv("jibe health/prevalence_id.csv")

prev <- read_csv("manchester/health/processed/prevalence_id.csv")

# Read zones dataset
# zones <- read_csv(here("jibe health/zoneSystem.csv"))
zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv"))

# Read risk factor
# disease_risks <<- read_csv("jibe health/mod_disease_risks.csv")

disease_risks <<- read_csv("health/mod_disease_risks.csv")

if (sample_prop > 0){
  synth_pop <- synth_pop  |> 
    group_by(age, gender, ladcd) |> 
    sample_frac(sample_prop)
}

synth_pop <- synth_pop |> rename(sex = gender)

names(synth_pop) <- gsub("(RR_|base_|base_)", "", names(synth_pop))

# Number of individuals
n.i <- synth_pop |> nrow()

# Function to multiply columns with a given suffix and remove original columns
multiply_columns_with_suffix <- function(df, suffix) {
  # Find columns that end with the given suffix
  columns_to_multiply <- names(df)[str_detect(names(df), paste0("_", suffix, "$"))]
  
  # If there are columns to multiply
  if (length(columns_to_multiply) > 0) {
    # Create a new column name
    new_column_name <- paste0("all_path_", suffix)
    
    # Multiply the columns
    df[[new_column_name]] <- apply(df[columns_to_multiply], 1, prod)
    
    # Remove original columns
    df <- df %>% dplyr::select(-all_of(columns_to_multiply))
  }
  
  return(df)
}

# Function to process all suffixes and remove original columns after multiplication
process_all_suffixes <- function(df, cause_df) {
  # Extract unique suffixes from the 'cause' column of the cause data frame
  suffixes <- unique(cause_df$cause)
  
  # Apply multiplication for each suffix and remove original columns
  for (suffix in suffixes) {
    df <- multiply_columns_with_suffix(df, suffix)
  }
  
  return(df)
}

synth_pop_wprob <- synth_pop

synth_pop_wprob <- process_all_suffixes(synth_pop_wprob, 
                                        hd |> dplyr::select(cause) |> distinct()) 

names(synth_pop_wprob) <- str_replace(names(synth_pop_wprob), "^all_path_|pm_|ap_|pa_", "")

synth_pop_wprob <- synth_pop_wprob |> tibble::rowid_to_column("rowname")

synth_pop_wprob <- synth_pop_wprob |>
  rename(
    "parkinson" = "parkinson's_disease"
  )


# Matrix to save current states
# with dimensions: (rows: number of individuals, cols: number of classes (or years) + 1 (for the 0th year))

m <- matrix(nrow = n.i, ncol = n.c + 1,
            dimnames = list(paste0("id", 1:n.i, sep = ""),
                            paste0("c", 0:n.c, sep = "")))


td <- synth_pop_wprob |> 
  left_join(prev) |> 
  dplyr::select(id, diseases) |> 
  mutate(diseases = case_when(is.na(diseases) ~ "healthy", 
                              TRUE ~ diseases)) |> 
  arrange(id)

# The default state is healthy for everyone - before simulation starts
m[,1] <- td$diseases

# Create a list of diseases from teh burden data
diseases <- unique(hd$cause)



# Vectorized version of get_state
get_state_vectorized <- function(rd, cycle, cause, cm, ind_spec_rate, cause_risk = 1) {
  # Convert inputs to vectors
  vi <- as.numeric(rd[, "rowname"])
  prev_state <- as.character(cm[, 1])
  curr_state <- as.character(cm[, 2])
  
  # Calculate disease probability
  dis_rate <- as.numeric(rd[, cause]) * ind_spec_rate * cause_risk
  dis_prob <- 1 - exp(-dis_rate)
  all_cause_prob <- as.numeric(rd[, "all_cause_mortality"])
  
  # Initialize result with current state (falling back to previous state if NA)
  result <- ifelse(is.na(curr_state), prev_state, curr_state)
  
  # Pre-compute conditions
  already_dead <- (!is.na(curr_state) & (prev_state == 'dead' | curr_state == 'dead')) | (all_cause_prob == 0)
  transition_condition <- !already_dead & !is.na(dis_prob) & (runif(length(dis_prob)) < dis_prob)
  
  # Handle all-cause mortality
  if (cause == "all_cause_mortality") {
    result[transition_condition] <- "dead"
  } else {
    # Handle other causes
    to_update <- which(transition_condition)
    if (length(to_update) > 0) {
      curr_state_upd <- curr_state[to_update]
      curr_state_upd[is.na(curr_state_upd)] <- prev_state[to_update][is.na(curr_state_upd)]
      
      # Check if cause already exists
      has_cause <- stri_detect_fixed(curr_state_upd, cause)
      
      # Update states - modified to ensure proper spacing
      new_states <- ifelse(has_cause,
                           curr_state_upd,
                           stri_replace_all_fixed(
                             stri_trim(
                               stri_paste(cause, " ", curr_state_upd)  # Added space between cause and curr_state_upd
                             ),
                             "healthy", ""))
      
      # Clean up any double spaces that might have been created
      new_states <- stri_replace_all_regex(new_states, "\\s+", " ")
      new_states <- stri_trim(new_states)
      
      result[to_update] <- new_states
    }
  }
  
  # Ensure dead stays dead
  result[already_dead] <- "dead"
  
  return(result)
}

# Pre-process data for faster access
prep_health_data <- function(hd) {
  # Convert to data.table for faster lookups
  setDT(hd)
  
  # Create keys for fast lookups
  if ("location_code" %in% names(hd)) {
    hd[, lookup_key := paste(age, sex, cause, location_code, measure, sep = "|")]
  } else {
    hd[, lookup_key := paste(age, sex, cause, measure, sep = "|")]
  }
  
  # Create a named vector of rates for O(1) lookups
  setkey(hd, lookup_key)
  return(hd)
}

# Pre-process disease risks
prep_disease_risks <- function(disease_risks) {
  setDT(disease_risks)
  return(disease_risks)
}

# Main simulation function
run_simulation <- function(synth_pop_wprob, m, hd, disease_risks, n.c, diseases, DISEASE_RISK = TRUE) {
  # Prepare data for fast access
  hd_prepped <- prep_health_data(hd)
  if (DISEASE_RISK) disease_risks_prepped <- prep_disease_risks(disease_risks)
  
  # Convert to matrix for faster column access
  synth_matrix <- as.matrix(synth_pop_wprob)
  
  # Main simulation loop
  for (incyc in 1:n.c) {
    for (dis in diseases) {
      # Calculate current age for all individuals
      current_age <- as.numeric(synth_matrix[, "age"]) + incyc - 1
      
      # Prepare lookup keys for health data
      if (dis == "all_cause_mortality") {
        location_col <- "lsoa21cd"
      } else {
        location_col <- "ladcd"
      }
      
      lookup_keys <- paste(current_age,
                           synth_matrix[, "sex"],
                           dis,
                           synth_matrix[, location_col],
                           ifelse(dis == "all_cause_mortality", "deaths", "incidence"),
                           sep = "|")
      
      # Get rates in bulk
      filtered_rates <- hd_prepped[.(lookup_keys), rate, on = "lookup_key"]
      filtered_rates[is.na(filtered_rates)] <- 0
      
      # Calculate risk factors in bulk if needed
      if (DISEASE_RISK) {
        risk_factors <- rep(1, nrow(synth_matrix))
        
        if (dis == "all_cause_mortality") {
          age_condition <- current_age >= 40 & current_age <= 70
          if (any(age_condition)) {
            # Get previous states for relevant individuals
            prev_states <- m[age_condition, paste0("c", incyc - 1)]
            
            # Process disease lists in bulk
            disease_lists <- stri_split_fixed(prev_states, " ")
            disease_lists <- lapply(disease_lists, function(x) {
              stri_replace_all_regex(x, ".*cancer.*|myeloid_leukemia|myeloma", "cancer")
            })
            
            # Calculate risk factors in bulk
            risk_factors[age_condition] <- sapply(disease_lists, function(dl) {
              if (length(dl) == 0) return(1)
              prod(disease_risks_prepped[sapply(risk_factor, function(rf) any(stri_detect_fixed(dl, rf))), 
                                         relative_risk], na.rm = TRUE)
            })
          }
        } else if (dis %in% c("coronary_heart_disease", "stroke")) {
          age_condition <- current_age >= 18
          sex_condition <- ifelse(synth_matrix[, "sex"] == 1, "male", "female")
          relevant_cases <- age_condition & (dis %in% c("coronary_heart_disease", "stroke"))
          
          if (any(relevant_cases)) {
            prev_states <- m[relevant_cases, paste0("c", incyc - 1)]
            disease_lists <- stri_split_fixed(prev_states, " ")
            sex_strings <- sex_condition[relevant_cases]
            
            risk_factors[relevant_cases] <- mapply(function(dl, sex_str) {
              if (length(dl) == 0) return(1)
              outcome <- if (dis == "coronary_heart_disease") "coronary_heart_disease" else "stroke"
              prod(disease_risks_prepped[
                sapply(risk_factor, function(rf) any(stri_detect_fixed(dl, rf))) & 
                  stri_detect_fixed(sex, sex_str) & 
                  outcome == outcome, 
                relative_risk], na.rm = TRUE)
            }, disease_lists, sex_strings)
          }
        }
      } else {
        risk_factors <- rep(1, nrow(synth_matrix))
      }
      
      # Get current and previous states in bulk
      cm <- m[, c(paste0("c", incyc - 1), paste0("c", incyc))]
      
      # Update states in bulk
      m[, incyc + 1] <- get_state_vectorized(
        rd = synth_matrix,
        cycle = incyc,
        cause = dis,
        cm = cm,
        ind_spec_rate = filtered_rates,
        cause_risk = risk_factors
      )
    }
  }
  
  return(m)
}

# Run the simulation
plan(multisession)
tic()
m <- run_simulation(synth_pop_wprob, m, hd, disease_risks, n.c, diseases, DISEASE_RISK)
toc()


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

