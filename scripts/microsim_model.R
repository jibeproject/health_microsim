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

library(glue)

plan(multisession)

# For reproducibility set seed
set.seed(2)

options(future.globals.maxSize = +Inf)

set_sep = ";"

sample_size <- 1000

# Data ----

## Synthetic population file with exposures and physical activity
## Belen (19/02/2025): for the paper we might want to start froom the synth pop with PA and exposures and assign RRs.
synth_pop <- read_csv(here("jibe health/base_pp_exposure_RR_2021.csv"))

## Health transitions

hd <- read_csv(here("jibe health/health_transitions_manchester.csv")) 


if (sample_size > 0)
  synth_pop <- sample_n(synth_pop, sample_size)

synth_pop <- synth_pop |> rename(sex = gender)

names(synth_pop) <- gsub("(RR_|base_)", "", names(synth_pop))

# Number of individuals
n.i <- synth_pop |> nrow()

# Number of cycles
n.c <- 5

# everyone begins in the healthy state 
v.M_1 <- rep("healthy", n.i)

# Function to return 

get_state <- function(rd, cycle = 1, cause = "allc", cm, ind_spec_rate) {
  
  # browser()
  # Get unique index for the agent
  vi <- rd["rowname"] |> as.numeric()
  
  # Get previous state of the agent
  prev_state <-  cm[1] |> as.character()
  # Get current state of the agent
  curr_state <-  cm[2] |> as.character()
  # Create cause specific state name
  # curr_cause <- paste0('S_', toupper(cause))
  curr_cause <- cause
  # Get age, sex and disease specific prob
  dis_rate <- rd[cause] |> as.numeric()
  
  if (length(ind_spec_rate) == 0)
    ind_spec_rate <- 0
  
  dis_rate <- dis_rate * ind_spec_rate
  
  dis_prob <- 1 - exp(-dis_rate)
  
    # Get age, sex and all-cause-mortality prob
  all_cause_prob <- rd["all_cause_mortality"] |> as.numeric()
  
  # If the agent has already died, then they remain in the same 'dead' state
  if ((!is.na(curr_state) && prev_state == 'dead') || (!is.na(curr_state) && curr_state == 'dead') || (all_cause_prob == 0))
    return('dead')
  else{
    # If however the uniform random probability is lower than age and sex specific sick_prob, then transition to the
    # specific cause (curr_cause) takes place
    prob <- runif(1)
    # if (!is.na(dis_prob) & prob < dis_prob){
    if (!any(is.na(dis_prob)) && all(prob < dis_prob)) {
      # All-cause mortality is a special cause that takes an agent to the 'dead' state
      if (cause == 'all_cause_mortality')
        return ('dead')
      else{
        # If the cause is not all-cause, use current cause (curr_cause) as the new state
        if(is.na(curr_state))
          return (curr_cause)
        else{
          # If the curr_cause already exists in the history of the agent, nothing happens as the agent already has that specific cause
          # In the else section, we make sure that if an agent catches a specific disease, we explicitly remove the healthy state from it
          if (grepl(curr_cause, curr_state, ignore.case = F))
            return(curr_state)
          else
            return(trimws(str_replace_all(paste(curr_cause, curr_state, sep = " "), " healthy", "")))
          # return(unique(unlist(strsplit(trimws(str_replace_all(paste(curr_cause, curr_state, sep = " "), " healthy", "")), " "))))
        }
      }
    }else{
      # The agent remains in the previous state if the transition probablity was greater than random uniform sample of 1
      return(ifelse(is.na(curr_state), prev_state, curr_state))
    }
  }
}


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

names(synth_pop_wprob) <- str_replace(names(synth_pop_wprob), "^all_path_|ap_|pa_", "")

synth_pop_wprob <- synth_pop_wprob |> tibble::rowid_to_column("rowname")

synth_pop_wprob <- synth_pop_wprob |> 
  rename(
    "parkinson" = "parkinson's_disease",
    "copd" = "COPD"
  ) 

# 
# Matrix to save current states
# with dimensions: (rows: number of individuals, cols: number of classes (or years) + 1 (for the 0th year))
m <- matrix(nrow = n.i, ncol = n.c + 1,
            dimnames = list(paste0("id", 1:n.i, sep = ""),
                            paste0("c", 0:n.c, sep = "")))


# Read prevalence dataset
prev <- read_csv("jibe health/prevalence_id.csv")

td <- synth_pop_wprob |> 
  left_join(prev) |> 
  dplyr::select(id, diseases) |> 
  mutate(diseases = case_when(is.na(diseases) ~ "healthy", 
                              TRUE ~ diseases)) |> 
  arrange(id)

# The default state is healthy for everyone - before simulation starts
m[,1] <- td$diseases

#rownames(m) <- td$id
# Read the names of all columns starting with the 'sick' word, so that list of causes (or diseases) can be generated
diseases <- unique(hd$cause)

# 
# Record the time it takes to run the piece of code with tic()
require(tictoc)
tic()
stop <- F

for (incyc in 1:n.c){
  for (dis in diseases){
    cstate <- future_apply(synth_pop_wprob, 1, function(row) {
      if (dis == "all_cause_mortality"){
        filtered_rate <- hd[hd$age == row["age"] |> as.numeric() + incyc - 1 &
                            hd$sex == row["sex"] |> as.numeric() &
                            hd$cause == dis &
                            hd$location_code == row["lsoa21cd"] |> as.character() &
                            hd$measure == "deaths", ] |>
          dplyr::select(rate) |> pull()
        }else{
          filtered_rate <- hd[hd$age == row["age"] |> as.numeric() + incyc - 1 &
                              hd$sex == row["sex"] |> as.numeric() &
                              hd$cause == dis &
                              hd$location_code == row["ladcd"] |> as.character() &
                              hd$measure == "incidence", ] |>
            dplyr::select(rate) |> pull()
        }
      get_state(row, cycle = incyc, cause = dis,
                cm = m[row["rowname"] |> as.numeric(),
                       c(paste0("c", incyc - 1), paste0("c", incyc))],
                ind_spec_rate = filtered_rate)
    }, future.seed = TRUE)
    m[, incyc + 1] <- cstate
  }
}

# Stop recording of time spent running the code
toc()

# Create individual states, while ignoring the all_cause_mortality state as dead state already captures it
l <- data.frame(states = c('healthy', 'dead', diseases |> str_subset(pattern = "all_cause_mortality", negate = TRUE)), freq = 0, c = 0)
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

l[l$c == 0 & l$states == 'healthy',]$freq <- 100

l$c <- as.factor(l$c)

l <- l |> filter(!is.na(states))

# Generate historic state transitions of all diseases + dead
ggplot(l |> filter(freq > 0)) +
  aes(x = c, y = freq, fill = states) +
  geom_col() +
  labs(x = "Years", y = "Frequency (%)", title = "State transitions over the years") +
  theme_minimal()


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


# # # Save the diagram
# ggsave(paste0("diagrams/state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".png"), height = 5, width = 10, units = "in", dpi = 600, scale = 1)
# # 
# # # Also save state transitions as a CSV
# arrow::write_dataset(m |> as.data.frame(), paste0("data/state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".parquet"))
