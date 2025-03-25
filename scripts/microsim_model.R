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

# For reproducibility set seed
set.seed(2)
options(future.globals.maxSize = +Inf)

# Set sample_size to be greater than zero
sample_size <- 10000

# Number of cycles/years the simulation works
n.c <- 2

# Define DISEASE RISK to incorporate disease interaction
DISEASE_RISK <- FALSE

# Data ----

## Synthetic population file with exposures and physical activity
## Belen (19/02/2025): for the paper we might want to start from the synth pop with PA and exposures and assign RRs.
synth_pop <- read_csv(here("jibe health/base_pp_exposure_RR_2021.csv"))

## Health transitions
hd <- read_csv(here("jibe health/health_transitions_manchester.csv")) 

# Read prevalence dataset
prev <- read_csv("jibe health/prevalence_id.csv")

# Read risk factor
disease_risks <<- read_csv("jibe health/mod_disease_risks.csv")

if (sample_size > 0)
  synth_pop <- sample_n(synth_pop, sample_size)

synth_pop <- synth_pop |> rename(sex = gender)

names(synth_pop) <- gsub("(RR_|base_)", "", names(synth_pop))

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

# 
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


get_state <- function(rd, cycle = 1, cause = "allc", cm, ind_spec_rate, cause_risk = 1) {
  
  # Get unique index for the agent
  vi <- rd["rowname"] |> as.numeric()
  # Get previous state of the agent
  prev_state <-  cm[1] |> as.character()
  # Get current state of the agent
  curr_state <-  cm[2] |> as.character()
  # Create cause specific state name
  curr_cause <- cause
  
  # Get age, sex and disease specific rate
  dis_rate <- rd[cause] |> as.numeric()
  
  if (length(ind_spec_rate) == 0)
    ind_spec_rate <- 0
  
  # Calculate rate by multiplying it with age, gender, disease/cause and type (incidence for diseases and deaths for all_cause_mortality) specific rate
  # that comes from the health dataset. Also multiply it with cause_risk which looks at joint risk of developing a disease for an existence disease/cause.
  # So for instance if someone has a diabetes, their chances of dying becomes higher. This dataset comes from 
  # and also 
  dis_rate <- dis_rate * ind_spec_rate * cause_risk
  
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

# Start multisession
plan(multisession)

# Record the time it takes to run the piece of code with tic()
require(tictoc)
tic()

for (incyc in 1:n.c){
  for (dis in diseases){
    cstate <- future_apply(synth_pop_wprob, 1, function(row) {
      # Set filtered_risk as 1
      risk_factor <- 1
      if (dis == "all_cause_mortality"){
        filtered_rate <- hd[hd$age == row["age"] |> as.numeric() + incyc - 1 &
                              hd$sex == row["sex"] |> as.numeric() &
                              hd$cause == dis &
                              hd$location_code == row["lsoa21cd"] |> as.character() &
                              hd$measure == "deaths", ] |>
          dplyr::select(rate) |> pull()
        
        if (DISEASE_RISK){
          # See if age is between 40 and 70
          if (row["age"] |> as.numeric() + incyc - 1 >= 40 &
              row["age"] |> as.numeric() + incyc - 1 <= 70){
            
            # browser()
            disease_list <- m[row["rowname"] |> as.numeric(),
                              paste0("c", incyc - 1)] |> 
              strsplit(" ") |> unlist()
            
            #Standardize cancer-related diseases to "cancer"
            disease_list <- gsub(".*cancer.*|myeloid_leukemia|myeloma", "cancer", disease_list)
            
            # Filter the data frame based on the given conditions
            filtered_df <- disease_risks[sapply(disease_risks$risk_factor, function(risk) {
              any(sapply(disease_list, function(disease) grepl(disease, risk)))
            }),]
            
            # Calculate the product of relative_risk
            relative_risk_product <- prod(filtered_df$relative_risk, na.rm = TRUE)
            
            if (length(relative_risk_product) != 0)
              risk_factor <- relative_risk_product
            
          }
          
        }
        
        
      }else{
        filtered_rate <- hd[hd$age == row["age"] |> as.numeric() + incyc - 1 &
                              hd$sex == row["sex"] |> as.numeric() &
                              hd$cause == dis &
                              hd$location_code == row["ladcd"] |> as.character() &
                              hd$measure == "incidence", ] |>
          dplyr::select(rate) |> pull()
        if (DISEASE_RISK){
          
          # See if age is over 18
          if (row["age"] |> as.numeric() + incyc - 1 >= 18 & 
              dis %in% c("coronary_heart_disease", "stroke")){
            
            sex_string <- ifelse(row["sex"] |> as.numeric() == 1, "male", "female")
            disease_list <- m[row["rowname"] |> as.numeric(),
                              paste0("c", incyc - 1)] |> 
              strsplit(" ") |> unlist()
            
            # If we are modelling coronary_heart_disease
            if (dis == "coronary_heart_disease"){
              # Filter the data frame based on the given conditions
              filtered_df <- disease_risks[sapply(disease_risks$risk_factor, function(risk) {
                any(sapply(disease_list, function(disease) grepl(disease, risk)))
              }) & grepl(sex_string, disease_risks$sex)&
                disease_risks$outcome == "coronary_heart_disease", ]
            }
            #if we are mdoelling stroke
            else{
              
              # Filter the data frame based on the given conditions
              filtered_df <- disease_risks[sapply(disease_risks$risk_factor, function(risk) {
                any(sapply(disease_list, function(disease) grepl(disease, risk)))
              }) & grepl(sex_string, disease_risks$sex) &
                disease_risks$outcome == "stroke", ]
              
            }
            
            # Calculate the product of relative_risk
            relative_risk_product <- prod(filtered_df$relative_risk, na.rm = TRUE)
            
            if (length(relative_risk_product) != 0)
              risk_factor <- relative_risk_product
            
          }
        }
      }
      
      get_state(row, cycle = incyc, cause = dis,
                cm = m[row["rowname"] |> as.numeric(),
                       c(paste0("c", incyc - 1), paste0("c", incyc))],
                ind_spec_rate = filtered_rate,
                cause_risk = risk_factor)
    }, future.seed = TRUE)
    m[, incyc + 1] <- cstate
  }
}

# Stop recording of time spent running the code
toc()

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

l[l$c == 0 & l$states == 'healthy',]$freq <- 100

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
