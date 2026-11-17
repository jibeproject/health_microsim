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
library(dbplyr)      # Needed for DuckDB + dplyr integration
library(duckdb)      # Database engine


plan(multisession)

# For reproducibility set seed
set.seed(2)

options(future.globals.maxSize = +Inf)

set_sep = ";"

sample_size <- 1000

# Data ----

## Synthetic population file with exposures and physical activity and relative risks for all cause mortality and diseases

synth_pop <- read_csv(here("manchester/simulationResults/ForUrbanTransition/reference/health/03_exposure_and_rr/pp_rr_2021.csv"))
 
## Wont be neccesary because Qin will produce files with areas. 


hh <- read_csv(here("manchester/simulationResults/ForUrbanTransition/reference/sp_2021_2050/hh_2021.csv"))

dd <- read_csv(here("manchester/simulationResults/ForUrbanTransition/reference/sp_2021_2050/dd_2021.csv"))

zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv")) %>%
  rename(zone=oaID)

# join zones 

synth_pop_2 <- synth_pop %>%
  left_join(hh, by = c("hhid" = "id")) %>%
  left_join(dd, by = c("dwelling" = "id")) %>%
  left_join(zones)

# Take sample to test calculations

if (sample_size > 0)
  synth_pop_2 <- sample_n(synth_pop_2, sample_size)

## Need to add areas to then add health data. 
# synth_pop <- synth_pop %>% 

## Combined rrs per health outcome (May be best to move to function, as has to be done for every future year)

data_long <- synth_pop_2 %>%
  pivot_longer(
    # Capture all columns starting with rr_PHYSICAL_ACTIVITY, rr_AIR_POLLUTION_PM25, or rr_AIR_POLLUTION_NO2
    cols = matches("^rr_PHYSICAL_ACTIVITY|^rr_AIR_POLLUTION_PM25|^rr_AIR_POLLUTION_NO2"),  
    names_to = c("risk_type", "cause"),  
    # Use a simpler pattern to separate risk_type and cause
    names_pattern = "^(rr_PHYSICAL_ACTIVITY|rr_AIR_POLLUTION_PM25|rr_AIR_POLLUTION_NO2)_(.*)",  
    values_to = "value"
  ) %>%
  drop_na(risk_type) %>%
  select(-hhID)


# Create a DuckDB connection
con <- dbConnect(duckdb::duckdb())

# Convert Arrow Table to a data frame (or tibble) for DuckDB
data_df <- as.data.frame(data_long)

# Copy data to DuckDB (temporary table)
copy_to(con, data_df, "data_long", temporary = TRUE, overwrite = TRUE)

# Process data in DuckDB
pif_ind_db <- tbl(con, "data_long") %>%
  pivot_wider(
    names_from = risk_type,   
    values_from = value        
  )

# Collect the data back into R after processing in DuckDB
pif_ind <- collect(pif_ind_db)

# Now perform the row-level processing using `mutate()` locally in R
pif_ind <- pif_ind %>%
  mutate(
    # Combine risk ratios into a list, removing NAs
    valid_rrs = pmap(list(rr_PHYSICAL_ACTIVITY, rr_AIR_POLLUTION_PM25, rr_AIR_POLLUTION_NO2), function(a, b, c) na.omit(c(a, b, c))),  
    
    # Compute PIF only if there are valid risk ratios
    pif = ifelse(
      lengths(valid_rrs) > 0, 
      map_dbl(valid_rrs, ~ prod(.)),  # Compute product of valid risk ratios
      NA_real_
    )
  ) %>%
  select(-valid_rrs)

# Disconnect from DuckDB
dbDisconnect(con, shutdown = TRUE)

## Health transitions

hd_inc <- read_csv(here("manchester/health/processed/health_transitions_manchester.csv")) %>%
  filter(measure != "prevalence") %>%
  mutate(measure = case_when(measure == "deaths" ~ "incidence"))

# hd <- read_csv("D:/Users/aa797/manchester/input/health/health_transitions_manchester.csv") 


# Number of individuals
n.i <- synth_pop_2 |> nrow()

# Number of cycles
n.c <- 5

# everyone begins in the healthy state 
v.M_1 <- rep("healthy", n.i) ### Ali to add prevalence based on health transition data (which is in rates)

# Belen: To add randomly who has the disease/s. How do we combine multiple diseases?

# Function to return 

## Add product of relative risks to multiply for rate for each cause

get_state <- function(rd, cycle = 1, cause = "allc", cm) {
  
  # rd: for each individual
  
  # Get unique index for the agent
  vi <- rd["rowname"] |> as.numeric()
  
  # browser()
  # Get previous state of the agent
  prev_state <-  cm[vi, 1] |> as.character()
  # Get current state of the agent
  curr_state <-  cm[vi, 2] |> as.character()
  # Create cause specific state name
  # curr_cause <- paste0('S_', toupper(cause))
  curr_cause <- cause
  # Get age, sex and disease specific prob (Belen=need to add risk factors-convert to rates, multiply and then back prob)
  dis_prob <- rd[cause] |> as.numeric()
    # Get age, sex and all-cause-mortality prob
  all_cause_prob <- rd["all_cause_mortality"] |> as.numeric()
  
  # print(vi)
  # print(cause)
  
  # If the agent has already died, then they remain in the same 'dead' state
  if (prev_state == 'dead' || (!is.na(curr_state) && curr_state == 'dead') || (all_cause_prob == 0))
    return('dead')
  else{
    # prob <- rbernoulli(1, p = rd[paste0("prob_", cause, "_", cycle)] |> as.numeric())
    # If however the uniform random probablity is greater (ALi, is this lower?) than age and sex specific sick_prob, then transition to the
    # specific cause (curr_cause) takes place
    prob <- runif(1)
    if (!is.na(dis_prob) && prob < dis_prob){ 
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


multiply_similar_suffix_columns <- function(df, un = "exp_") {
  # Get all column names
  col_names <- names(df)
  
  # Function to extract the common part of column names
  extract_common <- function(name) {
    parts <- strsplit(name, un)[[1]]
    length_parts <- length(parts)
    parts[length_parts]
  }
  
  # Get unique common parts
  common_parts <- unique(sapply(col_names, extract_common))
  
  # For each common part, multiply corresponding columns
  for (part in common_parts) {
    matching_cols <- col_names[sapply(col_names, function(x) grepl(part, x))]
    
    if (length(matching_cols) > 1) {
      new_col_name <- paste0("RR_", part)
      df[[new_col_name]] <- Reduce(`*`, df[matching_cols])
    }
  }
  
  return(df)
}


synth_pop <- synth_pop %>% rename_with(~ paste0("exp_", .x), -c("id","age", "sex", "lsoa21cd")) 

# Create a df for (does the same as Belen calculation of RRs)
synth_pop_wprob <- synth_pop_2 |> ## Synthetic pop will have combined RR
  rownames_to_column() |> 
  left_join(hd |> 
              pivot_wider(id_cols = c(age, sex, location_code), 
                          names_from = cause, values_from = rate), 
            join_by(age, sex, lsoa21cd == location_code))

# 
# hd_test <- hd |>
#   pivot_wider(id_cols = c(age, sex, location_code, measure),
#               names_from = cause, values_from = rate)
# 
# hd_group <- hd %>% distinct(age, sex, location_code, cause, .keep_all = TRUE) 


# Replace NAs with 1
#synth_pop_wprob[is.na(synth_pop_wprob)] <- 1 

synth_pop_wprob <- multiply_similar_suffix_columns(synth_pop_wprob |> dplyr::select(-c("id","age", "sex", "lsoa21cd"))) 

# 
# Matrix to save current states
# with dimensions: (rows: number of individuals, cols: number of classes (or years) + 1 (for the 0th year))
m <- matrix(nrow = n.i, ncol = n.c + 1,
            dimnames = list(paste0("id", 1:n.i, sep = ""),
                            paste0("c", 0:n.c, sep = "")))

# The default state is healthy for everyone - before simulation starts
m[,1] <- v.M_1
# Read the names of all columns starting with the 'sick' word, so that list of causes (or diseases) can be generated
diseases <- unique(hd$cause)

synth_pop_wprob <- synth_pop_wprob |> dplyr::select(c("rowname", contains("RR")))

names(synth_pop_wprob) <- sub("RR_", "", names(synth_pop_wprob), fixed = TRUE)

# sm_synth_pop_wprob <- synth_pop_wprob[1:1000,]
# sm_m <- m[1:1000,] |> as.matrix()

# 
# Record the time it takes to run the piece of code with tic()
require(tictoc)
tic()
stop <- F
for (incyc in 1:n.c){
  for (dis in diseases){
    cstate <- list()
    # dis <- diseases[1]
    # for (index in 1:nrow(synth_pop_wprob)){
    #   index <- 1
    #   incyc <- 1
    #       cstate[index] <- get_state(rd = synth_pop_wprob[index,], cycle = incyc,
    #                                  cause = dis, cm = m[, c(paste0("c", incyc - 1), paste0("c", incyc))])
    # 
    # }
    # 
    # print(table(unlist(cstate)))
    cstate <- future_apply(synth_pop_wprob, 1, get_state,
                           cycle = incyc, cause = dis,
                           cm = m[, c(paste0("c", incyc - 1), paste0("c", incyc))], future.seed = T)

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
    summarise(cn = n()) |> 
    mutate(freq = round(cn / sum(cn) * 100, 1), c = ind) |> 
    dplyr::select(-cn)
  l <- plyr::rbind.fill(l, tbl)
}

l[l$c == 0 & l$states == 'healthy',]$freq <- 100

l$c <- as.factor(l$c)

l <- l |> filter(!is.na(states))

# Generate historic state transitions of all diseases + dead
ggplot(l) +
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
arrow::write_dataset(m |> as.data.frame(), paste0("data/state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".parquet"))
