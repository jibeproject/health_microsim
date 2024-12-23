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

sample_size <- 0
synth_pop <- read_csv(here("data/manchester/cyc_pp_exposure_RR_2021.csv"))


if (sample_size > 0)
  synth_pop <- sample_n(synth_pop, sample_size)

## Clean column names
# Remove individual causes that have a combined effect
synth_pop <- synth_pop |> dplyr::select(-contains(c("all_cause", "IHD", "lung_cancer", "stroke", "T2D", "CVD")), starts_with("RR_cyc"))

# Remove prefix for AP, PA and combined RRs
synth_pop <- synth_pop |> rename_all(~ stringr::str_replace(., regex("^RR_ap_cyc_|^RR_pa_cyc_|^RR_cyc_", ignore_case = TRUE), ""))

synth_pop <- synth_pop |> rename(all_cause_mortality = all_cause, 
                                  diabetes = T2D,
                                  all_cause_dementia = total_dementia,
                                  coronary_heart_disease = IHD,
                                  endometrial_cancer = endo_cancer,
                                  sex = gender) |> janitor::clean_names()
# sort(names(synth_pop))
# [1] "age"              "all_cause"        "breast_cancer"    "colon_cancer"     "COPD"             "CVD"              "endo_cancer"     
# [8] "gender"           "head_neck_cancer" "id"               "IHD"              "liver_cancer"     "LRI"              "lsoa21cd"        
# [15] "lung_cancer"      "myeloid_leukemia" "myeloma"          "Parkinson"        "respiratory"      "stomach_cancer"   "stroke"          
# [22] "T2D"              "total_cancer"     "total_dementia"  

 

hd <- read_csv("D:/Users/aa797/manchester/input/health/health_transitions_manchester.csv") 


# vigorous_mmet = 3
# 
# # Read manchester specific synth_pop filename
# synth_pop <- synth_pop |> 
#   ungroup() |> 
#   dplyr::select(id, age, gender, mmetHr_cycle, mmetHr_walk) |> 
#   rename (sex = gender) %>%
#   # mutate(total_tr_pa = mmetHr_cycle + mmetHr_walk, total_non_tr_pa = otherSport_wkhr * vigorous_mmet,
#   #                                 total_mmet = total_tr_pa+total_non_tr_pa) 
#   {if (sample_size > 0) sample_n(., sample_size) else .}


# Number of individuals
n.i <- synth_pop |> nrow()

# Number of cycles
n.c <- 5

# everyone begins in the healthy state 
v.M_1 <- rep("healthy", n.i)

# Function to return 

get_state <- function(rd, cycle = 1, cause = "allc", cm) {
  
  
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
  # Get age, sex and disease specific prob
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
    # If however the uniform random probablity is greater than age and sex specific sick_prob, then transition to the
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

# Create a df for 
synth_pop_wprob <- synth_pop |> 
  rownames_to_column() |> 
  left_join(hd |> 
              pivot_wider(id_cols = c(age, sex, lsoa21cd), 
                          names_from = cause, values_from = prob), 
            join_by(age, sex, lsoa21cd))

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
