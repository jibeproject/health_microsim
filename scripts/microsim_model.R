rm(list = ls())  # remove any variables in R's memory 

# Load libraries
library(tidyverse)
# For efficient dealing of large datases
library(arrow)
library(future)     ## defines %<-%
plan(multisession)

# Read pp health dataset (of Munich) using read_csv_arrow from arrow library
synth_pop <- arrow::read_csv_arrow("../../siloMitoMatsim_modelOutput/pp_health_2012.csv")
synth_pop <- synth_pop |> dplyr::select(id, age, gender, rr_all) |> rename (sex = gender)

# slice 10k rows
synth_pop <- synth_pop |> slice_sample(n = 10000)

# Read probability dataset by age and sex for Australia
back_hdata <- arrow::read_csv_arrow("../../data/sample/mslt_df.csv")
back_hdata <-  back_hdata |>  mutate(sex = case_when(sex == 'male' ~ 1, sex == 'female' ~ 2)) |> dplyr::select(age, sex, deaths_rate_allc)

# Combine baseline demographics and exposure data with background health data
synth_pop <- left_join(synth_pop, back_hdata)

prob_age_sex <- function(data, num_simulations = 10000, seed = 1) {
  set.seed(seed)
  
  # Extract the sickness probability #Here we should also adjust for RRs for exposures and behaviours
  data$sickness_probability <- 1-(exp(-data$deaths_rate_allc))
  
  data$est_prob <- 0
  for (nr in 1:nrow(data)){
    
    # Initialize a variable to store the count of sick individuals
    sick_count <- 0
    
    # Perform Monte Carlo simulations
    for (i in 1:num_simulations) {
      # Simulate sickness for the individual based on probability
      if (runif(1) < data$sickness_probability[nr]) {
        sick_count <- sick_count + 1
      }
    }
    
    # Calculate the estimated probability of getting sick
    data[nr,]$est_prob <- sick_count / num_simulations
  }
  
  return(data)
}
# require(tictoc)
# tic()
synth_pop <- prob_age_sex(synth_pop)
# toc()
# # Model input
# n.i   <- round(synth_pop |> nrow()/10^2)   # number of simulated individuals
# n.t   <- 30                    # time horizon, 30 cycles
# v.n   <- c("H","D")            # the model states: Healthy (H), Sick (S1), Dead (D)
# n.s   <- length(v.n)           # the number of health states
# v.M_1 <- rep("H", n.i)         # everyone begins in the healthy state 
# v.Trt <- c("No Treatment", "Treatment") # store the strategy names
# 
# # Transition probabilities (per cycle)
# p.HD    <- 0.005               # probability to die when healthy
# r.HD    <- -log(1 - p.HD) 	   # rate of death when healthy 
# p.S1D   <- 1 - exp(- r.S1D)    # probability to die when sick
# # rp.S1 <- 0.2                   # increase of the mortality rate with every additional year being sick




