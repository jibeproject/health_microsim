# remove any variables in R's memory 
rm(list = ls()) 

# Load libraries
library(tidyverse)
# For parallel processing
library(future.apply)

plan(multisession)

# For reproducibility set seed
set.seed(1)

# Define global number of monte-carlo sims
num_sims <- 100

# Read pp health dataset (of Munich) using read_csv_arrow from arrow library
synth_pop <- read_csv("data/siloMitoMatsim_modelOutput/pp_health_2012.csv")

# slice 1k rows
synth_pop <- synth_pop |> slice_sample(n = 1000)

synth_pop <- synth_pop |> dplyr::select(id, age, gender, rr_all) |> 
  rename (sex = gender)


# Number of individuals
n.i <- synth_pop |> nrow()

# Number of cycles
n.c <- 5

# everyone begins in the healthy state 
v.M_1 <- rep("H", n.i)

# Read probability dataset by age and sex for Australia
back_hdata <- read_csv("data/sample/mslt_df.csv")

# Mutate sex to numeric values with male ~ 1 and female ~ 2
# Also modify all columns with deaths rate into prob by 1 - exp(-deaths_rate)
back_hdata <-  back_hdata |>  
  rename_with(~ gsub("deaths_rate", "sick_prob", .x, fixed = TRUE)) |> 
  mutate(sex = case_when(sex == 'male' ~ 1, sex == 'female' ~ 2),
         across(starts_with("sick_prob"), ~(1 - (exp(-.))))) |> 
  dplyr::select(age, sex, starts_with("sick_prob"))

# Combine baseline demographics and exposure data with background health data
synth_pop <- left_join(synth_pop, back_hdata)

prob_age_sex <- function(data, hdata, colname = "sick_prob_allc", num_sims = 1000, seed = 1, cycle = 1) {
  set.seed(seed)
  res <- list()
  # Get age and sex specific sick_prob from background death rate
  # The cycle is the current year and is added to the baseline's age of an individuals to get the current age
  # So age = baseline_age + cycle - 1 - as cycle starts with 1 hence subtracting 1 from it
  nsick_prob <- hdata |> 
    filter(age == (data["age"] |> as.numeric()) + cycle - 1, sex == c(data["sex"] |> as.numeric())) |> 
    dplyr::select(colname) |> 
    pull()
  # If sick_prob is undefined then return 0
  if (length(nsick_prob) == 0){
    nsick_prob <- 0
    res <- 0
  }else{
    # Run monte_carlo sim to calcualte sick_prob for each individual
    # Initialize a variable to store the count of sick individuals
    sick_count <- 0
    # Perform Monte Carlo simulations
    for (i in 1:num_sims) {
      # Simulate sickness for the individual based on probability
      if (runif(1) < nsick_prob) {
        sick_count <- sick_count + 1
      }
    }
    # Calculate the estimated probability of getting sick
    res <- sick_count / num_sims
  }
  return(res)
}

get_state <- function(rd, cycle = 1, cause = "allc", cm) {
  vi <- rd["rowname"] |> as.numeric()
  prev_state <-  cm[vi, 1] |> as.character()
  curr_state <-  cm[vi, 2] |> as.character()
  if (prev_state == 'D' || 
      (!is.na(curr_state) && curr_state == 'D') || 
      rd[paste0("prob_allc_", cycle)] |> as.numeric() == 0)
    return('D')
  if (!is.na(curr_state) && !curr_state %in% c('D','H'))
    return(curr_state)
  else{
    prob <- runif(1)
    if (cause != 'allc' && prob < rd[paste0("prob_", cause, "_", cycle)] |> as.numeric())
      return(paste0('S_', toupper(cause)))
    else if (cause == 'allc' && prob < rd[paste0("prob_allc_", cycle)] |> as.numeric())
      return('D')
    else if (prev_state == paste0('S_', toupper(cause)))
      return(paste0('S_', toupper(cause)))
    else
      return('H')
  }
  
}  
# Create a df for 
synth_pop_wprob <- synth_pop|> rownames_to_column()

# Matrix to save current states
m <- matrix(nrow = n.i, ncol = n.c + 1, 
            dimnames = list(paste0("id", 1:n.i, sep = ""), 
                            paste0("c", 0:n.c, sep = "")))

# The default state is healthy for everyone - before simulation starts
m[,1] <- v.M_1
diseases <- sapply(strsplit(back_hdata |> dplyr::select(starts_with("sick")) |> names(), "\\_"), "[", 3)
require(tictoc)
tic()
for (i in 1:n.c){
  for (dis in diseases){#diseases){
    # i <- 1; dis <- "adaod"
    # dis <- diseases[i]
    ind_prob <- future_apply(synth_pop_wprob, 1, prob_age_sex, future.seed = T,
                             hdata = back_hdata, cycle = i, num_sims = num_sims, colname = paste0("sick_prob_", dis))
    # if (sum(ind_prob) == 0){
    #   break
    # }
    synth_pop_wprob <- bind_cols(synth_pop_wprob, ind_prob)
    names(synth_pop_wprob)[synth_pop_wprob |> length()] <- paste0("prob_", dis, "_", i)
    # print(dis)
  }
  
  for (dis in diseases){
    cstate <- future_apply(synth_pop_wprob, 1, get_state, cycle = i, cause = dis, cm = m[, c(paste0("c", i - 1), paste0("c", i))], future.seed = T)
    m[, i + 1] <- cstate
    print(paste("cycle ", i))
    print(table(m[, i + 1]))
  }
  # Break the loop if everyone has died
  # if (cstate |> table() |> as.data.frame() |> dplyr::select(Freq) |> nrow() == 1)
  #   break
}
toc()