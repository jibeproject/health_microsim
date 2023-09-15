rm(list = ls())  # remove any variables in R's memory 

# Load libraries
library(tidyverse)
# For efficient dealing of large datases
library(arrow)
library(furrr)     ## defines %<-%
library(future.apply)
options(future.apply.debug = F)
plan(multisession)

# Define global number of sims
num_sim <- 10

# Read pp health dataset (of Munich) using read_csv_arrow from arrow library
synth_pop <- arrow::read_csv_arrow("data/siloMitoMatsim_modelOutput/pp_health_2012.csv")
synth_pop <- synth_pop |> dplyr::select(id, age, gender, rr_all) |> 
  rename (sex = gender)

# slice 1k rows
synth_pop <- synth_pop |> slice_sample(n = 1000)

# Number of individuals
n.i <- synth_pop |> nrow()

# Number of cycles
n.c <- 10

# everyone begins in the healthy state 
v.M_1 <- rep("H", n.i)

# Read probability dataset by age and sex for Australia
back_hdata <- arrow::read_csv_arrow("data/sample/mslt_df.csv")
back_hdata <-  back_hdata |>  
  mutate(sex = case_when(sex == 'male' ~ 1, sex == 'female' ~ 2),
         sick_prob = 1 - (exp(-deaths_rate_allc))) |> 
  dplyr::select(age, sex, sick_prob)

# Combine baseline demographics and exposure data with background health data
synth_pop <- left_join(synth_pop, back_hdata)

prob_age_sex <- function(data, hdata, num_simulations = 100, seed = 1, cycle = 1) {
  set.seed(seed)
  res <- list()
  # Get age and sex specific sick_prob from background death rate
  # The cycle is the current year and is added to the baseline's age of an individuals to get the current age
  # So age = baseline_age + cycle - 1 - as cycle starts with 1 hence subtracting 1 from it
  nsick_prob <- hdata |> 
    filter(age == (data["age"] |> as.numeric()) + cycle - 1, sex == c(data["sex"] |> as.numeric())) |> 
    dplyr::select(sick_prob) |> 
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
    for (i in 1:num_simulations) {
      # Simulate sickness for the individual based on probability
      if (runif(1) < nsick_prob) {
        sick_count <- sick_count + 1
      }
    }
    
    # Calculate the estimated probability of getting sick
    res <- sick_count / num_simulations
  }
  # # Reduce prob by a fixed probability of 0.005 
  # res <- ifelse(data[paste0("est_prob", cycle - 1)] - p.HD < 0, 0, 
  #                                           data[paste0("est_prob", cycle - 1)] - p.HD)
  return(res)
}

get_state <- function(rd, seed = 1, cycle = 1, cm) {
  set.seed(seed)
  vi <- rd["rowname"] |> as.numeric()
  vcm <-  cm[vi] |> as.character()
  if (vcm == 'D' || rd[paste0("est_prob", cycle)] |> as.numeric() == 0)
    return('D')
  else{
    return(ifelse(runif(1) > rd[paste0("est_prob", cycle)] |> as.numeric(), 'H', 'D'))
  }
  
  # print(class(rd))
  # print(rd[paste0("est_prob", cycle)] |> as.numeric())
  # if (rd[paste0("est_prob", cycle)] |> as.numeric() == 0)
  #   return('D')
  # else{
  #   return(ifelse(runif(1) > rd[paste0("est_prob", cycle)] |> as.numeric(), 'H', 'D'))
  # }
  
}  

synth_pop_wprob <- synth_pop

synth_pop_wprob <- synth_pop_wprob |> rownames_to_column()

# Matrix to save current states
m <- matrix(nrow = n.i, ncol = n.c + 1, 
            dimnames = list(paste0("id", 1:n.i, sep = " "), 
                            paste0("c", 0:n.c, sep = " ")))
m[,1] <- v.M_1
require(tictoc)
tic()
for (i in 1:n.c){
  # td <- future_apply(synth_pop_wprob, 1, prob_age_sex, future.seed = T, hdata = back_hdata, cycle = i)
  # if (sum(td) == 0){
  #   break
  # }
  # synth_pop_wprob <- bind_cols(synth_pop_wprob, td)
  # names(synth_pop_wprob)[synth_pop_wprob |> length()] <- paste0("est_prob", i)
  # i <- 1
  cstate <- future_apply(synth_pop_wprob, 1, get_state, cycle = i, cm = m[, i], future.seed = T)
  m[, i + 1] <- cstate
  print(paste("cycle ", i))
  print(table(m[, i + 1]))
  if (cstate |> table() |> as.data.frame() |> dplyr::select(Freq) |> nrow() == 1)
    break
  # df <- cbind(m[, i], cstate) |> as.data.frame()
  # if (df[df[,1] == 'D', ] |> nrow() > 0){
  #   df[df[,1] == 'D', ][,2] <- 'D'
  #   m[, i + 1] <- df[,2]
  # }
}
toc()



#################################################
## Matrix implementation which is a little faster
#################################################
# m <- synth_pop |> as.matrix()
# require(tictoc)
# tic()
# for (i in 1:10){
#   td <- future_apply(m,1,prob_age_sex, future.seed = T, cycle = i)
#   if (sum(td) == 0){
#     # print("Stopping at index ", i)
#     break
#   }
#   m <- bind_cols(m, td)
#   colnames(m)[m |> ncol()] <- paste0("est_prob", i)
# }
# toc()


# 
# tic()
# td1 <- t(future_apply(synth_pop,1,prob_age_sex, future.seed = T, cycle = 2)) |> as.data.frame()
# toc()
# 
# df <- list()
# df[[1]] <- td
# for (i in 2:10){
# df[[i]] <- t(future_apply(df[[i - 1]],1,prob_age_sex, future.seed = T, cycle = i)) |> as.data.frame()
# }
# 
# n.i <- td |> nrow()
# n.t <- 1000
# 
# # Create the matrix capturing the state name/costs/health outcomes for all individuals at each time point 
# m.M <- matrix(nrow = n.i, ncol = n.t + 2,
#                             dimnames = list(paste("ind", 1:n.i, sep = " "), 
#                                             c("est_prob", paste("cycle", 0:n.t, sep = " "))))  
# 
# m.M[, 1] <- td$est_prob1
# td$temp <-  apply(td, 1, function(x)
# {
#   result <- 0
#   if((x["est_prob1"] - p.HD) < 0) result <- 0
#   else result <- x["est_prob1"] - p.HD
#   return(result)
# })
# 
# for (t in 1:1000){#n.t) { # t <- 3
#   m.p <- apply(m.M, 1, function(x, cycle)
#   {
#     if (sum(m.M[, 1]) == 0)
#       break
#     # print(cycle)
#     result <- 0
#     if((x[cycle] - p.HD) < 0) result <- 0
#     else result <- x[cycle] - p.HD
#     return(result)
#   }, cycle = t)
#   m.M[, t + 1] <- m.p
# }
# 
# # # Model input
# # n.i   <- round(synth_pop |> nrow()/10^2)   # number of simulated individuals
# # n.t   <- 30                    # time horizon, 30 cycles
# # v.n   <- c("H","D")            # the model states: Healthy (H), Sick (S1), Dead (D)
# # n.s   <- length(v.n)           # the number of health states
# # v.M_1 <- rep("H", n.i)         # everyone begins in the healthy state 
# # v.Trt <- c("No Treatment", "Treatment") # store the strategy names
# # 
# # p.S1D   <- 1 - exp(- r.S1D)    # probability to die when sick
# # # rp.S1 <- 0.2                   # increase of the mortality rate with every additional year being sick
# 
# 
# 
# 
