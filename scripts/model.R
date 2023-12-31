############################################################################################
################# Microsimulation modeling #### 2023 ###################
############################################################################################
# This code is for microsimulation model with three states - Healthy, Disease, Death
############################################################################################


## @Citation
############################################################################################
################# Microsimulation modeling using R: a tutorial #### 2018 ###################
############################################################################################
# This code forms the basis for the microsimulation model of the article: 
#
# Krijkamp EM, Alarid-Escudero F, Enns EA, Jalal HJ, Hunink MGM, Pechlivanoglou P. 
# Microsimulation modeling for health decision sciences using R: A tutorial. 
# Med Decis Making. 2018;38(3):400–22.
#
# Please cite the article when using this code
#
# See GitHub for more information or code updates
# https://github.com/DARTH-git/Microsimulation-tutorial
# 
#
# To program this tutorial we made use of 
# R: 3.3.0 GUI 1.68 Mavericks build (7202)
# RStudio: Version 1.0.136 2009-2016 RStudio, Inc.

############################################################################################
################# Code of Appendix B with samplev #######################################################
############################################################################################
rm(list = ls())  # remove any variables in R's memory 

# Load libraries
library(tidyverse)
# For efficient dealing of large datases
library(arrow)

# Read pp health dataset (of Munich)
# synth_pop <- open_dataset(
#   sources = "../../siloMitoMatsim_modelOutput/pp_health_2012.csv",
#   format = "csv"
# )

# Read pp health dataset (of Munich) using read_csv_arrow from arrow library
# synth_pop <- arrow::read_csv_arrow("../../siloMitoMatsim_modelOutput/pp_health_2012.csv")

# Read probability dataset by age and sex for Australia
# back_hdata <- arrow::read_csv_arrow("../../data/sample/mslt_df.csv")

nr <- round(4459057/10^2) # synth_pop |> collect() |> nrow()
# nr <- 4459057

##################################### Model input #########################################

# Model input
n.i   <- nr                # number of simulated individuals
n.t   <- 30                    # time horizon, 30 cycles
v.n   <- c("H","S1","D")  # the model states: Healthy (H), Sick (S1), Dead (D)
n.s   <- length(v.n)           # the number of health states
v.M_1 <- rep("H", n.i)         # everyone begins in the healthy state 
d.c   <- d.e <- 0.03           # equal discounting of costs and QALYs by 3%
v.Trt <- c("No Treatment", "Treatment") # store the strategy names

# Transition probabilities (per cycle)
p.HD    <- 0.005               # probability to die when healthy
p.HS1   <- 0.15          	     # probability to become sick when healthy
p.S1H   <- 0.5           	     # probability to become healthy when sick
rr.S1   <- 3             	     # rate ratio of death when sick vs healthy
r.HD    <- -log(1 - p.HD) 	   # rate of death when healthy 
r.S1D   <- rr.S1 * r.HD  	     # rate of death when sick
p.S1D   <- 1 - exp(- r.S1D)    # probability to die when sick
rp.S1 <- 0.2                 # increase of the mortality rate with every additional year being sick

# Cost and utility inputs 
c.H     <- 2000                # cost of remaining one cycle healthy
c.S1    <- 4000                # cost of remaining one cycle sick 
# c.S2    <- 15000               # cost of remaining one cycle sicker
c.Trt   <- 12000               # cost of treatment (per cycle)

u.H     <- 1                   # utility when healthy 
u.S1    <- 0.75                # utility when sick 
# u.S2    <- 0.5                 # utility when sicker 
u.Trt   <- 0.95                # utility when sick(er) and being treated
ru.S1S2 <- 0.03                # decrease in utility of treated sick individuals with every additional year being sick/sicker
v.x     <- runif(n.i, 0.95, 1.05) # vector capturing individuals' effect modifier at baseline

##################################### Functions ###########################################

# THE NEW samplev() FUNCTION
# efficient implementation of the rMultinom() function of the Hmisc package #### 

samplev <- function (probs, m) {
  d <- dim(probs)
  n <- d[1]
  k <- d[2]
  lev <- dimnames(probs)[[2]]
  if (!length(lev)) 
    lev <- 1:k
  ran <- matrix(lev[1], ncol = m, nrow = n)
  U <- t(probs)
  for(i in 2:k) {
    U[i, ] <- U[i, ] + U[i - 1, ]
  }
  if (any((U[k, ] - 1) > 1e-05))
    stop("error in multinom: probabilities do not sum to 1")
  
  for (j in 1:m) {
    un <- rep(runif(n), rep(k, n))
    ran[, j] <- lev[1 + colSums(un > U)]
  }
  ran
}
# The MicroSim function for the simple microsimulation of the 'Sick-Sicker' model keeps track of what happens to each individual during each cycle. 

MicroSim <- function(v.M_1, n.i, n.t, v.n, X = NULL, d.c, d.e, TR.out = TRUE, TS.out = TRUE, Trt = FALSE, seed = 1) {
  # Arguments:  
  # v.M_1:   vector of initial states for individuals
  # n.i:     number of individuals
  # n.t:     total number of cycles to run the model
  # v.n:     vector of health state names
  # X:       vector or matrix of individual characteristics
  # d.c:     discount rate for costs
  # d.e:     discount rate for health outcome (QALYs)
  # TR.out:  should the output include a Microsimulation trace? (default is TRUE)
  # TS.out:  should the output include a matrix of transitions between states? (default is TRUE)
  # Trt:     are the n.i individuals receiving treatment? (scalar with a Boolean value, default is FALSE)
  # seed:    starting seed number for random number generator (default is 1)
  # Makes use of:
  # Probs:   function for the estimation of transition probabilities
  # Costs:   function for the estimation of cost state values
  # Effs:    function for the estimation of state specific health outcomes (QALYs)
  
  v.dwc <- 1 / (1 + d.c) ^ (0:n.t)   # calculate the cost discount weight based on the discount rate d.c 
  v.dwe <- 1 / (1 + d.e) ^ (0:n.t)   # calculate the QALY discount weight based on the discount rate d.e
  
  # Create the matrix capturing the state name/costs/health outcomes for all individuals at each time point 
  m.M <- m.C <- m.E <- matrix(nrow = n.i, ncol = n.t + 1, 
                              dimnames = list(paste("ind", 1:n.i, sep = " "), 
                                              paste("cycle", 0:n.t, sep = " ")))  
  
  m.M[, 1] <- v.M_1                     # indicate the initial health state   
  
  set.seed(seed)                  # set the seed for every individual for the random number generator
  
  # create the dur variable that stores the number of consecutive cycles the individual occupies either when sick or sicker
  dur <- rep(0, n.i)                # the individual start without history        
  m.C[, 1] <- Costs(m.M[, 1], Trt)  # estimate costs per individual for the initial health state
  m.E[, 1] <- Effs (m.M[, 1], dur, Trt, X = X)  # estimate QALYs per individual for the initial health state
  
  for (t in 1:n.t) { # t <- 3
    m.p <- Probs(m.M[, t], dur)           # calculate the transition probabilities at cycle t 
    
    m.M[, t + 1] <- samplev(prob = m.p, m = 1)  # sample the next health state and store that state in matrix m.M 
    m.C[, t + 1] <- Costs(m.M[, t + 1], Trt)   # estimate costs per individual during cycle t + 1 conditional on treatment
    m.E[, t + 1] <- Effs( m.M[, t + 1], dur, Trt, X = X)   # estimate QALYs per individual during cycle t + 1 conditional on treatment
    
    dur <- ifelse(m.M[, t + 1] == "S1", 
                  dur + 1, 
                  0)
    
    cat('\r', paste(round(t/n.t * 100), "% done", sep = " "))       # display the progress of the simulation
    
  } # close the loop for the time points 
  
  
  tc <- m.C %*% v.dwc       # total (discounted) cost per individual
  te <- m.E %*% v.dwe       # total (discounted) QALYs per individual 
  
  tc_hat <- mean(tc)        # average (discounted) cost 
  te_hat <- mean(te)        # average (discounted) QALYs
  
  if (TS.out == TRUE) {  # create a matrix of transitions across states
    TS <- paste(m.M, cbind(m.M[, -1], NA), sep = "->") # transitions from one state to the other
    TS <- matrix(TS, nrow = n.i)
    rownames(TS) <- paste("Ind",   1:n.i, sep = " ")   # name the rows 
    colnames(TS) <- paste("Cycle", 0:n.t, sep = " ")   # name the columns 
  } else {
    TS <- NULL
  }
  
  if (TR.out == TRUE) {
    TR <- t(apply(m.M, 2, function(x) table(factor(x, levels = v.n, ordered = TRUE))))
    TR <- TR / n.i                                       # create a distribution trace
    rownames(TR) <- paste("Cycle", 0:n.t, sep = " ")     # name the rows 
    colnames(TR) <- v.n                                  # name the columns 
  } else {
    TR <- NULL
  }
  results <- list(m.M = m.M, m.C = m.C, m.E = m.E, tc = tc, te = te, tc_hat = tc_hat, te_hat = te_hat, TS = TS, TR = TR) # store the results from the simulation in a list  
  return(results)  # return the results
}  # end of the MicroSim function  


#### Probability function
# The Probs function that updates the transition probabilities of every cycle is shown below.

Probs <- function(M_it, dur) { 
  # M_it:   health state occupied by individual i at cycle t (character variable)
  # dur:    the duration of being sick (sick/sicker)
  
  m.p.it <- matrix(NA, n.s, n.i)     # create vector of state transition probabilities
  rownames(m.p.it) <- v.n            # assign names to the vector
  
  # update probabilities of death after first converting them to rates and applying the rate ratio
  r.S1D <-  - log(1 - p.S1D)
  p.S1D <- 1 - exp(- r.S1D * (1 + dur[M_it == "S1"] * rp.S1)) # calculate p.S1D conditional on duration of being sick/sicker
  
  # update the v.p with the appropriate probabilities   
  m.p.it[,M_it == "H"]  <- c(1 - p.HS1 - p.HD, p.HS1, p.HD)                  # transition probabilities when healthy
  m.p.it[,M_it == "S1"] <- rbind(p.S1H, 1- p.S1H - p.S1D, p.S1D)   # transition probabilities when sick
  m.p.it[,M_it == "D"]  <- c(0, 0, 1)                                        # transition probabilities when dead   
  ifelse(colSums(m.p.it) == 1, return(t(m.p.it)), print("Probabilities do not sum to 1")) # return the transition probabilities or produce an error
}       


prob_age_sex <- function(data, Sex, age_group, num_simulations = 10000, seed = 1) {
  set.seed(seed)
  
  # data=data_mx
  # Sex="male"
  # age_group=30
  # num_simulations = 10000
  
  # Filter the data for the specified sex and age group
  filtered_data <- data %>%
    filter(sex == Sex, age == age_group)
  
  # Extract the sickness probability #Here we should also adjust for RRs for exposures and behaviours
  sickness_probability <- 1-(exp(-filtered_data$deaths_rate_allc))
  
  # Initialize a variable to store the count of sick individuals
  sick_count <- 0
  
  # Perform Monte Carlo simulations
  for (i in 1:num_simulations) {
    # Simulate sickness for the individual based on probability
    if (runif(1) < sickness_probability) {
      sick_count <- sick_count + 1
    }
  }
  
  # Calculate the estimated probability of getting sick
  estimated_probability <- sick_count / num_simulations
  
  return(estimated_probability)
}

### Costs function
# The Costs function estimates the costs at every cycle.

Costs <- function (M_it, Trt = FALSE) {
  # M_it: health state occupied by individual i at cycle t (character variable)
  # Trt:  is the individual being treated? (default is FALSE) 
  
  c.it <- 0                                  # by default the cost for everyone is zero 
  c.it[M_it == "H"]  <- c.H                  # update the cost if healthy
  c.it[M_it == "S1"] <- c.S1 + c.Trt * Trt   # update the cost if sick conditional on treatment
  # c.it[M_it == "S2"] <- c.S2 + c.Trt * Trt   # update the cost if sicker conditional on treatment
  c.it[M_it == "D"]  <- 0                    # update the cost if dead
  
  return(c.it)        		                   # return the costs
}


### Health outcome function 
# The Effs function to update the utilities at every cycle.

Effs <- function (M_it, dur, Trt = FALSE, cl = 1, X = NULL) {
  # M_it: health state occupied by individual i at cycle t (character variable)
  # dur:  the duration of being sick/sicker
  # Trt:  is the individual treated? (default is FALSE) 
  # cl:   cycle length (default is 1)
  # X:    the vector or matrix of individual characteristics (optional)
  
  u.it               <- 0        # by default the utility for everyone is zero
  u.it[M_it == "H"]  <- u.H      # update the utility if healthy
  u.it[M_it == "S1"] <- X * Trt * (u.Trt - dur * ru.S1S2) + (1 - Trt) * u.S1 # update the utility if sick conditional on treatment and duration of being sick/sicker
  u.it[M_it == "D"]  <- 0        # update the utility if dead
  
  QALYs <-  u.it * cl            # calculate the QALYs during cycle t
  return(QALYs)                  # return the QALYs
}


##################################### Run the simulation ##################################
# START SIMULATION
p = Sys.time()
sim_no_trt  <- MicroSim(v.M_1, n.i, n.t, v.n, X = v.x, d.c, d.e, Trt = FALSE, seed = 100) # run for no treatment
sim_trt     <- MicroSim(v.M_1, n.i, n.t, v.n, X = v.x, d.c, d.e, Trt = TRUE, seed = 100)  # run for treatment
comp.time = Sys.time() - p

comp.time

################################# Cost-effectiveness analysis #############################
# store the mean costs (and the MCSE) of each strategy in a new variable C (vector costs)
v.C  <- c(sim_no_trt$tc_hat, sim_trt$tc_hat) 
sd.C <- c(sd(sim_no_trt$tc), sd(sim_trt$tc)) / sqrt(n.i)
# store the mean QALYs (and the MCSE) of each strategy in a new variable E (vector effects)
v.E  <- c(sim_no_trt$te_hat, sim_trt$te_hat)
sd.E <- c(sd(sim_no_trt$te), sd(sim_trt$te)) / sqrt(n.i)

delta.C <- v.C[2] - v.C[1]                   # calculate incremental costs
delta.E <- v.E[2] - v.E[1]                   # calculate incremental QALYs
sd.delta.E <- sd(sim_trt$te - sim_no_trt$te) / sqrt(n.i) # Monte Carlo Squared Error (MCSE) of incremental costs
sd.delta.C <- sd(sim_trt$tc - sim_no_trt$tc) / sqrt(n.i) # Monte Carlo Squared Error (MCSE) of incremental QALYs
ICER    <- delta.C / delta.E                 # calculate the ICER
results <- c(delta.C, delta.E, ICER)         # store the values in a new variable


# Create full incremental cost-effectiveness analysis table
table_micro <- data.frame(
  c(round(v.C, 0),  ""),           # costs per arm
  c(round(sd.C, 0), ""),           # MCSE for costs
  c(round(v.E, 3),  ""),           # health outcomes per arm
  c(round(sd.E, 3), ""),           # MCSE for health outcomes
  c("", round(delta.C, 0),   ""),  # incremental costs
  c("", round(sd.delta.C, 0),""),  # MCSE for incremental costs
  c("", round(delta.E, 3),   ""),  # incremental QALYs 
  c("", round(sd.delta.E, 3),""),  # MCSE for health outcomes (QALYs) gained
  c("", round(ICER, 0),      "")   # ICER
)
rownames(table_micro) <- c(v.Trt, "* are MCSE values")  # name the rows
colnames(table_micro) <- c("Costs", "*",  "QALYs", "*", "Incremental Costs", "*", "QALYs Gained", "*", "ICER") # name the columns
table_micro  # print the table 

