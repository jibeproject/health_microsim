# Load libraries
library(tidyverse)
# For parallel processing
library(future.apply)
library(here)

plan(multisession)

# For reproducibility set seed
set.seed(2)

# Read pp health dataset (of Munich) using read_csv_arrow from arrow library
# synth_pop_base <- read_csv("data/siloMitoMatsim_modelOutput/pp_health_2012.csv")

### Belen 7.03.24: change folder to Jibe working group
synth_pop <- read_csv(here("data/siloMitoMatsim_modelOutput/pp_health_2012.csv"))

# synth_pop <- read_csv("data/siloMitoMatsim_modelOutput/pp_2021.csv")

# # Read pp health dataset (of Munich) using read_csv_arrow from arrow library
# synth_pop_scen <- read_csv("data/siloMitoMatsim_modelOutput/pp_health_2012_scen.csv")
# 
# cdf <- left_join(synth_pop_base, synth_pop_scen |> dplyr::select(id, hhid, rr_all) |> rename(scen_rr_all = rr_all))

# slice 1k rows
synth_pop <- synth_pop |> slice_sample(n = 100) |> rename(base_rr_all = rr_all)

synth_pop <- synth_pop |> dplyr::select(id, age, gender, base_rr_all) |> 
  rename (sex = gender)

# Number of individuals
n.i <- synth_pop |> nrow()

# Number of cycles
n.c <- 50

# everyone begins in the healthy state 
v.M_1 <- rep("H", n.i)

# Read probability dataset by age and sex for Australia
back_hdata <- read_csv(here("data/sample/mslt_df.csv"))

# Mutate sex to numeric values with male ~ 1 and female ~ 2
# Also modify all columns with deaths rate into prob by 1 - exp(-deaths_rate)
back_hdata <-  back_hdata |>  
  rename_with(~ gsub("deaths_rate", "sick_prob", .x, fixed = TRUE)) |> 
  mutate(sex = case_when(sex == 'male' ~ 1, sex == 'female' ~ 2),
         across(starts_with("sick_prob"), ~(1 - (exp(-.))))) |> 
  dplyr::select(age, sex, starts_with("sick_prob"))

# Combine baseline demographics and exposure data with background health data
synth_pop <- left_join(synth_pop, back_hdata)

# Function to return 
prob_age_sex <- function(data, hdata, colname = "sick_prob_allc", seed = 1, cycle = 1) {
  set.seed(seed)
  # Get age and sex specific sick_prob from background death rate
  # The cycle is the current year and is added to the baseline's age of an individuals to get the current age
  # So age = baseline_age + cycle - 1 - as cycle starts with 1 hence subtracting 1 from it
  nsick_prob <- hdata |> 
    filter(age == (data["age"] |> as.numeric()) + cycle - 1, sex == c(data["sex"] |> as.numeric())) |> 
    dplyr::select(colname) |> 
    pull()
  # If sick_prob is undefined then return 0
  if (length(nsick_prob) == 0)
    nsick_prob <- 0
  return(nsick_prob)
}


# Get the state for each individual by looking at the transition probability
# If transition probability is less than sick_prob, then it happens, otherwise the agent (or the individual)
# remain in the same state
get_state <- function(rd, cycle = 1, cause = "allc", cm) {
  # Get unique index for the agent
  vi <- rd["rowname"] |> as.numeric()
  # Get previous state of the agent
  prev_state <-  cm[vi, 1] |> as.character()
  # Get current state of the agent
  curr_state <-  cm[vi, 2] |> as.character()
  # Create cause specific state name
  curr_cause <- paste0('S_', toupper(cause))
  
  # If the agent has already died, then they remain in the same 'dead' state
  if (prev_state == 'D' ||
      (!is.na(curr_state) && curr_state == 'D') ||
      ((paste0("prob_allc_", cycle) %in% names(rd)) && 
       rd[paste0("prob_allc_", cycle)] |> as.numeric() == 0))
    return('D')
  else{
    # prob <- rbernoulli(1, p = rd[paste0("prob_", cause, "_", cycle)] |> as.numeric())
    # If however the uniform random probablity is greater than age and sex specific sick_prob, then transition to the
    # specific cause (curr_cause) takes place
    prob <- runif(1)
    if (prob < rd[paste0("prob_", cause, "_", cycle)] |> as.numeric()){
      # All-cause mortality is a special cause that takes an agent to the 'dead' state
      if (cause == 'allc')
        return ('D')
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
            return(trimws(str_replace_all(paste(curr_cause, curr_state, sep = " "), " H", "")))
          # return(unique(unlist(strsplit(trimws(str_replace_all(paste(curr_cause, curr_state, sep = " "), " H", "")), " "))))
        }
      }
    }else{
      # The agent remains in the previous state if the transition probablity was greater than random uniform sample of 1
      return(ifelse(is.na(curr_state), prev_state, curr_state))
    }
  }
}

# Create a df for 
synth_pop_wprob <- synth_pop|> rownames_to_column()
# 
# Matrix to save current states
# with dimensions: (rows: number of individuals, cols: number of classes (or years) + 1 (for the 0th year))
m <- matrix(nrow = n.i, ncol = n.c + 1,
            dimnames = list(paste0("id", 1:n.i, sep = ""),
                            paste0("c", 0:n.c, sep = "")))

# The default state is healthy for everyone - before simulation starts
m[,1] <- v.M_1
# Read the names of all columns starting with the 'sick' word, so that list of causes (or diseases) can be generated
diseases <- sapply(strsplit(back_hdata |> dplyr::select(starts_with("sick")) |> names(), "\\_"), "[", 3)
# Select only a handful of cause for now, which are: all-cause-mortality, ischaemic heart disease", stroke and lung Cancer
# diseases <- c("allc", "ishd", "tbalc", "strk")

# diseases <- c( "allc", "npls", "ishd", "tbalc", "carc", "strk", "hanc", "copd", "dmt2", "lvrc", "stmc",
#                "kdnc", "espc", "lwri", "mltm", "adaod", "brsc", "bldc", "prkd", "chml", "dprd", "prsc",
#                "utrc" )
# 
# diseases <- c("allc", "utrc")
# diseases <- c("npls", "ishd")
# diseases <- c("ishd", "npls")

# Record the time it takes to run the piece of code with tic()
require(tictoc)
tic()
stop <- F
for (i in 1:n.c){
  if (stop)
    break
  for (dis in diseases){
    ind_prob <- future_apply(synth_pop_wprob, 1, prob_age_sex, future.seed = T,
                             hdata = back_hdata, cycle = i, colname = paste0("sick_prob_", dis))
    synth_pop_wprob <- bind_cols(synth_pop_wprob, ind_prob)
    names(synth_pop_wprob)[synth_pop_wprob |> length()] <- paste0("prob_", dis, "_", i)
  }
  
  for (dis in diseases){
    cstate <- future_apply(synth_pop_wprob, 1, get_state, cycle = i, cause = dis, cm = m[, c(paste0("c", i - 1), paste0("c", i))], future.seed = T)
    print(table(cstate))
    m[, i + 1] <- cstate
    
    # Remove this piece of code
    # if (cstate |> table() |> as.data.frame() |> dplyr::select(Freq) |> nrow() == 1){
    #   stop <- T
    #   break
    # }
    
  }
}
# Stop recording of time spent running the code
toc()


l <- data.frame(Var1 = c('H', 'D', paste0('S_', toupper(diseases))), freq = 0, c = 0)
for (ind in 1:n.c){
  df <- unlist(strsplit(m[, ind], " ")) |> 
    as.data.frame()
  names(df) <- 'Var1'
  tbl <- df |> 
    group_by(Var1) |> 
    summarise(cn = n()) |> 
    mutate(freq = round(cn / sum(cn) * 100, 1), c = ind) |> 
    dplyr::select(-cn)
  l <- plyr::rbind.fill(l, tbl)
}

l[l$c == 0 & l$Var1 == 'H',]$freq <- 100

l$c <- as.factor(l$c)

l <- l |> filter(!is.na(Var1))

# Generate historic state transitions of all diseases + death
ggplot(l) +
  aes(x = c, y = freq, fill = Var1) +
  geom_col() +
  labs(x = "Years", y = "Frequency (%)", title = "State transitions over the years") +
  theme_minimal()

# Save the diagram
ggsave(paste0("diagrams/state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".png"), height = 5, width = 10, units = "in", dpi = 600, scale = 1)

# Also save state transitions as a CSV
write_csv(m |> as.data.frame(), paste0("data/state_trans-n.c-",n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".csv"))
