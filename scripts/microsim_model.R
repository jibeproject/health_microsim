# remove any variables in R's memory 
rm(list = ls()) 

# Load libraries
library(tidyverse)
# For parallel processing
library(future.apply)

plan(multisession)

# For reproducibility set seed
set.seed(1)

# Read pp health dataset (of Munich) using read_csv_arrow from arrow library
# synth_pop_base <- read_csv("data/siloMitoMatsim_modelOutput/pp_health_2012.csv")
synth_pop <- read_csv("data/siloMitoMatsim_modelOutput/pp_health_2012.csv")

# # Read pp health dataset (of Munich) using read_csv_arrow from arrow library
# synth_pop_scen <- read_csv("data/siloMitoMatsim_modelOutput/pp_health_2012_scen.csv")
# 
# cdf <- left_join(synth_pop_base, synth_pop_scen |> dplyr::select(id, hhid, rr_all) |> rename(scen_rr_all = rr_all))

# slice 1k rows
synth_pop <- synth_pop |> slice_sample(n = 100) |> rename(base_rr_all = rr_all)

# rm (cdf, synth_pop_scen, synth_pop_base)
#synth_pop <- synth_pop |> dplyr::select(id, age, gender, base_rr_all, scen_rr_all) |> 
synth_pop <- synth_pop |> dplyr::select(id, age, gender, base_rr_all) |> 
  rename (sex = gender)

# Number of individuals
n.i <- synth_pop |> nrow()

# Number of cycles
n.c <- 100

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

prob_age_sex <- function(data, hdata, colname = "sick_prob_allc", seed = 1, cycle = 1) {
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
  if (length(nsick_prob) == 0)
    nsick_prob <- 0
  return(nsick_prob)
}

get_state <- function(rd, cycle = 1, cause = "allc", cm) {
  vi <- rd["rowname"] |> as.numeric()
  prev_state <-  cm[vi, 1] |> as.character()
  curr_state <-  cm[vi, 2] |> as.character()
  curr_cause <- paste0('S_', toupper(cause))
  if (prev_state == 'D' ||
      (!is.na(curr_state) && curr_state == 'D') ||
      rd[paste0("prob_allc_", cycle)] |> as.numeric() == 0)
    return('D')
  else{
    # prob <- rbernoulli(1, p = rd[paste0("prob_", cause, "_", cycle)] |> as.numeric())
    prob <- runif(1)
    if (prob < rd[paste0("prob_", cause, "_", cycle)] |> as.numeric()){
      if (cause == 'allc')
        return ('D')
      else{
        if(is.na(curr_state))
          return (curr_cause)
        else{
          if (grepl(curr_cause, curr_state, ignore.case = F))
            return(curr_state)
          else
            return(trimws(str_replace_all(paste(curr_cause, curr_state, sep = " "), " H", "")))
          # return(unique(unlist(strsplit(trimws(str_replace_all(paste(curr_cause, curr_state, sep = " "), " H", "")), " "))))
        }
      }
    }else{
      return(ifelse(is.na(curr_state), prev_state, curr_state))
    }
  }
}

# Create a df for 
synth_pop_wprob <- synth_pop|> rownames_to_column()
# 
# # Matrix to save current states
m <- matrix(nrow = n.i, ncol = n.c + 1,
            dimnames = list(paste0("id", 1:n.i, sep = ""),
                            paste0("c", 0:n.c, sep = "")))

# The default state is healthy for everyone - before simulation starts
m[,1] <- v.M_1
diseases <- sapply(strsplit(back_hdata |> dplyr::select(starts_with("sick")) |> names(), "\\_"), "[", 3)
diseases <- c("allc", "ishd", "tbalc", "strk")
lbls <- c("Dead", "Healthy", "All-cause mortality", "Ischaemic Heart Disease", "Stroke", "Lung Cancer")

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
    
    if (cstate |> table() |> as.data.frame() |> dplyr::select(Freq) |> nrow() == 1){
      stop <- T
      break
    }
       
  }
}

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
toc()

l$c <- as.factor(l$c)

l <- l |> filter(!is.na(Var1)

ggplot(l) +
  aes(x = c, y = freq, fill = Var1) +
  geom_col() +
  scale_fill_manual(name = "State", labels = lbls, values = c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f')) + 
  labs(x = "Years", y = "Frequency (%)", title = "State transitions over the years") +
  theme_minimal()

ggsave(paste0("diagrams/state_trans-n.c-",n.c, "-n.i-", n.i, ".png"), height = 5, width = 10, units = "in", dpi = 600, scale = 1)

write_csv(m |> as.data.frame(), paste0("data/state_trans-n.c-",n.c, "-n.i-", n.i, ".csv"))

# # Print ggplot over cycles across all states - including healthy, deceased and individual diseases
# ggplot(m |> as.data.frame() |> rownames_to_column("id") |> pivot_longer(cols = -id)) +
#   aes(x = name, fill = value) +
#   geom_bar() +
#   scale_fill_hue(direction = 1) +
#   theme_minimal()
