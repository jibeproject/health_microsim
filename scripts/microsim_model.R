rm(list = ls())  # remove any variables in R's memory 

# Load libraries
library(tidyverse)
# For efficient dealing of large datases
library(arrow)

# Read pp health dataset (of Munich) using read_csv_arrow from arrow library
synth_pop <- arrow::read_csv_arrow("../../siloMitoMatsim_modelOutput/pp_health_2012.csv")

# Read probability dataset by age and sex for Australia
back_hdata <- arrow::read_csv_arrow("../../data/sample/mslt_df.csv")



