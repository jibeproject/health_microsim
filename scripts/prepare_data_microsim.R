##### Prepare data for health microsimulation in R version #####

# This data should be prepared for each scenario before running microsim_model_vectorised


# load libraries
require("tidyverse")
require("data.table")
require("drpa")
require("ithimr")
require("here")


source("functions/process_silo_output_function.R")
##### Input data

DISEASE_INVENTORY <- read_csv("health/disease_outcomes_lookup.csv")
# DISEASE_INVENTORY <- read_csv("jibe health/health1/disease_outcomes_lookup.csv") 
exposures_ref <- read_csv("manchester/simulationResults/ForPaper/1_reference/health/04_exposure_and_rr/pp_exposure_2021.csv")
exposures_safer <- read_csv("manchester/simulationResults/ForPaper/2_safestreet/health/04_exposure_and_rr/pp_exposure_2021.csv")
exposures_green <- read_csv("manchester/simulationResults/ForPaper/3_green/health/04_exposure_and_rr/pp_exposure_2021.csv")
exposures_both <- read_csv("manchester/simulationResults/ForPaper/4_both/health/04_exposure_and_rr/pp_exposure_2021.csv")


# ppdf <- read_csv("jibe health/pp_exposure_2021.csv")
prevalence <- read_csv("manchester/health/processed/health_transitions_manchester_prevalence.csv")
# prevalence <- read.csv("jibe health/health_transitions_manchester_prevalence.csv")
zones <- read_csv(here("manchester/synPop/sp_2021/zoneSystem.csv"))
# zones <- read_csv("jibe health/zoneSystem.csv")

# Relative risks
list_of_files <- list.files(
  path = "health/", # "jibe health/health1/",
  recursive = TRUE, pattern = "(pm|no|noise|ndvi)\\.csv$",
  full.names = TRUE
)

# Make them available as global datasets
for (i in 1:length(list_of_files)) {
  assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
         read.csv(list_of_files[[i]]),
         pos = 1
  )
}

# # Read all_cause_no2 DR
# all_cause_no2 <- read_csv("jibe health/health1/all_cause_no.csv") |>
# rename(RR = rr)
# 
all_cause_no2 <- read_csv("health/all_cause_no.csv") |>
  rename(RR = rr)

SCEN_SHORT_NAME <<- 'base' # check if need changing for scenarios


data_ref <- process_output_silo(DISEASE_INVENTORY, exposures_ref, prevalence, zones)

data_safer <- process_output_silo(DISEASE_INVENTORY, exposures_safer, prevalence, zones)
  
data_green <- process_output_silo(DISEASE_INVENTORY, exposures_green, prevalence, zones)
  
data_both <- process_output_silo(DISEASE_INVENTORY, exposures_both, prevalence, zones)