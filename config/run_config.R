###############################################################################
## config/run_config.R
##
## The Java model run that the health scripts work on, set once here. Sourced
## by data_prep/manchester/run_pifs.R, data_prep/manchester/transitions_calibration.R
## and engine/run_microsim.R, so the PIFs, the PIF-adjusted rates and the
## microsimulation always come from the same run.
##
## For a new run, change RUN_NAME, then re-run, in order:
##   1. data_prep/manchester/run_pifs.R                PIFs from the run's reference RRs
##   2. data_prep/manchester/transitions_calibration.R rates (rate = rate_raw x (1 - PAF))
##   3. engine/run_microsim.R                          microsimulation
## The files written in steps 1 and 2 carry RUN_NAME in their names, so a
## script fails (file not found) rather than silently using another run's file.
##
## The location of the shared drive (JIBE_ROOT) is set in config/paths.R.
###############################################################################

library(here)
source(here("config/paths.R"))           # JIBE_ROOT, MANCHESTER_ROOT, LOCAL_DATA

RUN_NAME <- "nonTransportPA_050526"      # folder under scenOutput/ of the Java run

RR_ROOT <- file.path(MANCHESTER_ROOT, "scenOutput", RUN_NAME, "RR")
rr_file <- function(scen) file.path(RR_ROOT, scen, "pp_rr_2021.csv")   # Java person-level RRs

HEALTH_PROCESSED <- here("manchester/health/processed")
PIFS_FILE  <- file.path(HEALTH_PROCESSED, paste0("pifs_reference_", RUN_NAME, ".csv"))                 # run_pifs.R
RATES_FILE <- file.path(HEALTH_PROCESSED, paste0("health_transitions_manchester_", RUN_NAME, ".csv"))  # transitions_calibration.R
