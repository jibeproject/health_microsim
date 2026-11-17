###############################################################################
## config/run_config.R
##
## The Java model run that the health scripts work on, set once here. Sourced
## by data_prep/manchester/run_pifs.R, data_prep/manchester/transitions_calibration.R,
## engine/run_microsim.R, outputs/process_all_data.R and outputs/process_exp.R,
## so the PIFs, the PIF-adjusted rates, the microsimulation and the app's
## health and exposure data all come from the same run. (Trips do not: see
## outputs/process_trips.R.)
##
## For a new run, change RUN_NAME, then re-run, in order:
##   1. data_prep/manchester/run_pifs.R                PIFs from the run's reference RRs
##   2. data_prep/manchester/transitions_calibration.R rates (rate = rate_raw x (1 - PAF))
##   3. engine/run_microsim.R                          microsimulation
##   4. outputs/process_all_data.R, outputs/process_exp.R   health and exposure data for the app
##      (process_all_data.R also needs SRC_FILE, the run's all_data parquet, set in it)
## The files written in steps 1 and 2 carry RUN_NAME in their names, so a
## script fails (file not found) rather than silently using another run's file.
##
## The location of the shared drive (JIBE_ROOT) is set in config/paths.R.
###############################################################################

library(here)
source(here("config/paths.R"))           # JIBE_ROOT, MANCHESTER_ROOT, LOCAL_DATA

RUN_NAME <- "health_and_accidents_140926"   # folder under scenOutput/ of the Java run

RUN_DIR <- file.path(MANCHESTER_ROOT, "scenOutput", RUN_NAME)
RR_ROOT <- file.path(RUN_DIR, "RR")
rr_file <- function(scen) file.path(RR_ROOT, scen, "pp_rr_2021.csv")   # Java person-level RRs

HEALTH_PROCESSED <- here("manchester/health/processed")
PIFS_FILE  <- file.path(HEALTH_PROCESSED, paste0("pifs_reference_", RUN_NAME, ".csv"))                 # run_pifs.R
RATES_FILE <- file.path(HEALTH_PROCESSED, paste0("health_transitions_manchester_", RUN_NAME, ".csv"))  # transitions_calibration.R
