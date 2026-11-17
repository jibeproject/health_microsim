###############################################################################
## engine/run_microsim.R
##
## Runs the health microsimulation (engine/microsim_engine.R, "with_mm"
## mortality) for the reference and the other scenarios of one Java model run.
## Run from the project folder (open health_microsim.Rproj).
##
## The Java run (RUN_NAME) is set in config/run_config.R. For a new run, change
## it there and re-run data_prep/manchester/run_pifs.R and transitions_calibration.R
## first: the rates file below carries the run's name, so this script stops if
## it has not been made for that run.
##
## Time and memory, full Greater Manchester population (2.8 million people,
## 30 cycles; tested on a 4-core machine): about 6-8 minutes per scenario and
## 7 GB of memory at peak. A 10% sample (sample_prop = 0.1 below) takes about
## 1 minute per scenario.
###############################################################################

library(data.table)
library(here)
source(here("engine/microsim_engine.R"))
source(here("config/run_config.R"))      # RUN_NAME, rr_file(), RATES_FILE

# 1. Java model run ==============================================================
message("Java run: ", RUN_NAME)

# Scenarios: every folder under RR/, with the reference first. To run only
# some, list them instead, e.g. SCENARIOS <- c("reference", "green").
SCENARIOS <- list.dirs(RR_ROOT, full.names = FALSE, recursive = FALSE)
SCENARIOS <- c("reference", setdiff(SCENARIOS, "reference"))
missing   <- SCENARIOS[!file.exists(rr_file(SCENARIOS))]
if (length(missing)) stop("No pp_rr_2021.csv for: ", paste(missing, collapse = ", "))

# 2. Health inputs ===============================================================
# Transition rates by LSOA for this run (data_prep/manchester/transitions_calibration.R, file name
# from config/run_config.R). The engine uses rate_raw and calibrates it itself.
if (!file.exists(RATES_FILE)) stop("No rates file for run ", RUN_NAME, " (", RATES_FILE,
                                   "). Run data_prep/manchester/run_pifs.R and transitions_calibration.R first.")
# ZONES_FILE (config/paths.R): the Manchester zone system
# Diseases at the start: BASE_PREVALENCE_FILE (config/run_config.R), shared with run_pifs.R
PREVALENCE_FILE <- BASE_PREVALENCE_FILE

# Depression remission, derived so that depression incidence reproduces its
# prevalence (as in UBD), by age x sex x IMD decile. Uses the depression rows of
# the prevalence file (proportions by LSOA11, age and sex), averaged over the
# LSOAs of each decile. Set to NULL to run without remission: depression is
# then lifelong once it starts and its prevalence climbs towards 90% over 30 years.
DEP_PREVALENCE_FILE <- here("manchester/health/processed/health_transitions_manchester_prevalence.csv")

depression_remission <- function(prev_file, rates_file, zones_file) {
  pv <- fread(prev_file)
  pv[, cause := normalise_cause(cause)]
  pv <- pv[cause == "depression"]
  if (!nrow(pv)) stop(prev_file, " has no depression rows.")
  z <- fread(zones_file, select = c("LSOA11CD", "imd10"))
  lsoa11_dec <- unique(z[!is.na(imd10), .(location_code = LSOA11CD, imd_decile = as.integer(imd10))])
  lsoa11_dec <- lsoa11_dec[!duplicated(location_code)]
  pv <- lsoa11_dec[pv, on = "location_code"][!is.na(imd_decile)]
  pv <- pv[, .(prevalence = mean(pmin(pmax(rate, 0), 0.99))), by = .(age, sex, imd_decile)]
  derive_remission(read_rates(rates_file), pv, read_zones(zones_file), cause = "depression")
}
REMISSION <- if (is.null(DEP_PREVALENCE_FILE)) NULL else
  depression_remission(DEP_PREVALENCE_FILE, RATES_FILE, ZONES_FILE)

# 3. Settings and output ==========================================================
SETTINGS <- engine_settings(
  n_cycles      = 30L,
  seed          = 2L,
  sample_prop   = 1,           # e.g. 0.1 for a quick test
  exposures     = c("AIR_POLLUTION_PM25", "AIR_POLLUTION_NO2", "PHYSICAL_ACTIVITY", "NOISE", "NDVI"),
  output_format = "parquet")
OUT_DIR <- here("output/microsim", RUN_NAME)

run_microsim(rates_file = RATES_FILE, zones_file = ZONES_FILE, rr_file = rr_file,
             prevalence_file = PREVALENCE_FILE, scenarios = SCENARIOS, out_dir = OUT_DIR,
             remission = REMISSION, settings = SETTINGS)
