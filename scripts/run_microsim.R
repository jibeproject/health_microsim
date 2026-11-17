###############################################################################
## scripts/run_microsim.R
##
## Runs the health microsimulation (functions/microsim_engine.R, "with_mm"
## mortality) for the reference and the other scenarios of one Java model run.
## Run from the project folder (open health_microsim.Rproj).
##
## To run a newer Java model run, change RUN_NAME. Everything else is read from
## that run's folder (one pp_rr_2021.csv per scenario under RR/).
##
## Time: a 10% sample (284,000 people, 30 cycles) takes about 1 minute per
## scenario, so expect about ten times that for the full population. Set
## sample_prop = 0.1 below for a quick test.
###############################################################################

library(data.table)
library(here)
source(here("functions/microsim_engine.R"))

# 1. Java model run ==============================================================
RUN_NAME  <- "nonTransportPA_050526"     # folder under scenOutput/: changes with each run
JIBE_ROOT <- "X:/HealthImpact/Data/Country/UK/JIBE/manchester"
RR_ROOT   <- file.path(JIBE_ROOT, "scenOutput", RUN_NAME, "RR")
rr_file   <- function(scen) file.path(RR_ROOT, scen, "pp_rr_2021.csv")

# Scenarios: every folder under RR/, with the reference first. To run only
# some, list them instead, e.g. SCENARIOS <- c("reference", "green").
SCENARIOS <- list.dirs(RR_ROOT, full.names = FALSE, recursive = FALSE)
SCENARIOS <- c("reference", setdiff(SCENARIOS, "reference"))
missing   <- SCENARIOS[!file.exists(rr_file(SCENARIOS))]
if (length(missing)) stop("No pp_rr_2021.csv for: ", paste(missing, collapse = ", "))

# 2. Health inputs ===============================================================
# Transition rates by LSOA (scripts/transitions_calibration.R); the engine uses rate_raw
RATES_FILE <- here("manchester/health/processed/health_transitions_manchester_280826.csv")
ZONES_FILE <- here("manchester/synPop/sp_2021/zoneSystem.csv")
# Diseases at the start (scripts/assign_prevalence.R writes base_prevalence_id_clean_<ddmmyy>.csv)
PREVALENCE_FILE <- here("manchester/health/processed/base_prevalence_id_clean_DDMMYY.csv")

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
