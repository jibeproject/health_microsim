###############################################################################
## data_prep/manchester/run_pifs.R
##
## PIFs (population impact fractions) of the Java run set in
## config/run_config.R, from its reference scenario's person-level RRs
## (RR/reference/pp_rr_2021.csv), by sex x 5-year age group x IMD decile
## (with a pooled row per sex x age group). functions/pif_calc.R (next to this script) does the work.
## Diseases at the start come from BASE_PREVALENCE_FILE (config/run_config.R),
## the same file the microsimulation starts from.
##
## Out: PIFS_FILE (manchester/health/processed/pifs_reference_<RUN_NAME>.csv),
## read by data_prep/manchester/transitions_calibration.R.
###############################################################################

library(readr)
library(here)
source(here("config/run_config.R"))
source(here("data_prep/manchester/functions/pif_calc.R"))

ref_rr <- rr_file("reference")
if (!file.exists(ref_rr)) stop("No reference RR file for run ", RUN_NAME, ": ", ref_rr)
message("Run ", RUN_NAME, ": reading ", ref_rr)

pifs <- calculate_pif(read_csv(ref_rr, show_col_types = FALSE), BASE_PREVALENCE_FILE)
pifs$run_name <- RUN_NAME                  # provenance, checked by transitions_calibration.R
write_csv(pifs, PIFS_FILE)
message("Wrote ", nrow(pifs), " rows to ", PIFS_FILE)
