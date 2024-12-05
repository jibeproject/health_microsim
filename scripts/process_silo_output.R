# load libraries
require("tidyverse")
require("data.table")

# Define name of the scenario
SCEN_SHORT_NAME <- 'cyc'
# 
all_cause_no2 <- read_csv("data/dose_response/air_pollution_no2/all_cause_mortality_fatal.csv")
source("scripts/NO2_dose_response.R")
DISEASE_INVENTORY <- read_csv("../ITHIM-R/inst/extdata/global/dose_response/disease_outcomes_lookup.csv")

list_of_files <- list.files(
  path = "../ITHIM-R/inst/extdata/global/dose_response/drap/extdata/",
  recursive = TRUE, pattern = "\\.csv$",
  full.names = TRUE
)

for (i in 1:length(list_of_files)) {
  assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
         read.csv(list_of_files[[i]]),
         pos = 1
  )
}

ppdf <- read_csv("D:/Users/aa797/manchester/scenOutput/disease/microData/pp_exposure_2021.csv")
ppdf <- ppdf |> mutate(cyc_mmet = mmetHr_walk + mmetHr_cycle + mmetHr_otherSport) |> rename(pm_conc_cyc = exposure_normalised_pm25)

ap <- ithimr::gen_ap_rr(pm_conc_pp = ppdf)
pa <- ithimr::gen_pa_rr(mmets_pp = ppdf, conf_int = F)
no2_all_cause <- NO2_dose_response(cause = "all_cause_no2", dose = ap |> arrange(id) |> dplyr::select(pm_conc_cyc) |> pull()) |> rename(RR_no2_cyc_all_cause = rr)

multiply_similar_columns <- function(df) {
  # Get all column names
  col_names <- names(df)
  
  # Function to extract the common part of column names
  extract_common <- function(name) {
    parts <- strsplit(name, "_")[[1]]
    paste(parts[3:length(parts)], collapse = "_")
  }
  
  # Get unique common parts
  common_parts <- unique(sapply(col_names, extract_common))
  
  # For each common part, multiply corresponding columns
  for (part in common_parts) {
    matching_cols <- col_names[sapply(col_names, function(x) grepl(part, x))]
    
    if (length(matching_cols) > 1) {
      new_col_name <- paste0("RR_", part)
      df[[new_col_name]] <- Reduce(`*`, df[matching_cols])
    }
  }
  
  return(df)
}

rr <- left_join(ap |> dplyr::select(id, contains("RR")), pa |> dplyr::select(id, contains("RR")), by = "id") |> arrange(id)
rr <- cbind(rr, no2_all_cause)
combine_rr <- multiply_similar_columns(rr)

hist(combine_rr$RR_cyc_all_cause)

#hh <- read_csv("D:/Users/aa797/manchester/scenOutput/disease/microData/hh_2021.csv")
dd <- read_csv("D:/Users/aa797/manchester/scenOutput/disease/microData/dd_2021.csv")

ppdf <- left_join(ppdf, dd |> dplyr::select(hhID, zone) |> rename(hhid = hhID))

dir_path <- 'D:/Users/aa797/RMIT University/JIBE working group - General/manchester/'

zones <- read_csv(paste0(dir_path, "synpop/sp_2021/zoneSystem.csv"))

ppdf <- left_join(ppdf, zones |> dplyr::select(oaID, lsoa21cd) |> rename(zone = oaID))

combine_rr <- left_join(combine_rr, ppdf |> dplyr::select(id, age, gender, lsoa21cd))

# write combine_rr
#write_csv(combine_rr, "data/manchester/cyc_pp_exposure_RR_2021.csv")
