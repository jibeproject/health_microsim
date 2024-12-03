SCEN_SHORT_NAME <- 'cyc'
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

ppdf <- read_csv("D:/Users/aa797/manchester/scenOutput/disease/microData/pp_exposure_2050.csv")
ppdf <- ppdf |> mutate(cyc_mmet = mmetHr_walk + mmetHr_cycle + mmetHr_otherSport) |> rename(pm_conc_cyc = exposure_normalised_pm25)

ap <- ithimr::gen_ap_rr(pm_conc_pp = ppdf)
pa <- ithimr::gen_pa_rr(mmets_pp = ppdf, conf_int = F)
no2_all_cause <- NO2_dose_response(cause = "all_cause_no2", dose = ap |> arrange(id) |> dplyr::select(pm_conc_cyc) |> pull()) |> rename(RR_no2_cyc_all_cause = rr)

# # Function to multiply columns with the same ending (multiple words allowed)
# multiply_columns_with_same_ending <- function(df, min_ending_words = 2) {
#   # Get all column names
#   col_names <- colnames(df)
#   
#   # Function to get all possible endings
#   get_endings <- function(name, min_words) {
#     parts <- strsplit(name, "_")[[1]]
#     n <- length(parts)
#     sapply(min_words:n, function(i) paste(parts[(n-i+1):n], collapse = "_"))
#   }
#   
#   # Get all possible endings
#   all_endings <- unique(unlist(lapply(col_names, get_endings, min_ending_words)))
#   #print(all_endings)
#   
#   # ul <- list()
#   # For each ending, multiply corresponding columns
#   for (ending in all_endings) {
#     # Find columns with this ending
#     cols_to_multiply <- grep(paste0("_", ending, "$"), col_names, value = TRUE)
#     #if (!all(c %in% l))
#     
#     # If there's more than one column with this ending
#     if (length(cols_to_multiply) > 1) {
#       #print(length(cols_to_multiply))
#       
#       # Create new column name
#       new_col_name <- paste0("RR_", ending)
#       
#       # Multiply columns and add to dataframe
#       df[[new_col_name]] <- apply(df[cols_to_multiply], 1, prod)
#     }
#   }
#   
#   #browser()
#   return(df)
# }

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

# # Example usage:
# my_data <- data.frame(
#    RR_ap_lung_cancer = 1:5,
#    RR_pa_lung_cancer = 2:6,
#    RR_other_lung_cancer = 3:7,
#    RR_something_else = 4:8
# )
#multiply_columns_with_same_ending(my_data)

