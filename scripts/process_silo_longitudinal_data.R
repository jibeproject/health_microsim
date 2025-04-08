# Using data.table package
library(data.table, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(dtplyr, warn.conflicts = FALSE)
library(arrow)
library(here)


# Specify the folder path
#dir_path <- "/home/ali/GH/health_microsim/jibe health/resultsUrbanTransitions/reference/03_exposure_and_rr/"

dir_path <- "jibe health/resultsUrbanTransitions/reference/03_exposure_and_rr/"

# List all files starting with "pp_rr" and ending with ".csv"
file_list <- list.files(path = here(dir_path), pattern = "^pp_rr.*\\.csv$", full.names = TRUE)

# Initialize an empty data frame for the final result
final_df <- NULL

# Loop through each file
for (file in file_list) {
  # Extract the suffix from the file name (e.g., "2021" from "pp_rr_2021.csv")
  suffix <- str_extract(basename(file), "(?<=pp_rr_)[^\\.]+")
  
  # Read the CSV file
  temp_df <- read_csv(file)
  
  # Rename columns by appending the suffix (except for the 'id' column)
  colnames(temp_df) <- ifelse(colnames(temp_df) == "id", 
                              "id", 
                              paste0(colnames(temp_df), "_", suffix))
  
  # Perform a left join with the final data frame
  if (is.null(final_df)) {
    final_df <- temp_df  # If it's the first file, initialize final_df
  } else {
    final_df <- left_join(final_df, temp_df, by = "id")
  }
}

arrow::write_dataset(final_df, "jibe health/resultsUrbanTransitions/reference/03_exposure_and_rr/pp_rr_all_years.parquet")

dt <- final_df |> dplyr::select(id, contains("rr_AIR_POLLUTION_NO2_all_cause_mortality")) |> pivot_longer(cols = -id) |> as.data.table()

# Split the 'name' column into 'variable' and 'year'
dt[, c("variable", "year") := tstrsplit(name, "_(?=\\d{4}$)", perl = TRUE)]

# Convert year to numeric for further processing
dt[, year := as.numeric(year)]

# Drop the original 'name' column if not needed
dt[, name := NULL]


# 
# # Make them available as global datasets
# for (i in 1:length(list_of_files)) {
#   assign(stringr::str_sub(basename(list_of_files[[i]]), end = -5),
#          read.csv(list_of_files[[i]]),
#          pos = 1
#   )
# }


# Function to extract and combine columns
combine_rr_columns <- function(df) {
  # Get all column names
  col_names <- colnames(df)
  
  # Extract unique base names for columns starting with "rr" and ending with a year
  rr_base_names <- unique(gsub("_[0-9]{4}$", "", col_names[grepl("^rr_.*_[0-9]{4}$", col_names)]))
  
  # Iterate over each base name to create new combined columns
  for (base_name in rr_base_names) {
    # Find all columns matching the base name pattern
    matching_cols <- col_names[grepl(paste0("^", base_name, "_[0-9]{4}$"), col_names)]
    
    # Sort matching_cols by the year extracted from their names
    matching_cols <- matching_cols[order(as.numeric(gsub(".*_([0-9]{4})$", "\\1", matching_cols)))]
    
    # Combine the columns into a simple list of values (ordered by year, without column names)
    df[[base_name]] <- df %>%
      select(all_of(matching_cols)) %>%
      pmap(~ unname(c(...)))
    
    # Optionally, remove the original columns if not needed
    df <- df %>% select(-all_of(matching_cols))
  }
  
  return(df)
}


# Apply the function to the data frame
df <- combine_rr_columns(final_df |> collect())

# Save reduced dataset with all the RR with the same name combined together in a single column
arrow::write_dataset(df, "jibe health/resultsUrbanTransitions/reference/03_exposure_and_rr/pp_rr_all_years_reduced.parquet")