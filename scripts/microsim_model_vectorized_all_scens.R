# Load libraries
library(tidyverse)
# For parallel processing
library(future.apply)
# For directory/file structure
library(here)
# For DR PA 
# library(drpa)
# For fast reading/processing
library(arrow)
library(tictoc)

library(data.table)  # For faster data operations
library(stringi)     # For faster string operations

# Boolean variable for dir/file paths
FILE_PATH_BELEN <- FALSE
FILE_PATH_HPC <- FALSE

options(future.globals.maxSize = +Inf)

# Set sample_pro to be greater than zero
sample_prop <- 0.0001

# Number of cycles/years the simulation works
n.c <- 10

# Define DISEASE RISK to incorporate disease interaction
DISEASE_RISK <- TRUE

for (scen in c("base", "safestreet", "green", "both"))
{
  # scen <- 'base'
  # For reproducibility across scenarios, set it inside the loop
  set.seed(2)

  SCEN_SHORT_NAME <- scen
  
  manchester_dir_path <- '/media/ali/Expansion/backup_tabea/Ali/manchester'
  
  dir_path <- scen
  #if (scen == "base")
  #  dir_path <- 'reference'
  
  
  if (FILE_PATH_HPC) {
    # Option 1: HPC path
    synth_pop <- arrow::open_dataset(here(paste0("health_data/processed", SCEN_SHORT_NAME, "_pp_exposure_RR_2021.csv.parquet"))) |> 
      collect()
    
  } else if (!FILE_PATH_BELEN) {
    
    # if (scen == "base"){
    #   f <- "pp_exposure_2021_base_140725.csv"
    # }else if(scen == "safestreet"){
    #   f <- "pp_exposure_2021_safeStreet_300725.csv"
    # }else if(scen == "green"){
    #   f <- "pp_exposure_2021_green_310725.csv"
    # }else if(scen == "both"){
    #   f <- "pp_exposure_2021_both_010825.csv"
    # }
    # 
    # # Option 2: Default (Ali)
    # #/media/ali/Expansion/backup_tabea/Ali/manchester
    # synth_pop <- read_csv(here(paste0(manchester_dir_path, "/input/health/", f)))
    
    synth_pop <- read_csv(here(paste0(manchester_dir_path, "/scenOutput/", SCEN_SHORT_NAME, "/microData/pp_rr_2021.csv")))
    
  } else {
    # Option 3: Manchester
    synth_pop <- read_csv(here(paste0("manchester/health/processed/", SCEN_SHORT_NAME, "_pp_exposure_RR_2021.csv")))
  }
  
  
  # Introduce agegroup
  synth_pop <- synth_pop |> 
    mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf),
                          right=FALSE, include.lowest = TRUE))
  
  synth_pop <- synth_pop |> collect()
  
  # Remove all but PA RRs
  # synth_pop <- synth_pop |> dplyr::select(-c(contains("ndvi") | contains("noise") | contains("pm") | contains("no2"))) 
  
  # Rename parkinson's disease to parkinson
  colnames(synth_pop) <- gsub("parkinson's_disease", "parkinson", colnames(synth_pop))
  
  # Rename head_neck_cancer to head_and_neck_cancer
  colnames(synth_pop) <- gsub("head_neck_cancer", "head_and_neck_cancer", colnames(synth_pop))
  
  if (FILE_PATH_HPC) {
    # Option 1: HPC path
    hd <- read_csv(here(paste0("health_data/health_transitions_manchester.csv")))
    
  } else if (!FILE_PATH_BELEN) {
    # Option 2: Default (e.g., Cambridge)
    hd <- read_csv(here(manchester_dir_path, "/input/health/health_transitions_manchester_04082025.csv"))
    
  } else {
    # Option 3: Manchester path (default if FILE_PATH_BELEN is TRUE and FILE_PATH_HPC is FALSE)
    hd <- read_csv(here("manchester/health/processed/health_transitions_manchester.csv"))
  }
  
  hd <- hd[!duplicated(hd),]
  
  
  hd[hd$cause == "head_neck_cancer",]$cause <- "head_and_neck_cancer"
  
  
  if (FILE_PATH_HPC) {
    # Option 1: HPC path
    prev <- read_csv(here(paste0("health_data/", SCEN_SHORT_NAME, "_prevalence_id.csv")))
    
  } else if (!FILE_PATH_BELEN) {
    # Option 2: Default (Ali)
    prev <- read_csv(here(paste0(manchester_dir_path, "/input/health/base_prevalence_id_clean_230725.csv")))
    
  } else {
    # Option 3: Manchester path (default if FILE_PATH_BELEN is TRUE and FILE_PATH_HPC is FALSE)
    prev <- read_csv(here(paste0("manchester/health/processed/", SCEN_SHORT_NAME, "_prevalence_id.csv"))) ## TO CHANGE with latest
  }
  
  
  if (FILE_PATH_HPC) {
    # Option 1: HPC path
    zones <- read_csv(here(paste0("health_data/zoneSystem.csv")))
    
  } else if (!FILE_PATH_BELEN) {
    # Option 2: Default (Ali)
    zones <-  read_csv(here(paste0(manchester_dir_path, "/input/zoneSystem.csv")))
    
  } else {
    # Option 3: Manchester path (default if FILE_PATH_BELEN is TRUE and FILE_PATH_HPC is FALSE)
    zones <- read_csv(here(paste0("manchester/health/processed/zoneSystem.csv")))
  }
  
  if (FILE_PATH_HPC) {
    # Option 1: HPC path
    disease_risks <<- read_csv("health_data/mod_disease_risks.csv")
    
  } else if (!FILE_PATH_BELEN) {
    # Option 2: Default (Ali)
    disease_risks <<- read_csv("jibe health/mod_disease_risks.csv")
    
  } else {
    # Option 3: Manchester path (default if FILE_PATH_BELEN is TRUE and FILE_PATH_HPC is FALSE)
    disease_risks <<- read_csv(here("health/mod_disease_risks.csv"))
  }
  
  synth_pop <- synth_pop |> left_join(zones  |> 
                                        rename(zone = oaID) |> 
                                        dplyr::select(zone, ladcd, lsoa21cd))
  
  if (sample_prop > 0){
    synth_pop <- synth_pop  |> 
      group_by(agegroup, gender, ladcd) |> 
      sample_frac(sample_prop)
  }
  
  synth_pop <- synth_pop |> rename(sex = gender) |> arrange(id)
  
  names(synth_pop) <- gsub("(RR_|rr_|base_|safestreet_|green_|both_)", "", names(synth_pop))
  
  # Number of individuals
  n.i <- synth_pop |> nrow()
  
  multiply_columns_with_suffix <- function(df, suffix) {
    # Find columns that end with the given suffix
    columns_to_multiply <- names(df)[str_detect(names(df), paste0("_", suffix, "$"))]
    
    # If there are columns to multiply
    if (length(columns_to_multiply) > 0) {
      # Create a new column name
      new_column_name <- paste0("all_path_", suffix)
      
      # Use apply to process rows
      df[[new_column_name]] <- apply(df[columns_to_multiply], 1, function(row) {
        # Replace NA values with "0"
        row[is.na(row)] <- "0"
        
        # Split each cell by commas and convert to numeric vectors
        numeric_lists <- lapply(row, function(cell) as.numeric(unlist(str_split(cell, ",\\s*"))))
        
        # Multiply the numeric vectors element-wise across all columns
        element_wise_product <- Reduce(function(x, y) mapply(`*`, x, y), numeric_lists)
        
        # Convert the numeric vector to a comma-separated string
        paste(element_wise_product, collapse = ",")
      })
      
      df <- df %>% dplyr::select(-all_of(columns_to_multiply))
    }
    
    return(df)
  }
  
  # Function to process all suffixes and remove original columns after multiplication
  process_all_suffixes <- function(df, cause_df) {
    # Extract unique suffixes from the 'cause' column of the cause data frame
    suffixes <- unique(cause_df$cause)
    
    # Apply multiplication for each suffix and remove original columns
    for (suffix in suffixes) {
      df <- multiply_columns_with_suffix(df, suffix)
    }
    
    return(df)
  }
  
  df <- synth_pop
  
  #existing_causes <- synth_pop |> ungroup() |> dplyr::select(contains("pa")) |> names()
  #hd <- hd |> filter(cause %in% gsub("pa_","", existing_causes)) 
  
  synth_pop <- process_all_suffixes(synth_pop, 
                                    hd |> 
                                      dplyr::select(cause) |> 
                                      distinct()) 
  
  names(synth_pop) <- str_replace(names(synth_pop), "^all_path_|pm_|ap_|pa_|PHYSICAL_ACTIVITY_|AIR_POLLUTION_", "")
  
  synth_pop <- synth_pop |> tibble::rowid_to_column("rowname")

  
  # Matrix to save current states
  # with dimensions: (rows: number of individuals, cols: number of classes (or years) + 1 (for the 0th year))
  m <- matrix(nrow = n.i, ncol = n.c + 1,
              dimnames = list(paste0("id", 1:n.i, sep = ""),
                              paste0("c", 0:n.c, sep = "")))
  
  # prev$diseases <- gsub("copd","", as.character(prev$diseases)) # not sure why we are removing copd
  prev$diseases <- trimws(prev$diseases)
  
  
  td <- synth_pop |> 
    left_join(prev) |> 
    dplyr::select(id, diseases) |> 
    mutate(diseases = case_when(is.na(diseases) ~ "healthy", 
                                TRUE ~ diseases)) |> 
    arrange(id)
  
  rownames(m) <- td$id
  
  # The default state is healthy for everyone - before simulation starts. If they have an existing disease
  # from prevalence, it is then assigned
  m[,1] <- td$diseases
  
  rm(td)
  
  # Create a list of diseases from teh burden data
  diseases <- unique(hd$cause)
  
  # Vectorized version of get_state
  get_state_vectorized <- function(rd, cycle, cause, cm, ind_spec_rate) {
    # rd = synth_matrix
    # cycle = incyc
    # cause = dis
    # cm = cm
    # ind_spec_rate = filtered_rates
    # cause_risk = risk_factors
    #browser()
    print(cause)
    prev_state <- as.character(cm[, 1])
    curr_state <- as.character(cm[, 2])
    current_age <- (as.numeric(rd[, "age"]) + cycle)
    
    rr_index <- 1
    
    # Calculate disease probability
    dis_rate <- as.numeric(sapply(rd[, cause], function(x) strsplit(x, ",")[[1]][rr_index]) |> as.numeric() 
                           * ind_spec_rate)
    dis_prob <- 1 - exp(-dis_rate)
    
    # print(paste(cycle, cause, rr_index))
    # print(summary(dis_prob))
    #all_cause_prob <- as.numeric(rd[, "all_cause_mortality"])
    
    
    # Initialize result with current state (falling back to previous state if NA)
    result <- ifelse(is.na(curr_state), prev_state, curr_state)
    
    # Pre-compute conditions
    already_dead <- (!is.na(curr_state) & (prev_state == 'dead' | curr_state == 'dead') | current_age >= 100) # | (all_cause_prob == 0)
    transition_condition <- !already_dead & !is.na(dis_prob) & (runif(length(dis_prob)) < dis_prob)
    
    # Handle all-cause mortality
    if (cause == "all_cause_mortality") {
      result[transition_condition] <- "dead"
    } else {
      # Handle other causes
      to_update <- which(transition_condition)
      if (length(to_update) > 0) {
        curr_state_upd <- curr_state[to_update]
        curr_state_upd[is.na(curr_state_upd)] <- prev_state[to_update][is.na(curr_state_upd)]
        
        # Check if cause already exists
        has_cause <- stri_detect_fixed(curr_state_upd, cause)
        
        # Update states - modified to ensure proper spacing
        new_states <- ifelse(has_cause,
                             curr_state_upd,
                             stri_replace_all_fixed(
                               stri_trim(
                                 stri_paste(cause, " ", curr_state_upd)  # Added space between cause and curr_state_upd
                               ),
                               "healthy", ""))
        
        # Clean up any double spaces that might have been created
        new_states <- stri_replace_all_regex(new_states, "\\s+", " ")
        new_states <- stri_trim(new_states)
        
        result[to_update] <- new_states
      }
    }
    
    # Ensure dead stays dead
    result[already_dead] <- "dead"
    
    return(result)
  }
  
  # Pre-process data for faster access
  prep_health_data <- function(hd) {
    # Convert to data.table for faster lookups
    setDT(hd)
    
    # Create keys for fast lookups
    if ("lsoa21cd" %in% names(hd)) {
      hd[, lookup_key := paste(age, sex, cause, lsoa21cd, measure, sep = "|")]
    } else {
      hd[, lookup_key := paste(age, sex, cause, measure, sep = "|")]
    }
    
    # Create a named vector of rates for O(1) lookups
    setkey(hd, lookup_key)
    return(hd)
  }
  
  # Pre-process disease risks
  prep_disease_risks <- function(disease_risks) {
    setDT(disease_risks)
    return(disease_risks)
  }
  
  # Main simulation function
  run_simulation <- function(synth_pop_wprob, m, hd, disease_risks, n.c, diseases, DISEASE_RISK = TRUE) {
    
    # synth_pop_wprob <- synth_pop
    # Prepare data for fast access
    
    # synth_pop_wprob <- synth_pop
    hd_prepped <- prep_health_data(hd)
    if (DISEASE_RISK) disease_risks_prepped <- prep_disease_risks(disease_risks)
    
    # Convert to matrix for faster column access
    synth_matrix <- as.matrix(synth_pop_wprob)
    
    # Main simulation loop
    for (incyc in 1:n.c) {
      for (dis in diseases) {
        # Calculate current age for all individuals
        current_age <- as.numeric(synth_matrix[, "age"]) + incyc - 1
        
        # Prepare lookup keys for health data
        if (dis == "all_cause_mortality") {
          location_col <- "lsoa21cd"
        } else {
          location_col <- "lsoa21cd" ## Updated data columns now matching java
        }
        
        lookup_keys <- paste(current_age,
                             synth_matrix[, "sex"],
                             dis,
                             synth_matrix[, location_col],
                             ifelse(dis == "all_cause_mortality", "deaths", "incidence"),
                             sep = "|")
        
        # Get rates in bulk
        filtered_rates <- hd_prepped[.(lookup_keys), rate, on = "lookup_key"]
        filtered_rates[is.na(filtered_rates)] <- 0
        
        if (length(filtered_rates) > nrow(synth_matrix)){
          filtered_rates <- filtered_rates[1:nrow(synth_matrix)]
        }
        
        # Get current and previous states in bulk
        cm <- m[, c(paste0("c", incyc - 1), paste0("c", incyc))]
        
        rdf <- get_state_vectorized(
          rd = synth_matrix,
          cycle = incyc,
          cause = dis,
          cm = cm,
          ind_spec_rate = filtered_rates
        )
        print(length(rdf))
        print(length(m[, incyc + 1]))
        #browser()
        
        # Update states in bulk
        m[, incyc + 1] <- rdf
      }
    }
    
    return(m)
  }
  
  # Run the simulation
  tic()
  m <- run_simulation(synth_pop, m, hd, disease_risks, n.c, diseases, DISEASE_RISK)
  toc()


  ## some plots to visualise results

  # Create individual states, while ignoring the all_cause_mortality state as dead state already captures it
  l <- data.frame(states = c('dead', diseases |> str_subset(pattern = "all_cause_mortality", negate = TRUE)), freq = 0, c = 0)
  for (ind in 1:n.c){
    df <- unlist(strsplit(m[, ind], " ")) |>
      as.data.frame()
    names(df) <- 'states'
    tbl <- df |>
      group_by(states) |>
      summarise(cn = dplyr::n()) |>
      mutate(freq = round(cn / sum(cn) * 100, 1), c = ind) |>
      dplyr::select(-cn)
    l <- plyr::rbind.fill(l, tbl)
  }

  l$c <- as.factor(l$c)

  l <- l |> filter(!is.na(states))

  # Generate historic state transitions of all diseases + dead
  ggplot(l |> filter(freq > 0)) +
    aes(x = c, y = freq, fill = states) +
    geom_col() +
    labs(x = "Years", y = "Frequency (%)", title = paste(SCEN_SHORT_NAME, "State transitions over the years")) +
    theme_minimal()

  plotly::ggplotly(ggplot(l |> filter(states != "healthy"), aes(x = c, y = freq, color = states, group = states)) + geom_line() + geom_point() +
                     labs(x = "Years", y = "Frequency (%)", title = paste(SCEN_SHORT_NAME, "State transitions over the years")))


  m |> as.data.frame() |>
    rownames_to_column("id") |>
    pivot_longer(cols = -c(id)) |>
    mutate(unpacked = str_split(value, " ")) |>
    unnest() |>
    mutate(value = str_trim(unpacked)) |>
    dplyr::select(-unpacked) |>
    mutate(value = str_replace_all(value, fixed("parkinson’s_disease"), "parkinson")) |>
    group_by(name, value)|>
    summarise(nv = dplyr::n(),
              freq = round(100 * nv / nrow(m), 1)) |>
    filter(nv > 0) |>
    pivot_wider(id_cols = value,
                names_from = name, values_from = nv) |> print()

  prep_trans_df <- function(m, measure = "freq"){

    # measure <- "freq"
    df <- m |> as.data.frame() |>
      rownames_to_column("id") |>
      pivot_longer(cols = -c(id)) |>
      mutate(unpacked = str_split(value, " ")) |>
      unnest() |>
      mutate(value = str_trim(unpacked)) |>
      mutate(value = str_replace_all(value, fixed("parkinson’s_disease"), "parkinson")) |>
      dplyr::select(-unpacked) |>
      group_by(name, value)|>
      summarise(nv = dplyr::n(),
                freq = round(100 * nv / nrow(m), 1)) |>
      filter(freq > 0) |>
      {\(.) if(measure == "freq"){
        pivot_wider(., id_cols = value, names_from = name, values_from = freq)
      }
        else{
          pivot_wider(., id_cols = value, names_from = name, values_from = nv)
        }
      }()  |>
      rename(cause = value) |>
      pivot_longer(cols = -cause) |>
      as.data.frame() |>
      filter(cause != "healthy")


    df$name <- gsub("c","",as.character(df$name))
    df$name <- as.numeric(df$name)

    df <- df |> arrange(cause, name)

    return(df)

  }

  plotly::ggplotly(ggplot(prep_trans_df(m, measure = "freq"),
                          aes(x = name, y = value, color = cause, group = cause) ) +
                     geom_point() +
                     geom_line() +
                     labs(title = paste(SCEN_SHORT_NAME, "Disease freq over time"), x = "years", y = "freq (%) "))


  plotly::ggplotly(ggplot(prep_trans_df(m, measure = "nv"),
                          aes(x = name, y = value, color = cause, group = cause) ) +
                     geom_point() +
                     geom_line() +
                     labs(title = paste(SCEN_SHORT_NAME, "Disease count over time"), x = "years", y = "count (n) "))
  

  df <- as.data.frame(m)
  df$id <- rownames(m)


  # if (FILE_PATH_HPC) {
  #   # Option 1: HPC path
  #   arrow::write_dataset(df, paste0("health_data/results/", SCEN_SHORT_NAME, "_dis_inter_", DISEASE_RISK, "_state_trans-n.c-", n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".parquet"))
  # 
  # } else if (!FILE_PATH_BELEN) {
  #   # Option 2: Default (Ali)
  #   arrow::write_dataset(df, paste0("data/", SCEN_SHORT_NAME, "_dis_inter_", DISEASE_RISK, "_state_trans-n.c-", n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".parquet"))
  # 
  # } else {
  #   # Option 3: Manchester path (default if FILE_PATH_BELEN is TRUE and FILE_PATH_HPC is FALSE)
  #   arrow::write_dataset(df, paste0("manchester/health/processed/", SCEN_SHORT_NAME, "_dis_inter_", DISEASE_RISK, "_state_trans-n.c-", n.c, "-n.i-", n.i, "-n.d-", length(diseases), ".parquet"))
  # }
  
}