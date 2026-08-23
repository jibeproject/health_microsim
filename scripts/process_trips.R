require(tidyverse)
require(gt)
require(ggplot2)
require(dplyr)
require(stringr)
require(arrow)
require(plotly)
require(qs2)
require(purrr)

# Change the path as needed, currently set to Manchester folder

dir <- "/run/user/1001/gvfs/smb-share:server=ifs-prod-1152-cifs.ifs.uis.private.cam.ac.uk,share=cedar-grp-drive/HealthImpact/Data/Country/UK/JIBE/manchester"

zone <- read_csv(paste0(dir,"/input/zoneSystem.csv"))

trips_ref <- read_csv(paste0(dir,"/scenOutput/base/2021/microData/trips.csv")) |> mutate(scen = "reference")

trips_ss <- read_csv(paste0(dir,"/scenOutput/safeStreet/2021/microData/trips.csv")) |> mutate(scen = "safeStreet")

trips_green <-read_csv(paste0(dir,"/scenOutput/green/2021/microData/trips.csv")) |> mutate(scen = "green")

trips_goDutch <- read_csv(paste0(dir,"/scenOutput/goDutch/2021/microData/trips.csv")) |> mutate(scen = "goDutch")

pp <- read_csv("/scenOutput/normalization_fix_052226/exposures/reference/pp_exposure_2021_base_220526.csv") |> 
  left_join(zone  |> 
              rename(zone = oaID) |> 
              dplyr::select(zone, ladnm, ladcd, lsoa21cd, imd10))

trips <- bind_rows(trips_ref, trips_green, trips_ss, trips_goDutch)

rm (trips_ref, trips_ss, trips_green, trips_goDutch)

#trips <- trips |> to_duckdb()

trips <- trips |>
  left_join(zone |>
              dplyr::select(LAD_origin = ladnm,imd_origin = imd10, oaID),
            by = c("originZone" = "oaID"), copy = T) |> 
  left_join(zone |> 
              dplyr::select(LAD_destination = ladnm, imd_destination = imd10, oaID),
            by = c("destinationZone" = "oaID"), copy = T) |> 
  left_join(pp  |> 
              dplyr::select(id, age, gender, ladnm, ladcd, lsoa21cd, imd10, occupation), by = c("p.ID" = "id"), copy = T)

unique(trips$ladnm)
rm(pp, zone)

trips$time_pt <- as.numeric(trips$time_pt)

trips <- trips |>
  mutate(mode = case_when(
    mode %in% c("autoDriver", "autoPassenger") ~ "Car",
    mode == "pt" ~ "Public Transport",
    mode == "walk" ~ "Walking",
    mode == "bicycle" ~ "Cycling",
    TRUE ~ "Other"),
    mode = factor(mode, levels = c("Car",
                                   "Public Transport",
                                   "Walking",
                                   "Cycling",
                                   "Other")),
    t.factor = if_else(t.purpose %in% c("HBW", 
                                        "HBE", 
                                        "HBA", 
                                        "HBS", 
                                        "HBR", 
                                        "HBO",
                                        "RRT"), 2,1),
    time_walk = time_walk/60,
    time_bike = time_bike/60,
    time_auto = time_auto/60,
    time_pt = time_pt/60,
    time = case_when(mode=="Cycling"~time_bike,
                     mode=="Walking"~time_walk,
                     mode=="Public Transport"~time_pt,
                     mode=="Car"~time_auto),
    distance = case_when(
      mode %in% c("Car", "Public Transport") ~ t.distance_auto,
      mode == "Cycling"~t.distance_bike,
      mode == "Walking"~t.distance_walk),
    time_factored = time * t.factor,
    dist_factored = distance * t.factor,
    distance_bracket = as.factor(cut(
      distance,
      breaks = c(0, 1, 3, 5, 10, 20, 40, Inf),
      labels = c("0-1", "1-3", "3-5", "5-10", "10-20", "20-40", "40+"),
      right = FALSE
    )
    )
  )

add_agegroups <- function(df) {
  df |> 
    mutate(
      agegroup = case_when(
        age >= 0   & age <= 20   ~ "0-20",
        age > 20  & age <= 45  ~ "21-45",
        age > 45  & age <= 65  ~ "46-65",
        age > 65                 ~ "65+",
        TRUE                            ~ NA_character_
      )
    )
}

trips <- add_agegroups(trips)

unique(trips$ladnm)

trips <- trips %>%
  mutate(LAD_group = case_when(
    ladnm %in% c("Manchester", "Salford") ~ "City core",
    ladnm %in% c("Oldham", "Rochdale", "Tameside") ~ "East",
    ladnm %in% c("Stockport", "Trafford") ~ "South",
    ladnm %in% c("Bolton", "Wigan", "Bury") ~ "West/North-west",
    TRUE ~ NA_character_  # Catch-all for unlisted values
  ),
  LAD_origin = case_when(
    LAD_origin %in% c("Manchester", "Salford") ~ "City core",
    LAD_origin %in% c("Oldham", "Rochdale", "Tameside") ~ "East",
    LAD_origin %in% c("Stockport", "Trafford") ~ "South",
    LAD_origin %in% c("Bolton", "Wigan", "Bury") ~ "West/North-west",
    TRUE ~ NA_character_  # Catch-all for unlisted values
  ),
  LAD_destination = case_when(
    LAD_destination %in% c("Manchester", "Salford") ~ "City core",
    LAD_destination %in% c("Oldham", "Rochdale", "Tameside") ~ "East",
    LAD_destination %in% c("Stockport", "Trafford") ~ "South",
    LAD_destination %in% c("Bolton", "Wigan", "Bury") ~ "West/North-west",
    TRUE ~ NA_character_  # Catch-all for unlisted values
  ))


trips <- trips %>%
  mutate(imd5 = ceiling(as.numeric(imd10) / 2),
         imd_origin = ceiling(as.numeric(imd_origin) / 2),
         imd_destination = ceiling(as.numeric(imd_destination) / 2))

arrow::write_dataset(dataset = trips, path = paste0(dir, "/scenOutput/trips/trips_200826.parquet"), partitioning = c("scen", "LAD_group"))

## Creating Visualizations

# Base path to trips parquet dataset
trips_path <- "Z:/HealthImpact/Data/Country/UK/JIBE/manchester/scenOutput/trips/trips_200826.parquet/"


# Open once as an Arrow dataset (lazy)
trips_ds <- arrow::open_dataset(trips_path)

# Get all scenarios
all_scens <- trips_ds |>
  distinct(scen) |>
  collect() |>
  pull(scen)

# Function to compute all metrics for a single scenario
compute_scenario <- function(scen_val, trips_ds) {
  # Filter to this scenario (lazy, then materialize as DuckDB)
  trips <- trips_ds |>
    filter(scen == scen_val) |>
    to_duckdb()
  
  ## ---- Trips percentage ----
  trips_percentage <- trips |>
    group_by(scen) |>
    mutate(total_trips = sum(t.factor, na.rm = TRUE)) |>
    ungroup() |>
    group_by(LAD_origin, imd_origin, mode, scen, gender, agegroup, t.purpose) |>
    summarise(
      trip_count = sum(t.factor, na.rm = TRUE),
      total_trips = first(total_trips),
      .groups = "drop"
    ) |>
    mutate(percentage_of_trips = (trip_count / total_trips) * 100) |>
    collect()
  
  ## ---- Trip counts ----
  trip_counts <- trips |>
    group_by(LAD_origin, imd_origin, mode, scen, gender, agegroup, t.purpose) |>
    summarise(trip_count = sum(t.factor, na.rm = TRUE), .groups = "drop") |>
    collect()
  
  ## ---- Mode share overall ----
  mode_share_overall <- trip_counts |>
    group_by(scen, mode) |>
    summarise(trip_count = sum(trip_count), .groups = "drop") |>
    group_by(scen) |>
    mutate(
      total_trips = sum(trip_count),
      percentage_of_trips = 100 * trip_count / total_trips
    ) |>
    ungroup()
  
  ## ---- Mode share by gender ----
  mode_share_gender <- trip_counts |>
    group_by(scen, gender, mode) |>
    summarise(trip_count = sum(trip_count), .groups = "drop") |>
    group_by(scen, gender) |>
    mutate(
      total_trips = sum(trip_count),
      percentage_of_trips = 100 * trip_count / total_trips
    ) |>
    ungroup()
  
  ## ---- Mode share by age group ----
  mode_share_age <- trip_counts |>
    group_by(scen, agegroup, mode) |>
    summarise(trip_count = sum(trip_count), .groups = "drop") |>
    group_by(scen, agegroup) |>
    mutate(
      total_trips = sum(trip_count),
      percentage_of_trips = 100 * trip_count / total_trips
    ) |>
    ungroup()
  
  ## ---- Mode share by LAD ----
  mode_share_lad <- trip_counts |>
    group_by(scen, LAD_origin, mode) |>
    summarise(trip_count = sum(trip_count), .groups = "drop") |>
    group_by(scen, LAD_origin) |>
    mutate(
      total_trips = sum(trip_count),
      percentage_of_trips = 100 * trip_count / total_trips
    ) |>
    ungroup()
  
  ## ---- Mode share by IMD ----
  mode_share_imd <- trip_counts |>
    group_by(scen, imd_origin, mode) |>
    summarise(trip_count = sum(trip_count), .groups = "drop") |>
    group_by(scen, imd_origin) |>
    mutate(
      total_trips = sum(trip_count),
      percentage_of_trips = 100 * trip_count / total_trips
    ) |>
    ungroup()
  
  ## ---- Average weekly distance by mode per person ----
  pp <- trips |>
    group_by(p.ID, imd5, LAD_group, scen, gender, agegroup) |>
    summarise(
      Cycling = sum(t.distance_bike[mode == "Cycling"] * t.factor[mode == "Cycling"], na.rm = TRUE),
      Walking = sum(t.distance_walk[mode == "Walking"] * t.factor[mode == "Walking"], na.rm = TRUE),
      `Public Transport` = sum(t.distance_auto[mode == "Public Transport"] * t.factor[mode == "Public Transport"], na.rm = TRUE),
      `Car` = sum(t.distance_auto[mode == "Car"] * t.factor[mode == "Car"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    collect() |>
    pivot_longer(
      cols = Cycling:`Car`,
      names_to = "mode",
      values_to = "dist"
    )
  
  summary_distance <- pp |>
    group_by(scen, gender, agegroup, imd5, LAD_group, mode) |>
    reframe(
      sumDistance = sum(dist, na.rm = TRUE),
      np = n(),
      avgDistance = sumDistance / np
    )
  
  ## ---- Average weekly duration by mode per person ----
  pp_dur <- trips |>
    group_by(p.ID, imd5, LAD_group, scen, gender, agegroup) |>
    summarise(
      Cycling = sum(time_bike[mode == "Cycling"] * t.factor[mode == "Cycling"], na.rm = TRUE),
      Walking = sum(time_walk[mode == "Walking"] * t.factor[mode == "Walking"], na.rm = TRUE),
      `Public Transport` = sum(time_pt[mode == "Public Transport"] * t.factor[mode == "Public Transport"], na.rm = TRUE),
      `Car` = sum(time_auto[mode == "Car"] * t.factor[mode == "Car"], na.rm = TRUE),
     .groups = "drop"
    ) |>
    collect() |>
    pivot_longer(
      cols = Cycling:`Car`,
      names_to = "mode",
      values_to = "dur"
    )
  
  summary_duration <- pp_dur |>
    group_by(scen, gender, agegroup, imd5, LAD_group, mode) |>
    reframe(
      sumDuration = sum(dur, na.rm = TRUE),
      np = n(),
      avgDuration = sumDuration / np
    )
  
  ## ---- Average time spent per person by mode and location ----
  tt <- trips |>
    group_by(p.ID, LAD_origin, imd_origin, scen, gender, agegroup) |>
    summarise(
      Cycling = sum(time_bike[mode == "Cycling"] * t.factor[mode == "Cycling"], na.rm = TRUE),
      Walking = sum(time_walk[mode == "Walking"] * t.factor[mode == "Walking"], na.rm = TRUE),
      `Public Transport` = sum(time_pt[mode == "Public Transport"] * t.factor[mode == "Public Transport"], na.rm = TRUE),
      `Car` = sum(time_auto[mode == "Car"] * t.factor[mode == "Car"], na.rm = TRUE),
     .groups = "drop"
    ) |>
    collect() |>
    pivot_longer(
      cols = Cycling:`Car`,
      names_to = "mode",
      values_to = "time"
    )
  
  summary_time <- tt |>
    filter(!is.na(time)) |>
    group_by(scen, gender, agegroup, imd_origin, LAD_origin, mode) |>
    reframe(
      avgTime = mean(time, na.rm = TRUE)
    )
  
  ## ---- Average trip time by mode ----
  trip_dur <- trips |>
    group_by(t.id, LAD_origin, imd_origin, gender, agegroup, scen) |>
    summarise(
      Cycling = sum(time_bike[mode == "Cycling"] * t.factor[mode == "Cycling"], na.rm = TRUE),
      Walking = sum(time_walk[mode == "Walking"] * t.factor[mode == "Walking"], na.rm = TRUE),
      `Public Transport` = sum(time_pt[mode == "Public Transport"] * t.factor[mode == "Public Transport"], na.rm = TRUE),
      `Car` = sum(time_auto[mode == "Car"] * t.factor[mode == "Car"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    collect() |>
    pivot_longer(
      cols = Cycling:`Car`,
      names_to = "mode",
      values_to = "time"
    )
  
  avg_trip_time <- trip_dur |>
    filter(!is.na(time)) |>
    group_by(scen, LAD_origin, imd_origin, gender, agegroup, mode) |>
    reframe(
      avgTime = mean(time, na.rm = TRUE)
    )
  
  ## ---- Average trip distance by mode ----
  trip_dist <- trips |>
    group_by(t.id, LAD_origin, imd_origin, gender, agegroup, scen) |>
    summarise(
      Cycling = sum(t.distance_bike[mode == "Cycling"] * t.factor[mode == "Cycling"], na.rm = TRUE),
      Walking = sum(t.distance_walk[mode == "Walking"] * t.factor[mode == "Walking"], na.rm = TRUE),
      `Public Transport` = sum(t.distance_auto[mode == "Public Transport"] * t.factor[mode == "Public Transport"], na.rm = TRUE),
      `Car` = sum(t.distance_auto[mode == "Car"] * t.factor[mode == "Car"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    collect() |>
    pivot_longer(
      cols = Cycling:`Car`,
      names_to = "mode",
      values_to = "dist"
    )
  
  avg_trip_dist <- trip_dist |>
    filter(!is.na(dist)) |>
    group_by(scen, LAD_origin, imd_origin, gender, agegroup, mode) |>
    reframe(
      avgDistance = mean(dist, na.rm = TRUE)
    )
  
  rm(trip_dist, trip_dur)
  
  ## ---- Distance bracket mode shares ----
  distance_counts <- trips |>
    group_by(distance_bracket, mode, scen, imd5, gender, agegroup) |>
    summarise(weighted_count = sum(t.factor, na.rm = TRUE), .groups = "drop") |>
    collect()
  
  distance_counts <- distance_counts |>
    mutate(
      distance_bracket = factor(
        distance_bracket,
        levels = c("0-1", "1-3", "3-5", "5-10", "10-20", "20-40", "40+")
      )
    )
  
  distance_mode_share <- distance_counts |>
    group_by(scen, distance_bracket, mode) |>
    summarise(weighted_count = sum(weighted_count), .groups = "drop") |>
    group_by(scen, distance_bracket) |>
    mutate(percent = 100 * weighted_count / sum(weighted_count)) |>
    ungroup()
  
  distance_mode_share_gender <- distance_counts |>
    group_by(scen, distance_bracket, gender, mode) |>
    summarise(weighted_count = sum(weighted_count), .groups = "drop") |>
    group_by(scen, distance_bracket, gender) |>
    mutate(percent = 100 * weighted_count / sum(weighted_count)) |>
    ungroup()
  
  distance_mode_share_age <- distance_counts |>
    group_by(scen, distance_bracket, agegroup, mode) |>
    summarise(weighted_count = sum(weighted_count), .groups = "drop") |>
    group_by(scen, distance_bracket, agegroup) |>
    mutate(percent = 100 * weighted_count / sum(weighted_count)) |>
    ungroup()
  
  distance_mode_share_imd <- distance_counts |>
    group_by(scen, distance_bracket, imd5, mode) |>
    summarise(weighted_count = sum(weighted_count), .groups = "drop") |>
    group_by(scen, distance_bracket, imd5) |>
    mutate(percent = 100 * weighted_count / sum(weighted_count)) |>
    ungroup()
  
  # Build scenario-level result list
  t_s <- list(
    mode_share_overall = mode_share_overall,
    mode_share_gender = mode_share_gender,
    mode_share_age = mode_share_age,
    mode_share_lad = mode_share_lad,
    mode_share_imd = mode_share_imd,
    distance_counts = distance_counts,
    distance_mode_share = distance_mode_share,
    distance_mode_share_gender = distance_mode_share_gender,
    distance_mode_share_age = distance_mode_share_age,
    distance_mode_share_imd = distance_mode_share_imd,
    avg_trip_time = avg_trip_time,
    avg_trip_dist = avg_trip_dist,
    summary_distance = summary_distance,
    summary_duration = summary_duration,
    summary_time = summary_time,
    #tt = tt,
    trips_percentage = trips_percentage
  )
  
  # Clean up large intermediates for this scenario
  rm(
    trips, trips_percentage, trip_counts,
    mode_share_overall, mode_share_gender, mode_share_age,
    mode_share_lad, mode_share_imd,
    pp, summary_distance,
    pp_dur, summary_duration,
    tt, summary_time,
    trip_dur, avg_trip_time,
    trip_dist, avg_trip_dist,
    distance_counts,
    distance_mode_share, distance_mode_share_gender,
    distance_mode_share_age, distance_mode_share_imd
  )
  
  return(t_s)
}

# Run loop over scenarios, storing results in a named list
t_list <- map(
  all_scens,
  ~ compute_scenario(.x, trips_ds)
)

names(t_list) <- all_scens

# Object names shared by the scenarios
object_names <- names(t_list[[1]])

# Combine each object across all scenarios
combined_list <- set_names(
  map(object_names, function(object_name) {
    map_dfr(
      t_list,
      ~ .x[[object_name]]
    )
  }),
  object_names
)

qs2::qs_save(combined_list, "Z:/HealthImpact/Data/Country/UK/JIBE/manchester/scenOutput/trips/trips_200826.qs2")
