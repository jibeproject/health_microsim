require(tidyverse)
require(gt)
require(ggplot2)
require(dplyr)
require(stringr)
require(arrow)
require(plotly)

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
    mode == "autoDriver" ~ "Driving Car",
    mode == "autoPassenger" ~ "Car Passenger",
    mode == "pt" ~ "Public Transport",
    mode == "walk" ~ "Walking",
    mode == "bicycle" ~ "Cycling",
    TRUE ~ "Other"),
    mode = factor(mode, levels = c("Driving Car",
                                   "Car Passenger",
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
                     mode=="Driving Car"~time_auto,
                     mode=="Car Passenger"~time_auto),
    distance = case_when(
      mode %in% c("Driving Car", "Car Passenger", "Public Transport") ~ t.distance_auto,
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

table(trips$LAD_group[trips$scen == "Reference"])

trips <- trips %>%
  mutate(imd5 = ceiling(as.numeric(imd10) / 2),
         imd_origin = ceiling(as.numeric(imd_origin) / 2),
         imd_destination = ceiling(as.numeric(imd_destination) / 2))

arrow::write_dataset(dataset = trips, path = paste0(dir, "/scenOutput/trips/trips.parquet"), partitioning = c("scen", "LAD_group"))

## Creating Visualizations

trips <- arrow::open_dataset(paste0(dir, "/scenOutput/trips/trips.parquet/")) %>% 
  to_duckdb() 

trips_percentage <- trips |>
  group_by(scen) |>
  mutate(total_trips = sum(t.factor, na.rm = TRUE)) |>
  ungroup() |>
  group_by(LAD_origin, imd_origin, mode, scen, gender, agegroup, t.purpose) |>
  # Use weighted count instead of raw count
  summarise(trip_count = sum(t.factor, na.rm = TRUE),
            total_trips = first(total_trips),
            .groups = 'drop') |>
  # Calculate weighted percentage
  mutate(percentage_of_trips = (trip_count / total_trips) * 100) |>
  collect()

trip_counts <- trips |>
  group_by(LAD_origin, imd_origin, mode, scen, gender, agegroup, t.purpose) |>
  summarise(trip_count = sum(t.factor, na.rm = TRUE), .groups = "drop") |>
  collect()

mode_share_overall <- trip_counts |>
  group_by(scen, mode) |>
  summarise(trip_count = sum(trip_count), .groups = "drop") |>
  group_by(scen) |>
  mutate(total_trips = sum(trip_count),
         percentage_of_trips = 100 * trip_count / total_trips) |>
  ungroup()

# Mode share BY GENDER 
mode_share_gender <- trip_counts |>
  group_by(scen, gender, mode) |>
  summarise(trip_count = sum(trip_count), .groups = "drop") |>
  group_by(scen, gender) |>
  mutate(total_trips = sum(trip_count),
         percentage_of_trips = 100 * trip_count / total_trips) |>
  ungroup()

# Mode share BY AGE GROUP
mode_share_age <- trip_counts |>
  group_by(scen, agegroup, mode) |>
  summarise(trip_count = sum(trip_count), .groups = "drop") |>
  group_by(scen, agegroup) |>
  mutate(total_trips = sum(trip_count),
         percentage_of_trips = 100 * trip_count / total_trips) |>
  ungroup()

# Mode share BY LAD
mode_share_lad <- trip_counts |>
  group_by(scen, LAD_origin, mode) |>
  summarise(trip_count = sum(trip_count), .groups = "drop") |>
  group_by(scen, LAD_origin) |>
  mutate(total_trips = sum(trip_count),
         percentage_of_trips = 100 * trip_count / total_trips) |>
  ungroup()

# Mode share BY IMD
mode_share_imd <- trip_counts |>
  group_by(scen, imd_origin, mode) |>
  summarise(trip_count = sum(trip_count), .groups = "drop") |>
  group_by(scen, imd_origin) |>
  mutate(total_trips = sum(trip_count),
         percentage_of_trips = 100 * trip_count / total_trips) |>
  ungroup()

# # Average weekly distance by mode of transportation per person
# pp <- trips |> #to_duckdb() |> 
#   group_by(p.ID, imd5, LAD_group, scen, gender, agegroup) |> 
#   summarise(Cycling = sum(t.distance_bike[mode == "Cycling"] * t.factor[mode=="Cycling"], na.rm = TRUE) ,
#             Walking = sum(t.distance_walk[mode=="Walking"]  * t.factor[mode=="Walking"], na.rm = TRUE) ,
#             `Public Transport` = sum(t.distance_auto[mode=="Public Transport"]  * t.factor[mode=="Public Transport"], na.rm = TRUE) ,
#             `Driving Car` = sum(t.distance_auto[mode=="Driving Car"] * t.factor[mode=="Driving Car"], na.rm = TRUE) ,
#             `Car Passenger` = sum(t.distance_auto[mode=="Car Passenger"]  * t.factor[mode=="Car Passenger"]), na.rm = TRUE)  |> 
#   collect() |> 
#   tidyr::pivot_longer(cols = Cycling:`Car Passenger`, names_to = "mode", values_to = "dist")

# summary_distance <- pp |> 
#   group_by(scen, gender, agegroup, imd5, LAD_group, mode) |> 
#   reframe(sumDistance = sum(dist, na.rm = T), np = dplyr::n(), avgDistance = sumDistance/np)
# 
# # Average weekly duration by mode of transportation per person
# pp_dur <- trips |> #to_duckdb() |> 
#   group_by(p.ID, imd5, LAD_group, scen, gender, agegroup) |> 
#   summarise(Cycling = sum(time_bike[mode == "Cycling"] * t.factor[mode=="Cycling"], na.rm = TRUE) ,
#             Walking = sum(time_walk[mode=="Walking"]  * t.factor[mode=="Walking"], na.rm = TRUE) ,
#             `Public Transport` = sum(time_pt[mode=="Public Transport"]  * t.factor[mode=="Public Transport"], na.rm = TRUE) ,
#             `Driving Car` = sum(time_auto[mode=="Driving Car"] * t.factor[mode=="Driving Car"], na.rm = TRUE) ,
#             `Car Passenger` = sum(time_auto[mode=="Car Passenger"]  * t.factor[mode=="Car Passenger"]), na.rm = TRUE)  |> 
#   collect() |> 
#   tidyr::pivot_longer(cols = Cycling:`Car Passenger`, names_to = "mode", values_to = "dur")
# 
# summary_duration <- pp_dur |> 
#   group_by(scen, gender, agegroup, imd5, LAD_group, mode) |> 
#   reframe(sumDuration = sum(dur, na.rm = T), np = dplyr::n(), avgDuration = sumDuration/np)

# Average time spent per person by mode and location
# tt <- trips |> #to_duckdb() |>
#   group_by(p.ID, LAD_origin, imd_origin, scen, gender, agegroup) |> 
#   summarise(Cycling=sum(time_bike[mode=="Cycling"] * t.factor[mode=="Cycling"], na.rm = TRUE) ,
#             Walking=sum(time_walk[mode=="Walking"]  * t.factor[mode=="Walking"], na.rm = T),
#             `Public Transport`=sum(time_pt[mode=="Public Transport"] * t.factor[mode=="Public Transport"], na.rm = T),
#             `Driving Car`=sum(time_auto[mode=="Driving Car"] * t.factor[mode=="Driving Car"], na.rm = T),
#             `Car Passenger`=sum(time_auto[mode=="Car Passenger"] * t.factor[mode=="Car Passenger"], na.rm = T)) |> 
#   collect() |> 
#   gather(mode, time, Cycling:`Car Passenger`)

# summary_time <- tt |>
#   filter(!is.na(time)) |> 
#   group_by(scen, gender, agegroup, imd_origin, LAD_origin, mode) |> 
#   reframe(avgTime = mean(time, na.rm = T))
# 
# trip_dur <- trips |>
#   group_by(t.id, LAD_origin, imd_origin, gender, agegroup, scen) |> 
#   summarise(Cycling=sum(time_bike[mode=="Cycling"] * t.factor[mode=="Cycling"], na.rm = TRUE) ,
#             Walking=sum(time_walk[mode=="Walking"]  * t.factor[mode=="Walking"], na.rm = T),
#             `Public Transport`=sum(time_pt[mode=="Public Transport"] * t.factor[mode=="Public Transport"], na.rm = T),
#             `Driving Car`=sum(time_auto[mode=="Driving Car"] * t.factor[mode=="Driving Car"], na.rm = T),
#             `Car Passenger`=sum(time_auto[mode=="Car Passenger"] * t.factor[mode=="Car Passenger"], na.rm = T)) |> 
#   collect() |> 
#   tidyr::pivot_longer(cols = Cycling:`Car Passenger`, names_to = "mode", values_to = "time")

# avg_trip_time <- trip_dur |> 
#   filter(!is.na(time)) |> 
#   group_by(scen, LAD_origin, imd_origin, gender, agegroup, mode) |> 
#   reframe(avgTime = mean(time, na.rm = T)) 
# 
# trip_dist <- trips |>
#   group_by(t.id, LAD_origin, imd_origin, gender, agegroup, scen) |>
#   summarise(Cycling = sum(t.distance_bike[mode == "Cycling"] * t.factor[mode=="Cycling"], na.rm = TRUE) ,
#             Walking = sum(t.distance_walk[mode=="Walking"]  * t.factor[mode=="Walking"], na.rm = TRUE) ,
#             `Public Transport` = sum(t.distance_auto[mode=="Public Transport"]  * t.factor[mode=="Public Transport"], na.rm = TRUE) ,
#             `Driving Car` = sum(t.distance_auto[mode=="Driving Car"] * t.factor[mode=="Driving Car"], na.rm = TRUE) ,
#             `Car Passenger` = sum(t.distance_auto[mode=="Car Passenger"]  * t.factor[mode=="Car Passenger"]), na.rm = TRUE)  |> 
#   collect() |> 
#   tidyr::pivot_longer(cols = Cycling:`Car Passenger`, names_to = "mode", values_to = "dist")
# 
# avg_trip_dist <- trip_dist |> 
#   filter(!is.na(dist)) |> 
#   group_by(scen, LAD_origin, imd_origin, gender, agegroup, mode) |> 
#   reframe(avgDistance = mean(dist, na.rm = T))

# rm(trip_dist, trip_dur)

# Stacked Bar Plots for Average Distance via Transport Mode

distance_counts <- trips |>
  group_by(distance_bracket, mode, scen, imd5, gender, agegroup) |>
  summarise(weighted_count = sum(t.factor, na.rm = TRUE), .groups = "drop") |>
  collect()

distance_counts$distance_bracket <- factor(distance_counts$distance_bracket,
                                           levels = c("0-1", "1-3", "3-5", "5-10", "10-20", "20-40", "40+"))



distance_mode_share <- distance_counts |>
  group_by(scen, distance_bracket, mode) |>
  summarise(weighted_count = sum(weighted_count), .groups = "drop") |>
  group_by(scen, distance_bracket) |>
  mutate(percent = 100 * weighted_count / sum(weighted_count)) |>
  ungroup()

# Mode share within distance bracket, BY GENDER
distance_mode_share_gender <- distance_counts |>
  group_by(scen, distance_bracket, gender, mode) |>
  summarise(weighted_count = sum(weighted_count), .groups = "drop") |>
  group_by(scen, distance_bracket, gender) |>
  mutate(percent = 100 * weighted_count / sum(weighted_count)) |>
  ungroup()

# Mode share within distance bracket, BY AGE GROUP
distance_mode_share_age <- distance_counts |>
  group_by(scen, distance_bracket, agegroup, mode) |>
  summarise(weighted_count = sum(weighted_count), .groups = "drop") |>
  group_by(scen, distance_bracket, agegroup) |>
  mutate(percent = 100 * weighted_count / sum(weighted_count)) |>
  ungroup()

# Mode share within distance bracket, BY IMD
distance_mode_share_imd <- distance_counts |>
  group_by(scen, distance_bracket, imd5, mode) |>
  summarise(weighted_count = sum(weighted_count), .groups = "drop") |>
  group_by(scen, distance_bracket, imd5) |>
  mutate(percent = 100 * weighted_count / sum(weighted_count)) |>
  ungroup()

t <- mget(c(
  "mode_share_overall",
  "mode_share_gender",
  "mode_share_age",
  "mode_share_lad",
  "mode_share_imd",
  "distance_counts",
  "distance_mode_share",
  "distance_mode_share_gender",
  "distance_mode_share_age",
  "distance_mode_share_imd"
))

qs2::qs_save(t, paste0(dir, "/scenOutput/trips/trips.qs2")
