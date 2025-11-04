require(tidyverse)
require(gt)
require(ggplot2)
require(dplyr)
require(stringr)
require(arrow)
require(plotly)

# Change the path as needed, currently set to Manchester folder
dir_path <- "/media/ali/Expansion/backup_tabea/manchester-main/"

zone <- read_csv(paste0(dir_path, "input/zoneSystem.csv"))

trips_ref <- read_csv("temp/trips/base/trips.csv") |> mutate(scen = "reference")

trips_both <- read_csv("temp/trips/both/trips.csv") |> mutate(scen = "both")

trips_ss <- read_csv("temp/trips/ss/trips.csv") |> mutate(scen = "safeStreet")

trips_green <- read_csv("temp/trips/green/trips.csv") |> mutate(scen = "green")

trips_goDutch <- read_csv("temp/trips/goDutch/trips.csv") |> mutate(scen = "goDutch")

pp <- read_csv("/media/ali/Expansion/backup_tabea/manchester/input/health/pp_exposure_2021_base_140725.csv") |> 
  left_join(zone  |> 
              rename(zone = oaID) |> 
              dplyr::select(zone, ladnm, ladcd, lsoa21cd))

trips <- bind_rows(trips_ref, trips_green, trips_ss, trips_both, trips_goDutch)

rm (trips_ref, trips_both, trips_ss, trips_green, trips_goDutch)


#trips <- trips |> to_duckdb()

trips <- trips |>
  left_join(zone |>
              dplyr::select(LAD_origin = ladnm,imd_origin = imd10, oaID),
            by = c("originZone" = "oaID"), copy = T) |> 
  left_join(zone |> 
              dplyr::select(LAD_destination = ladnm, imd_destination = imd10, oaID),
            by = c("destinationZone" = "oaID"), copy = T) |> 
  left_join(pp  |> 
              dplyr::select(id, age, gender, ladnm, ladcd, lsoa21cd, occupation), by = c("p.ID" = "id"), copy = T)

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

arrow::write_dataset(dataset = trips, path = "temp/041125_trips.parquet", partitioning = c("scen", "ladnm"))

## Creating Visualizations

### Table of the number of trips in each local authority in Greater Manchester

trips <- arrow::open_dataset("temp/041125_trips.parquet/") |> 
  to_duckdb()

# Distribution of trips by mode and location 
trips_percentage <- trips |>
  group_by(LAD_origin, scen) |>
  mutate(total_trips = sum(t.factor, na.rm = TRUE)) |>
  ungroup() |>
  group_by(LAD_origin, mode, scen, gender, agegroup, t.purpose) |>
  # Use weighted count instead of raw count
  summarise(trip_count = sum(t.factor, na.rm = TRUE),
            total_trips = first(total_trips),
            .groups = 'drop') |>
  # Calculate weighted percentage
  mutate(percentage_of_trips = (trip_count / total_trips) * 100) |>
  collect()

# 
# 
# trips |>
#   group_by(distance_bracket, mode, scen, gender, agegroup) |>
#   summarise(weighted_count = sum(t.factor), .groups = 'drop') |>
#   group_by(distance_bracket, scen) 


trips_percentage_all <- trips_percentage |>
  group_by(mode, scen) |>
  summarise(trip_count = mean(trip_count),
            total_trips = mean(total_trips),
            percentage_of_trips = (trip_count / total_trips) * 100, .groups = 'drop') |>
  mutate(LAD_origin = "All Locations") |> 
  collect()

trips_percentage_combined <- bind_rows(trips_percentage |> collect(), trips_percentage_all |> collect())

# By IMD

trips_percentage_imd <- trips |> #to_duckdb() |> 
  group_by(imd_origin, scen) |>
  mutate(total_trips = sum(t.factor, na.rm = TRUE)) |>
  ungroup() |> 
  group_by(imd_origin, mode, scen, gender, agegroup, t.purpose) |> 
  summarise(trip_count = sum(t.factor, na.rm = TRUE), 
            total_trips = first(total_trips), 
            .groups = 'drop') |>
  mutate(percentage_of_trips = (trip_count / total_trips) * 100) |> collect()

trips_percentage_imd$imd_origin <- as.factor(trips_percentage_imd$imd_origin) 

trips_percentage_all_imd <- trips_percentage_imd |>
  group_by(mode, scen) |>
  summarise(trip_count = mean(trip_count),
            total_trips = mean(total_trips),
            percentage_of_trips = (trip_count / total_trips) * 100, .groups = 'drop') |>
  mutate(imd_origin = "All IMDs") |> collect()

trips_percentage_combined_imd <- bind_rows(trips_percentage_all_imd,trips_percentage_imd)

trips_percentage_combined_imd$imd <- factor(trips_percentage_combined_imd$imd_origin,
                                            levels = c("All IMDs", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                            labels = c("All IMDs", "Most Deprived", "2", "3", "4", "5", "6", "7", "8", "9", "Least Deprived"))



# Average weekly distance by mode of transportation
pp <- trips |> #to_duckdb() |> 
  group_by(p.ID, ladnm, scen, gender, agegroup) |> 
  summarise(Cycling = sum(t.distance_bike[mode == "Cycling"] * t.factor[mode=="Cycling"], na.rm = TRUE) ,
            Walking = sum(t.distance_walk[mode=="Walking"]  * t.factor[mode=="Walking"], na.rm = TRUE) ,
            `Public Transport` = sum(t.distance_auto[mode=="Public Transport"]  * t.factor[mode=="Public Transport"], na.rm = TRUE) ,
            `Driving Car` = sum(t.distance_auto[mode=="Driving Car"] * t.factor[mode=="Driving Car"], na.rm = TRUE) ,
            `Car Passenger` = sum(t.distance_auto[mode=="Car Passenger"]  * t.factor[mode=="Car Passenger"]), na.rm = TRUE)  |> 
  collect()

pp <- pp |> 
  gather(mode,dist,Cycling:`Car Passenger`)

pop_lad <- trips |> 
  filter(scen == "reference") |> 
  #distinct(p.ID, LAD_origin) |> 
  group_by(ladnm) |> 
  summarise(pop = n_distinct(p.ID)) |> 
  collect()

tot_pop <- pop_lad |> 
  dplyr::select(pop) |> 
  pull() |> 
  sum()

summary_distance <- pp |>  
  filter(!is.na(dist)) |> 
  left_join(pop_lad, copy = T) |> 
  group_by(mode, ladnm, scen) |> 
  summarise(avgDistance = round(sum(dist) / first(pop), 1))

pp_all=trips|> # to_duckdb() |> 
  group_by(p.ID, scen)|>
  summarise(Cycling = sum(t.distance_bike[mode == "Cycling"] * t.factor[mode=="Cycling"], na.rm = TRUE) ,
            Walking = sum(t.distance_walk[mode=="Walking"]  * t.factor[mode=="Walking"], na.rm = TRUE) ,
            `Public Transport` = sum(t.distance_auto[mode=="Public Transport"]  * t.factor[mode=="Public Transport"], na.rm = TRUE) ,
            `Driving Car` = sum(t.distance_auto[mode=="Driving Car"] * t.factor[mode=="Driving Car"], na.rm = TRUE) ,
            `Car Passenger` = sum(t.distance_auto[mode=="Car Passenger"]  * t.factor[mode=="Car Passenger"]), na.rm = TRUE)  |> 
  collect()

pp_all <- pp_all |> gather(mode,dist,Cycling:`Car Passenger`)

summary_distance_all <- pp_all |> 
  filter(!is.na(dist)) |> 
  group_by(mode, scen) |>  
  summarise(avgDistance = round(sum(dist, na.rm = T) / tot_pop, 1)) |>  
  mutate(ladnm = "All Locations")

combined_distance <- bind_rows(summary_distance,summary_distance_all) 

# Average time spent per person by mode and location
tt = trips |> #to_duckdb() |>
  group_by(p.ID, LAD_origin, scen, gender, agegroup) |> 
  summarise(Cycling=sum(time_bike[mode=="Cycling"] * t.factor[mode=="Cycling"], na.rm = TRUE) ,
            Walking=sum(time_walk[mode=="Walking"]  * t.factor[mode=="Walking"], na.rm = T),
            `Public Transport`=sum(time_pt[mode=="Public Transport"] * t.factor[mode=="Public Transport"], na.rm = T),
            `Driving Car`=sum(time_auto[mode=="Driving Car"] * t.factor[mode=="Public Transport"], na.rm = T),
            `Car Passenger`=sum(time_auto[mode=="Car Passenger"] * t.factor[mode=="Public Transport"], na.rm = T)) |> 
  collect()

tt=tt|>gather(mode,time,Cycling:`Car Passenger`)

summary_time=tt|>
  group_by(mode, LAD_origin, scen)|>
  summarise(avgTime=mean(time, na.rm = T))

tt_all=trips|> #to_duckdb() |>
  group_by(p.ID, scen)|>
  summarise(Cycling=sum(time_bike[mode=="Cycling"] * t.factor[mode=="Cycling"], na.rm = TRUE) ,
            Walking=sum(time_walk[mode=="Walking"]  * t.factor[mode=="Walking"], na.rm = T),
            `Public Transport`=sum(time_pt[mode=="Public Transport"] * t.factor[mode=="Public Transport"], na.rm = T),
            `Driving Car`=sum(time_auto[mode=="Driving Car"] * t.factor[mode=="Public Transport"], na.rm = T),
            `Car Passenger`=sum(time_auto[mode=="Car Passenger"] * t.factor[mode=="Public Transport"], na.rm = T)) |> 
  collect()

tt_all <- 
  tt_all |> 
  gather(mode,time,Cycling:`Car Passenger`)

summary_time_all=tt_all|>
  group_by(mode, scen)|>
  summarise(avgTime = mean(time, na.rm = T)) |>
  mutate(LAD_origin = "All Locations")

avg_time_combined <- bind_rows(summary_time, summary_time_all)

# Stacked Bar Plots for Average Distance via Transport Mode

distance <- trips |>
  group_by(distance_bracket, mode, scen, gender, agegroup) |>
  summarise(weighted_count = sum(t.factor), .groups = 'drop') |>
  group_by(distance_bracket, scen) |>
  mutate(percent = weighted_count / sum(weighted_count) * 100) |>
  collect()

distance$distance_bracket <- factor(distance$distance_bracket,
                                         levels = c("0-1", "1-3", "3-5", "5-10",
                                                    "10-20", "20-40", "40+"))


# # People with Zero trips via mode 
# zero_mode <- trips |> #to_duckdb() |> 
#   group_by(scen) |>
#   summarise(total = n_distinct(p.ID),
#             .groups = 'drop') |>
#   right_join(
#     trips |>
#       group_by(mode, scen) |>
#       summarise(count = n_distinct(p.ID), .groups = 'drop'),
#     by = "scen") |>
#   mutate(zero = total - count,
#          zero_percent = round(zero/total*100,1)) |> 
#   collect()

t <- mget(c("trips_percentage_combined",
            "distance",
            "avg_time_combined",
            "combined_distance",
            "trips_percentage_combined"))


qs::qsave(t, "temp/241025_trips.qs")
