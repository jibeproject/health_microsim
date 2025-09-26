require(tidyverse)
require(sf)
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

pp <- read_csv(paste0(dir_path, "microData/pp_2021.csv"))

trips <- bind_rows(trips_ref, trips_green, trips_ss, trips_both)

rm (trips_ref, trips_both, trips_ss, trips_green)


trips <- trips |>
  left_join(zone |>
              dplyr::select(LAD_origin = ladnm,imd_origin = imd10, oaID),
            by = c("originZone" = "oaID")) |> 
  left_join(zone |> 
              dplyr::select(LAD_destination = ladnm, imd_destination = imd10, oaID),
            by = c("destinationZone" = "oaID")) |> 
  left_join(pp  |> 
              dplyr::select(id,age,gender,occupation), by = c("p.ID" = "id"))

rm(pp, zone)

trips <- trips |>
  #select(p.ID,t.distance_walk,t.distance_bike,t.distance_auto,t.distance_auto,time_auto,time_pt,mode,scenario,LAD_origin, imd_origin) |>
  mutate(time_walk = t.distance_walk/2.92,
         time_bike = t.distance_bike/10.44,
         time_pt = as.numeric(time_pt),
         time_auto = time_auto/60,
         time_pt = time_pt/60,
         mode = case_when(
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
                                        "Other")))

arrow::write_dataset(dataset = trips, path = "temp/260925_trips.parquet", partitioning = "scen")

## Creating Visualizations

### Table of the number of trips in each local authority in Greater Manchester

trips <- arrow::open_dataset("temp/260925_trips.parquet")
# Table of the number of trips in each local authority in Greater Manchester
trip_counts <- trips |>  
  select(t.id, LAD_origin, LAD_destination, scen) |>
  to_duckdb() |> 
  pivot_longer(cols = c(LAD_origin, LAD_destination),
               names_to = "type",
               values_to = c("location")) |>
  group_by(location, scen) |>
  summarise(trip_count = n()) |> 
  group_by(scen) |> 
  mutate(percent_of_total = (trip_count / sum(trip_count))) |> 
  collect()

trip_counts |>
  arrange(desc(percent_of_total)) |>
  #select(location, percent_of_total) |>  # Exclude trip_count column
  mutate(percent_of_total = percent_of_total * 100) %>%  # convert to %
  gt(rowname_col = "location", groupname_col = "scen") %>%
  fmt_number(
    columns = percent_of_total,
    decimals = 1,
    suffixing = FALSE
  ) %>%
  cols_label(
    percent_of_total = "Percent of Total"
  ) %>%
  tab_header(
    title = "Percent of Total Trips by Scenario"
  )



  gt() |>
  tab_header(
    title = "Trips by Location",
    subtitle = "Percentage of Trips Within Local Authority Districts"
  ) |>
  cols_label(
    location = "Location",
    percent_of_total = "% of Total"
  ) |> 
  fmt_percent(
    columns = percent_of_total,
    decimals = 1)

```

## Trips by Index of Multiple Deprivation (IMD) characteristics

```{r}
# Trips by various demographic characteristics

total_trips <- nrow(trips)

# Distribution of trips by mode and location 

trips_percentage <- trips |> to_duckdb() |> 
  group_by(LAD_origin, scen) |>
  mutate(total_trips = n()) |>
  ungroup() |> 
  group_by(LAD_origin, mode, scen) |>
  summarise(trip_count = n(), total_trips = first(total_trips), .groups = 'drop') |>
  mutate(percentage_of_trips = (trip_count / total_trips) * 100) |> collect()

trips_percentage_all <- trips_percentage |>
  group_by(mode, scen) |>
  summarise(trip_count = mean(trip_count),
            total_trips = mean(total_trips),
            percentage_of_trips = (trip_count / total_trips) * 100, .groups = 'drop') |>
  mutate(LAD_origin = "All Locations")

trips_percentage_combined <- bind_rows(trips_percentage |> collect(), trips_percentage_all |> collect())

ggplotly(ggplot(trips_percentage_combined) +
           aes(x = mode, y = percentage_of_trips, fill = scen) +
           geom_col(position = "dodge2") +
           geom_text(aes(label = round(percentage_of_trips, 1),
                         y = percentage_of_trips),
                     size = 2, 
                     hjust = 1.1, 
                     vjust = 0.2,
                     position = position_dodge(1),
                     inherit.aes = TRUE
           ) +
           scale_fill_hue(direction = 1) +
           coord_flip() +
           theme_minimal() +
           facet_wrap(vars(LAD_origin)) +
           labs(
             title = "% trips by mode and location",
             fill = "Scenario")
)



# By IMD

trips_percentage_imd <- trips |> to_duckdb() |> 
  group_by(imd_origin, scen) |>
  mutate(total_trips = n()) |>
  ungroup() |> 
  group_by(imd_origin, mode, scen) |>
  summarise(trip_count = n(), total_trips = first(total_trips), .groups = 'drop') |>
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


ggplotly(
  ggplot(trips_percentage_combined_imd, aes(x = mode, y = percentage_of_trips, fill = scen)) +
    geom_col(position = "dodge2") +
    scale_fill_hue(direction = 1) +
    geom_text(aes(label = round(percentage_of_trips, 1), y = percentage_of_trips),
              size = 2, #hjust = -0.1, 
              hjust = 1.1, 
              vjust = 0.2,
              position = position_dodge(1),
              inherit.aes = TRUE
    ) +
    coord_flip() +
    theme_minimal() +
    facet_wrap(vars(imd)) +
    labs(
      title = "% trips by mode IMD",
      fill = "Scenario")
)

# Average weekly distance by mode of transportation
pp=trips|> to_duckdb() |> 
  group_by(p.ID, LAD_origin, scen)|>
  summarise(Cycling = sum(t.distance_bike[mode=="Cycling"]),
            Walking = sum(t.distance_walk[mode=="Walking"]),
            Public_Transport = sum(t.distance_auto[mode=="Public Transport"]),
            Driving_Car = sum(t.distance_auto[mode=="Driving Car"]),
            Car_Passenger = sum(t.distance_auto[mode=="Car Passenger"])) |> collect()

pp <- pp |> gather(mode,dist,Cycling:Car_Passenger)

pop_lad <- trips |> distinct(p.ID, LAD_origin) |> group_by(LAD_origin) |> summarise(pop = n())

tot_pop <- pop_lad |> dplyr::select(pop) |> pull() |> sum()

summary_distance <- pp |>  
  filter(!is.na(dist)) |> 
  left_join(pop_lad, copy = T) |> 
  group_by(mode, LAD_origin, scen) |> 
  summarise(avgDistance = round(sum(dist) / first(pop), 1))

pp_all=trips|> to_duckdb() |> 
  group_by(p.ID, scen)|>
  summarise(Cycling=sum(t.distance_bike[mode=="Cycling"]),
            Walking=sum(t.distance_walk[mode=="Walking"]),
            Public_Transport=sum(t.distance_auto[mode=="Public Transport"]),
            Driving_Car=sum(t.distance_auto[mode=="Driving Car"]),
            Car_Passenger = sum(t.distance_auto[mode=="Car Passenger"])) |> collect()

pp_all <- pp_all|>gather(mode,dist,Cycling:Car_Passenger)

summary_distance_all <- pp_all |> 
  filter(!is.na(dist)) |> 
  group_by(mode, scen) |>  
  summarise(avgDistance = round(sum(dist, na.rm = T) / tot_pop, 1)) |>  
  mutate(LAD_origin = "All Locations")

combined_distance <- bind_rows(summary_distance,summary_distance_all) 

ggplotly(
  ggplot(combined_distance) +
    aes(x = mode, y = avgDistance, fill = scen) +
    geom_col(position = "dodge2") +
    scale_fill_hue(direction = 1) +
    geom_text(aes(label = round(avgDistance, 1), y = avgDistance),
              size = 2, #hjust = -0.1, 
              hjust = 1.1, 
              vjust = 0.2,
              position = position_dodge(1),
              inherit.aes = TRUE
    ) +
    coord_flip() +
    theme_minimal() +
    facet_wrap(vars(LAD_origin)) +
    labs(title = "Average weekly dist. pp by mode and location",
         fill = "Scenario")
)



# Average time spent per person by mode and location
tt = trips |> to_duckdb() |>
  group_by(p.ID, LAD_origin, scen)|>
  summarise(Cycling=sum(time_bike[mode=="Cycling"], na.rm = T),
            Walking=sum(time_walk[mode=="Walking"], na.rm = T),
            Public_Transport=sum(time_pt[mode=="Public Transport"], na.rm = T),
            Driving_Car=sum(time_auto[mode=="Driving Car"], na.rm = T),
            Car_Passenger=sum(time_auto[mode=="Car Passenger"], na.rm = T)) |> collect()

tt=tt|>gather(mode,time,Cycling:Car_Passenger)

summary_time=tt|>
  group_by(mode, LAD_origin, scen)|>
  summarise(avgTime=mean(time, na.rm = T))

tt_all=trips|> to_duckdb() |>
  group_by(p.ID, scen)|>
  summarise(Cycling=sum(time_bike[mode=="Cycling"], na.rm = T),
            Walking=sum(time_walk[mode=="Walking"], na.rm = T),
            `Public Transport`=sum(time_pt[mode=="Public Transport"], na.rm = T),
            `Driving Car`=sum(time_auto[mode=="Driving Car"], na.rm = T),
            `Car Passenger`=sum(time_auto[mode=="Car Passenger"], na.rm = T)) |> collect()

tt_all=tt_all|>gather(mode,time,Cycling:`Car Passenger`)

summary_time_all=tt_all|>
  group_by(mode, scen)|>
  summarise(avgTime=mean(time, na.rm = T)) |>
  mutate(LAD_origin = "All Locations")

avg_time_combined <- bind_rows(summary_time, summary_time_all)

ggplotly(ggplot(avg_time_combined) +
           aes(x = mode, y = avgTime, fill = scen) +
           geom_col(position = "dodge2") +
           geom_text(aes(label = round(avgTime, 1),
                         y = avgTime),
                     size = 2, #hjust = -0.1, 
                     hjust = 1.1, 
                     vjust = 0.2,
                     position = position_dodge(1),
                     inherit.aes = TRUE
           ) +
           scale_fill_hue(direction = 1) +
           labs(title = "Average weekly time (in hours) by mode per person and location",
                fill = "Scenario",
                x = "", y = "Hours") +
           coord_flip() +
           theme_minimal() +
           facet_wrap(vars(LAD_origin))
)

# Stacked Bar Plots for Average Distance via Transport Mode

trips <- trips |> 
  to_duckdb() |> 
  mutate(distance = case_when(
    mode %in% c("Driving Car", "Car Passenger", "Public Transport") ~ t.distance_auto,
    mode == "Walking" ~ t.distance_walk,
    mode == "Cycling" ~ t.distance_bike,
    TRUE ~ NA_real_)) |> 
  mutate(
    distance_bracket = cut(
      distance,
      breaks = c(0, 1, 3, 5, 10, 20, 40, Inf),
      labels = c("0-1", "1-3", "3-5", "5-10", "10-20", "20-40", "40+"),
      right = FALSE
    )
  )

# trips$distance_bracket <- cut(
#   trips$distance,
#   breaks = c(0, 1, 3, 5, 10, 20, 40, Inf),
#   labels = c("0-1", "1-3", "3-5", "5-10", "10-20", "20-40", "40+"),
#   right = FALSE)

distance <- trips |>
  group_by(distance_bracket, mode, scen) |>
  summarise(count = n(), .groups = 'drop') |>
  group_by(distance_bracket, scen) |>
  mutate(percent = count / sum(count) * 100) |> 
  collect()

ggplot(distance, aes(x = distance_bracket, y = percent, fill = mode)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = ifelse(percent > 1, paste0(round(percent, 1), "%"), "")), # Show label only if >= 1%
    position = position_fill(vjust = 0.5), 
    color = "white",
    size = 3
  ) +
  labs(
    title = "Transport Mode Share by Trip Distance",
    y = "Proportion (%)",
    x = "Distance (km)",
    fill = "Transport Mode"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    strip.placement = "outside", 
    strip.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  ) +
  facet_wrap(~ scen, scales = "free_x")


# People with Zero trips via mode 

zero_mode <- trips |>
  group_by(scen) |>
  summarise(total = n_distinct(p.ID),
            .groups = 'drop') |>
  right_join(
    trips |>
      group_by(mode, scen) |>
      summarise(count = n_distinct(p.ID), .groups = 'drop'),
    by = "scen") |>
  mutate(zero = total - count,
         zero_percent = round(zero/total*100,1))

ggplot(zero_mode, aes(x = mode, y = zero_percent, fill = scen)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = paste0(zero_percent, "%")),
    position = position_dodge(width = 0.9), 
    vjust = -0.25,                       
    size = 3) +
  labs(
    title = "Proportion of Individuals Reporting Non-Usage of Specific Transport Modes",
    y = "Proportion (%)",
    fill = "Scenario") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"), 
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"))
