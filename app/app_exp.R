library(shiny)
library(tidyverse)
library(readr)
library(gt)
library(gtExtras)
library(arrow)
library(here)

exposure <- arrow::open_dataset(here("temp/exp.parquet")) |> collect()

################### Adding Zone and Demographics ########################

exposure <- exposure %>% 
  filter(age >= 18) %>% 
  mutate(gender = factor(gender,
                         levels = c(1,2),
                         labels = c("Male","Female")),
         age_group = factor(case_when(
           age >= 18 & age <= 25 ~ "18-25",
           age >= 26 & age <= 65 ~ "26-65",
           age >= 66 ~ "65+",
           TRUE ~ "Other"), 
           levels = c("18-25", "26-65", "65+", "Other")),
         ethnicity = as.factor(ethnicity),
         imd10 = factor(imd10,
                        levels = c(1,2,3,4,5,6,7,8,9,10),
                        labels = c("Most Deprived", 2,3,4,5,6,7,8,9,"Least Deprived")),
         income_group = cut(income, 
                            breaks = quantile(income, probs = seq(0, 1, 0.2), na.rm = TRUE), 
                            labels = c("Lowest", "Low", "Middle", "High", "Highest"),
                            include.lowest = TRUE),
         mmetHr_total = round((mmetHr_cycle + mmetHr_otherSport + mmetHr_walk),2),
         age_group_inj = as.character(cut(
           age,
           breaks = c(-Inf, 16, 20, 29, 39, 49, 59, 69, Inf),
           labels = c("< 17", "17-20", "21-29", "30-39", "40-49", "50-59", "60-69", "70+"),
           right = TRUE
         ))
  )

manchester_folder <- "/media/ali/Expansion/backup_tabea/manchester-main/"
agp <- read_csv(paste0(manchester_folder, "input/accident/age_gender_rr.csv"))

agp <- agp |>  
  mutate(gender = case_when(
    gender == "Male" ~ 1,
    gender == "Female" ~ 2
  ))

add_agroup <- function(df){
  return(df |> 
           mutate(
             age_group = as.character(cut(
               age,
               breaks = c(-Inf, 16, 20, 29, 39, 49, 59, 69, Inf),
               labels = c("< 17", "17-20", "21-29", "30-39", "40-49", "50-59", "60-69", "70+"),
               right = TRUE
             )
             )))
}

calc_wshare <- function(df, agp, scen = "base"){
  # df <- ss
  # scen <- 'ss'
  cf <- 0
  if (scen == "base"){
    cf <- 0.741037452
  } else if (scen == "ss"){
    cf <- 0.734002674
  } else if (scen == "green"){
    cf <- 0.720561993
  } else if (scen == "both"){
    cf <- 0.714050054
  }
  df <- df |> left_join(filter(agp, mode == "Walk"), by = c("age_group", "gender")) |> 
    mutate(injuryRiskWalk = severeFatalInjuryWalk * RR * cf,
           fRiskWalk = severeFatalInjuryWalk * RR * cf * percent_killed)
  print(scen)
  print(paste("Casualties: ", round(sum(df$injuryRiskWalk), 2)))
  print(paste("Fatalities: ", round(sum(df$fRiskWalk), 2)))
  print(paste("Injuries: ", round(sum(df$injuryRiskWalk) - sum(df$fRiskWalk), 2)))
}

# Define UI
ui <- fluidPage(
  titlePanel("Weekly Person Centric Exposure Summary Tables"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender:", choices = c("All", levels(exposure$gender)), selected = "All"),
      selectInput("age_group", "Age Group:", choices = c("All", levels(exposure$age_group)), selected = "All"),
      selectInput("ethnicity", "Ethnicity:", choices = c("All", levels(exposure$ethnicity)), selected = "All"),
      selectInput("imd", "IMD:", choices = c("All", levels(exposure$imd10)), selected = "All"),
      selectInput("location", "Location:", choices = c("All", unique(exposure$ladnm)), selected = "All"),
      sliderInput("mmetHr_total", "mMET-hours/week:", min = min(exposure$mmetHr_total, na.rm = TRUE), 
                  max = max(exposure$mmetHr_total, na.rm = TRUE), value = range(exposure$mmetHr_total, na.rm = TRUE))
    ),
    
    mainPanel(
      h3("PM2.5"),
      gt_output(outputId = "summary_table_pm25"),

      h3("NO2"),
      gt_output("summary_table_no2"),

      h3("Total mMET-hours/week"),
      gt_output("summary_table_mmetHr"),

      h3("Noise Lden"),
      gt_output("summary_table_noiseLden"),

      #h3("Noise %High Annoyed"),
      gt_output("summary_table_noiseHA"),

      #h3("Noise %High Sleep Disturbance"),
      gt_output("summary_table_noiseHSD"),

      h3("NDVI"),
      gt_output("summary_table_ndvi"),
      
      #h3("Injuries"),
      #gt_output("summary_table_inj")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive dataset based on filters
  filtered_data <- reactive({
    data <- exposure 
    
    if (input$gender != "All") {
      data <- data %>% filter(gender == input$gender)
    }
    
    if (input$age_group != "All") {
      data <- data %>% filter(age_group == input$age_group)
    }
    
    if (input$ethnicity != "All") {
      data <- data %>% filter(ethnicity == input$ethnicity)
    }
    
    if (input$imd != "All") {
      data <- data %>% filter(imd10 == input$imd)
    }
    
    if (input$location != "All") {
      data <- data %>% filter(ladnm == input$location)
    }
    
    data <- data %>% filter(mmetHr_total >= input$mmetHr_total[1] & mmetHr_total <= input$mmetHr_total[2])
    
    data <- data |> mutate(scen = case_when(scen == "both" ~ "Greening + Safe Streets",
                                             scen == "safeStreet" ~ "Safe Streets",
                                             scen == "reference" ~ "Reference",
                                             scen == "green" ~ "Greening",
                                             .default = scen))
    
    return(data)
  })
  
  # Compute PM2.5 summary table
  summary_table_pm25 <- reactive({
    filtered_data() |> 
      group_by(scen) |> 
      summarise(
        Mean = round(mean(exposure_normalised_pm25, na.rm = TRUE), 2),
        "5th" = round(quantile(exposure_normalised_pm25, 0.05, na.rm = TRUE), 2),
        "25th" = round(quantile(exposure_normalised_pm25, 0.25, na.rm = TRUE), 2),
        "50th" = round(quantile(exposure_normalised_pm25, 0.50, na.rm = TRUE), 2),
        "75th" = round(quantile(exposure_normalised_pm25, 0.75, na.rm = TRUE), 2),
        "95th" = round(quantile(exposure_normalised_pm25, 0.95, na.rm = TRUE), 2)
      ) 
  })
  
  # Compute NO2 summary table
  summary_table_no2 <- reactive({
    filtered_data() %>%
      group_by(scen) %>%
      summarise(
        Mean = round(mean(exposure_normalised_no2, na.rm = TRUE), 2),
        "5th" = round(quantile(exposure_normalised_no2, 0.05, na.rm = TRUE), 2),
        "25th" = round(quantile(exposure_normalised_no2, 0.25, na.rm = TRUE), 2),
        "50th" = round(quantile(exposure_normalised_no2, 0.50, na.rm = TRUE), 2),
        "75th" = round(quantile(exposure_normalised_no2, 0.75, na.rm = TRUE), 2),
        "95th" = round(quantile(exposure_normalised_no2, 0.95, na.rm = TRUE), 2)
      )
  })
  
  # Compute MMET Hr Total summary table
  summary_table_mmetHr <- reactive({
    filtered_data() %>%
      group_by(scen) %>%
      summarise(
        Mean = round(mean(mmetHr_total, na.rm = TRUE), 2),
        "5th" = round(quantile(mmetHr_total, 0.05, na.rm = TRUE), 2),
        "20th" = round(quantile(mmetHr_total, 0.20, na.rm = TRUE), 2),
        "25th" = round(quantile(mmetHr_total, 0.25, na.rm = TRUE), 2),
        "35th" = round(quantile(mmetHr_total, 0.35, na.rm = TRUE), 2),
        "50th" = round(quantile(mmetHr_total, 0.50, na.rm = TRUE), 2),
        "95th" = round(quantile(mmetHr_total, 0.95, na.rm = TRUE), 2)
      )
  })
  
  # Compute Noise Lden summary table
  summary_table_noiseLden <- reactive({
    filtered_data() %>%
      group_by(scen) %>%
      summarise(
          Mean = round(mean(exposure_normalised_noise_Lden, na.rm = TRUE), 2),
          "5th" = round(quantile(exposure_normalised_noise_Lden, 0.05, na.rm = TRUE), 2),
          "20th" = round(quantile(exposure_normalised_noise_Lden, 0.20, na.rm = TRUE), 2),
          "25th" = round(quantile(exposure_normalised_noise_Lden, 0.25, na.rm = TRUE), 2),
          "35th" = round(quantile(exposure_normalised_noise_Lden, 0.35, na.rm = TRUE), 2),
          "50th" = round(quantile(exposure_normalised_noise_Lden, 0.50, na.rm = TRUE), 2),
          "95th" = round(quantile(exposure_normalised_noise_Lden, 0.95, na.rm = TRUE), 2)
        )
  })
  
  # Compute Noise HA summary table
  summary_table_noiseHA <- reactive({
    filtered_data() %>%
      group_by(scen) %>%
      summarise(
        "%" = round(mean(exposure_noise_HA, na.rm = TRUE), 2)
      )
  })
  
  # Compute Noise HSD summary table
  summary_table_noiseHSD <- reactive({
    filtered_data() %>%
      group_by(scen) %>%
      summarise(
        "%" = round(mean(exposure_noise_HSD, na.rm = TRUE), 2)
      )
  })
  
  # Compute Green NDVI summary table
  summary_table_ndvi <- reactive({
    filtered_data() %>%
      group_by(scen) %>%
      summarise(
        Mean = round(mean(exposure_normalised_ndvi, na.rm = TRUE), 2),
        "5th" = round(quantile(exposure_normalised_ndvi, 0.05, na.rm = TRUE), 2),
        "20th" = round(quantile(exposure_normalised_ndvi, 0.20, na.rm = TRUE), 2),
        "25th" = round(quantile(exposure_normalised_ndvi, 0.25, na.rm = TRUE), 2),
        "35th" = round(quantile(exposure_normalised_ndvi, 0.35, na.rm = TRUE), 2),
        "50th" = round(quantile(exposure_normalised_ndvi, 0.50, na.rm = TRUE), 2),
        "95th" = round(quantile(exposure_normalised_ndvi, 0.95, na.rm = TRUE), 2)
      )
  })
  
  summary_table_inj <- reactive({
    filtered_data() %>%
      group_by(scen) %>%
      summarise(
        "# of bike injuries" = sum(severeFatalInjuryBike),
        "# of pedestrian injuries" = sum(severeFatalInjuryWalk),
        "# of car injuries" = sum(severeFatalInjuryCar)
      )
  })
  
  get_gt_table <- function(df){
    return(df |> 
      gt() |> 
      data_color(
        columns = where(is.numeric),
        colors = scales::col_numeric(
          palette = "viridis", 
          domain = NULL),
        direction = "column"
      )
    )
  }
  
  output$summary_table_pm25 <- render_gt(expr = get_gt_table(summary_table_pm25()) |> 
                                           #opt_interactive(use_compact_mode = TRUE) |> 
                                           tab_options(table.width = "80%") |>
                                           # Set table header and subtitle
                                           tab_header(
                                             title = "Distribution of PM2.5 exposures by scenario",
                                             subtitle = "Percentile values are in µg/m³"
                                           )
  )
  
  
  # # Show different values of the PM2.5 table
  # output$summary_table_pm25 <- renderDT({
  #   datatable(summary_table_pm25(), 
  #             options = list(
  #               autoWidth = TRUE,
  #               searching = FALSE,  # Disable the search bar
  #               paging = FALSE,     # Optional: disable pagination if needed
  #               info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
  #             ))
  # })
  # 
  
  output$summary_table_no2 <- render_gt(expr = get_gt_table(summary_table_no2()) |> 
                                           #opt_interactive(use_compact_mode = TRUE) |> 
                                           tab_options(table.width = "80%") |>
                                           # Set table header and subtitle
                                           tab_header(
                                             title = "Distribution of NO2 exposures by scenario",
                                             subtitle = "Percentile values are in µg/m³"
                                           )
  )
  # # Show different values of the NO2 table
  # output$summary_table_no2 <- renderDT({
  #   datatable(summary_table_no2(), 
  #             options = list(
  #               autoWidth = TRUE,
  #               searching = FALSE,  # Disable the search bar
  #               paging = FALSE,     # Optional: disable pagination if needed
  #               info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
  #             ))
  # })
  
  # Show different values of the NO2 table
  output$summary_table_mmetHr <- renderDT({
    datatable(summary_table_mmetHr(), 
              options = list(
                autoWidth = TRUE,
                searching = FALSE,  # Disable the search bar
                paging = FALSE,     # Optional: disable pagination if needed
                info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
              ))
  })
  
  output$summary_table_mmetHr <- render_gt(expr = get_gt_table(summary_table_mmetHr()) |> 
                                          #opt_interactive(use_compact_mode = TRUE) |> 
                                          tab_options(table.width = "80%") |>
                                          # Set table header and subtitle
                                          tab_header(
                                            title = "Distribution of Physical Activity Volume by scenario",
                                            subtitle = "Percentile values are in Marginal MET hours per week"
                                          ))
  
  # # Show different values of the Noise Lden table
  # output$summary_table_noiseLden <- renderDT({
  #   datatable(summary_table_noiseLden(), 
  #             options = list(
  #               autoWidth = TRUE,
  #               searching = FALSE,  # Disable the search bar
  #               paging = FALSE,     # Optional: disable pagination if needed
  #               info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
  #             ))
  # })
  
  output$summary_table_noiseLden <- render_gt(expr = get_gt_table(summary_table_noiseLden()) |> 
                                             #opt_interactive(use_compact_mode = TRUE) |> 
                                             tab_options(table.width = "80%") |>
                                             # Set table header and subtitle
                                             tab_header(
                                               title = "Distribution of Noise pollution by scenario",
                                               subtitle = "Percentile values are in "
                                             ))
  
  # # Show different values of the Noise HA table
  # output$summary_table_noiseHA <- renderDT({
  #   datatable(summary_table_noiseHA(), 
  #             options = list(
  #               autoWidth = TRUE,
  #               searching = FALSE,  # Disable the search bar
  #               paging = FALSE,     # Optional: disable pagination if needed
  #               info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
  #             ))
  # })
  
  output$summary_table_noiseHA <- render_gt(expr = get_gt_table(summary_table_noiseHA()) |> 
                                                #opt_interactive(use_compact_mode = TRUE) |> 
                                                tab_options(table.width = "80%") |>
                                                # Set table header and subtitle
                                                tab_header(
                                                  title = "Proportion of highly annoyed by noise by scenario",
                                                  subtitle = "Percentile values "
                                                ))
  
  # Show different values of the Noise HSD table
  # output$summary_table_noiseHSD <- renderDT({
  #   datatable(summary_table_noiseHSD(), 
  #             options = list(
  #               autoWidth = TRUE,
  #               searching = FALSE,  # Disable the search bar
  #               paging = FALSE,     # Optional: disable pagination if needed
  #               info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
  #             ))
  # })
  
  output$summary_table_noiseHSD <- render_gt(expr = get_gt_table(summary_table_noiseHSD()) |> 
                                              #opt_interactive(use_compact_mode = TRUE) |> 
                                              tab_options(table.width = "80%") |>
                                              # Set table header and subtitle
                                              tab_header(
                                                title = "Proportion of highly sleep disturbed by scenario",
                                                subtitle = "Percentile values"
                                              ))
  
  # # Show different values of the NDVI table
  # output$summary_table_ndvi <- renderDT({
  #   datatable(summary_table_ndvi(), 
  #             options = list(
  #               autoWidth = TRUE,
  #               searching = FALSE,  # Disable the search bar
  #               paging = FALSE,     # Optional: disable pagination if needed
  #               info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
  #             ))
  # })
  
  
  output$summary_table_ndvi <- render_gt(expr = get_gt_table(summary_table_ndvi()) |> 
                                               #opt_interactive(use_compact_mode = TRUE) |> 
                                               tab_options(table.width = "80%") |>
                                               # Set table header and subtitle
                                               tab_header(
                                                 title = "Distribution of NDVI by scenario",
                                                 subtitle = ""
                                               ))
  
  
  output$summary_table_inj <- render_gt(expr = get_gt_table(summary_table_inj()) |> 
                                          fmt_number(
                                            columns = where(is.numeric),
                                            decimals = 1,
                                            use_seps = FALSE
                                          ) |> 
                                           #opt_interactive(use_compact_mode = TRUE) |> 
                                           tab_options(table.width = "80%") |>
                                           # Set table header and subtitle
                                           tab_header(
                                             title = "Total number of injuries by Travel Mode and Scenario",
                                             subtitle = ""
                                           ))
}

# Run the App
shinyApp(ui = ui, server = server)
