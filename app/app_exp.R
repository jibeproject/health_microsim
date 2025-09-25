rm(list = ls())

library(shiny)
library(dplyr)
library(readr)
library(DT)
library(arrow)

exposure <- arrow::open_dataset("temp/exp.parquet/") |> collect()

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
         mmetHr_total = round((mmetHr_cycle + mmetHr_otherSport + mmetHr_walk),2))

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
      DTOutput("summary_table_pm25"),
      
      h3("NO2"),
      DTOutput("summary_table_no2"),
      
      h3("Total mMET-hours/week"),
      DTOutput("summary_table_mmetHr"),
      
      h3("Noise Lden"),
      DTOutput("summary_table_noiseLden"),
      
      h3("Noise %High Annoyed"),
      DTOutput("summary_table_noiseHA"),
      
      h3("Noise %High Sleep Disturbance"),
      DTOutput("summary_table_noiseHSD"),
      
      h3("NDVI"),
      DTOutput("summary_table_ndvi")
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
    
    return(data)
  })
  
  # Compute PM2.5 summary table
  summary_table_pm25 <- reactive({
    filtered_data() %>%
      group_by(scen) %>%
      summarise(
        Mean = round(mean(exposure_normalised_pm25, na.rm = TRUE), 2),
        "5th" = round(quantile(exposure_normalised_pm25, 0.05, na.rm = TRUE), 2),
        "20th" = round(quantile(exposure_normalised_pm25, 0.20, na.rm = TRUE), 2),
        "25th" = round(quantile(exposure_normalised_pm25, 0.25, na.rm = TRUE), 2),
        "35th" = round(quantile(exposure_normalised_pm25, 0.35, na.rm = TRUE), 2),
        "50th" = round(quantile(exposure_normalised_pm25, 0.50, na.rm = TRUE), 2),
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
        "20th" = round(quantile(exposure_normalised_no2, 0.20, na.rm = TRUE), 2),
        "25th" = round(quantile(exposure_normalised_no2, 0.25, na.rm = TRUE), 2),
        "35th" = round(quantile(exposure_normalised_no2, 0.35, na.rm = TRUE), 2),
        "50th" = round(quantile(exposure_normalised_no2, 0.50, na.rm = TRUE), 2),
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
        Mean = round(mean(exposure_noise_HA, na.rm = TRUE), 2)
      )
  })
  
  # Compute Noise HSD summary table
  summary_table_noiseHSD <- reactive({
    filtered_data() %>%
      group_by(scen) %>%
      summarise(
        Mean = round(mean(exposure_noise_HSD, na.rm = TRUE), 2)
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
  
  # Show different values of the PM2.5 table
  output$summary_table_pm25 <- renderDT({
    datatable(summary_table_pm25(), 
              options = list(
                autoWidth = TRUE,
                searching = FALSE,  # Disable the search bar
                paging = FALSE,     # Optional: disable pagination if needed
                info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
              ))
  })
  
  # Show different values of the NO2 table
  output$summary_table_no2 <- renderDT({
    datatable(summary_table_no2(), 
              options = list(
                autoWidth = TRUE,
                searching = FALSE,  # Disable the search bar
                paging = FALSE,     # Optional: disable pagination if needed
                info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
              ))
  })
  
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
  
  # Show different values of the Noise Lden table
  output$summary_table_noiseLden <- renderDT({
    datatable(summary_table_noiseLden(), 
              options = list(
                autoWidth = TRUE,
                searching = FALSE,  # Disable the search bar
                paging = FALSE,     # Optional: disable pagination if needed
                info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
              ))
  })
  
  # Show different values of the Noise HA table
  output$summary_table_noiseHA <- renderDT({
    datatable(summary_table_noiseHA(), 
              options = list(
                autoWidth = TRUE,
                searching = FALSE,  # Disable the search bar
                paging = FALSE,     # Optional: disable pagination if needed
                info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
              ))
  })
  
  # Show different values of the Noise HSD table
  output$summary_table_noiseHSD <- renderDT({
    datatable(summary_table_noiseHSD(), 
              options = list(
                autoWidth = TRUE,
                searching = FALSE,  # Disable the search bar
                paging = FALSE,     # Optional: disable pagination if needed
                info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
              ))
  })
  
  # Show different values of the NDVI table
  output$summary_table_ndvi <- renderDT({
    datatable(summary_table_ndvi(), 
              options = list(
                autoWidth = TRUE,
                searching = FALSE,  # Disable the search bar
                paging = FALSE,     # Optional: disable pagination if needed
                info = FALSE        # Optional: disable information display (e.g., "Showing 1 to 10 of 50 entries")
              ))
  })
}

# Run the App
shinyApp(ui = ui, server = server)
