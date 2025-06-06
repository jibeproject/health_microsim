# health_scenario_analysis.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(DT)
  library(shiny)
})

## data_prep_shiny.R has to be run for each different run of the data (in scripts)


life_years_over_time <- readRDS("manchester/health/processed/life_years_overtime.RDS")
life_years_accumulated <- readRDS("manchester/health/processed/accumulated_life_years.RDS")
disease_dead_avoided <- readRDS("manchester/health/processed/avoided_events.RDS")
delay_events <- readRDS("manchester/health/processed/delay_days.RDS")
std_rates_table <- readRDS("manchester/health/processed/std_rates_tables.RDS")

# === Shiny App ===
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

shinyApp(
  ui = fluidPage(
    titlePanel("Health Scenario Analysis"),
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.tabs == 'Rates'",
          selectInput("value_select", "Select Condition:",
                      choices = unique(std_rates_table$value)),
          selectInput("cycle_select", "Select Cycle:",
                      choices = sort(unique(std_rates_table$cycle))),
          selectInput("facet_by", "Facet By (Rates):",
                      choices = unique(std_rates_table$type))
        ),
        conditionalPanel(
          condition = "input.tabs == 'Life Years Over Time'",
          selectInput("life_facet", "Facet Life Years by:",
                      choices = unique(life_years_over_time$type))
        ),
        conditionalPanel(
          condition = "input.tabs == 'Accumulated Life Years'",
          selectInput("acc_life_facet", "Facet Accumulated Life Years by:",
                      choices = unique(life_years_accumulated$type))
        ),
        conditionalPanel(
          condition = "input.tabs == 'Avoided Deaths by Disease'",
          selectInput("avoided_disease_select", "Select Disease:",
                      choices = unique(disease_dead_avoided$disease)),
          selectInput("avoided_facet", "Facet Avoided Deaths by:",
                      choices = unique(disease_dead_avoided$type))
        ),
        conditionalPanel(
          condition = "input.tabs == 'Delay in Events'",
          selectInput("delay_disease_select", "Select Disease:",
                      choices = unique(delay_events$disease))
        )
      ),
      mainPanel(
        tabsetPanel(
          id = "tabs",
          tabPanel("Rates",
                   plotOutput("typeRatePlot"),
                   dataTableOutput("typeRateTable")
          ),
          tabPanel("Life Years Over Time",
                   plotOutput("lifeYearsPlot"),
                   dataTableOutput("lifeYearsTable")
          ),
          tabPanel("Accumulated Life Years",
                   plotOutput("accLifeYearsPlot"),
                   dataTableOutput("accLifeYearsTable")
          ),
          tabPanel("Avoided Deaths by Disease",
                   plotOutput("avoidedDeathsPlot"),
                   dataTableOutput("avoidedDeathsTable")
          ),
          tabPanel("Delay in Events",
                   dataTableOutput("delayEventsTable")
          )
        )
      )
    )
  ),
  
  server = function(input, output) {
    # === Rates ===
    filtered_data <- reactive({
      std_rates_table %>%
        filter(
          value == input$value_select,
          cycle == input$cycle_select,
          type == input$facet_by
        )
    })
    
    output$typeRatePlot <- renderPlot({
      df <- filtered_data()
      req(nrow(df) > 0)
      
      ggplot(df, aes(x = scen, y = rate, fill = scen)) +
        geom_col(position = position_dodge()) +
        facet_wrap(~name, scales = "free_y") +
        labs(
          title = paste("Rates for", input$value_select, "at cycle", input$cycle_select, "by", input$facet_by),
          y = "Rate per 100,000",
          x = "Scenario"
        ) +
        theme_minimal()
    })
    
    output$typeRateTable <- renderDataTable({
      df <- filtered_data()
      
      if (input$facet_by != "overall" && input$facet_by %in% names(df)) {
        df <- df %>% select(scen, !!sym(input$facet_by), value, cycle, rate) %>% distinct()
      } else {
        df <- df %>% select(scen, value, cycle, rate, type, name) %>% distinct()
      }
      
      datatable(df, options = list(pageLength = 10))
    })
    
    # === Life Years Over Time ===
    filtered_life_data <- reactive({
      life_years_over_time %>%
        filter(type == input$life_facet)
    })
    
    output$lifeYearsPlot <- renderPlot({
      df <- filtered_life_data()
      req(nrow(df) > 0)
      
      ggplot(df, aes(x = cycle, y = alive, color = scen, group = scen)) +
        geom_line(linewidth = 1) +
        facet_wrap(~name, scales = "free_y") +
        labs(
          title = paste("Life Years Over Time by", input$life_facet),
          y = "Life Years",
          x = "Cycle"
        ) +
        theme_minimal()
    })
    
    output$lifeYearsTable <- renderDataTable({
      df <- filtered_life_data()
      df <- df %>% select(cycle, scen, alive, type, name)
      datatable(df, options = list(pageLength = 10))
    })
    
    # === Accumulated Life Years ===
    filtered_acc_life_data <- reactive({
      life_years_accumulated %>%
        filter(type == input$acc_life_facet)
    })
    
    output$accLifeYearsPlot <- renderPlot({
      df <- filtered_acc_life_data()
      req(nrow(df) > 0)
      
      ggplot(df, aes(x = scen, y = alive, fill = scen)) +
        geom_col(position = position_dodge()) +
        facet_wrap(~name, scales = "free_y") +
        labs(
          title = paste("Accumulated Life Years by", input$acc_life_facet),
          y = "Life Years (Accumulated)",
          x = "Scenario"
        ) +
        theme_minimal()
    })
    
    output$accLifeYearsTable <- renderDataTable({
      df <- filtered_acc_life_data()
      df <- df %>% select(scen, alive, type, name)
      datatable(df, options = list(pageLength = 10))
    })
    
    # === Avoided Deaths by Disease ===
    filtered_avoided_data <- reactive({
      disease_dead_avoided %>%
        filter(
          disease == input$avoided_disease_select,
          type == input$avoided_facet
        )
    })
    
    output$avoidedDeathsPlot <- renderPlot({
      df <- filtered_avoided_data()
      req(nrow(df) > 0)
      
      ggplot(df, aes(x = scenario, y = avoided_n, fill = scenario)) +
        geom_col(position = position_dodge()) +
        facet_wrap(~name, scales = "free_y") +
        labs(
          title = paste("Avoided cases for", input$avoided_disease_select, "by", input$avoided_facet),
          y = "Avoided Deaths",
          x = "Scenario"
        ) +
        theme_minimal()
    })
    
    output$avoidedDeathsTable <- renderDataTable({
      df <- filtered_avoided_data()
      df <- df %>% select(scenario, avoided_n, type, name)
      datatable(df, options = list(pageLength = 10))
    })
    
    # === Delay in Events ===
    filtered_delay_data <- reactive({
      delay_events %>%
        filter(disease == input$delay_disease_select)
    })
    
    output$delayEventsTable <- renderDataTable({
      df <- filtered_delay_data()
      df <- df %>%
        mutate(delay_days = round(delay_days, 0)) %>%
        select(scenario, delay_days, type, name)
      
      datatable(df, options = list(pageLength = 10))
    })
  }
)
