# health_scenario_analysis.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(DT)
  library(shiny)
  library(plotly)
  library(shinyWidgets)
})

## data_prep_shiny.R has to be run for each different run of the data (in scripts)

## Read closed cohort, no AP
# life_years_over_time <- readRDS("manchester/health/processed/life_years_overtime.RDS")
# life_years_accumulated <- readRDS("manchester/health/processed/accumulated_life_years.RDS")
# disease_dead_avoided <- readRDS("manchester/health/processed/avoided_events.RDS")
# delay_events <- readRDS("manchester/health/processed/delay_days.RDS")
# std_rates_table <- readRDS("manchester/health/processed/std_rates_tables.RDS")

## Read Java 1p all exposures

life_years_over_time <- readRDS("shiny_data/life_years_overtime_java_1p.RDS") |> filter(!is.na(name))
life_years_accumulated <- readRDS("shiny_data/accumulated_life_years_java_1p.RDS") |> filter(!is.na(name))
disease_dead_avoided <- readRDS("shiny_data/avoided_events_java_1p.RDS") |> filter(!is.na(name))
delay_events <- readRDS("shiny_data/delay_days_java_1p.RDS") |> filter(!is.na(name))
std_rates_table <- readRDS("shiny_data/std_rates_tables_java_1p.RDS")# |> filter(std_rate_inc > 0)

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
          condition = "input.tabs == 'Age standardised rate per 100,000'",
          awesomeRadio("rate_select", "Select Rate:",
                       choices = c("Incidence", "Prevalence")),
          awesomeRadio("value_select", "Select Condition:",
                      choices = unique(std_rates_table$value)),
          awesomeRadio("cycle_select", "Select Cycle:",
                      choices = sort(unique(std_rates_table$cycle))),
          awesomeRadio("facet_by", "Facet By (Rates):",
                      choices = unique(std_rates_table$type))
        ),
        conditionalPanel(
          condition = "input.tabs == 'Life years gained over time'",
          awesomeRadio("life_facet", "Facet Life Years by:",
                      choices = unique(life_years_over_time$type))
        ),
        conditionalPanel(
          condition = "input.tabs == 'Accumulated life years gained over time'",
          awesomeRadio("acc_life_facet", "Facet Accumulated Life Years by:",
                      choices = unique(life_years_accumulated$type))
        ),
        conditionalPanel(
          condition = "input.tabs == 'Avoided events'",
          awesomeRadio("avoided_disease_select", "Select Disease:",
                      choices = unique(disease_dead_avoided$disease)),
          awesomeRadio("avoided_facet", "Facet Avoided Deaths by:",
                      choices = unique(disease_dead_avoided$type))
        ),
        conditionalPanel(
          condition = "input.tabs == 'Delay in events'",
          awesomeRadio("delay_disease_select", "Select Disease:",
                      choices = unique(delay_events$disease))
        )
      ),
      mainPanel(
        tabsetPanel(
          id = "tabs",
          tabPanel("Age standardised rate per 100,000",
                   plotlyOutput("typeRatePlot"),
                   dataTableOutput("typeRateTable")
          ),
          tabPanel("Life years gained over time",
                   plotlyOutput("lifeYearsPlot"),
                   dataTableOutput("lifeYearsTable")
          ),
          tabPanel("Accumulated life years gained over time",
                   plotlyOutput("accLifeYearsPlot"),
                   dataTableOutput("accLifeYearsTable")
          ),
          tabPanel("Avoided events",
                   plotlyOutput("avoidedDeathsPlot"),
                   dataTableOutput("avoidedDeathsTable")
          ),
          tabPanel("Delay in events",
                   dataTableOutput("delayEventsTable")
          )
        )
      )
    )
  ),
  
  server = function(input, output) {
    # === Rates ===
    filtered_data <- reactive({
      
      if (input$rate_select == "Prevalence"){
      std_rates_table |>
        filter(
          value == input$value_select,
          cycle == input$cycle_select,
          type == input$facet_by
        ) |> 
          dplyr::select(-c(std_rate_inc)) |> 
          distinct()
      }else{
          std_rates_table |>
            filter(
              value == input$value_select,
              cycle == input$cycle_select,
              type == input$facet_by
            ) |> 
          dplyr::select(-c(std_rate_prev)) |> 
            distinct()
      }
    })
    
    output$typeRatePlot <- renderPlotly({
      df <- filtered_data()
      req(nrow(df) > 0)
      
      if(input$rate_select == "Prevalence"){
        plotly::ggplotly(ggplot(df, aes(x = scen, y = std_rate_prev, fill = scen, label = std_rate_prev)) +
                           geom_col(position=position_dodge2(preserve = "single")) +
                           # facet_grid(~name, scales = "free_y") +
                           facet_wrap(~name, scales = "free_y") +
                           geom_text(inherit.aes = T, size = 6/.pt,
                                     aes(label = round(std_rate_prev, 2), 
                                         y= std_rate_prev - .1 * after_scale(max(std_rate_prev)))) +
                           labs(
                             title = paste("Prevalence rates for", input$value_select, "at cycle", input$cycle_select, "by", input$facet_by),
                             y = "Rate per 100,000",
                             x = "Scenario"
                           ) +
                           theme_minimal()
        )
      }else{
          
          plotly::ggplotly(ggplot(df, aes(x = scen, y = std_rate_inc, fill = scen, label = std_rate_inc)) +
                             geom_col(position=position_dodge2(preserve = "single")) +
                             # facet_grid(~name, scales = "free_y") +
                             facet_wrap(~name, scales = "free_y") +
                             geom_text(inherit.aes = T, size = 6/.pt,
                                       aes(label = round(std_rate_inc, 2), 
                                           y= std_rate_inc - .1 * after_scale(max(std_rate_inc)))) +
                             labs(
                               title = paste("Incidence rates for", input$value_select, "at cycle", input$cycle_select, "by", input$facet_by),
                               y = "Rate per 100,000",
                               x = "Scenario"
                             ) +
                             theme_minimal()
          )
        }
      
    })
    
    output$typeRateTable <- renderDataTable({
      df <- filtered_data()
      
      if (input$rate_select == "Incidence")
        if (input$facet_by != "overall" && input$facet_by %in% names(df)) {
          df <- df |> select(scen, !!sym(input$facet_by), value, cycle, std_rate_inc) |> distinct()
        } else {
          df <- df |> select(scen, value, cycle, std_rate_inc, type, name) |> distinct()
        }
      else{
        if (input$facet_by != "overall" && input$facet_by %in% names(df)) {
          df <- df |> select(scen, !!sym(input$facet_by), value, cycle, std_rate_prev) |> distinct()
        } else {
          df <- df |> select(scen, value, cycle, std_rate_prev, type, name) |> distinct()
        }
      }
      
      datatable(df, options = list(pageLength = 10))
    })
    
    # === Life Years Over Time ===
    filtered_life_data <- reactive({
      life_years_over_time |>
        filter(type == input$life_facet)
    })
    
    output$lifeYearsPlot <- renderPlotly({
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
      df <- df |> select(cycle, scen, alive, type, name)
      datatable(df, options = list(pageLength = 10))
    })
    
    # === Accumulated Life Years ===
    filtered_acc_life_data <- reactive({
      life_years_accumulated |>
        filter(type == input$acc_life_facet)
    })
    
    output$accLifeYearsPlot <- renderPlotly({
      df <- filtered_acc_life_data()
      req(nrow(df) > 0)
      
      p <- ggplot(df, aes(x = scen, y = alive, fill = scen)) +
        geom_col(width = 0.9, position = position_dodge2(preserve = "single")) +
        facet_wrap(~name, scales = "free_y") +
        labs(
          title = paste("Accumulated Life Years by", input$acc_life_facet),
          y = "Life Years (Accumulated)",
          x = "Scenario"
        ) +
        theme_minimal()
      
      if (input$acc_life_facet == "overall") {
        p <- p + geom_text(
          inherit.aes = TRUE, size = 6/.pt,
          aes(
            label = formatC(alive, format = "e"),
            y = alive - .1 * after_scale(max(alive))
          )
        )
      }
      
      plotly::ggplotly(p)
      
      
      # plotly::ggplotly(
      #   ggplot(df, aes(x = scen, y = alive, fill = scen)) +
      #   geom_col(position=position_dodge2(preserve = "single")) +
      #   facet_wrap(~name, scales = "free_y") +
      #   if (input$acc_life_facet == "overall")  {
      #     geom_text(inherit.aes = T, size = 6/.pt,
      #                                                 aes(label = formatC(alive, format = "e"), # round(alive, 2) 
      #                                                     y= alive - .1 * after_scale(max(alive))))
      #     } +
      #   labs(
      #     title = paste("Accumulated Life Years by", input$acc_life_facet),
      #     y = "Life Years (Accumulated)",
      #     x = "Scenario"
      #   ) +
      #   theme_minimal()
      # )
    })
    
    output$accLifeYearsTable <- renderDataTable({
      df <- filtered_acc_life_data()
      df <- df |> select(scen, alive, type, name)
      datatable(df, options = list(pageLength = 10))
    })
    
    # === Avoided Deaths by Disease ===
    filtered_avoided_data <- reactive({
      disease_dead_avoided |>
        filter(
          disease == input$avoided_disease_select,
          type == input$avoided_facet
        )
    })
    
    output$avoidedDeathsPlot <- renderPlotly({
      df <- filtered_avoided_data()
      req(nrow(df) > 0)
      
      ggplot(df, aes(x = scenario, y = avoided_n, fill = scenario)) +
        
        geom_col(position=position_dodge2(preserve = "single")) +
        facet_wrap(~name, scales = "free_y") +
        geom_text(inherit.aes = T, size = 6/.pt,
                  aes(label = formatC(avoided_n, format = "e"), # round(alive, 2) 
                      y= avoided_n - .1 * after_scale(max(avoided_n)))) +
        labs(
          title = paste("Avoided cases for", input$avoided_disease_select, "by", input$avoided_facet),
          y = "Avoided Deaths",
          x = "Scenario"
        ) +
        theme_minimal()
    })
    
    output$avoidedDeathsTable <- renderDataTable({
      df <- filtered_avoided_data()
      df <- df |> select(scenario, avoided_n, type, name)
      datatable(df, options = list(pageLength = 10))
    })
    
    # === Delay in Events ===
    filtered_delay_data <- reactive({
      delay_events |>
        filter(disease == input$delay_disease_select)
    })
    
    output$delayEventsTable <- renderDataTable({
      df <- filtered_delay_data()
      df <- df |>
        mutate(delay_days = round(delay_days, 0)) |>
        select(scenario, delay_days, type, name)
      
      datatable(df, options = list(pageLength = 10))
    })
  }
)
