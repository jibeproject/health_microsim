library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(qs)
library(bslib)
library(plotly)
library(here)

print("printing wd")
print(getwd())
t <- qs::qread(here("bapp/data/221025_trips.qs"))
list2env(t, envir = environment())

all_scenarios <- c("Reference" = "reference",
             "Greening" = "green",
             "Safer Streets" = "safeStreet",
             "Safer Streets & Greening" = "both",
             "Go Dutch" = "goDutch")

all_locations <- unique(t$avg_time_combined$LAD_origin)


ui <- fluidPage(
  titlePanel("Manchester Transport and Health model results"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxInput("use_plotly", "Interactive (Plotly)", value = TRUE),
      selectInput("scen_sel", "Scenarios:", choices = all_scenarios,
                  selected = all_scenarios, multiple = TRUE),
      selectInput("view_level", "View by:", choices = c("Overall","LAD"),
                  selected = "Overall"),
      conditionalPanel(
        "input.view_level == 'LAD'",
        selectizeInput("lad_sel", "LAD(s):",
                       choices = all_locations, multiple = TRUE,
                       options = list(placeholder = "Pick LADs (optional)"))
      ),
      tags$hr(),
      
      ## --- Exact-tab selector (dropdown mirrors tabs) ---
      radioButtons(
        "metrics_picker", "Metrics:",
        choices = c(
          "Trip Mode Share (%)",
          "Combined Trip Distance by Modes",
          "Trip Duration by Mode",
          "Zero Mode"
        ),
        selected = "Trip Mode Share (%)"
      ),
      
     ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        # ---- Differences (4 metric tabs) ----
        tabPanel(
          title = "test",
          id= "test",
          conditionalPanel(
            condition = "input.metrics_picker == 'Zero Mode'",
            plotlyOutput("out_zm")
          ),
          conditionalPanel("input.metrics_picker == 'Trip Mode Share (%)'", 
                           plotlyOutput("out_mshare")),
          conditionalPanel("input.metrics_picker == 'Combined Trip Distance by Modes'", 
                           plotlyOutput("out_cdist")),
          conditionalPanel("input.metrics_picker == 'Trip Duration by Mode'", 
                           plotlyOutput("out_dur"))
          
        )
        
        
      )
    )
  )
)

server <- function(input, output) {
  
  output$out_zm <- renderPlotly({
    
    # req(input$in_scens)
    # req(input$in_cities)
    # req(input$in_level)
    # req(input$in_measure)
    # # req(input$in_CIs)
    # req(input$in_pathways)
    # req(!is.null(input$in_strata))
    
    t$zero_mode <- t$zero_mode |> mutate(scen = case_when(scen == "both" ~ "Greening + Safe Streets",
                            scen == "safeStreet" ~ "Safer Streets",
                            scen == "reference" ~ "Reference",
                            scen == "green" ~ "Greening",
                            .default = scen))
    
    
    plotly::ggplotly(ggplot(t$zero_mode, aes(x = mode, y = zero_percent, fill = scen)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(
        aes(label = paste0(zero_percent, "%")),
        position = position_dodge(width = 0.9), 
        vjust = -0.25,                       
        size = 3) +
        # scale_fill_manual(values = c("both", "green", "safeStreet", "reference"), 
        #                   labels = c("Greening + Safe Streets", "Greening",
        #                              "Safer Streets", "Reference")) +
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
    )
    
  })
  
  output$out_mshare <- renderPlotly({
    
    t$distance <- t$distance |> mutate(scen = case_when(scen == "both" ~ "Greening + Safe Streets",
                                                          scen == "safeStreet" ~ "Safer Streets",
                                                          scen == "reference" ~ "Reference",
                                                          scen == "green" ~ "Greening",
                                                          .default = scen))
    
    ggplot(t$distance, aes(x = distance_bracket, y = percent, fill = mode)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(
        aes(label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")), # Show label only if >= 1%
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
      facet_wrap(vars(scen), scales = "free_x")
    
  })
  
  output$out_cdist <- renderPlotly({
    
    t$combined_distance <- t$combined_distance |> mutate(scen = case_when(scen == "both" ~ "Greening + Safe Streets",
                                                        scen == "safeStreet" ~ "Safer Streets",
                                                        scen == "reference" ~ "Reference",
                                                        scen == "green" ~ "Greening",
                                                        .default = scen))
    
    ggplotly(
      ggplot(t$combined_distance) +
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
  })
  
  output$out_dur <- renderPlotly({
    
    t$avg_time_combined <- t$avg_time_combined |> mutate(scen = case_when(scen == "both" ~ "Greening + Safe Streets",
                                                                          scen == "safeStreet" ~ "Safer Streets",
                                                                          scen == "reference" ~ "Reference",
                                                                          scen == "green" ~ "Greening",
                                                                          .default = scen))
    
    ggplotly(ggplot(t$avg_time_combined) +
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
    
  })
}

shinyApp(ui = ui, server = server)


# asr_overall_all |> 
#   group_by(cause, scen) |> 
#   reframe(age_std_rate = mean(age_std_rate)) |> 
#   group_by(cause) |> 
#   mutate(diff_ref  = age_std_rate[scen == "reference"] - age_std_rate) |> 
#   filter(grepl("depression|dementia|heart|stroke|breast|lung|diabetes|copd", cause)) |> 
#   mutate(
#     value_label = ifelse(
#       scen == "reference", 
#       as.character(round(age_std_rate, 1)), 
#       str_glue("{round(age_std_rate, 1)} ({round(diff_ref, 2)})")
#     )
#   ) |> 
#   select(cause, scen, value_label) |> 
#   pivot_wider(names_from = scen, values_from = value_label) |> 
#   dplyr::select(cause, reference, safeStreet, green, both, goDutch)