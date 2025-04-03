# Read libraries
library(shiny)
library(bslib)
library(shinyWidgets)
library(here)
library(arrow)
library(plotly)
library(tidyverse)



# m <- arrow::open_dataset(sources = here("data/exp_dis_inter_trans-n.c-10-n.i-2827285-n.d-19.parquet"))
# 
# m <- m |> to_duckdb() |> as.data.frame() |> tibble::rowid_to_column("rid")
# 
# # m <- m |> as.data.frame() |> tibble::rowid_to_column("rid")
# 
# m <- m |> cbind(synth_pop |> select(id, age, gender, ladcd, lsoa21cd))

m <- arrow::open_dataset(sources = here("data/temp"))

zones <- read_csv(here("jibe health/zoneSystem.csv"))
    
lad <- zones |> distinct(ladcd) |> dplyr::select(ladcd) |> pull()

# m |> filter(ladcd %in% filtered_lads) |> 
#     group_by(ladcd, name, value) |> 
#     summarise(count = dplyr::n(), .groups = "drop") |> 
#     mutate(freq = round(count / sum(count) * 100, 1)) |> 
#     dplyr::filter(value != "healthy") |> 
#     collect() |> 
#     arrange(parse_number(name)) |> 
#     mutate(name = as.factor(name))

# mutate(AgeGroup = factor(AgeGroup, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
#                                               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
#                                               "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", 
#                                               "90-94", "95+"), ordered = TRUE),
# 
# zones_var <- c("Sex" = "imd10",
#            "Age " = "CAR_SC",
#            "Bus" = "BUS_SC",
#            "Motorcycle" = "MOT_SC")

ui <- page_sidebar(
    theme = bs_theme(bootswatch = "yeti"),
    title = paste0("Health outcomes"),
    sidebar = sidebar(
        pickerInput(inputId = "in_lads", 
                    label = "Local Authority",
                    choices = lad,
                    selected = lad,
                    options = list(`actions-box` = TRUE), 
                    multiple = TRUE),
        
        radioButtons(inputId = "in_strata", 
                     label = "Stratification",
                     choices = c("None", "Sex", "Age Group"),
                     inline = TRUE,
                     select = "None")
    ),
    navset_card_underline(
        id = "main_tab",
        full_screen = TRUE,
        nav_panel("Health Outcomes", 
                  plotlyOutput("out_health_plot"))
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_health_data <- reactive({
        
        filtered_lads <- input$in_lads
        strata <- input$in_strata
        # c("None", "Sex", "Age Group"
        return(
        m |> 
            filter(ladcd %in% filtered_lads) %>%
            { 
                if (strata == "None") {
                    group_by(., ladcd, name, value)
                } else if (strata == "Sex") {
                    group_by(., ladcd, name, gender, value)
                } else {
                    group_by(., ladcd, name, age_group, value)
                }
            } %>% 
            summarise(count = dplyr::n(), .groups = "drop") |> 
            mutate(freq = round(count / sum(count) * 100, 1)) |> 
            dplyr::filter(value != "healthy") |> 
            collect() |> 
            arrange(parse_number(name)) |> 
            mutate(name = as.factor(name))
        )
        
        
        # return(m |> filter(ladcd %in% filtered_lads) |> {
        #     if (strata == "None") {
        #         group_by(., ladcd, name, value)
        #     } else if (strata == "Sex"){
        #         group_by(., ladcd, name, gender, value) 
        #     } else{
        #         group_by(., ladcd, name, age_group, value) 
        #         
        #     } 
        # }
        # |> 
        #     summarise(count = dplyr::n(), .groups = "drop") |> 
        #     mutate(freq = round(count / sum(count) * 100, 1)) |> 
        #     dplyr::filter(value != "healthy") |> 
        #     collect() |> 
        #     arrange(parse_number(name)) |> 
        #     mutate(name = as.factor(name)))
        
    })
    
    
    output$out_health_plot <- renderPlotly({
        
        req(input$in_lads)
        req(input$in_strata)
        
        filtered_lads <- input$in_lads
        strata <- input$in_strata
        
        local_df <- get_health_data()
        
        # write_csv(local_df, "local_df.csv")
        
        text_colour <- "black"
        
        # fname <- do.call(paste, c(as.list(filtered_scens),
        #                           as.list(filtered_modes),
        #                           as.list(input$in_risk_type),
        #                           sep = "-"))
        
        if(nrow(local_df) < 1)
            plotly::ggplotly(ggplot(data.frame()))
        else{
            
            gg <- ggplot(local_df) +
                aes(x = freq, y = fct_inorder(name), fill = value, group = value, colour = value) +
                geom_point() + 
                geom_line() +
                scale_fill_hue(direction = 1) +
                coord_flip() +
                theme_minimal() +
                facet_wrap(vars(ladcd))
                #bslib::card(full_screen = TRUE)
            # browser()
            # if (SAVE_FIGURES)
            #     ggsave(paste0("figures/", fname, ifelse(SVG, ".svg", ".png")), plot = gg, width=10, height=8)
            
            
            plotly::ggplotly(gg)
            
            
            # |>       plotly::config(
            #     toImageButtonOptions = list(
            #         format = "svg",
            #         filename = "abc",
            #         width = NULL,
            #         height = NULL
            #     )) |> layout(
            #         margin = list(b = 50, l = 50) # to fully display the x and y axis labels
            #     )
            
        }
        
        # browser()
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
