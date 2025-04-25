# Read libraries
library(shiny)
library(bslib)
library(shinyWidgets)
library(here)
library(arrow)
library(plotly)
library(tidyverse)


# Boolean variable for dir/file paths
FILE_PATH_BELEN <- FALSE

n.i <- 282727#28269#2824#282727
n.c <- 30

get_summary <- function(SCEN_NAME, n.i, n.c){
    
    # SCEN_NAME <- 'base'
    
    if (!FILE_PATH_BELEN){
        m <- arrow::open_dataset(here(paste0("data/", SCEN_NAME, "_dis_inter_state_trans-n.c-", n.c, "-n.i-", n.i, "-n.d-19.parquet")))
    }else{
        m <- arrow::open_dataset(here(paste0("manchester/health/processed/", SCEN_NAME, "_dis_inter_state_trans-n.c-10-n.i-28269-n.d-19.parquet")))
    }
    
    
    ## Synthetic population file with exposures and physical activity
    if (!FILE_PATH_BELEN){
        synth_pop <- read_csv(here(paste0("jibe health/", SCEN_NAME, "_pp_exposure_RR_2021.csv")))
    }else{
        synth_pop <- read_csv(here(paste0("manchester/health/processed/", SCEN_NAME, "_pp_exposure_RR_2021.csv")))
    }
    
    # Introduce agegroup
    synth_pop <- synth_pop |> 
        mutate(agegroup = cut(age, c(0, 25, 45, 65, 85, Inf),
                              right=FALSE, include.lowest = TRUE))
    
    m <- m |> collect() |> mutate(id = as.numeric(id)) 
    
    #m$id <- as.numeric(m$id)
    
    m <- m |> left_join(synth_pop |> select(id, age, agegroup, gender, ladcd, lsoa21cd))
    
    return(m |> pivot_longer(cols = starts_with("c")) |> 
               mutate(unpacked = str_split(value, " ")) |> 
               unnest() |> 
               mutate(value = str_trim(unpacked)) |> 
               dplyr::select(-unpacked) |> 
               mutate(value = str_replace_all(value, fixed("parkinson’s_disease"), "parkinson")) |> 
               group_by(ladcd, name, agegroup, gender, value) |> 
               summarise(count = dplyr::n()) |> 
               mutate(freq = round(count / sum(count) * 100, 1)) |> 
               arrange(parse_number(name)) |> 
               mutate(name = as.factor(name))
    )
    
    
    
}

dc_base <- get_summary("base", n.i, n.c) |> mutate(scen = "reference")
dc_green <- get_summary("green", n.i, n.c) |> mutate(scen = "green")
dc_safestreet <- get_summary("safestreet", n.i, n.c) |> mutate(scen = "safestreet")
dc_both <- get_summary("both", n.i, n.c) |> mutate(scen = "both")

dc <- plyr::rbind.fill(dc_base, dc_green, dc_safestreet, dc_both)

# Create unique causes
ud <- unique(dc$value)

zones <- read_csv(here("jibe health/zoneSystem.csv"))

zones <- zones |> distinct(ladcd, ladnm)


# Convert the data frame into a named vector for pickerInput
zones_in <- setNames(zones$ladcd, zones$ladnm)

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
                    choices = zones_in,
                    selected = zones_in,
                    options = list(`actions-box` = TRUE), 
                    multiple = TRUE),
        
        pickerInput(inputId = "in_causes", 
                    label = "Diseases/causes",
                    choices = ud,
                    selected = "healthy",
                    options = list(`actions-box` = TRUE), 
                    multiple = TRUE),
        
        
        radioButtons(inputId = "in_strata", 
                     label = "Stratification",
                     choices = c("None", "Sex", "Age Group"),
                     inline = TRUE,
                     select = "None"),
        checkboxInput(inputId = "in_cumulative",
                      label = "Cumulative")
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
        causes <- input$in_causes
        cumulative <- input$in_cumulative
        
        # reference_df <- dc |> 
        #     filter(value != "dead", scen == "reference") |> 
        #     group_by(cycle, ladcd, scen) |> summarise(count = sum(count)) # futher grouping here as there are people in diff health states
        # # Join the reference data with the original data to calculate the change in count
        # df_change <- dc |>
        #     filter(value != "dead") |>
        #     group_by(cycle, ladcd, scen) |> summarise(count = sum(count)) |>
        #     ungroup() |>
        #     left_join(reference_df, by = c("ladcd", "cycle"), suffix = c("", "_ref")) |>
        #     mutate(count_change = count - count_ref) |>
        #     dplyr::select(ladcd, cycle, scen, count_change) |>
        #     left_join(zones)
    
        if (cumulative){
            
            reference_df <- dc |> filter(value != "dead", scen == "reference") %>%
                {
                    if (strata == "None"){
                        group_by(., ladcd, scen)
                    }
                    else if (strata == "Sex") {
                        group_by(., gender, ladcd, scen)
                    }else {
                        group_by(., agegroup, ladcd, scen)
                    }
                } %>%
                summarise(count = sum(count))
            
            
            return(dc |> 
                filter(value != "dead" & ladcd %in% filtered_lads) %>%
                {
                    if (strata == "None") {
                        group_by(., ladcd, scen) |> 
                            summarise(count = sum(count)) |> 
                            left_join(., reference_df, by = c("ladcd"), suffix = c("", "_ref")) 
                    } else if (strata == "Sex") {
                        group_by(., gender, ladcd, scen) |> 
                            summarise(count = sum(count)) |> 
                            left_join(., reference_df, by = c("ladcd", "gender"), suffix = c("", "_ref")) 
                    } else {
                        group_by(., agegroup, ladcd, scen) |>
                            summarise(count = sum(count)) |> 
                            left_join(., reference_df, by = c("ladcd", "agegroup"), suffix = c("", "_ref")) 
                    }
                } %>% 
                mutate(count_change = count - count_ref) %>% {
                    if (strata == "None") {
                        group_by(., ladcd, name, scen) 
                    } else if (strata == "Sex") {
                        group_by(., ladcd, name, gender, scen)
                    } else {
                        group_by(., ladcd, name, agegroup, scen)
                    }
                } %>%
                summarise(count_change = sum(count_change), .groups = "keep") |> 
                left_join(zones)
            )
            
            # browser()
            
        }else{
            
            # Filter the data for the "healthy" value and reference scenario
            reference_df <- dc |> filter(value %in% causes, scen == "reference")
            
            
            return(
                # Join the reference data with the original data to calculate the change in count
                dc |> 
                    filter(value %in% causes & ladcd %in% filtered_lads) %>%
                    {
                        if (strata == "None") {
                            left_join(., reference_df, by = c("ladcd", "name"), suffix = c("", "_ref")) 
                        } else if (strata == "Sex") {
                            left_join(., reference_df, by = c("ladcd", "name", "gender"), suffix = c("", "_ref")) 
                        } else {
                            left_join(., reference_df, by = c("ladcd", "name", "agegroup"), suffix = c("", "_ref")) 
                        }
                    } %>%
                    mutate(count_change = count - count_ref) %>% {
                        if (strata == "None") {
                            group_by(., ladcd, name, scen) 
                        } else if (strata == "Sex") {
                            group_by(., ladcd, name, gender, scen)
                        } else {
                            group_by(., ladcd, name, agegroup, scen)
                        }
                    } %>%
                    summarise(count_change = sum(count_change), .groups = "keep") |> 
                    left_join(zones)
            )
            
            
        }

        # c("None", "Sex", "Age Group"
        
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
            gg <- ggplot(local_df, aes(x = factor(name, levels = paste0("c", 0:n.c)), y = count_change, color = scen, group = scen)) +
                geom_line() +
                geom_point() +
                {
                    if (strata == "None") {
                        facet_wrap(~ ladnm)
                    } else if (strata == "Sex") {
                        facet_wrap(~ladnm + gender)
                    } else {
                        facet_wrap(~ladnm + agegroup)
                    }
                } +
                labs(title = "Change in Count for Healthy Value Compared to Reference",
                     x = "Year",
                     y = "Change in Count") +
                theme_minimal()
            
                
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
