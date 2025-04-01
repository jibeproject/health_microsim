# Read libraries
library(shiny)
library(bslib)
library(shinyWidgets)
library(here)
library(arrow)


# Load datasets
synth_pop <- arrow::open_dataset(sources = here("data/base_pp_exposure_RR_2021.parquet")) |> collect() |> 
    mutate(age_group = case_when(
        age >= 0 & age <= 4 ~ "0-4 years",
        age >= 5 & age <= 9 ~ "5-9 years",
        age >= 10 & age <= 14 ~ "10-14 years",
        age >= 15 & age <= 19 ~ "15-19 years",
        age >= 20 & age <= 24 ~ "20-24 years",
        age >= 25 & age <= 29 ~ "25-29 years",
        age >= 30 & age <= 34 ~ "30-34 years",
        age >= 35 & age <= 39 ~ "35-39 years",
        age >= 40 & age <= 44 ~ "40-44 years",
        age >= 45 & age <= 49 ~ "45-49 years",
        age >= 50 & age <= 54 ~ "50-54 years",
        age >= 55 & age <= 59 ~ "55-59 years",
        age >= 60 & age <= 64 ~ "60-64 years",
        age >= 65 & age <= 69 ~ "65-69 years",
        age >= 70 & age <= 74 ~ "70-74 years",
        age >= 75 & age <= 79 ~ "75-79 years",
        age >= 80 & age <= 84 ~ "80-84 years",
        age >= 85 & age <= 89 ~ "85-89 years",
        age >= 90 ~ "90plus years",
        TRUE ~ "Unknown"))

zones <- read_csv(here("jibe health/zoneSystem.csv"))
m <- arrow::open_dataset(sources = here("data/exp_dis_inter_trans-n.c-10-n.i-2827285-n.d-19.parquet"))
m <- m |> collect() |> as.data.frame() |> tibble::rowid_to_column("rid")
m <- m |> cbind(synth_pop |> select(id, age, gender, ladcd, lsoa21cd))
lad <- zones |> distinct(ladnm) |> select(ladnm) |> pull()

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
                  plotOutput("disPlot"))
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    get_health_data <- reactive({
        
        filtered_scens <- input$in_scens
        filtered_cities <- cities |> filter(city %in% input$in_cities) |> dplyr::select(city) |> pull()
    })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    
    
    
    # m <- m |> as.data.frame() |> tibble::rowid_to_column("rid")
    
    dc <- m |> pivot_longer(cols = starts_with("c")) |> 
        mutate(unpacked = str_split(value, " ")) |> 
        unnest() |> 
        mutate(value = str_trim(unpacked)) |> 
        dplyr::select(-unpacked) |> 
        # |> 
        group_by(ladcd, name, value) |> 
        summarise(count = dplyr::n()) |> 
        mutate(freq = round(count / sum(count) * 100, 1)) |> 
        filter(value != "healthy") |> 
        left_join(zones)
    
    dc <- dc |> arrange(parse_number(name)) |> 
        mutate(name = as.factor(name))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
