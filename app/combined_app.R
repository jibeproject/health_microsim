suppressPackageStartupMessages({
  library(shiny)
  library(dplyr) 
  library(tidyr) 
  library(arrow) 
  library(readr)
  library(ggplot2)
  library(plotly) 
  library(scales) 
  library(here)
  library(qs)
  library(DT)
  library(gt)
  library(gtExtras)
  library(bslib)
})

# pc <- qs::qread(here("temp/precomputed_mcr_wogd_100%V3.qs"))
pc <- qs::qread(here("temp/061225/precomputed_mcr_wgd_100%V4_imd.qs"))

#pc <- qs::qread(here("temp/processed_data/shiny_app_data/precomputed_mcr_wogd_100%V3.qs"))
list2env(pc, envir = environment())
SCALING <- 1L


t <- qs::qread(here("temp/061125_trips.qs"))
exp <- qs::qread(here("temp/091125/exp.qs"))

#t <- qs::qread(here("temp/processed_data/shiny_app_data/061125_trips.qs"))
#exp <- qs::qread(here("temp/processed_data/shiny_app_data/exp.qs"))  


MIN_CYCLE <- 1
MAX_CYCLE <- 30

col_fun <- col_numeric(palette = c("lightpink", "lightgreen"), domain = c(0, 1))


# ------------------- Helpers -------------------------------------------
add_zero_line <- function() geom_hline(yintercept = 0, linewidth = 0.3)

theme_clean <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          strip.text = element_text(lineheight = 0.9))
}
add_zero_line <- function() geom_hline(yintercept = 0, linewidth = 0.3)

pop_share <- function(df, group_vars = c("cycle","scen")) {
  df |>
    group_by(across(all_of(c(group_vars, "agegroup_cycle")))) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop_last") |>
    mutate(share = pop / sum(pop, na.rm = TRUE)) |>
    ungroup()
}

diff_vs_reference <- function(df, by = character(0), value_col = "value") {
  ref <- df |>
    filter(scen == "reference") |>
    rename(value_ref = !!rlang::sym(value_col)) |>
    select(cycle, all_of(by), value_ref)
  df |>
    filter(scen != "reference") |>
    left_join(ref, by = c("cycle", by)) |>
    mutate(diff = .data[[value_col]] - value_ref) |>
    select(scen, cycle, all_of(by), diff)
}

get_age_levels <- function(x) if (is.factor(x)) levels(x) else sort(unique(x))
align_age_levels <- function(w, people_age) {
  lv <- get_age_levels(people_age)
  w |> mutate(agegroup_cycle = factor(as.character(agegroup_cycle), levels = lv))
}

# ------------------- Precompute (with cache) ---------------------------
death_values <- c("dead","dead_car","dead_bike","dead_walk")

# ------------------- UI -------------------------------------------------
all_scenarios <- sort(unique(people_overall$scen))
pop_cycles    <- sort(unique(people_overall$cycle))
trend_cycles  <- sort(unique(asr_overall_all$cycle))
all_lads_nm   <- sort(unique(people_lad$ladnm))
all_genders   <- sort(unique(people_gender$gender))
all_causes_asr <- asr_overall_all |> distinct(cause) |> pull() #filter(!grepl("sev", cause)) |> 
selected_views <- c("Overall","Gender","LAD")
additional_selected_views <- c("IMD")

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "yeti"),
  title = paste0("Travel and Health Explorer"),
  sidebar = sidebar(
      selectInput("scen_sel", "Scenarios:", choices = all_scenarios,
                  selected = all_scenarios, multiple = TRUE),
      selectInput("view_level", "View by:", choices = selected_views,
                  selected = "Overall"),
      conditionalPanel(
        "input.view_level == 'LAD'",
        selectizeInput("lad_sel", "LAD(s):",
                       choices = all_lads_nm, multiple = TRUE,
                       options = list(placeholder = "Pick LADs (optional)"))
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'Travel Behaviour'",
        radioButtons(
          "metrics_picker", "Metrics:",
          choices = c(
            "Trip Mode Share (%)",
            "Trip Mode Share by Distance (%)",
            "Combined Trip Distance by Modes",
            "Trip Duration by Mode"
          ),
          selected = "Trip Mode Share (%)"
        )
      ),
      
      
      conditionalPanel(
        condition = "input.main_tabs == 'Differences vs reference'",
        checkboxInput("diff_table", "Table", FALSE)
      ),
      
      
      tags$hr(),
      tabsetPanel(
        id = "control_tabs", type = "pills",
        conditionalPanel(
          condition = "input.main_tabs == 'Population'",
          h2("Population"),
          selectizeInput("pop_cycles", "Cycles to show (bars):",
                         choices = pop_cycles, selected = c(2,10,MAX_CYCLE), multiple = TRUE),
          radioButtons("pop_style", "Bar style:", c("Stacked"="stack","Side-by-side"="dodge"),
                       inline = TRUE),
          checkboxInput("pop_share", "Show shares (else counts)", value = TRUE)
        ),
        
        conditionalPanel(
          condition = "input.main_tabs == 'Travel Behaviour'",
          
        ),
        
        conditionalPanel(
          condition = "input.main_tabs == 'Differences vs reference'",
          h2("Differences"),
          radioButtons("metric_kind", "Metric:", 
                       choices = c(
            "Diseases postponed (Δ diseases)" = "diseases",
            "Δ Impact factor"                 = "imp_fac",
            "Deaths postponed (Δ deaths)"     = "deaths",
            "Δ Healthy years"                 = "healthy",
            "Δ Life years"                    = "life"
            
          )),
          sliderInput("diff_min_cycle", "Start cycle:",
                      min = min(trend_cycles), max = max(trend_cycles),
                      value = MIN_CYCLE, step = 1),
          checkboxInput("diff_cumulative", "Cumulative over cycles", value = TRUE)
        ),
        conditionalPanel(
          condition = "input.main_tabs == 'Average onset ages'",
          h2("Average onset ages"),
          selectInput("avg_kind", "Average age of:", choices = c("Death"="death","Disease onset"="onset")),
          uiOutput("avg_cause_ui")
        ),
        conditionalPanel(
          condition = "input.main_tabs == 'ASR'",
          h2("ASR"),
          selectInput("asr_mode", "ASR view:",
                      choices = c("Average 1-30 (bars)"="avg","Over time (smoothed)"="trend"))
        ),
        conditionalPanel(
          condition = "input.main_tabs == 'ASR' || (input.main_tabs == 'Differences vs reference' && 
          input.metric_kind == 'diseases')",
          selectizeInput("asr_causes", "Causes:", choices = all_causes_asr,
                         selected = c("coronary_heart_disease","stroke"),
                         multiple = TRUE)#,
        )
        
      ),
      tags$hr(),
      downloadButton("download_csv", "Download current table (CSV)")
    ),
  navset_card_underline(
    id = "main_tabs",
    full_screen = TRUE,
    nav_panel("Travel Behaviour",
          plotlyOutput("out_mshare", height = "85vh")
        ),
    nav_panel("Exposures",
          gt_output("plot_exp")
        ),
    nav_panel("Differences vs reference",
          plotlyOutput("plot_diffly", height = "35vh"),
          tags$hr(),
          h5("Summary (cumulative at latest cycle, or sum if non-cumulative)"),
          uiOutput("table_diff_summary", height = "35vh")
        ),
    nav_panel("Average onset ages", 
                 gt_output("table_avg")),
    nav_panel("ASR",
          uiOutput("plot_asrly", height = "85vh")
        ),
    nav_panel("Population",
          plotlyOutput("plot_poply")#, height = "85vh")
        )
        
      )
  )


# ------------------- SERVER --------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$main_tabs, {
    
    # current selection (if any)
    current <- isolate(input$view_level)
    
    if (input$main_tabs == "Differences vs reference") {
      
      # selected_views <- c("Overall","Gender","LAD")
      # additional_selected_views <- c("IMD")
      
      new_choices <- c(selected_views, additional_selected_views)
    } else {
      new_choices <- selected_views
      # if current selection not in base choices, drop it
      if (!is.null(current) && !current %in% new_choices) {
        current <- NULL
      }
    }
    
    updateSelectInput(
      session,
      "view_level",
      choices  = new_choices,
      selected = current
    )
    
  })
  
  # ---------- Avg ages cause picker ----------
  output$avg_cause_ui <- renderUI({
    if (input$avg_kind == "onset") {
      selectInput("avg_cause", "Disease (onset):",
                  choices = sort(unique(incidence_src$value)),
                  selected = "coronary_heart_disease")
    } else {
      selectizeInput("avg_death_causes", "Death cause(s):",
                     choices = c("dead","dead_car","dead_bike","dead_walk"),
                     selected = c("dead","dead_car","dead_bike","dead_walk"),
                     multiple = TRUE)
    }
  })
  
  
  set_ag <- function(df){
    
    # Define the correct order of age groups
    age_levels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                    "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", 
                    "95-99", "100+")
    
    # Convert to ordered factor
    df$agegroup_cycle <- factor(df$agegroup_cycle, levels = age_levels, ordered = TRUE)
    
    return(df)
  }
  # ---------- Population ----------
  pop_data <- reactive({
    req(input$pop_cycles, input$scen_sel)
    view <- input$view_level
    
    
    
    if (view == "Overall") {
      dat <- set_ag(people_overall) |> filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen")), y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, y = "pop", y_lab = "Population count")
      }
    } else if (view == "Gender") {
      dat <- set_ag(people_gender) |> 
        filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles) |> 
        mutate(gender = case_when(gender == 1 ~ "Male",
                                  gender == 2 ~ "Female"))
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen","gender")), 
             facet = "gender", y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, facet = "gender", y = "pop", y_lab = "Population count")
      }
    } else {
      dat <- set_ag(people_lad) |> filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (length(input$lad_sel)) dat <- dat |> filter(ladnm %in% input$lad_sel)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen","ladnm")), facet = "ladnm", y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, facet = "ladnm", y = "pop", y_lab = "Population count")
      }
    }
  })
  
  build_pop_plot <- reactive({
    pd <- pop_data(); d <- pd$data; req(nrow(d) > 0)
    pos <- if (input$pop_style == "dodge") position_dodge(width = 0.8) else "stack"
    base <- ggplot(d, aes(x = agegroup_cycle, y = .data[[pd$y]], fill = scen)) +
      geom_col(position = pos) +
      scale_y_continuous(labels = if (pd$y == "share") percent else label_comma()) +
      labs(title = "Population by age group", x = "Age group", y = pd$y_lab, fill = "Scenario") +
      theme_clean() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    if (!is.null(pd$facet)) {
      base + facet_grid(as.formula(paste(pd$facet, "~ cycle")), scales = "free_x")
    } else {
      base + facet_wrap(~ cycle, nrow = 1)
    }
  })
  output$plot_poply <- renderPlotly({ ggplotly(build_pop_plot(), tooltip = c("x","y","fill")) })
  
  # ---------- Differences vs reference ----------
  diff_long <- reactive({
    req(input$metric_kind, input$view_level, input$diff_min_cycle, input$asr_causes)
    scen_keep <- setdiff(input$scen_sel, "reference")
    validate(need(length(scen_keep) > 0, "Select at least one non-reference scenario."))
    minc <- input$diff_min_cycle; view <- input$view_level; cumu <- isTRUE(input$diff_cumulative)
    
    dl <- deaths_lad
    dil <- diseases_lad
    hl <- healthy_lad
    ll <- lifey_lad
    if (length(input$lad_sel)){
      dl <- dl |> filter(ladnm %in% input$lad_sel)
      dil <- dil |> filter(ladnm %in% input$lad_sel)
      hl <- hl |> filter(ladnm %in% input$lad_sel)
      ll <- ll |> filter(ladnm %in% input$lad_sel)

    }
    
    do <- diseases_overall
    dg <- diseases_gender
    dimd <- diseases_imd
    
    if (length(input$asr_causes)){
      do <- do |> filter(cause %in% input$asr_causes)
      dg <- dg |> filter(cause %in% input$asr_causes)
      dimd <- dimd |> filter(cause %in% input$asr_causes)
      dil <- dil |> filter(cause %in% input$asr_causes)
    }
    
    pick <- switch(input$metric_kind,
                   deaths   = list(Overall=deaths_overall, Gender=deaths_gender,  LAD=dl, IMD = deaths_imd, label="Δ Deaths"),
                   diseases = list(Overall=do, Gender=dg, LAD=dil,IMD = dimd, label="Δ Diseases"),
                   healthy  = list(Overall=healthy_overall, Gender=healthy_gender, LAD=hl,IMD = healthy_imd, label="Δ Healthy years"),
                   life     = list(Overall=lifey_overall, Gender=lifey_gender, LAD=ll,  IMD = lifey_imd, label="Δ Life years"),
                   imp_fac  = list(Overall = plyr::rbind.fill(lifey_overall |> mutate(factor = "Δ Life years"),
                                                              healthy_overall |> mutate(factor = "Δ Healthy years"),
                                                              deaths_overall |> mutate(factor = "Δ Deaths")), 
                                   Gender=plyr::rbind.fill(lifey_gender  |> mutate(factor = "Δ Life years"),
                                                           healthy_gender   |> mutate(factor = "Δ Healthy years"),
                                                           deaths_gender |> mutate(factor = "Δ Deaths")), 
                                   LAD=plyr::rbind.fill(ll |> mutate(factor = "Δ Life years"),
                                                        hl |> mutate(factor = "Δ Healthy years"),
                                                        dl |> mutate(factor = "Δ Deaths")),  
                                   IMD=plyr::rbind.fill(lifey_imd  |> mutate(factor = "Δ Life years"),
                                                           healthy_imd |> mutate(factor = "Δ Healthy years"),
                                                           deaths_imd |> mutate(factor = "Δ Deaths")),
                                   label="Δ Impact factor"))
    base <- pick[[view]]
    if (input$metric_kind == "diseases"){
      by <- switch(view, Overall="cause", Gender=c("cause", "gender"), LAD=c("cause", "ladnm"), IMD = c("cause", "imd10"))
    }else if (input$metric_kind == "imp_fac"){
      by <- switch(view, Overall="factor", Gender=c("factor", "gender"), LAD=c("factor", "ladnm"), IMD = c("factor", "imd10"))
    }else{
      by <- switch(view, Overall=character(0), Gender="gender", LAD="ladnm", IMD = "imd10")
    }
    
    df <- base |> filter(cycle >= minc, scen %in% scen_keep); grp <- c("scen", by)
    
    if ("gender" %in% names(df)) {
      df <- df |> 
        mutate(gender = case_when(gender == 1 ~ "Male",
                                  gender == 2 ~ "Female")) 
    }
    
    df |> group_by(across(all_of(c(grp, "cycle")))) |>
      summarise(diff = sum(diff, na.rm = TRUE), .groups = "drop") |>
      group_by(across(all_of(grp))) |>
      mutate(y = if (cumu) cumsum(diff) else diff) |>
      ungroup() |>
      mutate(metric = pick$label)
    
  })
  
  build_diff_plot <- reactive({
    d <- diff_long(); req(nrow(d) > 0)
    ylab <- if (isTRUE(input$diff_cumulative)) "Cumulative Δ vs reference" else "Δ vs reference"
    
    ttl  <- d$metric[1]
    if ("gender" %in% names(d)) {
      
      # browser()
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE, method = "loess") + add_zero_line() +
        facet_wrap(~ gender, nrow = 2, scales = "free_y") + 
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean()
    } else if ("ladnm" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE, method = "loess") + add_zero_line() +
        facet_wrap(~ ladnm, nrow = 2, scales = "free_y") +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean()
    } else {
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE, method = "loess") + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean()
    }
  })
  output$plot_diffly <- renderPlotly({ ggplotly(build_diff_plot())})#, tooltip = c("x","y","colour","linetype")) })
  
  # Define a function to process and return the processed data
  get_processed_data <- function() {
    d <- diff_long()
    
    req(nrow(d) > 0)
    
    by <- if ("gender" %in% names(d)) {
      if ("cause" %in% names(d)) {
        c("cause", "gender")
      } else if ("factor" %in% names(d)) {
        c("factor", "gender")
      }else{
        "gender"
      }
    } else if ("imd10" %in% names(d)) {
      if ("cause" %in% names(d)) {
        c("cause", "imd10")
      } else if ("factor" %in% names(d)) {
        c("factor", "imd10")
      }else{
        "imd10"
      }
    } else if ("ladnm" %in% names(d)) {
      if ("cause" %in% names(d)) {
        c("cause", "ladnm")
      } else if ("factor" %in% names(d)) {
        c("factor", "ladnm")
      } else {
        "ladnm"
      }
    } else {
      if ("cause" %in% names(d)) {
        "cause"
      } else if ("factor" %in% names(d)) {
        "factor"
      }else {
        character(0)
      }
    }
    
    metric_lab <- unique(d$metric)[1]
    
    if (isTRUE(input$diff_cumulative)) {
      
      d <- d |> 
        group_by(across(all_of(c("scen", by)))) |>
        slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
        ungroup() |>
        transmute(
          metric = metric_lab, scen,
          !!!(if (length(by)) rlang::syms(by) else NULL),
          final_cycle = cycle,
          cumulative_value = y,
          cumulative_value_scaled = y * SCALING
        ) |>
        arrange(scen, across(all_of(by)))
      
    } else {
      d <- d |> 
        group_by(across(all_of(c("scen", by)))) |>
        summarise(
          final_cycle = max(cycle, na.rm = TRUE),
          cumulative_value = sum(diff, na.rm = TRUE), 
          .groups = "drop"
        ) |>
        mutate(
          metric = metric_lab, 
          cumulative_value_scaled = cumulative_value * SCALING,
          .before = 1
        ) |>
        arrange(scen, across(all_of(by)))
        
    }
    
    list(raw = d, by = by, metric_lab = metric_lab)
  }
  
  # Use the function in renderUI
  output$table_diff_summary <- renderUI({
    data <- get_processed_data()
    
    if (isTRUE(input$diff_table)) {
      gt_output("diff_summary_gt")
    } else {
      plotlyOutput("diff_summary_plot")
    }
  })
  
  # Use the function in render_gt
  output$diff_summary_gt <- render_gt({
    data <- get_processed_data()
    cumdf <- data$raw
    by <- data$by
    
    get_normalized_table(
      cumdf |>
        dplyr::select(-any_of(c("cumulative_value_scaled", "final_cycle"))) |>
        tidyr::pivot_wider(names_from = scen, values_from = cumulative_value)
    ) |>
      dplyr::select(-matches("min|max|norm")) |>
      gt::gt() |>
      gt::tab_options(table.font.size = "small") |>
      opt_interactive(
        use_filters = TRUE,
        use_sorting = FALSE,
        use_compact_mode = TRUE
      )
  })
  
  # Use the function in renderPlot
  output$diff_summary_plot <- renderPlotly({
    data <- get_processed_data()
    cumdf <- data$raw
    by <- data$by
    
    # ggplot(td) +
    #   aes(x = value, y = imd10, colour = scen) +
    #   geom_smooth(se = FALSE) +
    #   scale_color_hue(direction = 1) +
    #   theme_minimal() +
    #   facet_wrap(vars(cause))
    # 
    
    if (grepl("Diseases", data$metric_lab)){
      
      if (!"imd10" %in% names(cumdf)){
        p <- ggplot(cumdf) +
          aes(x = cause, y = cumulative_value, fill = scen) +
          geom_bar(
            stat = "summary",
            fun = "mean",
            position = "dodge2"
          ) +
          scale_fill_hue(direction = 1) +
          labs(
            fill = "Scenario",
            y = "",
            x = ""
          ) +
          geom_text(
            aes(label = cumulative_value), 
            hjust = -2.5, 
            size = 4,
            position = position_dodge(width = 1),
            inherit.aes = TRUE
          ) + 
          coord_flip() +
          theme_minimal()
          
      }else{
        
        p <- ggplot(cumdf) +
          aes(x = imd10, y = cumulative_value, colour = scen) +
          geom_smooth(se = FALSE, method = "loess") +
          scale_color_hue(direction = 1) +
          theme_minimal() + 
          labs(
            x = "Index of Multiple Deprivation (IMD)",
            color = "Scenario",
            y = "Cumulative Δ"
          ) 
      }
    }
    else if (grepl("Impact", data$metric_lab)){
      
      if (!"imd10" %in% names(cumdf)){
      
      p <- ggplot(cumdf) +
        aes(x = scen, y = cumulative_value, fill = factor) +
        geom_bar(stat = "summary", fun = "sum", position = "dodge2") +
        scale_fill_hue(direction = 1) +
        geom_text(
          aes(label = cumulative_value), 
          hjust = -2.5, 
          size = ifelse("gender" %in% names(cumdf), 2, 3),
          position = position_dodge(width = 1),
          inherit.aes = TRUE
        ) + 
        coord_flip() +
        labs(x = "") +
        theme_minimal() 
      }else{
        
        p <- ggplot(cumdf) +
          aes(x = imd10, y = cumulative_value, colour = scen) +
          geom_smooth(se = FALSE, method = "loess") +
          scale_color_hue(direction = 1) +
          theme_minimal() + 
          labs(
            x = "Index of Multiple Deprivation (IMD)",
            color = "Scenario"
          )

      }
      
      
    }else{
      
      if (!"imd10" %in% names(cumdf)){
      
        p <- ggplot(cumdf, aes(x = scen, y = cumulative_value, fill = scen)) +
          geom_col(position = "dodge") +
          geom_text(
            aes(label = cumulative_value, y = cumulative_value / 2),
            size = ifelse("gender" %in% names(cumdf), 2, 3),
            position = position_dodge(width = 1),
            color = "white"
          ) +
          labs(
            title = paste("Cumulative", data$metric_lab, "by Scenario"),
            x = "Scenario", 
            y = "Cumulative Value",
            fill = "Scenario"
          ) +
          coord_flip() +
          theme_minimal()
      }else{
        p <- ggplot(cumdf) +
          aes(x = imd10, y = cumulative_value, colour = scen) +
          geom_smooth(se = FALSE, method = "loess") +
          scale_color_hue(direction = 1) +
          theme_minimal() + 
          labs(
            x = "Index of Multiple Deprivation (IMD)",
            color = "Scenario"
          )
      }
      
    }
    
    if ("gender" %in% names(cumdf))  {
      p <- p + facet_wrap(~gender)
    }else if ("imd10" %in% names(cumdf) && "cause" %in% names(cumdf))  {
      p <- p + facet_wrap(~cause)
    }else if ("imd10" %in% names(cumdf) && "factor" %in% names(cumdf))  {
      p <- p + facet_wrap(~factor, scales = "free_y")
    }else if("ladnm" %in% names(cumdf))  {
      p <- p + facet_wrap(~ladnm)
    }
    
    plotly::ggplotly(p)
    
  })
  
  # ---------- Average ages (death / onset) ----------
  output$avg_cause_ui <- renderUI({
    if (input$avg_kind == "onset") {
      selectInput("avg_cause", "Disease (onset):",
                  choices = sort(unique(incidence_src$value)),
                  selected = "coronary_heart_disease")
    } else {
      selectizeInput("avg_death_causes", "Death cause(s):",
                     choices = c("dead","dead_car","dead_bike","dead_walk"),
                     selected = c("dead","dead_car","dead_bike","dead_walk"),
                     multiple = TRUE)
    }
  })
  
  output$table_avg <- renderUI({
    view <- input$view_level
    dt <- NULL
    
    if (input$avg_kind == "death") {
      causes <- input$avg_death_causes; req(causes)
      if (view == "Overall") {
        dt <- mean_age_dead_raw_by_scen_val |>
          filter(value %in% causes) |>
          left_join(mean_age_dead_weight_by_scen_val |> filter(value %in% causes),
                    by = c("scen","value")) |>
          arrange(scen, value) |>
          rename(mean_age_raw_years = mean_age_raw)
      } else if (view == "Gender") {
        dt <- mean_age_dead_raw_by_scen_val_gender |>
          filter(value %in% causes) |>
          left_join(mean_age_dead_weight_by_scen_val_gender |> filter(value %in% causes),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender, value) |>
          rename(mean_age_raw_years = mean_age_raw)
      } else {
        dt <- mean_age_dead_raw_by_scen_val_lad |>
          (\(df) if(length(input$lad_sel) > 0) filter(df, ladnm %in% input$lad_sel) else df)() |>
          filter(value %in% causes) |>
          arrange(scen, ladnm, value) |>
          rename(mean_age_raw_years = mean_age_raw)
      }
    } else {
      cause <- input$avg_cause; req(cause)
      if (view == "Overall") {
        dt <- mean_age_onset_raw_by_scen_val |>
          filter(value == cause) |>
          left_join(mean_age_onset_weight_by_scen_val |> filter(value == cause),
                    by = c("scen","value")) |>
          arrange(scen) |>
          select(scen, value,
                 mean_age_raw_years = mean_age_raw)
      } else if (view == "Gender") {
        dt <- mean_age_onset_raw_by_scen_val_gender |>
          filter(value == cause) |>
          left_join(mean_age_onset_weight_by_scen_val_gender |> filter(value == cause),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender) |>
          select(scen, gender, value,
                 mean_age_raw_years = mean_age_raw)
      } else {
        dt <- mean_age_onset_raw_by_scen_val_lad |>
          (\(df) if(length(input$lad_sel) > 0) filter(df, ladnm %in% input$lad_sel) else df)() |>
          filter(value == cause) |>
          arrange(scen, ladnm) |>
          rename(mean_age_raw_years = mean_age_raw)
      }
    }
    
    dt <- dt |> dplyr::select(-any_of(c("mean_age_weighted")))
    
    if (length(input$scen_sel))
      dt <- dt |> filter(grepl(paste(input$scen_sel, collapse = "|"), scen))
    
    get_normalized_table(dt |> 
                            pivot_wider(names_from = scen, values_from = mean_age_raw_years)) |>
      dplyr::select(-matches("min|max|norm")) |> 
      gt() |>
      tab_options(table.font.size = "small") |>
      opt_interactive(use_filters = TRUE,
                      use_sorting = FALSE,
                      use_compact_mode = TRUE)
  })
  
  # ---------- ASR ----------
  to_chr_cause <- function(df) if ("cause" %in% names(df)) dplyr::mutate(df, cause = as.character(cause)) else df
  asr_overall_all                 <- to_chr_cause(asr_overall_all)
  asr_overall_avg_1_30            <- to_chr_cause(asr_overall_avg_1_30)
  asr_gender_all                  <- to_chr_cause(asr_gender_all)
  asr_gender_all_avg_1_30         <- to_chr_cause(asr_gender_all_avg_1_30)
  asr_lad_all_per_cycle           <- to_chr_cause(asr_lad_all_per_cycle)
  asr_lad_all_avg_1_30            <- to_chr_cause(asr_lad_all_avg_1_30)
  asr_healthy_years_overall       <- to_chr_cause(asr_healthy_years_overall)
  asr_healthy_years_overall_avg_1_30 <- to_chr_cause(asr_healthy_years_overall_avg_1_30)
  
  build_asr_plot <- reactive({
    req(input$asr_mode, input$asr_causes, input$view_level, input$scen_sel)
    causes <- input$asr_causes
    scens <- input$scen_sel
    if (input$asr_mode == "avg") {
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
          filter(cause %in% causes, scen %in% scens)
        req(nrow(df) > 0)
        
        df |>
          group_by(cause, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate) 
        
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all_avg_1_30 |> 
          filter(cause %in% causes, scen %in% scens)
        req(nrow(df) > 0)
        
        df |> 
          mutate(gender = case_when(gender == 1 ~ "Male",
                                                    gender == 2 ~ "Female")) |> 
          group_by(cause, gender, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate)
        
      } else {
        df <- asr_lad_all_avg_1_30 |> 
          filter(cause %in% causes, scen %in% scens)
          
        if (length(input$lad_sel)) df <- df |> filter(ladnm %in% input$lad_sel)
        req(nrow(df) > 0)
        top_ids <- df |>
          filter(cause == causes[1], scen == "reference") |>
          #slice_max(order_by = age_std_rate, n = input$lad_topn, with_ties = FALSE) |>
          distinct(ladnm)
        dplot <- df |> filter(ladnm %in% top_ids$ladnm)
        req(nrow(dplot) > 0)
        
        dplot |> 
          group_by(cause, ladnm, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate)
        
      }
    } else {
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_all, asr_healthy_years_overall) |>
          filter(cause %in% causes, cycle >= MIN_CYCLE)
        req(nrow(df) > 0)
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen, group = scen)) +
          geom_smooth(se = FALSE, method = "loess") +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle (smoothed, cycles 1–30)", x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean()
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all |> filter(cause %in% causes, cycle >= MIN_CYCLE)
        req(nrow(df) > 0)
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen, linetype = gender)) +
          geom_smooth(se = FALSE, method = "loess") +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle by gender (smoothed, cycles 1-30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario", linetype = "Gender") +
          theme_clean()
      } else {
        dat <- asr_lad_all_per_cycle |> filter(cause %in% causes, cycle >= MIN_CYCLE)
        req(nrow(dat) > 0)
        if (length(input$lad_sel)) {
          dat <- dat |> filter(ladnm %in% input$lad_sel)
        } 
        req(nrow(dat) > 0)
        ggplot(dat, aes(x = cycle, y = age_std_rate, colour = scen)) +
          geom_smooth(se = FALSE, method = "loess") +
          facet_grid(ladnm ~ cause, scales = "free_y") +
          labs(title = "ASR per cycle by LAD (smoothed, cycles 1-30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean()
      }
    }
  })
  
  get_normalized_table <- function(df){
    scen_cols <- all_scenarios
    
    if (length(input$scen_sel))
      scen_cols <- input$scen_sel
    
    scen_cols <- intersect(scen_cols, names(df))
    
    norm_df <- df |>
      rowwise() |>
      mutate(
        row_min = min(c_across(all_of(scen_cols)), na.rm = TRUE),
        row_max = max(c_across(all_of(scen_cols)), na.rm = TRUE)
      ) |>
      mutate(across(
        all_of(scen_cols),
        function(x) if_else(row_max == row_min, 0.5, (x - row_min) / (row_max - row_min)),
        .names = "{.col}_norm"
      )) |>
      ungroup()
    
    # Create html-colored cell content
    for (scen in scen_cols) {
      norm_col <- paste0(scen, "_norm")
      norm_df[[scen]] <- mapply(function(val, norm) {
        color <- col_fun(norm)
        sprintf("<div style='background-color:%s; padding:2px;'>%s</div>", color, round(val, 2))
      }, norm_df[[scen]], norm_df[[norm_col]], SIMPLIFY = TRUE)
    }
    
    return(norm_df)
  }
  
  output$plot_asrly <- renderUI({
    plot_obj <- build_asr_plot()
    if (inherits(plot_obj, "ggplot")) {
      output$plot_asrly <- renderPlotly({
        ggplotly(plot_obj)#, tooltip = c("x", "y", "colour", "fill", "linetype"))
      })
      plotlyOutput("plot_asrly")
    } else {#if (inherits(plot_obj, "gt_tbl")) {
      output$asr_gt <- render_gt({
        
        
        req(nrow(plot_obj) > 0)
        
        gt_tbl <- get_normalized_table(plot_obj) |>
          dplyr::select(-matches("min|max|norm")) |> 
          #dplyr::select(cause, any_of(c("gender", "ladnm")), all_of(html_cols)) |>
          gt() |>
          #cols_label(!!!setNames(html_cols, html_cols)) |>
          #fmt_markdown(columns = all_of(html_cols)) |>
          tab_options(table.font.size = "small") |>
          opt_interactive(use_filters = TRUE,
                          use_sorting = FALSE,
                          use_compact_mode = TRUE)
        
      })
      gt_output("asr_gt")
    }
  })
  
  
  # ---------- CSV download ----------
  current_table <- reactive({
    tab <- input$main_tabs
    if (tab == "Population") {
      pd <- pop_data(); pd$data |> mutate(across(where(is.numeric), ~ round(., 6)))
    } else if (tab == "Differences vs reference") {
      d <- diff_long()
      if (isTRUE(input$diff_cumulative)) {
        by <- if ("gender" %in% names(d)) "gender" else if ("ladnm" %in% names(d)) "ladnm" else character(0)
        d |> group_by(across(all_of(c("scen", by)))) |> slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
          ungroup() |> transmute(scen, across(all_of(by)), final_cycle = cycle, cumulative_value = y, cumulative_value_scaled = y * SCALING)
      } else d
    } else if (tab == "Average onset ages") {
      output$table_avg |> req(); isolate({ output$table_avg() })
    } else if (tab == "ASR") {
      if (input$asr_mode == "avg") {
        if (input$view_level == "Overall") {
          bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
            filter(cause %in% input$asr_causes)
        } else if (input$view_level == "Gender") {
          asr_gender_all_avg_1_30 |> filter(cause %in% input$asr_causes)
        } else {
          asr_lad_all_avg_1_30 |> filter(cause %in% input$asr_causes)
        }
      } else {
        if (input$view_level == "Overall") {
          bind_rows(asr_overall_all, asr_healthy_years_overall) |>
            filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE)
        } else if (input$view_level == "Gender") {
          asr_gender_all |> filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE)
        } else {
          asr_lad_all_per_cycle |> filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE)
        }
      }
    }else if (tab == "Exposures"){
      
      view <- input$view_level
      # Handle missing or non-"Overall" selections
      lexp <- if (view == "Overall") {
        exp |> filter(grepl("Overall", grouping))
      } else if (view == "Gender") {
        exp |> filter(grepl("Gender", grouping))
      } else if  (view == "LAD") {
        exp |> filter(grepl("LAD", grouping))
      }
      
      if (view == "LAD" && length(input$lad_sel)) {
        lexp <- lexp |> filter(grepl(paste(input$lad_sel, collapse = "|"), grouping))
      } 
      
      if (length(input$scen_sel)) {
        lexp <- lexp |> filter(grepl(paste(input$scen_sel, collapse = "|"), scen))
      }
      
      lexp
    }
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("export_", gsub("\\s+","_", tolower(input$main_tabs)), "_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(current_table(), file, na = "")
  )
  
  
  output$out_zm <- renderPlotly({
    
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
    req(input$scen_sel)
    # req(input$lad_sel)
    # req(input$view_sel)
    
    
    if (input$metrics_picker == "Trip Mode Share (%)") {
      
      facet_vars <- vars("")
      
      fs <- 3

      if (input$view_level == "Overall") {
        tp <- t$trips_percentage_combined |> 
          group_by(scen, mode) |> 
          reframe(trip_count = sum(trip_count)) |> 
          group_by(scen) |> 
          mutate(tt = sum(trip_count)) |> 
          ungroup() |> 
          mutate(pt = trip_count/tt * 100)
        
      }else if(input$view_level == "Gender"){
        tp <- t$trips_percentage_combined |> 
          filter(!is.na(gender)) |> 
          group_by(scen, mode, gender) |> 
          reframe(trip_count = sum(trip_count)) |> 
          group_by(scen, gender) |> 
          mutate(tt = sum(trip_count)) |> 
          ungroup() |> 
          mutate(pt = trip_count/tt * 100,
                 gender = as.factor(case_when(gender == 1 ~ "Male", 
                                                            gender == 2 ~ "Female")))
                 
        
        
        facet_vars <- vars(gender)
      }else if(input$view_level == "LAD"){
        tp <- t$trips_percentage_combined |> 
          group_by(LAD_origin, scen, mode) |> 
          reframe(trip_count = sum(trip_count)) |> 
          group_by(scen, LAD_origin) |> 
          mutate(tt = sum(trip_count)) |> 
          ungroup() |> 
          mutate(pt = trip_count/tt * 100)
        
        facet_vars <- vars(LAD_origin)
        fs <- 1
      
        
      }
      
      if (length(input$scen_sel)){
        tp <- tp |> 
          filter(scen %in% input$scen_sel)
        fs <- 3
      }
      
      if(input$view_level == "LAD" && length(input$lad_sel)){
        tp <- tp |> 
          filter(LAD_origin %in% input$lad_sel)
      }
      
      
      ggplotly(ggplot(tp) +
                 aes(x = scen, y = pt, fill = mode) +
                 geom_col() +
                 scale_fill_hue(direction = 1) +
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
                 geom_text(aes(label = ifelse(pt > 2, paste0(round(pt, 1), "%"), "")),
                           position = position_stack(vjust = .5),
                           size = fs) +
                 
                 facet_wrap(facet_vars) +
                   labs(
                     title = "Transport Mode Share (%)",
                     y = "Proportion (%)",
                     x = "Scenario",
                     fill = "Transport Mode"
                   )
                 )
    }
    
    else if (input$metrics_picker == "Combined Trip Distance by Modes") {
      
      
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
            facet_wrap(vars(ladnm)) +
            labs(title = "Average weekly dist. pp by mode and location",
                 fill = "Scenario")
        )
        
      } else if (input$metrics_picker == "Trip Duration by Mode") {
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
      } else if (input$metrics_picker == "Zero Mode") {
        plot_ly(
          data = data.frame(category = LETTERS[1:4], count = c(10, 5, 15, 20)),
          x = ~category, y = ~count, type = "bar"
        ) %>%
          layout(title = "Zero Mode Metrics", yaxis = list(title = "Count"))
      }

    
  })
  
  output$plot_exp <- render_gt({
    req(input$view_level)  # Ensure input is available
    
    lexp <- current_table()
    
    # Ensure data isn’t empty
    req(nrow(lexp) > 0)
    
    # col_fun <- col_numeric(palette = c("lightpink", "lightgreen"), domain = c(0, 1))
    
    wide_df <- lexp |>
      pivot_wider(
        #id_cols = c(grouping, variable),
        names_from = scen,
        values_from = value
      )
    
    scen_cols <- setdiff(names(wide_df), c("grouping", "variable", "stat"))
    
    norm_df <- wide_df |>
      rowwise() |>
      mutate(
        row_min = min(c_across(all_of(scen_cols)), na.rm = TRUE),
        row_max = max(c_across(all_of(scen_cols)), na.rm = TRUE)
      ) |>
      mutate(across(
        all_of(scen_cols),
        function(x) if_else(row_max == row_min, 0.5, (x - row_min) / (row_max - row_min)),
        .names = "{.col}_norm"
      )) |>
      ungroup()
    
    # Create html-colored cell content
    for (scen in scen_cols) {
      norm_col <- paste0(scen, "_norm")
      #html_col <- paste0(scen, "_html")
      norm_df[[scen]] <- mapply(function(val, norm) {
        color <- col_fun(norm)
        sprintf("<div style='background-color:%s; padding:2px;'>%s</div>", color, round(val, 2))
      }, norm_df[[scen]], norm_df[[norm_col]], SIMPLIFY = TRUE)
    }
    
    html_cols <- scen_cols
    
    gt_tbl <- norm_df |>
      dplyr::select(grouping, variable, stat, all_of(html_cols)) |>
      #rename_at(vars(everything()), ~ sub("_html$", "", .x)) |> 
      gt(groupname_col = "grouping") |>
      cols_label(!!!setNames(html_cols, html_cols)) |>
      fmt_markdown(columns = all_of(html_cols)) |>
      tab_options(table.font.size = "small") |>
      opt_interactive(use_filters = TRUE,
                      use_sorting = FALSE,
                      use_compact_mode = TRUE)
    
    gt_tbl
  })
  
  

}

shinyApp(ui, server)