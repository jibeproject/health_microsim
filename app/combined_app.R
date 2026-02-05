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

pc <- qs::qread("processed_data/seed = 3/shiny/precomputed_040226_100%V6.qs")
t <- qs::qread("processed_data/seed = 3/shiny/121225_trips.qs")
exp <- qs::qread("processed_data/seed = 3/shiny/exp_050226.qs")

SCALING <- 1L


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
death_values <- c("Death (all causes)" = "dead",
                  "Death (car)" = "dead_car",
                  "Death (cyclist)" = "dead_bike",
                  "Death (pedestrian)" = "dead_walk")

# ------------------- UI -------------------------------------------------
all_scenarios <- sort(unique(pc$people_overall$scen))
pop_cycles    <- sort(unique(pc$people_overall$cycle))
trend_cycles  <- sort(unique(pc$asr_overall_all$cycle))
all_lads_nm   <- sort(unique(pc$people_lad$ladnm))
all_genders   <- sort(unique(pc$people_gender$gender))
all_causes_asr <- pc$asr_overall_all |> distinct(cause) |> filter(!grepl("dead", cause)) |> pull() #
all_causes_except_dead <- pc$asr_overall_all |> distinct(cause) |> filter(!grepl("dead", cause)) |> pull()
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
        #condition = "input.tabs == 'Travel & Exposures'",
        condition = "input.main_tabs == 'Travel & Exposures' && 
        input.inner_tabs == 'Travel Behaviour'",
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
          selectizeInput("pop_cycles", "Cycles to show (bars):",
                         choices = pop_cycles, selected = c(2,10,MAX_CYCLE), multiple = TRUE),
          radioButtons("pop_style", "Bar style:", c("Stacked"="stack","Side-by-side"="dodge"),
                       inline = TRUE),
          checkboxInput("pop_share", "Show shares (else counts)", value = TRUE)
        ),
        
        conditionalPanel(
          condition = "input.main_tabs == 'Differences vs reference'",
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
          selectInput("avg_kind", "Average age of:", choices = c("Death"="death","Disease onset"="onset")),
          uiOutput("avg_cause_ui")
        ),
        conditionalPanel(
          condition = "input.main_tabs == 'Age Standardised Rates'",
          selectInput("asr_mode", "ASR view:",
                      choices = c("Average 1-30 (bars)"="avg","Over time (smoothed)"="trend"),
                      selected = "Over time (smoothed)")
        ),
        conditionalPanel(
          condition = "input.main_tabs == 'Age Standardised Rates' || (input.main_tabs == 'Differences vs reference' && 
          input.metric_kind == 'diseases')",
          selectizeInput("asr_causes", "Causes:", choices =  append(all_causes_asr, death_values),
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
    nav_panel("Differences vs reference",
              uiOutput("table_diff_summary", height = "100vh"),
              #tags$hr(),
              #h5("Summary (cumulative at latest cycle, or sum if non-cumulative)"),
              #plotlyOutput("plot_diffly", height = "35vh")
    ),
    nav_panel("Age Standardised Rates",
              uiOutput("plot_asrly")#, height = "85vh"),
    ),
    nav_panel("Average onset ages", 
              gt_output("table_avg")
    ),
    nav_panel("Population",
              plotlyOutput("plot_poply")#, height = "85vh")
    ),
    nav_panel("Exposures",
              value = "Exposures",
              gt_output("plot_exp")
    )
  )
)


# ------------------- SERVER --------------------------------------------
server <- function(input, output, session) {
  
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
  
  
  observeEvent({
    list(input$main_tabs)
  }, {
    req(input$main_tabs)
    
    current <- isolate(input$view_level)
    
    if (input$main_tabs %in% c("Differences vs reference", "Age Standardised Rates")) {
      new_choices <- c(selected_views, additional_selected_views)
    } else if (input$main_tabs %in% c("Population", "Average onset ages")) {
      new_choices <- selected_views
    } else if (input$main_tabs == "Exposures") {
      new_choices <- c(selected_views, additional_selected_views, "Agegroup")
    } else {
      new_choices <- selected_views
    }
    
    # Ensure current selection is valid within new choices
    if (!is.null(current) && !current %in% new_choices) current <- NULL
    
    updateSelectInput(session, "view_level",
                      choices = new_choices,
                      selected = current)
    
  }, ignoreNULL = TRUE)
  
  
  # ---------- Avg ages cause picker ----------
  output$avg_cause_ui <- renderUI({
    if (input$avg_kind == "onset") {
      selectizeInput("avg_cause", "Disease (onset):",
                     multiple = TRUE,
                  choices = all_causes_except_dead,
                  selected = "coronary_heart_disease")
    } else {
      selectizeInput(
        "avg_death_causes", 
        "Death cause(s):",
        choices = c(
          "Death (all causes)" = "dead",
          "Death (car)" = "dead_car",
          "Death (cyclist)" = "dead_bike",
          "Death (pedestrian)" = "dead_walk"
        ),
        selected = c("dead", "dead_car", "dead_bike", "dead_walk"),
        multiple = TRUE
      )
      
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
      dat <- set_ag(pc$people_overall) |> filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen")), y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, y = "pop", y_lab = "Population count")
      }
    } else if (view == "Gender") {
      dat <- set_ag(pc$people_gender) |> 
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
      dat <- set_ag(pc$people_lad) |> filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
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
    req(input$metric_kind, input$view_level, input$diff_min_cycle, input$asr_causes)#, input$diff_cumulative)
    
    scen_keep <- setdiff(input$scen_sel, "reference")
    validate(need(length(scen_keep) > 0, "Select at least one non-reference scenario."))
    minc <- input$diff_min_cycle; view <- input$view_level; cumu <- isTRUE(input$diff_cumulative)
    
    dl <- pc$deaths_lad
    dil <- pc$diseases_lad
    hl <- pc$healthy_lad
    ll <- pc$lifey_lad
    if (length(input$lad_sel)){
      dl <- dl |> filter(ladnm %in% input$lad_sel)
      dil <- dil |> filter(ladnm %in% input$lad_sel)
      hl <- hl |> filter(ladnm %in% input$lad_sel)
      ll <- ll |> filter(ladnm %in% input$lad_sel)

    }
    
    do <- pc$diseases_overall
    dg <- pc$diseases_gender
    dimd <- pc$diseases_imd
    
    if (length(input$asr_causes)){
      do <- do |> filter(cause %in% input$asr_causes)
      dg <- dg |> filter(cause %in% input$asr_causes)
      dimd <- dimd |> filter(cause %in% input$asr_causes)
      dil <- dil |> filter(cause %in% input$asr_causes)
    }
    
    pick <- switch(input$metric_kind,
                   deaths   = list(Overall=pc$deaths_overall, Gender=pc$deaths_gender,  LAD=dl, IMD = pc$deaths_imd, label="Δ Deaths"),
                   diseases = list(Overall=do, Gender=dg, LAD=dil,IMD = dimd, label="Δ Diseases"),
                   healthy  = list(Overall=pc$healthy_overall, Gender=pc$healthy_gender, LAD=hl,IMD = pc$healthy_imd, label="Δ Healthy years"),
                   life     = list(Overall=pc$lifey_overall, Gender=pc$lifey_gender, LAD=ll,  IMD = pc$lifey_imd, label="Δ Life years"),
                   imp_fac  = list(Overall = plyr::rbind.fill(pc$lifey_overall |> mutate(factor = "Δ Life years"),
                                                              pc$healthy_overall |> mutate(factor = "Δ Healthy years"),
                                                              pc$deaths_overall |> mutate(factor = "Δ Deaths")), 
                                   Gender=plyr::rbind.fill(pc$lifey_gender  |> mutate(factor = "Δ Life years"),
                                                           pc$healthy_gender   |> mutate(factor = "Δ Healthy years"),
                                                           pc$deaths_gender |> mutate(factor = "Δ Deaths")), 
                                   LAD=plyr::rbind.fill(ll |> mutate(factor = "Δ Life years"),
                                                        hl |> mutate(factor = "Δ Healthy years"),
                                                        dl |> mutate(factor = "Δ Deaths")),  
                                   IMD=plyr::rbind.fill(pc$lifey_imd  |> mutate(factor = "Δ Life years"),
                                                        pc$healthy_imd |> mutate(factor = "Δ Healthy years"),
                                                        pc$deaths_imd |> mutate(factor = "Δ Deaths")),
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
      summarise(diff = (if (cumu) median else sum)(diff, na.rm = TRUE)) |>
      group_by(across(all_of(grp))) |>
      mutate(y = diff) |> #if (cumu) cumsum(diff) else diff) |>
      ungroup() |>
      mutate(metric = pick$label)
    
  })
  
  build_diff_plot <- reactive({
    d <- diff_long(); req(nrow(d) > 0)
    ylab <- if (isTRUE(input$diff_cumulative)) "Cumulative Δ vs reference" else "Δ vs reference"
    bar_chart_func <- if (isTRUE(input$diff_cumulative)) "sum" else "mean"
    
    ttl  <- d$metric[1]
    if ("gender" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE, method = "loess") + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        {
          if ("factor" %in% names(d)) {
            facet_wrap(vars(gender, factor), scales = "free_y")
          } else {
            facet_wrap(vars(gender), nrow = 2, scales = "free_y")
          }
        } +
        theme_clean()
    } else if ("ladnm" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE, method = "loess") + add_zero_line() +
        facet_wrap(~ ladnm, nrow = 2, scales = "free_y") +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean()
    } else {
      
      if (all(c("imd10", "cause") %in% names(d))){
        ggplot(
          reframe(group_by(d,
                           cause,
                           scen,
                           imd10),
                  y = sum(y))
        ) +
          aes(x = imd10, y = y, fill = scen) +
          geom_bar(
            stat = "summary",
            fun = bar_chart_func,
            position = "dodge2"
          )+
          theme_minimal() +
          facet_wrap(vars(cause), scales = "free_y") + 
          scale_x_continuous(breaks = c(1:10)) +
          labs(title = ttl, x = "Index of Multiple Deprivation (IMD)", y = ylab, colour = "Scenario")
        
      }else{
      
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE, method = "loess") + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        {
          if ("factor" %in% names(d)) {
            facet_wrap(~ factor, scales = "free_y")
          } else if  ("cause" %in% names(d)) {
            # if ("imd10" %in% names(d))
            #   write_csv(d, "imd_cum.csv")
            facet_wrap(~ cause, scales = "free_y")
          }else {
            list()   # add nothing
          }
        } +
        theme_clean()
      }
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
      plotlyOutput("diff_summary_plot", height = "100vh")
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
    
    bar_chart_func <- if (isTRUE(input$diff_cumulative)) "sum" else "mean"
    
    if (grepl("Diseases", data$metric_lab)){
      
      if (!"imd10" %in% names(cumdf)){
        p <- ggplot(cumdf) +
          aes(x = cause, y = cumulative_value, fill = scen) +
          geom_bar(
            stat = "summary",
            fun = bar_chart_func,
            position = "dodge2"
          ) +
          scale_fill_hue(direction = 1) +
          labs(
            fill = "Scenario",
            y = "",
            x = ""
          ) +
          
          geom_text(
            aes(label = cumulative_value, y = cumulative_value / 2),
            size = ifelse("gender" %in% names(cumdf), 2, 3),
            position = position_dodge(width = 1),
            color = "black"
          ) +
        
          coord_flip() +
          theme_minimal()
          
      }else{
        
        p <- ggplot(cumdf) +
          aes(x = imd10, y = cumulative_value, colour = scen) +
          geom_col(position = position_dodge(width = 0.9), aes(fill = scen)) +
          scale_x_continuous(breaks = c(1:10)) +
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
        geom_bar(stat = "summary", fun = bar_chart_func, position = "dodge2") +
        scale_fill_hue(direction = 1) +
        geom_text(
          aes(label = cumulative_value, y = cumulative_value / 2),
          size = ifelse("gender" %in% names(cumdf), 2, 3),
          position = position_dodge(width = 1),
          color = "black"
        ) +
        coord_flip() +
        labs(x = "") +
        theme_minimal() 
      }else{
        
        p <- ggplot(cumdf) +
          aes(x = imd10, y = cumulative_value, fill = scen) +
          geom_col(position = position_dodge(width = 0.9)) +
          scale_color_hue(direction = 1) +
          theme_minimal() +
          scale_x_continuous(breaks = c(1:10)) +
          labs(
            x = "Index of Multiple Deprivation (IMD)",
            color = "Scenario"
          ) +
          guides(color = "none")
        
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
          geom_col(position = position_dodge(width = 0.9), aes(fill = scen)) +
          scale_x_continuous(breaks = c(1:10)) + 
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
  
  get_onset_ages <- reactive({
    # req(input$avg_kind, input$view_level, input$scen_sel, input$avg_cause, input$avg_death_causes,
    #     input$lad_sel)          
          
    view <- input$view_level
    
    dt <- NULL
    
    if (input$avg_kind == "death") {
      causes <- input$avg_death_causes; req(causes)
      
      if (view == "Overall") {
        dt <- pc$mean_age_dead_raw_by_scen_val |>
          filter(value %in% causes) |>
          left_join(pc$mean_age_dead_weight_by_scen_val |> filter(value %in% causes),
                    by = c("scen","value")) |>
          arrange(scen, value) |>
          rename(mean_age_raw_years = mean_age_raw) |> 
          mutate(value = case_when(value == "dead" ~ "Death (all causes)",
                                   value == "dead_car" ~ "Death (car)",
                                   value == "dead_bike" ~ "Death (cyclist)",
                                   value == "dead_walk" ~ "Death (pedestrian)"))
      } else if (view == "Gender") {
        dt <- pc$mean_age_dead_raw_by_scen_val_gender |>
          filter(value %in% causes) |>
          left_join(pc$mean_age_dead_weight_by_scen_val_gender |> filter(value %in% causes),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender, value) |>
          rename(mean_age_raw_years = mean_age_raw) |> 
          mutate(value = case_when(value == "dead" ~ "Death (all causes)",
                                   value == "dead_car" ~ "Death (car)",
                                   value == "dead_bike" ~ "Death (cyclist)",
                                   value == "dead_walk" ~ "Death (pedestrian)"))
      } else {
        
        dt <- pc$mean_age_dead_raw_by_scen_val_lad |>
          (\(df) if(length(input$lad_sel) > 0) filter(df, ladnm %in% input$lad_sel) else df)() |>
          filter(value %in% causes) |>
          arrange(scen, ladnm, value) |>
          rename(mean_age_raw_years = mean_age_raw) |> 
          mutate(value = case_when(value == "dead" ~ "Death (all causes)",
                                   value == "dead_car" ~ "Death (car)",
                                   value == "dead_bike" ~ "Death (cyclist)",
                                   value == "dead_walk" ~ "Death (pedestrian)"))
        
      }
    } else {
      cause <- input$avg_cause; req(cause)
      if (view == "Overall") {
        dt <- pc$mean_age_onset_raw_by_scen_val |>
          filter(value %in% cause) |>
          left_join(pc$mean_age_onset_weight_by_scen_val |> filter(value %in% cause),
                    by = c("scen","value")) |>
          arrange(scen) |>
          select(scen, value,
                 mean_age_raw_years = mean_age_raw)
      } else if (view == "Gender") {
        dt <- pc$mean_age_onset_raw_by_scen_val_gender |>
          filter(value %in% cause) |>
          left_join(pc$mean_age_onset_weight_by_scen_val_gender |> filter(value %in% cause),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender) |>
          select(scen, gender, value,
                 mean_age_raw_years = mean_age_raw)
      } else {
        dt <- pc$mean_age_onset_raw_by_scen_val_lad |>
          (\(df) if(length(input$lad_sel) > 0) filter(df, ladnm %in% input$lad_sel) else df)() |>
          filter(value %in% cause) |>
          arrange(scen, ladnm) |>
          rename(mean_age_raw_years = mean_age_raw)
      }
    }
    
    dt <- dt |> dplyr::select(-any_of(c("mean_age_weighted")))
    
    if (length(input$scen_sel))
      dt <- dt |> filter(grepl(paste(input$scen_sel, collapse = "|"), scen))
    
    if ("gender" %in% names(dt)){
      dt$gender <- ifelse(dt$gender == 1, "Male",
                          ifelse(dt$gender == 2, "Female", NA))
    }
    
    return(dt)
  })
  
  output$table_avg <- renderUI({
    
    # #req(input$avg_kind, input$view_level, input$scen_sel, input$avg_cause, input$avg_death_causes)
    # req(input$avg_kind, input$view_level, input$scen_sel, input$avg_cause, input$avg_death_causes,
    #     input$lad_sel)
    
    dt <- get_onset_ages()
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
  asr_overall_all                 <- to_chr_cause(pc$asr_overall_all)
  asr_overall_avg_1_30            <- to_chr_cause(pc$asr_overall_avg_1_30)
  asr_gender_all                  <- to_chr_cause(pc$asr_gender_all)
  asr_gender_all_avg_1_30         <- to_chr_cause(pc$asr_gender_all_avg_1_30)
  asr_lad_all_per_cycle           <- to_chr_cause(pc$asr_lad_all_per_cycle)
  asr_lad_all_avg_1_30            <- to_chr_cause(pc$asr_lad_all_avg_1_30)
  asr_healthy_years_overall       <- to_chr_cause(pc$asr_healthy_years_overall)
  asr_healthy_years_overall_avg_1_30 <- to_chr_cause(pc$asr_healthy_years_overall_avg_1_30)
  
  # New reactive to handle all data fetching and processing
  get_asr_data <- reactive({
    
    #req(input$asr_mode, input$asr_causes, input$view_level, input$scen_sel)
    causes <- input$asr_causes
    scens <- input$scen_sel
    
    
    df <- NULL
    
    if (input$asr_mode == "avg") {
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
          filter(cause %in% causes, scen %in% scens) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause)))
        
        req(nrow(df) > 0)
        return(df |>
          group_by(cause, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate))
        
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all_avg_1_30 |> 
          filter(cause %in% causes, scen %in% scens) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause)))
        
        req(nrow(df) > 0)
        return(df |> 
          mutate(gender = case_when(
            gender == 1 ~ "Male",
            gender == 2 ~ "Female"
          )) |> 
          group_by(cause, gender, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate)
        )
        
      } else if (input$view_level == "IMD") {
        df <- pc$asr_imd_all_avg_1_30 |> 
          filter(cause %in% causes, scen %in% scens) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause)))
        
        req(nrow(df) > 0)
        return(df |>  
          group_by(cause, imd10, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate))
        
      } else {
        df <- asr_lad_all_avg_1_30 |> 
          filter(cause %in% causes, scen %in% scens) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause)))
        
        if (length(input$lad_sel)) 
          df <- df |> filter(ladnm %in% input$lad_sel)
        
        req(nrow(df) > 0)
        top_ids <- df |> 
          filter(cause == causes[1], scen == "reference") |> 
          distinct(ladnm)
        
        dplot <- df |> filter(ladnm %in% top_ids$ladnm)
        req(nrow(dplot) > 0)
        
        return(dplot |> 
          group_by(cause, ladnm, scen) |> 
          reframe(age_std_rate = mean(age_std_rate, na.rm = TRUE)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate))
      }
    } else {
      # Non-average mode datasets
      if (input$view_level == "Overall") {
        return(bind_rows(asr_overall_all, asr_healthy_years_overall) |>
          filter(cause %in% causes, scen %in% scens, cycle >= MIN_CYCLE) |> 
            mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                     cause == "dead_car" ~ "Death (car)",
                                     cause == "dead_bike" ~ "Death (cyclist)",
                                     cause == "dead_walk" ~ "Death (pedestrian)",
                                     .default = as.character(cause))))
      } else if (input$view_level == "Gender") {
        return(asr_gender_all |> 
          filter(cause %in% causes, scen %in% scens, cycle >= MIN_CYCLE) |> 
            mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                     cause == "dead_car" ~ "Death (car)",
                                     cause == "dead_bike" ~ "Death (cyclist)",
                                     cause == "dead_walk" ~ "Death (pedestrian)",
                                     .default = as.character(cause))) |>
          mutate(gender = ifelse(gender == 1, "Male",
                                 ifelse(gender == 2, "Female", NA))))
      } else if (input$view_level == "IMD") {
        return(pc$asr_imd_all |> 
          filter(cause %in% causes, scen %in% scens, cycle >= MIN_CYCLE) |> 
            mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                     cause == "dead_car" ~ "Death (car)",
                                     cause == "dead_bike" ~ "Death (cyclist)",
                                     cause == "dead_walk" ~ "Death (pedestrian)",
                                     .default = as.character(cause)))
          )
      } else {
        df <- asr_lad_all_per_cycle |> 
          filter(cause %in% causes, scen %in% scens, cycle >= MIN_CYCLE) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause)))
        if (length(input$lad_sel)) {
          df <- df |> filter(ladnm %in% input$lad_sel)
        }
        
        return(df)
      }
      
    }
    
  })
  
  
  build_asr_plot <- reactive({
    df <- get_asr_data()
    req(df)
    
    if (input$asr_mode == "avg") {
      df  # return processed table
    } else {
      if (input$view_level == "Overall") {
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen, group = scen)) +
          geom_col(position = position_dodge(width = 0.9), aes(fill = scen)) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle (summed over cycles 1-30)\n\n", 
               x = "Cycle (year)", y = "ASR per 100,000") +
          theme_clean()
        
      } else if (input$view_level == "Gender") {
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen)) +
          geom_col(position = position_dodge(width = 0.9), aes(fill = scen)) +
          facet_wrap(vars(cause, gender), scales = "free_y") +
          labs(title = "ASR per cycle by gender (summed over cycles 1-30)\n\n",
               x = "Cycle (year)", y = "ASR per 100,000") +
          theme_clean()
        
      } else if (input$view_level == "IMD") {
        ggplot(df, aes(x = imd10, y = age_std_rate, colour = scen)) +
          geom_col(position = position_dodge(width = 0.9), aes(fill = scen)) +
          scale_x_continuous(breaks = c(1:10)) + 
          facet_wrap(vars(cause), scales = "free_y") +
          labs(title = "ASR by IMD (summed over cycles 1-30)\n\n",
               x = "IMD", y = "ASR per 100,000") +
          theme_clean()
        
      } else {
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen)) +
          geom_col(position = position_dodge(width = 0.9), aes(fill = scen)) +
          facet_grid(ladnm ~ cause, scales = "free_y") +
          labs(title = "ASR per cycle by LAD (total for cycles 1-30)",
               x = "Cycle (year)", y = "ASR per 100,000") +
          theme_clean()
      }
    }
  })
  
  
  output$plot_asrly <- renderUI({
      plot_obj <- build_asr_plot()
    
    if (inherits(plot_obj, "ggplot")) {
      output$plot_asr <- renderPlotly({
        #req(nrow(plot_obj) > 0)
        ggplotly(plot_obj)
      })
      plotlyOutput("plot_asr", height = "100vh")
    } else {
      output$asr_gt <- render_gt({
        
        
        req(nrow(plot_obj) > 0)
        
        gt_tbl <- get_normalized_table(plot_obj) |>
          dplyr::select(-matches("min|max|norm")) |> 
          gt() |>
          tab_options(table.font.size = "small") |>
          opt_interactive(use_filters = TRUE,
                          use_sorting = FALSE,
                          use_compact_mode = TRUE) |> 
          tab_header(
            title = "Age-Standardised Rates per 100,000"
          )
        
      })
      gt_output("asr_gt")
    }
  })
  
  
  # ---------- CSV download ----------
  current_table <- reactive({
    tab <- input$main_tabs
    #inner_tab <- input$inner_tabs
    d <- NULL
    if (tab == "Population") {
      pd <- pop_data()
      d <- pd$data |> mutate(across(where(is.numeric), ~ round(., 6)))
    } else if (tab == "Differences vs reference") {
      d <- diff_long()
      
      if (isTRUE(input$diff_cumulative)) {
        by <- if ("gender" %in% names(d)) "gender" else if ("ladnm" %in% names(d)) "ladnm" else character(0)
        d |> group_by(across(all_of(c("scen", by)))) |> slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
          ungroup() |> transmute(scen, across(all_of(by)), final_cycle = cycle, cumulative_value = y, cumulative_value_scaled = y * SCALING) |> 
          as.data.frame()
      } else d
    } else if (tab == "Average onset ages") {
      req(input$avg_kind, input$view_level, input$scen_sel, input$avg_cause, input$avg_death_causes)
      isolate({ d <- get_onset_ages() })
    } else if (tab == "Age Standardised Rates") {
      isolate({ d <- get_asr_data() })
    }
    
    if (tab == "Exposures"){
      
      view <- input$view_level
      # Handle missing or non-"Overall" selections
      lexp <- if (view == "Overall") {
        d <- exp |> filter(grepl("Overall", grouping))
      } else if (view == "Gender") {
        d <- exp |> 
          filter(grepl("Gender", grouping)) |> 
          mutate(
            grouping = case_when(
              grouping == "Gender: 1" ~ "Gender: Male",
              grouping == "Gender: 2" ~ "Gender: Female",
              TRUE                  ~ grouping      # keep all other values as-is
            )
          )
        
      } else if  (view == "LAD") {
        d <- exp |> filter(grepl("LAD", grouping))
      } else if (view == "IMD"){
        d <- exp |> filter(grepl("IMD", grouping))
      } else if  (view == "Agegroup"){
        d <- exp |> filter(grepl("Age", grouping))
      }
      
      if (view == "LAD" && length(input$lad_sel)) {
        d <- lexp <- lexp |> filter(grepl(paste(input$lad_sel, collapse = "|"), grouping))
      } 
      
      if (length(input$scen_sel)) {
        d <- lexp <- lexp |> filter(grepl(paste(input$scen_sel, collapse = "|"), scen))
      }
    }
    d
    
  })
  output$download_csv <- downloadHandler(
    filename = function() { 
      paste0(
        "export_",
        gsub("\\s+","_", tolower(input$main_tabs)),
        "_",
        Sys.Date(),
        ".csv"
      )
    },
    content = function(file) {
      # If current_table is a reactive:
      dat <- current_table()
      print(dat)
      validate(
        need(!is.null(dat), "No data to download")
      )
      readr::write_csv(dat, file)
    }
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
      
      if (input$view_level == "Overall") {
      
      pop <- people_overall |> 
        filter(cycle == 0) |> 
        group_by(scen) |> 
        reframe(pop = sum(pop))
      
      td <- t$combined_distance |> 
        filter(grepl("All", ladnm)) |> 
        group_by(scen, mode) |> 
        reframe(total_dist = sum(sumDistance)) |> 
        left_join(pop) |> mutate(med_dist = total_dist/pop)
      
      ggplotly(ggplot(td) +
        aes(x = med_dist, y = mode, fill = scen) +
        geom_bar(stat = "summary", fun = "sum", position = "dodge2") +
        scale_fill_hue(direction = 1) +
        theme_minimal()
      )
      }else{
      
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
      }
        
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
      gt() |> #groupname_col = "grouping") |>
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