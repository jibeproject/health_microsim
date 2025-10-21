# ======================================================================
# Local Shiny: Population, Differences, Mean Ages (death/onset), ASR
# with simple on-disk caching in ./data to avoid recomputation
# ======================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr) 
  library(tidyr) 
  library(arrow) 
  library(readr)
  library(ggplot2)
  library(plotly) 
  library(scales) 
  library(forcats)
  library(purrr)
  library(here)
  library(qs)
  library(DT)
})

# ------------------- Paths (edit these if needed) -------------------
zones    <- readr::read_csv("/media/ali/Expansion/backup_tabea/manchester/input/zoneSystem.csv", show_col_types = FALSE)
stopifnot(all(c("ladcd","ladnm") %in% names(zones)))
lads <- zones |> distinct(ladcd, ladnm)

MIN_CYCLE <- 1
MAX_CYCLE <- 30#ax(all_data$cycle)


# ------------------- Helpers -------------------------------------------
ageband <- function(x) cut(
  x, breaks = c(seq(0,100,5), Inf),
  labels = c(paste(seq(0,95,5), seq(4,99,5), sep = "-"), "100+"),
  right = FALSE, include.lowest = TRUE
)

add_agegroups <- function(df) {
  df |>
    mutate(agegroup_cycle = ageband(age_cycle))
}

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

pc <- qs::qread(here("temp/precomputed_mcr_100%V2.qs"))
list2env(pc, envir = environment())
SCALING <- 1L

# ------------------- UI -------------------------------------------------
all_scenarios <- sort(unique(people_overall$scen))
pop_cycles    <- sort(unique(people_overall$cycle))
trend_cycles  <- sort(unique(asr_overall_all$cycle))
all_lads_nm   <- sort(unique(people_lad$ladnm))
all_genders   <- sort(unique(people_gender$gender))
all_causes_asr <- sort(unique(c(
  unique(asr_overall_all$cause),
  "healthy_years"
)))

ui <- fluidPage(
  titlePanel("Local Health Explorer — Population, Differences, Mean Ages & ASR"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxInput("use_plotly", "Interactive (Plotly)", value = TRUE),
      selectInput("scen_sel", "Scenarios:", choices = all_scenarios,
                  selected = all_scenarios, multiple = TRUE),
      selectInput("view_level", "View by:", choices = c("Overall","Gender","LAD"),
                  selected = "Overall"),
      conditionalPanel(
        "input.view_level == 'LAD'",
        selectizeInput("lad_sel", "LAD(s):",
                       choices = all_lads_nm, multiple = TRUE,
                       options = list(placeholder = "Pick LADs (optional)"))
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
          condition = "input.main_tabs == 'Differences vs reference'",
          h2("Differences"),
          selectInput("metric_kind", "Metric:", choices = c(
            "Diseases postponed (Δ diseases)" = "diseases",
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
          condition = "input.main_tabs == 'Average ages'",
          h2("Average ages"),
          selectInput("avg_kind", "Average age of:", choices = c("Death"="death","Disease onset"="onset")),
          uiOutput("avg_cause_ui")
        ),
        conditionalPanel(
          condition = "input.main_tabs == 'ASR'",
          h2("ASR"),
          selectInput("asr_mode", "ASR view:",
                      choices = c("Average 1-30 (bars)"="avg","Over time (smoothed)"="trend")),
          selectizeInput("asr_causes", "Causes:", choices = all_causes_asr,
                         selected = c("coronary_heart_disease","stroke","healthy_years"),
                         multiple = TRUE)#,
          # conditionalPanel(
          #   "input.view_level == 'LAD' && input.asr_mode == 'avg'",
          #   numericInput("lad_topn", "Top N LADs:", min = 5, max = 50, value = 25, step = 1)
          # )
        )
      ),
      tags$hr(),
      downloadButton("download_csv", "Download current table (CSV)")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Differences vs reference",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_diffly", height = 520)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_diff", height = 520)),
          tags$hr(),
          h5("Summary (cumulative at latest cycle, or sum if non-cumulative)"),
          DT::dataTableOutput("table_diff_summary")
        ),
        tabPanel("Average ages", tableOutput("table_avg")),
        tabPanel(
          "ASR",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_asrly", height = 640)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_asr", height = 640))
        ),
        tabPanel(
          "Population",
          conditionalPanel("input.use_plotly", plotlyOutput("plot_poply", height = 520)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_pop", height = 520))
        )
        
      )
    )
  )
)

# ------------------- SERVER --------------------------------------------
server <- function(input, output, session) {
  
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
  
  # ---------- Population ----------
  pop_data <- reactive({
    req(input$pop_cycles, input$scen_sel)
    view <- input$view_level
    if (view == "Overall") {
      dat <- people_overall |> filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen")), y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, y = "pop", y_lab = "Population count")
      }
    } else if (view == "Gender") {
      dat <- people_gender |> filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (isTRUE(input$pop_share)) {
        list(data = pop_share(dat, c("cycle","scen","gender")), facet = "gender", y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, facet = "gender", y = "pop", y_lab = "Population count")
      }
    } else {
      dat <- people_lad |> filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
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
  output$plot_pop   <- renderPlot({ build_pop_plot() })
  output$plot_poply <- renderPlotly({ ggplotly(build_pop_plot(), tooltip = c("x","y","fill")) })
  
  # ---------- Differences vs reference ----------
  diff_long <- reactive({
    req(input$metric_kind, input$view_level, input$diff_min_cycle)
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
                 
    
    pick <- switch(input$metric_kind,
                   deaths   = list(Overall=deaths_overall,  Gender=deaths_gender,  LAD=dl,
                                     #ifelse(length(input$lad_sel), deaths_lad |> filter(ladnm %in% input$lad_sel), deaths_lad),
                                   label="Δ Deaths"),
                   diseases = list(Overall=diseases_overall,Gender=diseases_gender,LAD=dil,label="Δ Diseases"),
                   healthy  = list(Overall=healthy_overall, Gender=healthy_gender, LAD=hl,label="Δ Healthy years"),
                   life     = list(Overall=lifey_overall,   Gender=lifey_gender,   LAD=ll,  label="Δ Life years"))
    base <- pick[[view]]
    if (input$metric_kind == "diseases")
      by <- switch(view, Overall="cause", Gender=c("cause", "gender"), LAD=c("cause", "ladnm"))
    else
      by <- switch(view, Overall=character(0), Gender="gender", LAD="ladnm")
    df <- base |> filter(cycle >= minc, scen %in% scen_keep); grp <- c("scen", by)
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
    
    #browser()
    
    
    ttl  <- d$metric[1]
    if ("gender" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen, linetype = gender)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario", linetype = "Gender") +
        theme_clean()
    } else if ("ladnm" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        facet_wrap(~ ladnm, nrow = 2, scales = "free_y") +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean()
    } else {
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean()
    }
  })
  output$plot_diff   <- renderPlot({ build_diff_plot() })
  output$plot_diffly <- renderPlotly({ ggplotly(build_diff_plot(), tooltip = c("x","y","colour","linetype")) })
  
  # output$table_diff_summary <- renderTable({
  #   d <- diff_long(); req(nrow(d) > 0)
  #   by <- if ("gender" %in% names(d)) c("cause", "gender") else if ("ladnm" %in% names(d)) c("cause", "ladnm") else "cause"
  #   metric_lab <- unique(d$metric)[1]
  #   if (isTRUE(input$diff_cumulative)) {
  #     d |> group_by(across(all_of(c("scen", by)))) |>
  #       slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
  #       ungroup() |>
  #       transmute(
  #         metric = metric_lab, scen,
  #         !!!(if (length(by)) rlang::syms(by) else NULL),
  #         final_cycle = cycle,
  #         cumulative_value = y,
  #         cumulative_value_scaled = y * SCALING
  #       ) |> arrange(scen, across(all_of(by)))
  #   } else {
  #     d |> group_by(across(all_of(c("scen", by)))) |>
  #       summarise(final_cycle = max(cycle, na.rm = TRUE),
  #                 cumulative_value = sum(diff, na.rm = TRUE), .groups = "drop") |>
  #       mutate(metric = metric_lab, cumulative_value_scaled = cumulative_value * SCALING, .before = 1) |>
  #       arrange(scen, across(all_of(by)))
  #   }
  #   
  # })
  
  output$table_diff_summary <- DT::renderDT({
    d <- diff_long()
    req(nrow(d) > 0)
    
    by <- if ("gender" %in% names(d)) {
      if ("cause" %in% names(d)) {
        c("cause", "gender")
      } else {
        "gender"
      }
    }
    else if ("ladnm" %in% names(d)) {
      if ("cause" %in% names(d)) {
        c("cause", "ladnm")
      } else {
        "ladnm"
      }
    } else {
      if ("cause" %in% names(d)) {
        "cause"
      }else {character(0)
      }
    }
    
    metric_lab <- unique(d$metric)[1]
    
    if (isTRUE(input$diff_cumulative)) {
      d |> 
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
        arrange(scen, across(all_of(by))) |>
        DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
    } else {
      d |> 
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
        arrange(scen, across(all_of(by))) |>
        DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
    }
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
  
  output$table_avg <- renderTable({
    view <- input$view_level
    if (input$avg_kind == "death") {
      causes <- input$avg_death_causes; req(causes)
      if (view == "Overall") {
        mean_age_dead_raw_by_scen_val |>
          filter(value %in% causes) |>
          left_join(mean_age_dead_weight_by_scen_val |> filter(value %in% causes),
                    by = c("scen","value")) |>
          arrange(scen, value) |>
          rename(mean_age_raw_years = mean_age_raw)
      } else if (view == "Gender") {
        mean_age_dead_raw_by_scen_val_gender |>
          filter(value %in% causes) |>
          left_join(mean_age_dead_weight_by_scen_val_gender |> filter(value %in% causes),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender, value) |>
          rename(mean_age_raw_years = mean_age_raw)
      } else {
        mean_age_dead_raw_by_scen_val_lad |>
          (\(df) if(length(input$lad_sel) > 0) filter(df, ladnm %in% input$lad_sel) else df)() |>
          filter(value %in% causes) |>
          arrange(scen, ladnm, value) |>
          rename(mean_age_raw_years = mean_age_raw)
      }
    } else {
      cause <- input$avg_cause; req(cause)
      if (view == "Overall") {
        mean_age_onset_raw_by_scen_val |>
          filter(value == cause) |>
          left_join(mean_age_onset_weight_by_scen_val |> filter(value == cause),
                    by = c("scen","value")) |>
          arrange(scen) |>
          select(scen, value,
                 mean_age_raw_years = mean_age_raw)
      } else if (view == "Gender") {
        mean_age_onset_raw_by_scen_val_gender |>
          filter(value == cause) |>
          left_join(mean_age_onset_weight_by_scen_val_gender |> filter(value == cause),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender) |>
          select(scen, gender, value,
                 mean_age_raw_years = mean_age_raw)
      } else {
        mean_age_onset_raw_by_scen_val_lad |>
          (\(df) if(length(input$lad_sel) > 0) filter(df, ladnm %in% input$lad_sel) else df)() |>
          filter(value == cause) |>
          arrange(scen, ladnm) |>
          rename(mean_age_raw_years = mean_age_raw)
      }
    }
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
    req(input$asr_mode, input$asr_causes)
    causes <- input$asr_causes
    if (input$asr_mode == "avg") {
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
          filter(cause %in% causes)
        req(nrow(df) > 0)
        ggplot(df, aes(x = scen, y = age_std_rate, fill = scen)) +
          geom_col(width = 0.8) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1)), hjust = -0.12, size = 3) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
          coord_flip(clip = "off") +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR (avg cycles 1–30)", x = NULL, y = "ASR per 100,000") +
          theme_clean() + guides(fill = "none") +
          theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all_avg_1_30 |> filter(cause %in% causes)
        req(nrow(df) > 0)
        pos <- position_dodge2(width = 0.75, padding = 0.05, preserve = "single")
        ggplot(df, aes(x = scen, y = age_std_rate, fill = gender)) +
          geom_col(position = pos, width = 0.75) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1)), position = pos, hjust = -0.12, size = 3) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
          coord_flip(clip = "off") +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR by gender (avg cycles 1–30)", x = NULL, y = "ASR per 100,000", fill = "Gender") +
          theme_clean() + theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
      } else {
        df <- asr_lad_all_avg_1_30 |> filter(cause %in% causes)
        if (length(input$lad_sel)) df <- df |> filter(ladnm %in% input$lad_sel)
        req(nrow(df) > 0)
        top_ids <- df |>
          filter(cause == causes[1], scen == "reference") |>
          #slice_max(order_by = age_std_rate, n = input$lad_topn, with_ties = FALSE) |>
          distinct(ladnm)
        dplot <- df |> filter(ladnm %in% top_ids$ladnm)
        req(nrow(dplot) > 0)
        pos <- position_dodge2(width = 0.8, padding = 0.08, preserve = "single")
        ggplot(dplot, aes(x = reorder(ladnm, age_std_rate), y = age_std_rate, fill = scen)) +
          geom_col(position = pos, width = 0.8) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1)), position = pos, hjust = -0.10, size = 2.6) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
          coord_flip(clip = "off") +
          labs(title = paste0("ASR by LAD (avg cycles 1–30) — ", causes[1]),
               x = NULL, y = "ASR per 100,000", fill = "Scenario") +
          theme_clean() + theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
      }
    } else {
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_all, asr_healthy_years_overall) |>
          filter(cause %in% causes, cycle >= MIN_CYCLE)
        req(nrow(df) > 0)
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen, group = scen)) +
          geom_smooth(se = FALSE) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle (smoothed, cycles 1–30)", x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean()
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all |> filter(cause %in% causes, cycle >= MIN_CYCLE)
        req(nrow(df) > 0)
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen, linetype = gender)) +
          geom_smooth(se = FALSE) +
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
        # else {
        #   top_ids <- dat |> filter(cause == causes[1], scen == "reference") |>
        #     group_by(ladnm) |> summarise(m = mean(age_std_rate, na.rm = TRUE), .groups = "drop") |>
        #     slice_max(order_by = m, n = min(9, dplyr::n())) |> pull(ladnm)
        #   dat <- dat |> filter(ladnm %in% top_ids)
        # }
        req(nrow(dat) > 0)
        ggplot(dat, aes(x = cycle, y = age_std_rate, colour = scen)) +
          geom_smooth(se = FALSE) +
          facet_grid(ladnm ~ cause, scales = "free_y") +
          labs(title = "ASR per cycle by LAD (smoothed, cycles 1-30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean()
      }
    }
  })
  output$plot_asr   <- renderPlot({ build_asr_plot() })
  output$plot_asrly <- renderPlotly({ ggplotly(build_asr_plot(), tooltip = c("x","y","colour","fill","linetype")) })
  
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
    } else if (tab == "Average ages") {
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
    }
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("export_", gsub("\\s+","_", tolower(input$main_tabs)), "_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(current_table(), file, na = "")
  )
}

shinyApp(ui, server)