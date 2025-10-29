# ======================================================================
# Local Shiny: Population, Differences, Mean Ages (death/onset), ASR
# + Disease picker for Δ diseases
# + Robust gender fix (handles 1/2, M/F strings)
# + NEW: Impact summary tab (deaths prevented, life years, healthy) by scenario
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

options(scipen = 999)

# ------------------- Inputs / Data -------------------------------------
zones <- readr::read_csv(here("app/data/zoneSystem.csv"), show_col_types = FALSE)
stopifnot(all(c("ladcd","ladnm") %in% names(zones)))
lads <- zones |> distinct(ladcd, ladnm)

# Adjust path if needed
pc <- qs::qread(here("Y:/HealthImpact/Data/Country/UK/JIBE_health_output_data/211025/precomputed_mcr_wgd_100%V2.qs"))
list2env(pc, envir = environment())

MIN_CYCLE <- 1
MAX_CYCLE <- 30
SCALING   <- 1L
death_values <- c("dead","dead_car","dead_bike","dead_walk")

# ------------------- Helpers -------------------------------------------
theme_clean_base <- function(base_size = 18) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold"),
      strip.text       = element_text(lineheight = 0.9, face = "bold"),
      axis.title       = element_text(face = "bold"),
      plot.margin      = margin(10, 16, 10, 10)
    )
}
add_zero_line <- function() geom_hline(yintercept = 0, linewidth = 0.4, colour = "#555555")

# Scenario pretty labels
scen_label <- function(x) dplyr::recode(
  x,
  safeStreets = "Safer Streets",
  safeStreet  = "Safer Streets",
  green       = "Greening",
  both        = "Safer Streets & Greening",
  .default = x
)

# ---- Gender normalisation (handles 1/2 numeric and text) ----
norm_gender <- function(x) {
  lx <- tolower(trimws(as.character(x)))
  out <- dplyr::case_when(
    lx %in% c("2","f","female","woman","women") ~ "Female",
    lx %in% c("1","m","male","man","men")       ~ "Male",
    TRUE                                        ~ NA_character_
  )
  factor(out, levels = c("Female","Male"))
}

# unify any gender-like column to a standard "gender" factor and drop NAs
fix_gender_cols <- function(df) {
  gcol <- intersect(names(df), c("gender","sex","sex_group"))
  if (length(gcol) == 0) return(df)
  df |>
    mutate(gender = norm_gender(.data[[gcol[1]]])) |>
    filter(!is.na(gender))
}

# Apply to known gender tables if present
for (nm in c(
  "people_gender",
  "deaths_gender","diseases_gender","healthy_gender","lifey_gender",
  "asr_gender_all","asr_gender_all_avg_1_30",
  "mean_age_dead_raw_by_scen_val_gender",
  "mean_age_dead_weight_by_scen_val_gender",
  "mean_age_onset_raw_by_scen_val_gender",
  "mean_age_onset_weight_by_scen_val_gender"
)) {
  if (exists(nm, inherits = FALSE)) {
    obj <- get(nm, envir = environment())
    if (is.data.frame(obj)) assign(nm, fix_gender_cols(obj), envir = environment())
  }
}

# Gender colours / scales
pal_gender <- c(Female = "#0072B2", Male = "#D55E00")
scale_gender_fill     <- function() scale_fill_manual(values = pal_gender, drop = FALSE)
scale_gender_colour   <- function() scale_colour_manual(values = pal_gender, drop = FALSE)
scale_gender_linetype <- function() scale_linetype_manual(values = c(Female = "solid", Male = "dashed"), drop = FALSE)

# Metric palette (Impact summary)
pal_metric <- c(
  "Premature deaths avoided"                 = "#E69F00",
  "Life years gained"                        = "#56B4E9",
  "Life years without major NCDs"            = "#009E73"
)

# Age helpers for Population shares
ageband <- function(x) cut(
  x, breaks = c(seq(0,100,5), Inf),
  labels = c(paste(seq(0,95,5), seq(4,99,5), sep = "-"), "100+"),
  right = FALSE, include.lowest = TRUE
)
add_agegroups <- function(df) df |> mutate(agegroup_cycle = ageband(age_cycle))

pop_share <- function(df, group_vars = c("cycle","scen")) {
  df |>
    group_by(across(all_of(c(group_vars, "agegroup_cycle")))) |>
    summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop_last") |>
    mutate(share = pop / sum(pop, na.rm = TRUE)) |>
    ungroup()
}

# Label placement for horizontal bars
compute_bar_labels <- function(df, value_col, facet_cols = character(), inside_frac = 0.08) {
  vcol <- rlang::sym(value_col)
  df |>
    group_by(across(all_of(facet_cols))) |>
    mutate(
      .v      = !!vcol,
      .vmax   = max(.v, na.rm = TRUE),
      .inside = .v >= inside_frac * .vmax,
      label_hjust = ifelse(.inside, 0.98, -0.10)
    ) |>
    ungroup()
}

# ------------------- UI -------------------------------------------------
all_scenarios <- sort(unique(people_overall$scen))
all_scenarios_lab <- stats::setNames(all_scenarios, scen_label(all_scenarios))
pop_cycles    <- sort(unique(people_overall$cycle))
trend_cycles  <- sort(unique(asr_overall_all$cycle))
all_lads_nm   <- sort(unique(zones$ladnm))
all_causes_asr <- sort(unique(c(unique(asr_overall_all$cause), "healthy_years")))
all_disease_causes <- sort(unique(as.character(diseases_overall$cause)))

ui <- fluidPage(
  titlePanel("Local Health Explorer — Population, Differences, Mean Ages & ASR"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxInput("use_plotly", "Interactive (Plotly)", value = TRUE),
      selectInput("scen_sel", "Scenarios:", choices = all_scenarios_lab,
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
        # --- Population controls ---
        conditionalPanel(
          condition = "input.main_tabs == 'Population'",
          h2("Population"),
          selectizeInput("pop_cycles", "Cycles to show (bars):",
                         choices = pop_cycles, selected = c(2,10,MAX_CYCLE), multiple = TRUE),
          radioButtons("pop_style", "Bar style:", c("Stacked"="stack","Side-by-side"="dodge"),
                       inline = TRUE),
          checkboxInput("pop_share", "Show shares (else counts)", value = TRUE)
        ),
        # --- Differences controls ---
        conditionalPanel(
          condition = "input.main_tabs == 'Differences vs reference'",
          h2("Differences"),
          selectInput("metric_kind", "Metric:", choices = c(
            "Diseases postponed (Δ diseases)"      = "diseases",
            "Premature deaths avoided"             = "deaths",
            "Life years gained without major NCDs" = "healthy",
            "Life years gained"                    = "life"
          )),
          conditionalPanel(
            condition = "input.metric_kind == 'diseases'",
            selectizeInput(
              "diff_diseases",
              "Select diseases to show:",
              choices  = all_disease_causes,
              selected = all_disease_causes,
              multiple = TRUE,
              options  = list(plugins = list("remove_button"), placeholder = "Choose diseases...")
            )
          ),
          sliderInput("diff_min_cycle", "Start cycle:",
                      min = min(trend_cycles), max = max(trend_cycles),
                      value = MIN_CYCLE, step = 1),
          checkboxInput("diff_cumulative", "Cumulative over cycles", value = TRUE)
        ),
        # --- Average ages controls ---
        conditionalPanel(
          condition = "input.main_tabs == 'Average ages'",
          h2("Average ages"),
          selectInput("avg_kind", "Average age of:", choices = c("Death"="death","Disease onset"="onset")),
          uiOutput("avg_cause_ui")
        ),
        # --- ASR controls ---
        conditionalPanel(
          condition = "input.main_tabs == 'ASR'",
          h2("ASR"),
          selectInput("asr_mode", "ASR view:",
                      choices = c("Average 1-30 (bars)"="avg","Over time (smoothed)"="trend")),
          selectizeInput("asr_causes", "Causes:", choices = all_causes_asr,
                         selected = c("coronary_heart_disease","stroke","healthy_years"),
                         multiple = TRUE)
        ),
        # --- Impact summary controls (NEW) ---
        conditionalPanel(
          condition = "input.main_tabs == 'Impact summary'",
          h2("Impact summary"),
          sliderInput("impact_min_cycle", "Start cycle:",
                      min = min(trend_cycles), max = max(trend_cycles),
                      value = MIN_CYCLE, step = 1),
          checkboxInput("impact_cumulative", "Cumulative over cycles", value = TRUE),
          helpText("Shows Premature deaths avoided, Life years gained, and Life years without major NCDs by scenario.")
        )
      ),
      tags$hr(),
      downloadButton("download_csv", "Download current table (CSV)")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        # ----------------- Differences -----------------
        tabPanel(
          "Differences vs reference",
          fluidRow(
            column(6, strong("Download summary bars: "),
                   downloadButton("dl_diffbar_png", "PNG"),
                   downloadButton("dl_diffbar_svg", "SVG")),
            column(6, strong("Download trend lines: "),
                   downloadButton("dl_diff_png", "PNG"),
                   downloadButton("dl_diff_svg", "SVG"))
          ),
          br(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_diffbarly", height = 300)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_diffbar", height = 300)),
          tags$hr(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_diffly", height = 560)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_diff", height = 560)),
          tags$hr(),
          h5("Summary (cumulative at latest cycle, or sum if non-cumulative)"),
          DT::dataTableOutput("table_diff_summary")
        ),
        
        # ----------------- Average ages -----------------
        tabPanel("Average ages", tableOutput("table_avg")),
        
        # ----------------- ASR -----------------
        tabPanel(
          "ASR",
          fluidRow(
            column(12, strong("Download ASR chart: "),
                   downloadButton("dl_asr_png", "PNG"),
                   downloadButton("dl_asr_svg", "SVG"))
          ),
          br(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_asrly", height = 680)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_asr", height = 680))
        ),
        
        # ----------------- Population -----------------
        tabPanel(
          "Population",
          fluidRow(
            column(12, strong("Download population chart: "),
                   downloadButton("dl_pop_png", "PNG"),
                   downloadButton("dl_pop_svg", "SVG"))
          ),
          br(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_poply", height = 560)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_pop", height = 560))
        ),
        
        # ----------------- Impact summary (NEW) -----------------
        tabPanel(
          "Impact summary",
          fluidRow(
            column(12, strong("Download impact chart: "),
                   downloadButton("dl_impact_png", "PNG"),
                   downloadButton("dl_impact_svg", "SVG"))
          ),
          br(),
          conditionalPanel("input.use_plotly", plotlyOutput("plot_impactly", height = 560)),
          conditionalPanel("!input.use_plotly", plotOutput("plot_impact", height = 560)),
          tags$hr(),
          DT::dataTableOutput("table_impact")
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
      dat <- people_overall |>
        filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles) |>
        mutate(scen_lab = scen_label(scen))
      if (isTRUE(input$pop_share)) {
        tmp <- pop_share(dat, c("cycle","scen")) |> mutate(scen_lab = scen_label(scen))
        list(data = tmp, y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, y = "pop", y_lab = "Population count")
      }
      
    } else if (view == "Gender") {
      dat <- people_gender |>
        filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles) |>
        mutate(scen_lab = scen_label(scen))
      if (!"agegroup_cycle" %in% names(dat) && "age_cycle" %in% names(dat)) dat <- add_agegroups(dat)
      if (isTRUE(input$pop_share)) {
        tmp <- pop_share(dat, c("cycle","scen","gender")) |> mutate(scen_lab = scen_label(scen))
        list(data = tmp, facet = "gender", y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, facet = "gender", y = "pop", y_lab = "Population count")
      }
      
    } else { # LAD
      dat <- people_lad |>
        filter(scen %in% input$scen_sel, cycle %in% input$pop_cycles)
      if (length(input$lad_sel)) dat <- dplyr::filter(dat, ladnm %in% input$lad_sel)
      dat <- mutate(dat, scen_lab = scen_label(scen))
      if (isTRUE(input$pop_share)) {
        tmp <- pop_share(dat, c("cycle","scen","ladnm")) |> mutate(scen_lab = scen_label(scen))
        list(data = tmp, facet = "ladnm", y = "share", y_lab = "Share of pop.")
      } else {
        list(data = dat, facet = "ladnm", y = "pop", y_lab = "Population count")
      }
    }
  })
  
  build_pop_plot <- reactive({
    pd <- pop_data(); d <- pd$data; req(nrow(d) > 0)
    pos <- if (input$pop_style == "dodge") position_dodge(width = 0.8) else "stack"
    base <- ggplot(d, aes(x = agegroup_cycle, y = .data[[pd$y]], fill = scen_lab)) +
      geom_col(position = pos) +
      scale_y_continuous(labels = if (pd$y == "share") percent else label_number(big.mark=",")) +
      labs(title = "Population by age group", x = "Age group", y = pd$y_lab, fill = "Scenario") +
      theme_clean_base()
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
    
    dl  <- deaths_lad
    dil <- diseases_lad
    hl  <- healthy_lad
    ll  <- lifey_lad
    if (length(input$lad_sel)){
      dl  <- dplyr::filter(dl,  ladnm %in% input$lad_sel)
      dil <- dplyr::filter(dil, ladnm %in% input$lad_sel)
      hl  <- dplyr::filter(hl,  ladnm %in% input$lad_sel)
      ll  <- dplyr::filter(ll,  ladnm %in% input$lad_sel)
    }
    
    pick <- switch(input$metric_kind,
                   deaths   = list(Overall=deaths_overall,  Gender=deaths_gender,  LAD=dl,  label="Premature deaths avoided"),
                   diseases = list(Overall=diseases_overall,Gender=diseases_gender,LAD=dil, label="Δ Diseases"),
                   healthy  = list(Overall=healthy_overall, Gender=healthy_gender, LAD=hl,  label="Life years without major NCDs"),
                   life     = list(Overall=lifey_overall,   Gender=lifey_gender,   LAD=ll,  label="Life years gained")
    )
    base <- pick[[view]]
    
    # Choose grouping strategy
    if (input$metric_kind == "diseases") {
      by <- switch(view, Overall="cause", Gender=c("cause","gender"), LAD=c("cause","ladnm"))
    } else {
      by <- switch(view, Overall=character(0), Gender="gender", LAD="ladnm")
    }
    
    df <- base |>
      filter(cycle >= minc, scen %in% scen_keep)
    
    # Apply disease filter only when metric is diseases
    if (input$metric_kind == "diseases" && length(input$diff_diseases)) {
      df <- df |> filter(as.character(cause) %in% input$diff_diseases)
    }
    
    df |>
      group_by(across(all_of(c("scen", by, "cycle")))) |>
      summarise(diff = sum(diff, na.rm = TRUE), .groups = "drop") |>
      group_by(across(all_of(c("scen", by)))) |>
      mutate(y = if (cumu) cumsum(diff) else diff) |>
      ungroup() |>
      mutate(metric = pick$label, scen_lab = scen_label(scen))
  })
  
  # ---- Summary for bars ----
  diff_summary <- reactive({
    d <- diff_long(); req(nrow(d) > 0)
    by <- c()
    if ("cause"  %in% names(d)) by <- c(by, "cause")
    if ("gender" %in% names(d)) by <- c(by, "gender")
    if ("ladnm"  %in% names(d)) by <- c(by, "ladnm")
    
    metric_lab <- unique(d$metric)[1]
    
    out <- if (isTRUE(input$diff_cumulative)) {
      d |>
        group_by(across(all_of(c("scen", by)))) |>
        slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
        ungroup() |>
        transmute(
          metric = metric_lab, scen,
          !!!(if (length(by)) rlang::syms(by) else NULL),
          final_cycle = cycle,
          cumulative_value = y
        )
    } else {
      d |>
        group_by(across(all_of(c("scen", by)))) |>
        summarise(
          final_cycle = max(cycle, na.rm = TRUE),
          cumulative_value = sum(diff, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(metric = metric_lab, .before = 1)
    }
    
    mutate(out, scen_lab = scen_label(scen))
  })
  
  # ---- Summary bars (gender gets its own colours) ----
  build_diffbar_plot <- reactive({
    ds <- diff_summary(); req(nrow(ds) > 0)
    
    has_cause  <- "cause"  %in% names(ds)
    has_gender <- "gender" %in% names(ds)
    has_lad    <- "ladnm"  %in% names(ds)
    
    facet_cols <- character(0)
    if (has_cause)  facet_cols <- c(facet_cols, "cause")
    if (has_lad)    facet_cols <- c(facet_cols, "ladnm")
    if (has_gender) facet_cols <- c(facet_cols, "gender")
    
    ds <- compute_bar_labels(ds, value_col = "cumulative_value", facet_cols = facet_cols)
    
    lev <- ds |>
      group_by(scen_lab) |>
      summarise(total = sum(cumulative_value, na.rm = TRUE), .groups = "drop") |>
      arrange(total) |>
      pull(scen_lab)
    ds$scen_lab <- factor(ds$scen_lab, levels = lev)
    
    if (has_gender) {
      pos <- position_dodge2(width = 0.75, padding = 0.05, preserve = "single")
      p <- ggplot(ds, aes(x = scen_lab, y = cumulative_value, fill = gender)) +
        geom_col(position = pos, width = 0.75) +
        geom_text(
          aes(label = number(cumulative_value, accuracy = 1), hjust = label_hjust),
          position = pos, vjust = 0.5, colour = "black", fontface = "bold", size = 4.2,
          show.legend = FALSE
        ) +
        scale_gender_fill()
    } else {
      p <- ggplot(ds, aes(x = scen_lab, y = cumulative_value, fill = scen_lab)) +
        geom_col(width = 0.75) +
        geom_text(
          aes(label = number(cumulative_value, accuracy = 1), hjust = label_hjust),
          vjust = 0.5, colour = "black", fontface = "bold", size = 4.2, show.legend = FALSE
        ) +
        guides(fill = "none")
    }
    
    if (has_lad && has_cause) {
      p <- p + facet_grid(ladnm ~ cause, scales = "free_y")
    } else if (has_lad) {
      p <- p + facet_wrap(~ ladnm, nrow = 2, scales = "free_y")
    } else if (has_cause) {
      p <- p + facet_wrap(~ cause, ncol = 4, scales = "free_y")
    }
    
    ylab <- if (isTRUE(input$diff_cumulative)) "Cumulative difference" else "Sum Δ vs reference"
    
    p +
      coord_flip(clip = "off") +
      labs(title = unique(ds$metric)[1], x = NULL, y = ylab) +
      scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                         expand = expansion(mult = c(0.05, 0.05))) +
      theme_clean_base()
  })
  output$plot_diffbar   <- renderPlot({ build_diffbar_plot() })
  output$plot_diffbarly <- renderPlotly({ ggplotly(build_diffbar_plot(), tooltip = c("x","y","fill")) })
  
  # ---- Trends (lines) ----
  build_diff_plot <- reactive({
    d <- diff_long(); req(nrow(d) > 0)
    ylab <- if (isTRUE(input$diff_cumulative)) "Cumulative Δ vs reference" else "Δ vs reference"
    ttl  <- d$metric[1]
    
    if ("gender" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen_lab, linetype = gender)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        scale_gender_linetype() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario", linetype = "Gender") +
        theme_clean_base()
    } else if ("ladnm" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen_lab)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        facet_wrap(~ ladnm, nrow = 2, scales = "free_y") +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean_base()
    } else {
      ggplot(d, aes(x = cycle, y = y, colour = scen_lab)) +
        geom_smooth(se = FALSE) + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = ylab, colour = "Scenario") +
        theme_clean_base()
    }
  })
  output$plot_diff   <- renderPlot({ build_diff_plot() })
  output$plot_diffly <- renderPlotly({ ggplotly(build_diff_plot(), tooltip = c("x","y","colour","linetype")) })
  
  # ---------- Diff summary table ----------
  output$table_diff_summary <- DT::renderDT({
    d <- diff_long(); req(nrow(d) > 0)
    
    by <- if ("gender" %in% names(d)) {
      if ("cause" %in% names(d)) c("cause","gender") else "gender"
    } else if ("ladnm" %in% names(d)) {
      if ("cause" %in% names(d)) c("cause","ladnm") else "ladnm"
    } else {
      if ("cause" %in% names(d)) "cause" else character(0)
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
        mutate(scen = scen_label(scen)) |>
        arrange(scen, across(all_of(by))) |>
        DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
    } else {
      d |>
        group_by(across(all_of(c("scen", by)))) |>
        summarise(final_cycle = max(cycle, na.rm = TRUE),
                  cumulative_value = sum(diff, na.rm = TRUE),
                  .groups = "drop") |>
        mutate(metric = metric_lab, cumulative_value_scaled = cumulative_value * SCALING, .before = 1) |>
        mutate(scen = scen_label(scen)) |>
        arrange(scen, across(all_of(by))) |>
        DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
    }
  })
  
  # ---------- Average ages (death / onset) ----------
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
          mutate(scen = scen_label(scen)) |>
          rename(mean_age_raw_years = mean_age_raw)
      } else if (view == "Gender") {
        mean_age_dead_raw_by_scen_val_gender |>
          filter(value %in% causes) |>
          left_join(mean_age_dead_weight_by_scen_val_gender |> filter(value %in% causes),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender, value) |>
          mutate(scen = scen_label(scen)) |>
          rename(mean_age_raw_years = mean_age_raw)
      } else {
        tmp <- mean_age_dead_raw_by_scen_val_lad
        if (length(input$lad_sel) > 0) tmp <- filter(tmp, ladnm %in% input$lad_sel)
        tmp |>
          filter(value %in% causes) |>
          arrange(scen, ladnm, value) |>
          mutate(scen = scen_label(scen)) |>
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
          mutate(scen = scen_label(scen)) |>
          select(scen, value, mean_age_raw_years = mean_age_raw)
      } else if (view == "Gender") {
        mean_age_onset_raw_by_scen_val_gender |>
          filter(value == cause) |>
          left_join(mean_age_onset_weight_by_scen_val_gender |> filter(value == cause),
                    by = c("scen","value","gender")) |>
          arrange(scen, gender) |>
          mutate(scen = scen_label(scen)) |>
          select(scen, gender, value, mean_age_raw_years = mean_age_raw)
      } else {
        tmp <- mean_age_onset_raw_by_scen_val_lad
        if (length(input$lad_sel) > 0) tmp <- filter(tmp, ladnm %in% input$lad_sel)
        tmp |>
          filter(value == cause) |>
          arrange(scen, ladnm) |>
          mutate(scen = scen_label(scen)) |>
          rename(mean_age_raw_years = mean_age_raw)
      }
    }
  })
  
  # ---------- ASR (respect scenario selection) ----------
  to_chr_cause <- function(df) if ("cause" %in% names(df)) dplyr::mutate(df, cause = as.character(cause)) else df
  asr_overall_all                   <<- to_chr_cause(asr_overall_all)
  asr_overall_avg_1_30              <<- to_chr_cause(asr_overall_avg_1_30)
  asr_gender_all                    <<- to_chr_cause(asr_gender_all)
  asr_gender_all_avg_1_30           <<- to_chr_cause(asr_gender_all_avg_1_30)
  asr_lad_all_per_cycle             <<- to_chr_cause(asr_lad_all_per_cycle)
  asr_lad_all_avg_1_30              <<- to_chr_cause(asr_lad_all_avg_1_30)
  asr_healthy_years_overall         <<- to_chr_cause(asr_healthy_years_overall)
  asr_healthy_years_overall_avg_1_30<<- to_chr_cause(asr_healthy_years_overall_avg_1_30)
  
  build_asr_plot <- reactive({
    req(input$asr_mode, input$asr_causes, input$scen_sel)
    causes    <- input$asr_causes
    scen_keep <- input$scen_sel
    
    if (input$asr_mode == "avg") {
      
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
          filter(cause %in% causes, scen %in% scen_keep) |>
          mutate(scen_lab = scen_label(scen)) |>
          compute_bar_labels(value_col = "age_std_rate", facet_cols = "cause")
        lev <- df |>
          group_by(scen_lab) |>
          summarise(t = sum(age_std_rate, na.rm = TRUE), .groups = "drop") |>
          arrange(t) |>
          pull(scen_lab)
        df$scen_lab <- factor(df$scen_lab, levels = lev)
        
        ggplot(df, aes(x = scen_lab, y = age_std_rate, fill = scen_lab)) +
          geom_col(width = 0.8) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1), hjust = label_hjust),
                    vjust = 0.5, colour = "black", fontface = "bold", size = 4.2, show.legend = FALSE) +
          guides(fill = "none") +
          coord_flip(clip = "off") +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR (avg cycles 1–30)", x = NULL, y = "ASR per 100,000") +
          scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                             expand = expansion(mult = c(0.05, 0.05))) +
          theme_clean_base()
        
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all_avg_1_30 |>
          filter(cause %in% causes, scen %in% scen_keep) |>
          mutate(scen_lab = scen_label(scen)) |>
          compute_bar_labels(value_col = "age_std_rate", facet_cols = c("cause","gender"))
        lev <- df |>
          group_by(scen_lab) |>
          summarise(t = sum(age_std_rate, na.rm = TRUE), .groups = "drop") |>
          arrange(t) |>
          pull(scen_lab)
        df$scen_lab <- factor(df$scen_lab, levels = lev)
        
        pos <- position_dodge2(width = 0.75, padding = 0.05, preserve = "single")
        ggplot(df, aes(x = scen_lab, y = age_std_rate, fill = gender)) +
          geom_col(position = pos, width = 0.75) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1), hjust = label_hjust),
                    position = pos, vjust = 0.5, colour = "black", fontface = "bold", size = 4.0, show.legend = FALSE) +
          scale_gender_fill() +
          coord_flip(clip = "off") +
          facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
          labs(title = "ASR by gender (avg cycles 1–30)", x = NULL, y = "ASR per 100,000") +
          scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                             expand = expansion(mult = c(0.05, 0.05))) +
          theme_clean_base()
        
      } else { # LAD
        df <- asr_lad_all_avg_1_30 |>
          filter(cause %in% causes, scen %in% scen_keep)
        if (length(input$lad_sel)) df <- dplyr::filter(df, ladnm %in% input$lad_sel)
        df <- df |>
          mutate(scen_lab = scen_label(scen)) |>
          compute_bar_labels(value_col = "age_std_rate", facet_cols = "ladnm")
        
        pos <- position_dodge2(width = 0.8, padding = 0.08, preserve = "single")
        ggplot(df, aes(x = reorder(ladnm, age_std_rate), y = age_std_rate, fill = scen_lab)) +
          geom_col(position = pos, width = 0.8) +
          geom_text(aes(label = number(age_std_rate, accuracy = 0.1), hjust = label_hjust),
                    position = pos, vjust = 0.5, colour = "black", fontface = "bold", size = 3.6, show.legend = FALSE) +
          coord_flip(clip = "off") +
          labs(title = paste0("ASR by LAD (avg cycles 1–30) — ", causes[1]),
               x = NULL, y = "ASR per 100,000", fill = "Scenario") +
          scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ","),
                             expand = expansion(mult = c(0.05, 0.05))) +
          theme_clean_base()
      }
      
    } else { # trend
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_all, asr_healthy_years_overall) |>
          filter(cause %in% causes, cycle >= MIN_CYCLE, scen %in% scen_keep) |>
          mutate(scen_lab = scen_label(scen))
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen_lab, group = scen_lab)) +
          geom_smooth(se = FALSE) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle (smoothed, cycles 1–30)", x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean_base()
        
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all |>
          filter(cause %in% causes, cycle >= MIN_CYCLE, scen %in% scen_keep) |>
          mutate(scen_lab = scen_label(scen))
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = scen_lab, linetype = gender)) +
          geom_smooth(se = FALSE) +
          scale_gender_linetype() +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = "ASR per cycle by gender (smoothed, cycles 1–30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario", linetype = "Gender") +
          theme_clean_base()
        
      } else {
        dat <- asr_lad_all_per_cycle |>
          filter(cause %in% causes, cycle >= MIN_CYCLE, scen %in% scen_keep)
        if (length(input$lad_sel)) dat <- dplyr::filter(dat, ladnm %in% input$lad_sel)
        dat <- mutate(dat, scen_lab = scen_label(scen))
        ggplot(dat, aes(x = cycle, y = age_std_rate, colour = scen_lab)) +
          geom_smooth(se = FALSE) +
          facet_grid(ladnm ~ cause, scales = "free_y") +
          labs(title = "ASR per cycle by LAD (smoothed, cycles 1–30)",
               x = "Cycle (year)", y = "ASR per 100,000", colour = "Scenario") +
          theme_clean_base()
      }
    }
  })
  output$plot_asr   <- renderPlot({ build_asr_plot() })
  output$plot_asrly <- renderPlotly({ ggplotly(build_asr_plot(), tooltip = c("x","y","colour","fill","linetype")) })
  
  # ---------- IMPACT SUMMARY (NEW) ----------
  # Combine deaths / life years / healthy into one bar chart
  impact_summary <- reactive({
    req(input$impact_min_cycle, input$scen_sel)
    scen_keep <- setdiff(input$scen_sel, "reference")
    validate(need(length(scen_keep) > 0, "Select at least one non-reference scenario."))
    minc <- input$impact_min_cycle; view <- input$view_level; cumu <- isTRUE(input$impact_cumulative)
    
    # Choose base tables by view
    tbls <- switch(view,
                   "Overall" = list(
                     deaths = deaths_overall,
                     lifey  = lifey_overall,
                     healthy= healthy_overall
                   ),
                   "Gender" = list(
                     deaths = deaths_gender,
                     lifey  = lifey_gender,
                     healthy= healthy_gender
                   ),
                   "LAD" = list(
                     deaths = deaths_lad,
                     lifey  = lifey_lad,
                     healthy= healthy_lad
                   )
    )
    
    # Optional LAD filter
    if (view == "LAD" && length(input$lad_sel)) {
      tbls <- lapply(tbls, \(d) d |> filter(ladnm %in% input$lad_sel))
    }
    
    # Stack three metrics with their labels
    lab_map <- c(
      deaths = "Premature deaths avoided",
      lifey  = "Life years gained",
      healthy= "Life years without major NCDs"
    )
    
    long <- bind_rows(
      tbls$deaths |> mutate(metric = lab_map["deaths"]),
      tbls$lifey  |> mutate(metric = lab_map["lifey"]),
      tbls$healthy|> mutate(metric = lab_map["healthy"])
    ) |>
      filter(cycle >= minc, scen %in% scen_keep)
    
    # Grouping columns by view
    by <- switch(view,
                 "Overall" = c(),
                 "Gender"  = c("gender"),
                 "LAD"     = c("ladnm")
    )
    
    long |>
      group_by(scen, across(all_of(by)), metric, cycle) |>
      summarise(value = sum(diff, na.rm = TRUE), .groups = "drop") |>
      group_by(scen, across(all_of(by)), metric) |>
      summarise(
        y = if (cumu) cumsum(value) else value,
        cycle = cycle,
        .groups = "keep"
      ) |>
      slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
      ungroup() |>
      mutate(scen_lab = scen_label(scen))
  })
  
  build_impact_plot <- reactive({
    df <- impact_summary(); req(nrow(df) > 0)
    view <- input$view_level
    
    # Order scenarios by total across metrics
    lev <- df |>
      group_by(scen_lab) |>
      summarise(t = sum(y, na.rm = TRUE), .groups = "drop") |>
      arrange(t) |>
      pull(scen_lab)
    df$scen_lab <- factor(df$scen_lab, levels = lev)
    
    base <- ggplot(df, aes(x = scen_lab, y = y, fill = metric)) +
      geom_col(position = position_dodge2(width = 0.78, padding = 0.05, preserve = "single"), width = 0.78) +
      scale_fill_manual(values = pal_metric) +
      coord_flip() +
      scale_y_continuous(labels = label_number(big.mark = ",")) +
      labs(
        title = "Impact summary by scenario",
        x = NULL,
        y = if (isTRUE(input$impact_cumulative)) "Cumulative effect since start cycle" else "Effect in selected range",
        fill = NULL
      ) +
      theme_clean_base()
    
    if (view == "Gender") {
      base + facet_wrap(~ gender, nrow = 1, drop = TRUE)
    } else if (view == "LAD") {
      base + facet_wrap(~ ladnm, nrow = 2, scales = "free_y")
    } else base
  })
  
  output$plot_impact   <- renderPlot({ build_impact_plot() })
  output$plot_impactly <- renderPlotly({ ggplotly(build_impact_plot(), tooltip = c("x","y","fill")) })
  
  output$table_impact <- DT::renderDT({
    df <- impact_summary(); req(nrow(df) > 0)
    df |>
      transmute(
        Scenario = scen_lab,
        Group    = dplyr::coalesce(as.character(get0("gender", ifnotfound = NULL)), as.character(get0("ladnm", ifnotfound = NULL))),
        Metric   = metric,
        Value    = round(y, 2),
        Final_Cycle = cycle
      ) |>
      DT::datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  # ---------- CSV download ----------
  current_table <- reactive({
    tab <- input$main_tabs
    if (tab == "Population") {
      pd <- pop_data()
      pd$data |> mutate(across(where(is.numeric), ~ round(., 6)))
    } else if (tab == "Differences vs reference") {
      d <- diff_long()
      if (isTRUE(input$diff_cumulative)) {
        by <- if ("gender" %in% names(d)) "gender" else if ("ladnm" %in% names(d)) "ladnm" else character(0)
        d |>
          group_by(across(all_of(c("scen", by)))) |>
          slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
          ungroup() |>
          transmute(scen = scen_label(scen), across(all_of(by)),
                    final_cycle = cycle,
                    cumulative_value = y,
                    cumulative_value_scaled = y * SCALING)
      } else {
        d |> mutate(scen = scen_label(scen))
      }
    } else if (tab == "Average ages") {
      out <- output$table_avg |> req(); isolate(out()) |> mutate(scen = scen_label(scen))
    } else if (tab == "ASR") {
      if (input$asr_mode == "avg") {
        if (input$view_level == "Overall") {
          bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
            filter(cause %in% input$asr_causes, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        } else if (input$view_level == "Gender") {
          asr_gender_all_avg_1_30 |>
            filter(cause %in% input$asr_causes, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        } else {
          asr_lad_all_avg_1_30 |>
            filter(cause %in% input$asr_causes, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        }
      } else {
        if (input$view_level == "Overall") {
          bind_rows(asr_overall_all, asr_healthy_years_overall) |>
            filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        } else if (input$view_level == "Gender") {
          asr_gender_all |>
            filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        } else {
          asr_lad_all_per_cycle |>
            filter(cause %in% input$asr_causes, cycle >= MIN_CYCLE, scen %in% input$scen_sel) |>
            mutate(scen = scen_label(scen))
        }
      }
    } else if (tab == "Impact summary") {
      impact_summary() |>
        mutate(scen = scen_lab) |>
        select(scen, gender = any_of("gender"), ladnm = any_of("ladnm"),
               metric, value = y, final_cycle = cycle)
    }
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("export_", gsub("\\s+","_", tolower(input$main_tabs)), "_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(current_table(), file, na = "")
  )
  
  # ---------- Downloads (PNG/SVG) ----------
  save_png <- function(p, file, width_in = 11, height_in = 6.5, dpi = 300) {
    ggplot2::ggsave(file, p, width = width_in, height = height_in, dpi = dpi, units = "in", device = "png")
  }
  save_svg <- function(p, file, width_in = 11, height_in = 6.5) {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      stop("To export SVG, please install the 'svglite' package: install.packages('svglite')")
    }
    svglite::svglite(file, width = width_in, height = height_in)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(p)
  }
  
  output$dl_diffbar_png <- downloadHandler(
    filename = function() paste0("differences_summary_", Sys.Date(), ".png"),
    content = function(file) { save_png(build_diffbar_plot(), file) }
  )
  output$dl_diffbar_svg <- downloadHandler(
    filename = function() paste0("differences_summary_", Sys.Date(), ".svg"),
    content = function(file) { save_svg(build_diffbar_plot(), file) }
  )
  output$dl_diff_png <- downloadHandler(
    filename = function() paste0("differences_trends_", Sys.Date(), ".png"),
    content = function(file) { save_png(build_diff_plot(), file) }
  )
  output$dl_diff_svg <- downloadHandler(
    filename = function() paste0("differences_trends_", Sys.Date(), ".svg"),
    content = function(file) { save_svg(build_diff_plot(), file) }
  )
  output$dl_asr_png <- downloadHandler(
    filename = function() paste0("asr_", Sys.Date(), ".png"),
    content = function(file) { save_png(build_asr_plot(), file) }
  )
  output$dl_asr_svg <- downloadHandler(
    filename = function() paste0("asr_", Sys.Date(), ".svg"),
    content = function(file) { save_svg(build_asr_plot(), file) }
  )
  output$dl_pop_png <- downloadHandler(
    filename = function() paste0("population_", Sys.Date(), ".png"),
    content = function(file) { save_png(build_pop_plot(), file) }
  )
  output$dl_pop_svg <- downloadHandler(
    filename = function() paste0("population_", Sys.Date(), ".svg"),
    content = function(file) { save_svg(build_pop_plot(), file) }
  )
  output$dl_impact_png <- downloadHandler(
    filename = function() paste0("impact_summary_", Sys.Date(), ".png"),
    content = function(file) { save_png(build_impact_plot(), file) }
  )
  output$dl_impact_svg <- downloadHandler(
    filename = function() paste0("impact_summary_", Sys.Date(), ".svg"),
    content = function(file) { save_svg(build_impact_plot(), file) }
  )
}

shinyApp(ui, server)
