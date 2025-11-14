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
})

pc <- qs::qread(here("temp/precomputed_mcr_wgd_100%V2.qs"))
list2env(pc, envir = environment())
SCALING <- 1L

t <- qs::qread(here("temp/061125_trips.qs"))
exp <- qs::qread(here("temp/091125/exp.qs"))


MIN_CYCLE <- 1
MAX_CYCLE <- 30

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
all_causes_asr <- asr_overall_all |> filter(!grepl("dead|sev", cause)) |> distinct(cause) |> pull()

ui <- fluidPage(
  titlePanel("Travel and Health Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
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
          condition = "input.main_tabs == 'Average onset ages'",
          h2("Average onset ages"),
          selectInput("avg_kind", "Average age of:", choices = c("Death"="death","Disease onset"="onset")),
          uiOutput("avg_cause_ui")
        ),
        conditionalPanel(
          condition = "input.main_tabs == 'ASR'",
          h2("ASR"),
          selectInput("asr_mode", "ASR view:",
                      choices = c("Average 1-30 (bars)"="avg","Over time (smoothed)"="trend")),
          selectizeInput("asr_causes", "Causes:", choices = all_causes_asr,
                         selected = c("coronary_heart_disease","stroke"),
                         multiple = TRUE)#,
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
          "Travel Behaviour",
          plotlyOutput("out_mshare", height = "85vh")
        ),
        tabPanel(
          "Exposures",
          gt_output("plot_exp")
        ),
        tabPanel(
          "Differences vs reference",
          plotlyOutput("plot_diffly", height = "50vh"),
          tags$hr(),
          h5("Summary (cumulative at latest cycle, or sum if non-cumulative)"),
          DT::dataTableOutput("table_diff_summary")
        ),
        tabPanel("Average onset ages", tableOutput("table_avg")),
        tabPanel(
          "ASR",
          uiOutput("plot_asrly", height = "85vh")
        ),
        tabPanel(
          "Population",
          plotlyOutput("plot_poply", height = "85vh")
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
  output$plot_diffly <- renderPlotly({ ggplotly(build_diff_plot(), tooltip = c("x","y","colour","linetype")) })
  

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
    req(input$asr_mode, input$asr_causes, input$view_level, input$scen_sel)
    causes <- input$asr_causes
    scens <- input$scen_sel
    if (input$asr_mode == "avg") {
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
          filter(cause %in% causes, scen %in% scens)
        req(nrow(df) > 0)
        
        sdf <- df |>
          group_by(cause, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate) |> 
          rowwise() |> 
          mutate(
            row_min = min(c_across(where(is.numeric)), na.rm = TRUE),
            row_max = max(c_across(where(is.numeric)), na.rm = TRUE)
          )
        
          sdf |> 
          gt() |> 
          opt_interactive(use_filters = T,
                          use_sorting = F,
                          use_compact_mode = T) |> 
          fmt_number(
            columns = where(is.numeric),
            decimals = 2
          )
        # ggplot(df, aes(x = scen, y = age_std_rate, fill = scen)) +
        #   geom_col(width = 0.8) +
        #   geom_text(aes(label = number(age_std_rate, accuracy = 0.1)), hjust = -0.12, size = 3) +
        #   scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
        #   coord_flip(clip = "off") +
        #   facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
        #   labs(title = "ASR (avg cycles 1–30)", x = NULL, y = "ASR per 100,000") +
        #   theme_clean() + guides(fill = "none") +
        #   theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all_avg_1_30 |> 
          filter(cause %in% causes, scen %in% scens)
        req(nrow(df) > 0)
        
        
        df |> 
          mutate(gender = case_when(gender == 1 ~ "Male",
                                                    gender == 2 ~ "Female")) |> 
          group_by(cause, gender, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate) |> 
          gt() |> 
          opt_interactive(use_filters = T,
                          use_sorting = F,
                          use_compact_mode = T) |> 
          fmt_number(
            columns = where(is.numeric),
            decimals = 2
          )
        
        # pos <- position_dodge2(width = 0.75, padding = 0.05, preserve = "single")
        # ggplot(df, aes(x = scen, y = age_std_rate, fill = gender)) +
        #   geom_col(position = pos, width = 0.75) +
        #   geom_text(aes(label = number(age_std_rate, accuracy = 0.1)), position = pos, hjust = -0.12, size = 3) +
        #   scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
        #   coord_flip(clip = "off") +
        #   facet_wrap(vars(cause), scales = "free_x", ncol = 4) +
        #   labs(title = "ASR by gender (avg cycles 1–30)", x = NULL, y = "ASR per 100,000", fill = "Gender") +
        #   theme_clean() + theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
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
          pivot_wider(names_from = scen, values_from = age_std_rate) |> 
          gt() |> 
          opt_interactive(use_filters = T,
                          use_sorting = F,
                          use_compact_mode = T) |> 
          fmt_number(
            columns = where(is.numeric),
            decimals = 2
          )
        
        # pos <- position_dodge2(width = 0.8, padding = 0.08, preserve = "single")
        # ggplot(dplot, aes(x = reorder(ladnm, age_std_rate), y = age_std_rate, fill = scen)) +
        #   geom_col(position = pos, width = 0.8) +
        #   geom_text(aes(label = number(age_std_rate, accuracy = 0.1)), position = pos, hjust = -0.10, size = 2.6) +
        #   scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
        #   coord_flip(clip = "off") +
        #   labs(title = paste0("ASR by LAD (avg cycles 1–30) — ", causes[1]),
        #        x = NULL, y = "ASR per 100,000", fill = "Scenario") +
        #   theme_clean() + theme(plot.margin = margin(5.5, 18, 5.5, 5.5))
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
  
  # output$plot_asrly <- renderPlotly({ ggplotly(build_asr_plot(), tooltip = c("x","y","colour","fill","linetype")) })
  
  output$plot_asrly <- renderUI({
    plot_obj <- build_asr_plot()
    if (inherits(plot_obj, "ggplot")) {
      output$plot_asrly <- renderPlotly({
        ggplotly(plot_obj)#, tooltip = c("x", "y", "colour", "fill", "linetype"))
      })
      plotlyOutput("plot_asrly")
    } else if (inherits(plot_obj, "gt_tbl")) {
      output$asr_gt <- render_gt({
        plot_obj
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
    }
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("export_", gsub("\\s+","_", tolower(input$main_tabs)), "_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(current_table(), file, na = "")
  )
  
  
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
        
        #"Gender","LAD"
        
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
            facet_wrap(vars(LAD_origin)) +
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
  
  # In your server function
  output$plot_exp <- render_gt({
    exp   # exp should be a gt table prepared earlier
  })
  
  

}

shinyApp(ui, server)