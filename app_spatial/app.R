# =============================================================================
# Shiny app — Exposure + demographic profile per LSOA, Greater Manchester
#
# Map unit: LSOA (Lower layer Super Output Area).
# Controls:  Metric (7), Statistic (percentile 5/25/50/75 or mean for HA/HSD),
#            Compare scenarios (colour = base, hover = all four).
# Hover popup also shows the LSOA's population profile:
#   mean/min/max age, gender split (1=male, 2=female), IMD decile.
#
# Requires (run first, once):
#   01_preprocess_medians.R -> data/exposure_medians_by_lsoa.rds
#   01b_demographics.R      -> data/lsoa_profile.rds
#   02_get_boundaries.R     -> data/gm_lsoa_boundaries.rds
# =============================================================================

library(shiny)
library(sf)
library(dplyr)
library(tidyr)
library(leaflet)
library(ggplot2)
library(viridisLite)

# ---- locate data ------------------------------------------------------------
app_dir <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e) getwd()
)

data_dir <- "app_spatial"


med  <- readRDS("exposure_medians_by_lsoa.rds")   # list(wide, long)
prof <- readRDS("lsoa_profile.rds")               # demographics
lsoa <- readRDS("gm_lsoa_boundaries.rds")        # sf, EPSG:27700
lsoa_wgs <- st_transform(lsoa, 4326)

long <- med$long

# ---- metric definitions -----------------------------------------------------
metric_labels <- c(
  exposure_normalised_pm25       = "PM2.5 (normalised)",
  exposure_normalised_no2        = "NO2 (normalised)",
  exposure_normalised_noise_Lden = "Noise Lden (normalised)",
  exposure_normalised_ndvi       = "Greenness NDVI (normalised)",
  mmets                          = "mMETs (sum walk+cycle+sport)",
  exposure_noise_HA              = "Noise - Highly Annoyed",
  exposure_noise_HSD             = "Noise - Sleep Disturbed"
)
mean_only_metrics <- c("exposure_noise_HA", "exposure_noise_HSD")
stat_labels <- c(p05="5th percentile", p25="25th percentile",
                 p50="50th (median)", p75="75th percentile", mean="mean")

metrics   <- names(metric_labels)
scenarios <- sort(unique(long$scenario))
base_name <- if ("base" %in% scenarios) "base" else scenarios[1]
other_scn <- setdiff(scenarios, base_name)
resolve_stat <- function(metric, pctile)
  if (metric %in% mean_only_metrics) "mean" else pctile

# pre-format the demographic profile strings once (same across scenarios)
prof_fmt <- prof |>
  mutate(
    profile_html = sprintf(
      paste0("<hr style='margin:3px 0'/><b>Profile</b><br/>",
             "Population: %s<br/>",
             "Age: mean %.0f (min %d, max %d)<br/>",
             "Gender: %.0f%% male / %.0f%% female<br/>",
             "IMD decile: %s"),
      formatC(n_people, big.mark = ",", format = "d"),
      age_mean, as.integer(age_min), as.integer(age_max),
      pct_male, pct_female, imd10)
  ) |>
  select(lsoa21cd, profile_html, imd10)

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Exposure + population profile per LSOA - Greater Manchester"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("metric", "Metric",
                  choices = setNames(metrics, metric_labels[metrics]),
                  selected = "exposure_normalised_pm25"),
      selectInput("pctile", "Percentile",
                  choices = setNames(c("p05","p25","p50","p75"),
                                     stat_labels[c("p05","p25","p50","p75")]),
                  selected = "p50"),
      uiOutput("stat_note"),
      hr(),
      checkboxInput("compare", "Compare scenarios (colour = base, hover = all)",
                    value = TRUE),
      conditionalPanel("!input.compare",
                       selectInput("scenario", "Scenario", choices = scenarios,
                                   selected = base_name)),
      radioButtons("classify", "Colour scale",
                   c("Quantile (7)" = "quantile", "Continuous" = "continuous"),
                   selected = "quantile"),
      hr(),
      downloadButton("dl_png", "Save faceted PNG"),
      helpText("Stats computed across all individuals in each LSOA.")
    ),
    mainPanel(width = 9, leafletOutput("map", height = 720))
  )
)

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$metric, {
    if (input$metric %in% mean_only_metrics) {
      updateSelectInput(session, "pctile", choices = c("mean only" = "p50"))
    } else {
      updateSelectInput(session, "pctile",
                        choices = setNames(c("p05","p25","p50","p75"),
                                           stat_labels[c("p05","p25","p50","p75")]),
                        selected = "p50")
    }
  })
  output$stat_note <- renderUI(
    if (input$metric %in% mean_only_metrics)
      tags$em("This metric is summarised as the mean only."))
  
  stat_used <- reactive(resolve_stat(input$metric, input$pctile))
  
  map_sf <- reactive({
    st <- stat_used()
    colour_scn <- if (input$compare) base_name else input$scenario
    colour_vals <- long |>
      filter(metric == input$metric, stat == st, scenario == colour_scn) |>
      select(lsoa21cd, colour_value = value, n_people)
    dat <- lsoa_wgs |>
      left_join(colour_vals, by = "lsoa21cd") |>
      left_join(prof_fmt,     by = "lsoa21cd")
    if (input$compare) {
      wide_scn <- long |>
        filter(metric == input$metric, stat == st) |>
        select(lsoa21cd, scenario, value) |>
        pivot_wider(names_from = scenario, values_from = value)
      dat <- dat |> left_join(wide_scn, by = "lsoa21cd")
    }
    dat
  })
  
  output$map <- renderLeaflet(
    leaflet() |> addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -2.24, lat = 53.48, zoom = 10))
  
  observe({
    dat <- map_sf(); v <- dat$colour_value; st <- stat_used()
    if (all(is.na(v))) {
      leafletProxy("map", data = dat) |> clearShapes() |> clearControls() |>
        addPolygons(fillColor = "#dddddd", fillOpacity = 0.5,
                    color = "white", weight = 0.4, label = ~lsoa21cd)
      return(invisible())
    }
    
    make_quantile_pal <- function(values, n = 7) {
      vv <- values[!is.na(values)]
      if (length(unique(vv)) < 3) return(NULL)
      ok <- function(k) {
        br <- quantile(vv, probs = seq(0,1,length.out=k+1), names=FALSE, type=7)
        length(unique(br)) == length(br)
      }
      k <- n; while (k > 2 && !ok(k)) k <- k - 1
      if (!ok(k)) return(NULL)
      colorQuantile("viridis", vv, n = k, na.color = "#dddddd")
    }
    pal <- NULL
    if (input$classify == "quantile") pal <- make_quantile_pal(v, 7)
    if (is.null(pal)) pal <- colorNumeric("viridis", v, na.color = "#dddddd")
    
    fmt <- function(x) formatC(x, digits = 3, format = "g")
    
    if (input$compare) {
      scn_order <- c(base_name, other_scn)
      scn_lines <- vapply(seq_len(nrow(dat)), function(i)
        paste(vapply(scn_order, function(s)
          sprintf("%s: %s", s, fmt(dat[[s]][i])), character(1)),
          collapse = "<br/>"), character(1))
      labels <- sprintf("<b>%s</b><br/>%s (%s)<br/>%s%s",
                        dat$lsoa21cd, metric_labels[[input$metric]], stat_labels[[st]],
                        scn_lines, dat$profile_html) |> lapply(htmltools::HTML)
      legend_title <- paste0(metric_labels[[input$metric]],
                             "<br/><small>", stat_labels[[st]], " - base</small>")
    } else {
      labels <- sprintf("<b>%s</b><br/>%s (%s)<br/>%s: %s%s",
                        dat$lsoa21cd, metric_labels[[input$metric]], stat_labels[[st]],
                        input$scenario, fmt(v), dat$profile_html) |> lapply(htmltools::HTML)
      legend_title <- paste0(metric_labels[[input$metric]],
                             "<br/><small>", stat_labels[[st]], "</small>")
    }
    
    leafletProxy("map", data = dat) |> clearShapes() |> clearControls() |>
      addPolygons(fillColor = ~pal(v), fillOpacity = 0.8,
                  color = "white", weight = 0.4, label = labels,
                  highlightOptions = highlightOptions(weight = 2, color = "#222",
                                                      bringToFront = TRUE)) |>
      addLegend(pal = pal, values = v, opacity = 0.9,
                title = HTML(legend_title), position = "bottomright")
  })
  
  output$dl_png <- downloadHandler(
    filename = function() paste0("exposure_", input$metric, "_", stat_used(), ".png"),
    content = function(file) {
      st <- stat_used()
      facet_dat <- long |>
        filter(metric == input$metric, stat == st) |>
        select(lsoa21cd, scenario, value)
      sf_facet <- lsoa |> left_join(facet_dat, by = "lsoa21cd") |>
        mutate(scenario = factor(scenario, levels = c(base_name, other_scn)))
      p <- ggplot(sf_facet) +
        geom_sf(aes(fill = value), colour = NA) +
        facet_wrap(~scenario, nrow = 1) +
        scale_fill_viridis_c(na.value = "grey90", name = "value") +
        labs(title = paste0(metric_labels[[input$metric]],
                            "  (", stat_labels[[st]], ")"),
             caption = "Boundaries: ONS Open Geography Portal (OGL v3)") +
        theme_void(base_size = 11) +
        theme(plot.title = element_text(face = "bold"),
              strip.text = element_text(face = "bold"))
      ggsave(file, p, width = 16, height = 5.5, dpi = 200, bg = "white")
    }
  )
}

shinyApp(ui, server)