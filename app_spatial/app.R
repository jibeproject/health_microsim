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
library(base64enc)

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
lad      <- readRDS("gm_lad_boundaries.rds")      # sf, EPSG:27700 (10 GM LADs)
lad_wgs  <- st_transform(lad, 4326)
lad_prof <- readRDS("lad_profile.rds")            # per-LAD demographic profile

# point-on-surface for placing LAD name labels (inside each polygon)
lad_centroids <- suppressWarnings(
  sf::st_point_on_surface(lad_wgs)
)

# pre-build the click-popup HTML for each LAD, keyed by lad code
lad_popup <- setNames(
  vapply(seq_len(nrow(lad_prof)), function(i) {
    r <- lad_prof[i, ]
    sprintf(
      paste0("<b>%s</b><hr style='margin:4px 0'/>",
             "Population: %s<br/>",
             "Age groups: &lt;18 %.0f%%, 18-24 %.0f%%, 25-44 %.0f%%, ",
             "45-65 %.0f%%, 66+ %.0f%%<br/>",
             "Gender: %.0f%% male / %.0f%% female<hr style='margin:4px 0'/>",
             "<b>IMD quintile (%% of population)</b><br/>",
             "Q1 (most deprived): %.0f%%<br/>",
             "Q2: %.0f%%<br/>Q3: %.0f%%<br/>Q4: %.0f%%<br/>",
             "Q5 (least deprived): %.0f%%"),
      r$ladnm,
      formatC(r$n_people, big.mark = ",", format = "d"),
      r$pct_u18, r$pct_18_24, r$pct_25_44, r$pct_45_65, r$pct_66plus,
      r$pct_male, r$pct_female,
      r$imd_q1, r$imd_q2, r$imd_q3, r$imd_q4, r$imd_q5)
  }, character(1)),
  lad_prof$ladcd
)

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
    imd_quintile = ceiling(imd10 / 2),   # 1-2->1, 3-4->2, ... 9-10->5
    profile_html = sprintf(
      paste0("<hr style='margin:3px 0'/><b>Profile</b><br/>",
             "Population: %s<br/>",
             "Age groups:<br/>",
             "&nbsp;&lt;18: %.0f%% &nbsp; 18-24: %.0f%%<br/>",
             "&nbsp;25-44: %.0f%% &nbsp; 45-65: %.0f%%<br/>",
             "&nbsp;66+: %.0f%%<br/>",
             "Gender: %.0f%% male / %.0f%% female<br/>",
             "IMD quintile: %d"),
      formatC(n_people, big.mark = ",", format = "d"),
      pct_u18, pct_18_24, pct_25_44, pct_45_65, pct_66plus,
      pct_male, pct_female, as.integer(imd_quintile))
  ) |>
  select(lsoa21cd, profile_html, imd10, imd_quintile)

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
      radioButtons("view", "Map shows",
                   c("Absolute (base)"   = "absolute",
                     "% change vs base"  = "pctchange"),
                   selected = "absolute"),
      conditionalPanel(
        "input.view == 'pctchange'",
        selectInput("cmp_scenario", "Scenario (vs base)",
                    choices = other_scn,
                    selected = other_scn[1])),
      selectInput("imd_filter", "IMD quintile",
                  choices = c("All quintiles" = "all",
                              "Q1 (most deprived)" = "1",
                              "Q2" = "2", "Q3" = "3", "Q4" = "4",
                              "Q5 (least deprived)" = "5"),
                  selected = "all"),
      hr(),
      downloadButton("dl_png", "Save faceted PNG"),
      helpText("Click an LSOA to see how it changes across scenarios.")
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
    # all scenarios wide, per LSOA, for this metric+stat
    wide_scn <- long |>
      filter(metric == input$metric, stat == st) |>
      select(lsoa21cd, scenario, value) |>
      pivot_wider(names_from = scenario, values_from = value)
    
    dat <- lsoa_wgs |>
      left_join(wide_scn, by = "lsoa21cd") |>
      left_join(prof_fmt, by = "lsoa21cd")
    
    # colour value depends on view mode
    if (input$view == "pctchange") {
      scn <- input$cmp_scenario
      base_v <- dat[[base_name]]
      scn_v  <- dat[[scn]]
      # % change vs base; guard divide-by-zero -> NA
      dat$colour_value <- ifelse(is.na(base_v) | base_v == 0,
                                 NA_real_, 100 * (scn_v - base_v) / base_v)
    } else {
      dat$colour_value <- dat[[base_name]]
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
                    color = "white", weight = 0.4, label = ~lsoa21cd) |>
        addPolylines(data = lad_wgs, color = "#111111", weight = 2.5,
                     opacity = 1, group = "LAD boundaries")
      return(invisible())
    }
    
    # ---- palette: viridis for absolute, diverging centred on 0 for % change
    if (input$view == "pctchange") {
      rng <- max(abs(v), na.rm = TRUE)
      if (!is.finite(rng) || rng == 0) rng <- 1
      # reverse RdBu so red = increase vs base, blue = decrease
      pal <- colorNumeric(rev(RColorBrewer::brewer.pal(11, "RdBu")),
                          domain = c(-rng, rng), na.color = "#dddddd")
      legend_title <- paste0(metric_labels[[input$metric]],
                             "<br/><small>% change: ", input$cmp_scenario,
                             " vs base (", stat_labels[[st]], ")</small>")
    } else {
      pal <- colorNumeric("viridis", v, na.color = "#dddddd")
      legend_title <- paste0(metric_labels[[input$metric]],
                             "<br/><small>", stat_labels[[st]], " - base</small>")
    }
    
    fmt <- function(x) formatC(x, digits = 3, format = "g")
    
    # hover label: name + all scenario values (absolute) + profile
    scn_order <- c(base_name, other_scn)
    scn_lines <- vapply(seq_len(nrow(dat)), function(i)
      paste(vapply(scn_order, function(s)
        sprintf("%s: %s", s, fmt(dat[[s]][i])), character(1)),
        collapse = "<br/>"), character(1))
    if (input$view == "pctchange") {
      head_line <- sprintf("<b>%s</b><br/>%s (%s)<br/><i>%s vs base: %s%%</i><br/>",
                           dat$lsoa21cd, metric_labels[[input$metric]], stat_labels[[st]],
                           input$cmp_scenario,
                           ifelse(is.na(v), "-", formatC(v, digits = 1, format = "f")))
    } else {
      head_line <- sprintf("<b>%s</b><br/>%s (%s)<br/>",
                           dat$lsoa21cd, metric_labels[[input$metric]], stat_labels[[st]])
    }
    labels <- paste0(head_line, scn_lines, dat$profile_html) |> lapply(htmltools::HTML)
    
    fill_cols <- pal(v)
    fill_op   <- rep(0.8, nrow(dat))
    # IMD quintile filter: dim + grey LSOAs not in the selected quintile
    if (input$imd_filter != "all") {
      keep <- !is.na(dat$imd_quintile) &
        dat$imd_quintile == as.integer(input$imd_filter)
      fill_cols[!keep] <- "#e8e8e8"
      fill_op[!keep]   <- 0.15
    }
    
    leafletProxy("map", data = dat) |> clearShapes() |> clearControls() |>
      addPolygons(layerId = ~lsoa21cd,
                  fillColor = fill_cols, fillOpacity = fill_op,
                  color = "white", weight = 0.4, label = labels,
                  highlightOptions = highlightOptions(weight = 2, color = "#222",
                                                      bringToFront = TRUE)) |>
      addPolylines(data = lad_wgs, color = "#111111", weight = 2.5, opacity = 1,
                   label = lapply(lad_wgs$lad21cd, function(cd) {
                     h <- lad_popup[[cd]]
                     htmltools::HTML(if (is.null(h)) cd else h)
                   }),
                   highlightOptions = highlightOptions(weight = 4, color = "#000",
                                                       bringToFront = TRUE),
                   group = "LAD boundaries") |>
      addLabelOnlyMarkers(data = lad_centroids,
                          label = ~lad21nm,
                          labelOptions = labelOptions(noHide = TRUE, direction = "center",
                                                      textOnly = TRUE,
                                                      style = list("font-weight" = "bold",
                                                                   "color" = "#111111",
                                                                   "text-shadow" =
                                                                     "0 0 3px #fff, 0 0 3px #fff")),
                          group = "LAD boundaries") |>
      addLegend(pal = pal, values = if (input$view == "pctchange") c(-rng, rng) else v,
                opacity = 0.9, title = HTML(legend_title), position = "bottomright") |>
      addLayersControl(
        overlayGroups = "LAD boundaries",
        options = layersControlOptions(collapsed = FALSE),
        position = "topright")
  })
  
  # ---- LSOA click -> scenario comparison chart popup ------------------------
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    id <- click$id
    if (is.null(id) || !grepl("^E01", id)) return()   # only LSOA ids (E01...)
    
    st <- stat_used()
    vals <- long |>
      filter(metric == input$metric, stat == st, lsoa21cd == id) |>
      select(scenario, value)
    if (nrow(vals) == 0) return()
    ord <- c(base_name, other_scn)
    vals <- vals[match(ord, vals$scenario), ]
    vals$scenario <- ord
    base_v <- vals$value[vals$scenario == base_name]
    
    # draw a small bar chart to a temp PNG and embed as base64 in the popup
    f <- tempfile(fileext = ".png")
    png(f, width = 360, height = 240, res = 96)
    op <- par(mar = c(4.5, 4, 3, 1))
    cols <- ifelse(vals$scenario == base_name, "#666666",
                   ifelse(vals$value >= base_v, "#b2182b", "#2166ac"))
    bp <- barplot(vals$value, names.arg = vals$scenario, col = cols, border = NA,
                  las = 2, cex.names = 0.8,
                  main = id, ylab = metric_labels[[input$metric]])
    abline(h = base_v, lty = 2, col = "#444444")
    par(op); dev.off()
    
    uri <- paste0("data:image/png;base64,",
                  base64enc::base64encode(f))
    pct_lines <- paste(vapply(other_scn, function(s) {
      sv <- vals$value[vals$scenario == s]
      pc <- if (is.na(base_v) || base_v == 0) NA else 100 * (sv - base_v) / base_v
      sprintf("%s: %s%%", s, ifelse(is.na(pc), "-", formatC(pc, digits = 1, format = "f")))
    }, character(1)), collapse = "<br/>")
    
    popup_html <- sprintf(
      "<b>%s</b> (%s)<br/><img src='%s' width='340'/><br/><small>%% change vs base:<br/>%s</small>",
      metric_labels[[input$metric]], stat_labels[[st]], uri, pct_lines)
    
    leafletProxy("map") |> clearPopups() |>
      addPopups(lng = click$lng, lat = click$lat, popup = popup_html)
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