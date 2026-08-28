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
library(patchwork)
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

# dissolved outer boundary of Greater Manchester (all 10 LADs as one outline)
gm_outline <- suppressWarnings(
  sf::st_union(lad_wgs) |> sf::st_boundary()
)
lad_prof <- readRDS("lad_profile.rds")            # per-LAD demographic profile

# LSOA <-> LAD lookup for the cascading LSOA picker
lsoa_lad_lkp <- prof |>
  dplyr::mutate(imd_quintile = ceiling(imd10 / 2)) |>
  dplyr::select(lsoa21cd, lsoa21nm, ladcd, ladnm, imd_quintile) |>
  dplyr::distinct() |>
  dplyr::arrange(ladnm, lsoa21nm)
lad_imd_metric <- readRDS("lad_imd_metric.rds")   # LAD x metric x stat x quintile x scenario
lad_metrics <- readRDS("lad_metrics_by_scenario.rds")$long  # true pooled LAD metrics
lad_metrics_long <- lad_metrics                   # ladcd, ladnm, n_people, scenario, metric, stat, value
gm_metrics_long  <- readRDS("gm_metrics_by_scenario.rds")$long  # GM-wide pooled, all individuals
gm_gender_long   <- readRDS("gm_metrics_by_gender.rds")$long     # GM-wide by gender
lad_gender_long  <- readRDS("lad_metrics_by_gender.rds")$long    # LAD by gender

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
# short labels (no parenthetical) for plot titles, to avoid clipping
metric_short <- c(
  exposure_normalised_pm25       = "PM2.5",
  exposure_normalised_no2        = "NO2",
  exposure_normalised_noise_Lden = "Noise Lden",
  exposure_normalised_ndvi       = "Greenness NDVI",
  mmets                          = "mMETs",
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

# pretty scenario names for tables/labels
scn_display <- c(base = "Reference", goDutch = "Go Dutch",
                 green = "Green", safeStreet = "Safer Streets")
scn_label <- function(s) ifelse(s %in% names(scn_display), scn_display[s], s)

# gender label: handle 1/2 codes or "male"/"female" strings
gender_label <- function(g) {
  g <- as.character(g)
  dplyr::case_when(
    g %in% c("1", "male", "Male", "M")   ~ "Male",
    g %in% c("2", "female", "Female", "F") ~ "Female",
    TRUE ~ g
  )
}
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
      selectInput("lad_pick", "LAD detail (metric by IMD)",
                  choices = c("Select a LAD..." = "",
                              setNames(lad_prof$ladcd, lad_prof$ladnm)),
                  selected = ""),
      uiOutput("lad_chart"),
      hr(),
      h4("LSOA detail (downloadable)"),
      selectInput("lsoa_lad", "1. Pick LAD",
                  choices = c("Select a LAD..." = "",
                              setNames(lad_prof$ladcd, lad_prof$ladnm)),
                  selected = ""),
      selectInput("lsoa_imd", "2. Pick IMD quintile",
                  choices = c("All quintiles" = "all",
                              "Q1 (most deprived)" = "1", "Q2" = "2",
                              "Q3" = "3", "Q4" = "4", "Q5 (least deprived)" = "5"),
                  selected = "all"),
      selectInput("lsoa_pick", "3. Pick LSOA", choices = c("Select an LSOA..." = "")),
      downloadButton("dl_lsoa_png", "Save LSOA sheet (PNG)"),
      downloadButton("dl_lad_q1q5", "Save LAD Q1 vs Q5 map (PNG)"),
      helpText("Uses the LAD selected in '1. Pick LAD' above and the current",
               "scenario picker. Pick a LAD first."),
      hr(),
      downloadButton("dl_png", "Save change map - all scenarios (PNG)"),
      div(style = "margin-top:6px",
          downloadButton("dl_png_imd",
                         "Save current view (scenario + IMD filter)")),
      div(style = "margin-top:6px",
          helpText("Per-scenario change maps:"),
          downloadButton("dl_png_s1", scn_label(other_scn[1])),
          downloadButton("dl_png_s2", scn_label(other_scn[2])),
          downloadButton("dl_png_s3", scn_label(other_scn[3]))),
      helpText("Click an LSOA to see how it changes across scenarios. ",
               "Click a LAD border (or use the dropdown) for its IMD breakdown."),
      hr(),
      h4("Greater Manchester (all individuals)"),
      uiOutput("gm_tables"),
      hr(),
      h4("Greater Manchester by gender"),
      uiOutput("gm_gender_tables")
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
        addPolylines(data = lad_wgs, layerId = ~paste0("LAD_", lad21cd),
                     color = "#111111", weight = 2.5, opacity = 1,
                     label = lapply(lad_wgs$lad21cd, function(cd) {
                       h <- lad_popup[[cd]]
                       htmltools::HTML(if (is.null(h)) cd else h)
                     }),
                     group = "LAD boundaries")
      return(invisible())
    }
    
    # ---- palette: viridis for absolute, diverging centred on 0 for % change
    if (input$view == "pctchange") {
      # data-driven symmetric cap: use the 98th percentile of |value| so the
      # colours fill the real range (PM2.5 changes ~3%, mMETs can be ~80%),
      # with a small floor so near-zero data still shows some colour.
      cap <- stats::quantile(abs(v), 0.98, na.rm = TRUE)
      if (!is.finite(cap) || cap < 1) cap <- 1
      cap <- as.numeric(cap)
      v_clamped <- pmax(pmin(v, cap), -cap)
      pal <- colorNumeric(rev(RColorBrewer::brewer.pal(11, "RdBu")),
                          domain = c(-cap, cap), na.color = "#dddddd")
      legend_title <- paste0(metric_labels[[input$metric]],
                             "<br/><small>% change: ", input$cmp_scenario,
                             " vs base (", stat_labels[[st]], "), &plusmn;",
                             formatC(cap, digits = 1, format = "f"), "%</small>")
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
    
    fill_cols <- if (input$view == "pctchange") pal(v_clamped) else pal(v)
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
      addPolylines(data = lad_wgs, layerId = ~paste0("LAD_", lad21cd),
                   color = "#111111", weight = 2.5, opacity = 1,
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
      # invisible target at each LAD name: hover shows profile, click opens detail
      addCircleMarkers(data = lad_centroids, layerId = ~paste0("LAD_", lad21cd),
                       radius = 22, stroke = FALSE, fillOpacity = 0,
                       label = lapply(lad_centroids$lad21cd, function(cd) {
                         h <- lad_popup[[cd]]
                         htmltools::HTML(if (is.null(h)) cd else h)
                       }),
                       group = "LAD boundaries") |>
      addPolylines(data = gm_outline, color = "#1a1a1a", weight = 5, opacity = 1,
                   group = "LAD boundaries") |>
      addLegend(pal = pal, values = if (input$view == "pctchange") c(-cap, cap) else v,
                opacity = 0.9, title = HTML(legend_title), position = "bottomright") |>
      addLayersControl(
        overlayGroups = "LAD boundaries",
        options = layersControlOptions(collapsed = FALSE),
        position = "topright")
  })
  
  # ---- LSOA click -> scenario comparison chart popup ------------------------
  # build the LAD-by-IMD chart for a given LAD code; returns a base64 data URI
  lad_imd_chart_uri <- function(ladcode) {
    st <- stat_used()
    cmp <- if (is.null(input$cmp_scenario)) other_scn[1] else input$cmp_scenario
    d <- lad_imd_metric |>
      filter(ladcd == ladcode, metric == input$metric, stat == st,
             scenario %in% c(base_name, cmp))
    if (nrow(d) == 0) return(NULL)
    wide <- d |>
      select(imd_quintile, scenario, value) |>
      tidyr::pivot_wider(names_from = scenario, values_from = value) |>
      arrange(imd_quintile)
    q   <- wide$imd_quintile
    bv  <- wide[[base_name]]
    sv  <- wide[[cmp]]
    pc  <- ifelse(is.na(bv) | bv == 0, NA, 100 * (sv - bv) / bv)
    ladnm <- lad_prof$ladnm[match(ladcode, lad_prof$ladcd)]
    
    f <- tempfile(fileext = ".png")
    png(f, width = 440, height = 340, res = 96)
    op <- par(mfrow = c(2, 1), mar = c(2.2, 4, 2.2, 1), oma = c(2, 0, 1.5, 0))
    m <- rbind(base = bv, scn = sv)
    barplot(m, beside = TRUE, names.arg = paste0("Q", q),
            col = c("#999999", "#1f78b4"), border = NA,
            ylab = "value", main = "Absolute (base vs scenario)",
            cex.main = 0.95, cex.names = 0.85)
    legend("topright", c("base", cmp), fill = c("#999999", "#1f78b4"),
           border = NA, bty = "n", cex = 0.8)
    cols <- ifelse(is.na(pc), "#cccccc", ifelse(pc >= 0, "#b2182b", "#2166ac"))
    barplot(pc, names.arg = paste0("Q", q), col = cols, border = NA,
            ylab = "% change", main = "% change vs base",
            cex.main = 0.95, cex.names = 0.85)
    abline(h = 0, col = "#444444")
    mtext(sprintf("%s - %s (%s)", ladnm, metric_labels[[input$metric]], stat_labels[[st]]),
          outer = TRUE, cex = 0.95, font = 2)
    mtext("IMD quintile (Q1 = most deprived)", side = 1, outer = TRUE, cex = 0.8)
    par(op); dev.off()
    paste0("data:image/png;base64,", base64enc::base64encode(f))
  }
  
  # the chart panel below the map: driven by the LAD dropdown
  output$lad_chart <- renderUI({
    if (is.null(input$lad_pick) || input$lad_pick == "")
      return(helpText("Select a LAD (or click a border) for its metric change by IMD quintile."))
    uri <- lad_imd_chart_uri(input$lad_pick)
    if (is.null(uri)) return(helpText("No data for this LAD / metric combination."))
    tags$img(src = uri, width = "100%", style = "max-width:440px;")
  })
  
  # shared builder: full LAD popup (profile + pooled metrics + IMD chart)
  build_lad_popup <- function(ladcode) {
    st <- stat_used()
    prof_html <- lad_popup[[ladcode]]; if (is.null(prof_html)) prof_html <- ""
    md <- lad_metrics_long |>
      filter(ladcd == ladcode, metric == input$metric, stat == st) |>
      arrange(match(scenario, c(base_name, other_scn)))
    metric_html <- ""
    if (nrow(md) > 0) {
      bval <- md$value[md$scenario == base_name]
      rows <- vapply(seq_len(nrow(md)), function(i) {
        s <- md$scenario[i]; vv <- md$value[i]
        pc <- if (length(bval) && !is.na(bval) && bval != 0)
          sprintf(" (%+.1f%%)", 100 * (vv - bval) / bval) else ""
        sprintf("%s: %s%s", s, formatC(vv, digits = 3, format = "g"),
                if (s == base_name) "" else pc)
      }, character(1))
      metric_html <- paste0(
        "<hr style='margin:4px 0'/><b>", metric_labels[[input$metric]],
        " (", stat_labels[[st]], ", all individuals)</b><br/>",
        paste(rows, collapse = "<br/>"))
    }
    
    # by-gender block: current metric/stat, base + scenarios, split by gender
    gender_html <- ""
    gd <- lad_gender_long |>
      filter(ladcd == ladcode, metric == input$metric, stat == st)
    if (nrow(gd) > 0) {
      genders <- unique(gd$gender)
      gblocks <- vapply(genders, function(g) {
        sub <- gd |> filter(gender == g) |>
          arrange(match(scenario, c(base_name, other_scn)))
        bv <- sub$value[sub$scenario == base_name]
        rows <- vapply(seq_len(nrow(sub)), function(i) {
          s <- sub$scenario[i]; vv <- sub$value[i]
          pc <- if (length(bv) && !is.na(bv) && bv != 0)
            sprintf(" (%+.1f%%)", 100 * (vv - bv) / bv) else ""
          sprintf("&nbsp;%s: %s%s", scn_label(s),
                  formatC(vv, digits = 3, format = "g"),
                  if (s == base_name) "" else pc)
        }, character(1))
        paste0("<u>", gender_label(g), "</u><br/>", paste(rows, collapse = "<br/>"))
      }, character(1))
      gender_html <- paste0(
        "<hr style='margin:4px 0'/><b>By gender</b><br/>",
        paste(gblocks, collapse = "<br/>"))
    }
    uri <- lad_imd_chart_uri(ladcode)
    chart_html <- if (is.null(uri)) "" else
      sprintf("<hr style='margin:4px 0'/><img src='%s' width='400'/>", uri)
    paste0(prof_html, metric_html, gender_html, chart_html)
  }
  
  # LAD name-circle clicks fire map_marker_click; route to the same handler
  # ---- GM-wide stacked tables (one per metric, scenarios x stats) -----------
  output$gm_tables <- renderUI({
    stat_order  <- c("p05", "p25", "p50", "p75", "mean")
    stat_head   <- c(p05 = "5%", p25 = "25%", p50 = "50%", p75 = "75%", mean = "mean")
    scn_order   <- c(base_name, other_scn)
    
    # build one metric's scenario x stat table from a long df, optionally
    # filtered to a single gender value
    one_metric_table <- function(df, m) {
      md <- df |> filter(metric == m)
      if (nrow(md) == 0) return(NULL)
      present_stats <- stat_order[stat_order %in% unique(md$stat)]
      header <- paste0("<tr><th style='text-align:left'></th>",
                       paste0("<th>", stat_head[present_stats], "</th>", collapse = ""),
                       "</tr>")
      body <- paste(vapply(scn_order, function(s) {
        cells <- vapply(present_stats, function(st) {
          v <- md$value[md$scenario == s & md$stat == st]
          if (length(v) == 0 || is.na(v)) "-" else formatC(v, digits = 1, format = "f")
        }, character(1))
        paste0("<tr><td style='text-align:left;font-weight:bold'>", scn_label(s), "</td>",
               paste0("<td style='text-align:center'>", cells, "</td>", collapse = ""),
               "</tr>")
      }, character(1)), collapse = "")
      HTML(sprintf(
        "<div style='margin-bottom:10px'><div style='font-weight:bold;margin:4px 0'>%s</div>
         <table style='width:100%%;border-collapse:collapse;font-size:12px'>
         <thead>%s</thead><tbody>%s</tbody></table></div>",
        metric_labels[[m]], header, body))
    }
    
    # overall (all individuals)
    tagList(lapply(metrics, function(m) one_metric_table(gm_metrics_long, m)))
  })
  
  # GM-wide by gender (one block per gender, all metrics)
  output$gm_gender_tables <- renderUI({
    stat_order  <- c("p05", "p25", "p50", "p75", "mean")
    stat_head   <- c(p05 = "5%", p25 = "25%", p50 = "50%", p75 = "75%", mean = "mean")
    scn_order   <- c(base_name, other_scn)
    genders <- unique(gm_gender_long$gender)
    
    sections <- lapply(genders, function(g) {
      gtl <- gm_gender_long |> filter(gender == g)
      tbls <- lapply(metrics, function(m) {
        md <- gtl |> filter(metric == m)
        if (nrow(md) == 0) return(NULL)
        present_stats <- stat_order[stat_order %in% unique(md$stat)]
        header <- paste0("<tr><th style='text-align:left'></th>",
                         paste0("<th>", stat_head[present_stats], "</th>", collapse = ""), "</tr>")
        body <- paste(vapply(scn_order, function(s) {
          cells <- vapply(present_stats, function(st) {
            v <- md$value[md$scenario == s & md$stat == st]
            if (length(v) == 0 || is.na(v)) "-" else formatC(v, digits = 1, format = "f")
          }, character(1))
          paste0("<tr><td style='text-align:left;font-weight:bold'>", scn_label(s), "</td>",
                 paste0("<td style='text-align:center'>", cells, "</td>", collapse = ""), "</tr>")
        }, character(1)), collapse = "")
        HTML(sprintf(
          "<div style='margin-bottom:8px'><div style='font-weight:bold;margin:3px 0'>%s</div>
           <table style='width:100%%;border-collapse:collapse;font-size:12px'>
           <thead>%s</thead><tbody>%s</tbody></table></div>",
          metric_labels[[m]], header, body))
      })
      tagList(tags$h4(paste0(gender_label(g)), style = "margin-top:12px"), tbls)
    })
    tagList(sections)
  })
  
  # cascading LSOA picker: LSOAs for the chosen LAD, filtered by IMD quintile
  observeEvent(list(input$lsoa_lad, input$lsoa_imd), {
    if (is.null(input$lsoa_lad) || input$lsoa_lad == "") {
      updateSelectInput(session, "lsoa_pick",
                        choices = c("Select an LSOA..." = ""))
      return()
    }
    sub <- lsoa_lad_lkp |> filter(ladcd == input$lsoa_lad)
    if (!is.null(input$lsoa_imd) && input$lsoa_imd != "all") {
      sub <- sub |> filter(imd_quintile == as.integer(input$lsoa_imd))
    }
    if (nrow(sub) == 0) {
      updateSelectInput(session, "lsoa_pick",
                        choices = c("No LSOAs in this quintile" = ""))
      return()
    }
    updateSelectInput(session, "lsoa_pick",
                      choices = c("Select an LSOA..." = "",
                                  setNames(sub$lsoa21cd, sub$lsoa21nm)))
  })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click$id) || !grepl("^LAD_", click$id)) return()
    ladcode <- sub("^LAD_", "", click$id)
    updateSelectInput(session, "lad_pick", selected = ladcode)
    leafletProxy("map") |> clearPopups() |>
      addPopups(lng = click$lng, lat = click$lat, popup = build_lad_popup(ladcode))
  })
  
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    id <- click$id
    if (is.null(id)) return()
    
    st <- stat_used()
    
    # ---- LAD border click -> demographic profile + pooled metrics + IMD chart
    if (grepl("^LAD_", id)) {
      ladcode <- sub("^LAD_", "", id)
      updateSelectInput(session, "lad_pick", selected = ladcode)
      leafletProxy("map") |> clearPopups() |>
        addPopups(lng = click$lng, lat = click$lat, popup = build_lad_popup(ladcode))
      return()
    }
    
    # ---- LSOA click -> scenario comparison chart -----------------------------
    if (!grepl("^E01", id)) return()   # only LSOA ids (E01...)
    
    vals <- long |>
      filter(metric == input$metric, stat == st, lsoa21cd == id) |>
      select(scenario, value)
    if (nrow(vals) == 0) return()
    ord <- c(base_name, other_scn)
    vals <- vals[match(ord, vals$scenario), ]
    vals$scenario <- ord
    base_v <- vals$value[vals$scenario == base_name]
    
    # LSOA name + LAD for a friendly title (fall back to code if missing)
    lsoa_nm  <- lsoa_lad_lkp$lsoa21nm[match(id, lsoa_lad_lkp$lsoa21cd)]
    lsoa_lad <- lsoa_lad_lkp$ladnm[match(id, lsoa_lad_lkp$lsoa21cd)]
    title_nm <- if (is.na(lsoa_nm)) id else lsoa_nm
    
    # draw a small bar chart to a temp PNG and embed as base64 in the popup
    f <- tempfile(fileext = ".png")
    png(f, width = 360, height = 240, res = 96)
    op <- par(mar = c(4.5, 4, 3, 1))
    cols <- ifelse(vals$scenario == base_name, "#666666",
                   ifelse(vals$value >= base_v, "#b2182b", "#2166ac"))
    bp <- barplot(vals$value, names.arg = vals$scenario, col = cols, border = NA,
                  las = 2, cex.names = 0.8,
                  main = title_nm, ylab = metric_labels[[input$metric]])
    abline(h = base_v, lty = 2, col = "#444444")
    par(op); dev.off()
    
    uri <- paste0("data:image/png;base64,", base64enc::base64encode(f))
    pct_lines <- paste(vapply(other_scn, function(s) {
      sv <- vals$value[vals$scenario == s]
      pc <- if (is.na(base_v) || base_v == 0) NA else 100 * (sv - base_v) / base_v
      sprintf("%s: %s%%", scn_label(s), ifelse(is.na(pc), "-", formatC(pc, digits = 1, format = "f")))
    }, character(1)), collapse = "<br/>")
    
    popup_html <- sprintf(
      "<b>%s</b> <small>(%s, %s)</small><br/>%s (%s)<br/><img src='%s' width='340'/><br/><small>%% change vs base:<br/>%s</small>",
      title_nm, lsoa_lad, id,
      metric_labels[[input$metric]], stat_labels[[st]], uri, pct_lines)
    
    leafletProxy("map") |> clearPopups() |>
      addPopups(lng = click$lng, lat = click$lat, popup = popup_html)
  })
  
  # ---- shared change-map builder -------------------------------------------
  # computes per-LSOA % change vs base for the given scenarios and returns a
  # ggplot. If `scns` is one scenario -> single map; if several -> faceted.
  make_change_plot <- function(scns, show_title = TRUE, imd_q = "all") {
    st <- stat_used()
    w <- long |>
      filter(metric == input$metric, stat == st) |>
      select(lsoa21cd, scenario, value) |>
      tidyr::pivot_wider(names_from = scenario, values_from = value)
    pc <- w |>
      mutate(across(all_of(scns),
                    ~ ifelse(is.na(.data[[base_name]]) | .data[[base_name]] == 0,
                             NA_real_, 100 * (.x - .data[[base_name]]) / .data[[base_name]]))) |>
      select(lsoa21cd, all_of(scns)) |>
      tidyr::pivot_longer(all_of(scns), names_to = "scenario", values_to = "pct")
    pc$scenario <- factor(scn_label(pc$scenario), levels = scn_label(scns))
    
    # cap from the FULL (unfiltered) change distribution, so every IMD selection
    # shares one fixed colour scale and quintile maps are comparable
    cap <- stats::quantile(abs(pc$pct), 0.98, na.rm = TRUE)
    if (!is.finite(cap) || cap < 1) cap <- 1
    
    sf_facet <- lsoa |> left_join(pc, by = "lsoa21cd")
    # IMD quintile filter: NA-out the pct for LSOAs outside the selected quintile
    # so they render grey, exactly like the on-screen filter
    if (!is.null(imd_q) && imd_q != "all") {
      q <- as.integer(imd_q)
      qmap <- prof |> dplyr::mutate(imd_quintile = ceiling(imd10 / 2)) |>
        dplyr::select(lsoa21cd, imd_quintile)
      sf_facet <- sf_facet |> left_join(qmap, by = "lsoa21cd")
      sf_facet$pct[is.na(sf_facet$imd_quintile) | sf_facet$imd_quintile != q] <- NA_real_
    }
    lad_p   <- sf::st_transform(lad, sf::st_crs(lsoa))
    lad_lab <- suppressWarnings(sf::st_point_on_surface(lad_p))
    lad_lab_xy <- cbind(as.data.frame(sf::st_coordinates(lad_lab)),
                        ladnm = lad_p$lad21nm)
    
    single <- length(scns) == 1
    lab_size  <- if (single) 4.2 else 3.2
    q_suffix <- if (!is.null(imd_q) && imd_q != "all")
      paste0(" - IMD Q", imd_q) else ""
    p <- ggplot(sf_facet) +
      geom_sf(aes(fill = pct), colour = NA) +
      geom_sf(data = lad_p, fill = NA, colour = "grey15", linewidth = 0.5) +
      geom_text(data = lad_lab_xy, aes(X, Y, label = ladnm),
                size = lab_size, fontface = "bold", colour = "black") +
      scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
                           midpoint = 0, limits = c(-cap, cap),
                           oob = scales::squish, na.value = "grey90",
                           name = "% change\nvs base") +
      labs(title = if (!show_title) NULL else if (single)
        sprintf("%s - %% change vs base: %s%s",
                metric_short[[input$metric]], scn_label(scns), q_suffix)
        else
          sprintf("%s - %% change vs base%s",
                  metric_short[[input$metric]], q_suffix),
        subtitle = stat_labels[[st]],
        caption = "Boundaries: ONS Open Geography Portal (OGL v3)") +
      theme_void(base_size = 13) +
      theme(plot.title    = element_text(face = "bold", size = 22, hjust = 0.5,
                                         margin = margin(b = 2)),
            plot.subtitle = element_text(size = 16, hjust = 0.5, colour = "grey30",
                                         margin = margin(b = 8)),
            strip.text   = element_text(face = "bold", size = 18),
            legend.title = element_text(face = "bold", size = 13),
            legend.text  = element_text(size = 12),
            plot.caption = element_text(size = 10, colour = "grey40"))
    if (!single) p <- p + facet_wrap(~scenario, nrow = 1)
    p
  }
  
  # ---- one GM map highlighting a single LAD x quintile (rest greyed) --------
  # shared `cap` is passed in so multiple panels use one colour scale.
  make_lad_quintile_panel <- function(scn, ladcode, q, cap, panel_title) {
    st <- stat_used()
    w <- long |>
      filter(metric == input$metric, stat == st) |>
      select(lsoa21cd, scenario, value) |>
      tidyr::pivot_wider(names_from = scenario, values_from = value)
    pc <- w |>
      mutate(pct = if (!scn %in% names(w) || !base_name %in% names(w)) NA_real_
             else ifelse(is.na(.data[[base_name]]) | .data[[base_name]] == 0,
                         NA_real_, 100 * (.data[[scn]] - .data[[base_name]]) / .data[[base_name]])) |>
      select(lsoa21cd, pct)
    qmap <- lsoa_lad_lkp |> dplyr::select(lsoa21cd, ladcd, imd_quintile)
    sf_p <- lsoa |> left_join(pc, by = "lsoa21cd") |> left_join(qmap, by = "lsoa21cd")
    # keep colour only for LSOAs in this LAD AND this quintile
    keep <- !is.na(sf_p$ladcd) & sf_p$ladcd == ladcode &
      !is.na(sf_p$imd_quintile) & sf_p$imd_quintile == q
    message(sprintf("[Q%s panel] ladcode=%s | LSOAs in LAD=%d | in quintile=%d | kept=%d | non-NA pct kept=%d",
                    q, ladcode,
                    sum(!is.na(sf_p$ladcd) & sf_p$ladcd == ladcode),
                    sum(!is.na(sf_p$imd_quintile) & sf_p$imd_quintile == q),
                    sum(keep),
                    sum(keep & !is.na(sf_p$pct))))
    sf_p$pct[!keep] <- NA_real_
    
    lad_p <- sf::st_transform(lad, sf::st_crs(lsoa))
    ggplot(sf_p) +
      geom_sf(aes(fill = pct), colour = NA) +
      geom_sf(data = lad_p, fill = NA, colour = "grey55", linewidth = 0.3) +
      geom_sf(data = lad_p[lad_p$lad21cd == ladcode, ], fill = NA,
              colour = "grey10", linewidth = 0.9) +
      scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
                           midpoint = 0, limits = c(-cap, cap),
                           oob = scales::squish, na.value = "grey90",
                           name = "% change\nvs base") +
      labs(title = panel_title) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
            legend.title = element_text(face = "bold", size = 13),
            legend.text = element_text(size = 12))
  }
  
  # ---- Q1 vs Q5 side-by-side for one LAD, shared scale ---------------------
  make_lad_q1q5 <- function(scn, ladcode) {
    st <- stat_used()
    # shared cap from the full change distribution (so both panels match)
    w <- long |> filter(metric == input$metric, stat == st) |>
      select(lsoa21cd, scenario, value) |>
      tidyr::pivot_wider(names_from = scenario, values_from = value)
    allpct <- ifelse(is.na(w[[base_name]]) | w[[base_name]] == 0, NA_real_,
                     100 * (w[[scn]] - w[[base_name]]) / w[[base_name]])
    cap <- stats::quantile(abs(allpct), 0.98, na.rm = TRUE)
    if (!is.finite(cap) || cap < 1) cap <- 1
    ladnm <- lad_prof$ladnm[match(ladcode, lad_prof$ladcd)]
    
    left  <- make_lad_quintile_panel(scn, ladcode, 1, cap, "Q1 (most deprived)")
    right <- make_lad_quintile_panel(scn, ladcode, 5, cap, "Q5 (least deprived)")
    (left | right) +
      patchwork::plot_layout(guides = "collect") +
      patchwork::plot_annotation(
        title = sprintf("%s - %s: %% change vs base, %s",
                        ladnm, metric_short[[input$metric]], scn_label(scn)),
        subtitle = stat_labels[[st]],
        caption = "Boundaries: ONS Open Geography Portal (OGL v3)",
        theme = theme(plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
                      plot.subtitle = element_text(size = 16, hjust = 0.5, colour = "grey30"),
                      plot.caption = element_text(size = 10, colour = "grey40")))
  }
  
  
  # ---- LAD population table (one row, all 10 LADs as columns) ---------------
  make_lad_pop_table <- function() {
    pop <- lad_prof |> dplyr::arrange(ladnm)
    n   <- nrow(pop)
    poplab <- formatC(pop$n_people, big.mark = ",", format = "d")
    hdr  <- data.frame(col = seq_len(n), txt = pop$ladnm)
    cell <- data.frame(col = seq_len(n), txt = poplab)
    ggplot() +
      geom_text(data = hdr,  aes(col, 2, label = txt), fontface = "bold",
                size = 4.2, angle = 0) +
      geom_text(data = cell, aes(col, 1, label = txt), size = 4.2) +
      annotate("text", x = 0, y = 2, label = "LAD", fontface = "bold",
               hjust = 1, size = 4.2) +
      annotate("text", x = 0, y = 1, label = "Population", fontface = "bold",
               hjust = 1, size = 4.2) +
      scale_x_continuous(limits = c(-1.6, n + 0.5)) +
      scale_y_continuous(limits = c(0.5, 2.6)) +
      labs(caption = "Population: all residents per LAD") +
      theme_void() +
      theme(plot.caption = element_text(size = 10, colour = "grey40", hjust = 0))
  }
  
  # ---- per-LAD x IMD-quintile % change bar grid for one scenario ------------
  make_lad_imd_grid <- function(scn) {
    st <- stat_used()
    d <- lad_imd_metric |>
      filter(metric == input$metric, stat == st,
             scenario %in% c(base_name, scn)) |>
      select(ladnm, imd_quintile, scenario, value) |>
      tidyr::pivot_wider(names_from = scenario, values_from = value)
    if (nrow(d) == 0) return(NULL)
    d$pct <- ifelse(is.na(d[[base_name]]) | d[[base_name]] == 0,
                    NA_real_, 100 * (d[[scn]] - d[[base_name]]) / d[[base_name]])
    d$dir <- ifelse(is.na(d$pct), "na", ifelse(d$pct >= 0, "up", "down"))
    ggplot(d, aes(factor(imd_quintile), pct, fill = dir)) +
      geom_col(width = 0.8) +
      geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
      facet_wrap(~ladnm, ncol = 2) +
      scale_fill_manual(values = c(up = "#b2182b", down = "#2166ac", na = "grey80"),
                        guide = "none") +
      labs(x = "IMD quintile (Q1 = most deprived)", y = "% change vs base",
           title = "% change by IMD quintile, per LAD") +
      theme_bw(base_size = 12) +
      theme(plot.title  = element_text(face = "bold", size = 16),
            strip.text  = element_text(face = "bold", size = 11),
            axis.title  = element_text(size = 11))
  }
  
  # ---- composite sheet: map (left) + per-LAD IMD grid (right) ---------------
  make_scenario_sheet <- function(scn) {
    st <- stat_used()
    map_p  <- make_change_plot(scn, show_title = FALSE)
    grid_p <- make_lad_imd_grid(scn)
    if (is.null(grid_p)) return(make_change_plot(scn))
    (map_p | grid_p) +
      patchwork::plot_layout(widths = c(1, 1)) +
      patchwork::plot_annotation(
        title = sprintf("%s (%s) - %s vs base",
                        metric_labels[[input$metric]], stat_labels[[st]],
                        scn_label(scn)),
        theme = theme(plot.title = element_text(face = "bold", size = 24,
                                                hjust = 0.5)))
  }
  
  # ---- LSOA detail sheet: scenario chart + all-metrics table ---------------
  output$dl_lsoa_png <- downloadHandler(
    filename = function() {
      id <- input$lsoa_pick
      paste0("lsoa_", if (is.null(id) || id == "") "none" else id, ".png")
    },
    content = function(file) {
      id <- input$lsoa_pick
      validate(need(!is.null(id) && id != "", "Select an LSOA first"))
      st <- stat_used()
      nm <- lsoa_lad_lkp$lsoa21nm[match(id, lsoa_lad_lkp$lsoa21cd)]
      ladnm <- lsoa_lad_lkp$ladnm[match(id, lsoa_lad_lkp$lsoa21cd)]
      
      # --- locator map: GM with this LSOA highlighted ---
      lad_p <- sf::st_transform(lad, sf::st_crs(lsoa))
      hl <- lsoa[lsoa$lsoa21cd == id, ]
      hl_ctr <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(hl)))
      ctr_xy <- as.data.frame(sf::st_coordinates(hl_ctr))
      locator <- ggplot() +
        geom_sf(data = lsoa, fill = "grey92", colour = NA) +
        geom_sf(data = lad_p, fill = NA, colour = "grey55", linewidth = 0.3) +
        geom_sf(data = hl, fill = "#b2182b", colour = "#b2182b") +
        geom_point(data = ctr_xy, aes(X, Y), shape = 21, size = 9,
                   stroke = 1.4, colour = "#b2182b", fill = NA) +
        labs(title = "Location in Greater Manchester") +
        theme_void(base_size = 16) +
        theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5))
      
      # --- chart: SELECTED metric/stat across scenarios (big fonts) ---
      cd <- long |>
        filter(lsoa21cd == id, metric == input$metric, stat == st) |>
        mutate(scenario = factor(scn_label(scenario), levels = scn_label(c(base_name, other_scn))))
      bval <- cd$value[cd$scenario == scn_label(base_name)]
      cd$dir <- ifelse(cd$scenario == scn_label(base_name), "base",
                       ifelse(cd$value >= bval, "up", "down"))
      cd$lbl <- formatC(cd$value, digits = 3, format = "g")
      chart <- ggplot(cd, aes(scenario, value, fill = dir)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = lbl), vjust = -0.4, size = 9, fontface = "bold") +
        geom_hline(yintercept = bval, linetype = "dashed", colour = "grey40") +
        scale_fill_manual(values = c(base = "#888888", up = "#b2182b", down = "#2166ac"),
                          guide = "none") +
        labs(x = NULL, y = NULL,
             title = metric_labels[[input$metric]],
             subtitle = stat_labels[[st]]) +
        theme_bw(base_size = 20) +
        theme(plot.title    = element_text(face = "bold", size = 24),
              plot.subtitle = element_text(size = 19, colour = "grey30"),
              axis.text.x = element_text(size = 20, face = "bold"),
              axis.text.y = element_text(size = 17),
              panel.grid.major.x = element_blank()) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.12)))
      
      # --- demographic profile strip ---
      pr <- prof[match(id, prof$lsoa21cd), ]
      prof_txt <- sprintf(
        "Population %s    Age <18 %.0f%% | 18-24 %.0f%% | 25-44 %.0f%% | 45-65 %.0f%% | 66+ %.0f%%    %.0f%% male / %.0f%% female    IMD quintile %d",
        formatC(pr$n_people, big.mark = ",", format = "d"),
        pr$pct_u18, pr$pct_18_24, pr$pct_25_44, pr$pct_45_65, pr$pct_66plus,
        pr$pct_male, pr$pct_female, as.integer(ceiling(pr$imd10 / 2)))
      strip <- ggplot() +
        annotate("text", x = 0.5, y = 0, label = prof_txt, size = 7) +
        theme_void()
      
      # layout: top row = locator | chart ; then profile strip
      sheet <- (locator | chart) / strip +
        patchwork::plot_layout(heights = c(5, 0.6)) +
        patchwork::plot_annotation(
          title = sprintf("%s  -  %s  (%s)", nm, ladnm, id),
          theme = theme(plot.title = element_text(face = "bold", size = 30, hjust = 0.5)))
      ggsave(file, sheet, width = 16, height = 10, dpi = 200, bg = "white")
    }
  )
  
  # faceted: all non-base scenarios side by side
  output$dl_png <- downloadHandler(
    filename = function() paste0("change_", input$metric, "_", stat_used(), ".png"),
    content = function(file) {
      combined <- make_change_plot(other_scn) / make_lad_pop_table() +
        patchwork::plot_layout(heights = c(5, 0.9))
      ggsave(file, combined, width = 16, height = 7.5, dpi = 200, bg = "white")
    }
  )
  
  # current scenario + IMD selection, tracked reactively so the download
  # always reflects the live control values (avoids stale-input on download)
  imd_dl_params <- reactive({
    list(
      scn = if (is.null(input$cmp_scenario)) other_scn[1] else input$cmp_scenario,
      q   = if (is.null(input$imd_filter)) "all" else input$imd_filter
    )
  })
  
  # single scenario + current IMD filter (mirrors the on-screen view)
  # LAD Q1 vs Q5 side-by-side map (uses LAD picker + current scenario)
  output$dl_lad_q1q5 <- downloadHandler(
    filename = function() {
      lc <- input$lsoa_lad
      scn <- if (is.null(input$cmp_scenario)) other_scn[1] else input$cmp_scenario
      paste0("lad_Q1Q5_", input$metric, "_", stat_used(), "_",
             if (is.null(lc) || lc == "") "none" else lc, "_", scn, ".png")
    },
    content = function(file) {
      lc <- input$lsoa_lad
      scn <- if (is.null(input$cmp_scenario)) other_scn[1] else input$cmp_scenario
      if (is.null(lc) || lc == "") {
        # no LAD chosen yet: produce a clear message instead of a broken file
        msg <- ggplot() +
          annotate("text", x = 0, y = 0,
                   label = "Pick a LAD in '1. Pick LAD' first,\nthen click this button.",
                   size = 8, fontface = "bold", colour = "grey30") +
          theme_void()
        ggsave(file, msg, width = 16, height = 8, dpi = 120, bg = "white")
        return()
      }
      ggsave(file, make_lad_q1q5(scn, lc),
             width = 16, height = 8, dpi = 200, bg = "white")
    }
  )
  
  output$dl_png_imd <- downloadHandler(
    filename = function() {
      p <- imd_dl_params()
      paste0("change_", input$metric, "_", stat_used(), "_", p$scn,
             if (p$q != "all") paste0("_IMDq", p$q) else "", ".png")
    },
    content = function(file) {
      p <- imd_dl_params()
      ggsave(file, make_change_plot(p$scn, imd_q = p$q),
             width = 11, height = 9, dpi = 200, bg = "white")
    }
  )
  
  # one composite sheet (map + per-LAD IMD grid) per non-base scenario
  output$dl_png_s1 <- downloadHandler(
    filename = function() paste0("change_", input$metric, "_", stat_used(),
                                 "_", other_scn[1], ".png"),
    content = function(file)
      ggsave(file, make_change_plot(other_scn[1]),
             width = 10, height = 9, dpi = 200, bg = "white")
  )
  output$dl_png_s2 <- downloadHandler(
    filename = function() paste0("change_", input$metric, "_", stat_used(),
                                 "_", other_scn[2], ".png"),
    content = function(file)
      ggsave(file, make_change_plot(other_scn[2]),
             width = 10, height = 9, dpi = 200, bg = "white")
  )
  output$dl_png_s3 <- downloadHandler(
    filename = function() paste0("change_", input$metric, "_", stat_used(),
                                 "_", other_scn[3], ".png"),
    content = function(file)
      ggsave(file, make_change_plot(other_scn[3]),
             width = 10, height = 9, dpi = 200, bg = "white")
  )
}

shinyApp(ui, server)