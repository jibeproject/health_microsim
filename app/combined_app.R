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
  library(qs2)
  library(DT)
  library(gt)
  library(gtExtras)
  library(bslib)
  library(matrixStats)
  library(shinyWidgets)
})

# ---- Data location -------------------------------------------------------
# 2026-08 FIX. Paths were hard-coded as "app/data/...", which fails under
#   shiny::runApp("app/combined_app.R")
# because runApp() sets the working directory to the app folder FIRST, so
# "app/data/x" resolves to app/app/data/x. Writing "data/..." instead breaks
# the opposite case, sourcing the file from the project root. This helper
# accepts either, and says where it looked when it cannot find the file.
data_path <- function(f) {
  cands <- c(
    file.path("data", f),                    # wd = the app folder (runApp)
    file.path("app", "data", f),             # wd = the project root
    file.path("..", "app", "data", f)        # wd = a sibling of app/
  )
  hit <- cands[file.exists(cands)]
  if (!length(hit))
    stop("cannot find '", f, "'. Looked in:\n  ",
         paste(normalizePath(cands, mustWork = FALSE), collapse = "\n  "),
         "\nWorking directory is: ", getwd(), call. = FALSE)
  hit[1]
}

pc <- qs2::qs_read(data_path("precomputed_100%V6.qs2"))

SCALING <- 1L

# ---- Display labels ------------------------------------------------------
# Areas and their constituent districts. MUST match AREA_MAP in
# process_all_data.R and process_exp.R.
AREA_DISTRICTS <- c(
  "City core"       = "Manchester, Salford",
  "East"            = "Oldham, Rochdale, Tameside",
  "South"           = "Stockport, Trafford",
  "West/North-west" = "Bolton, Wigan, Bury"
)

# "City core" -> "City core (Manchester, Salford)". Leaves unknown values be.
label_area <- function(x) {
  x <- as.character(x)
  d <- unname(AREA_DISTRICTS[x])
  ifelse(is.na(d), x, paste0(x, " (", d, ")"))
}

IMD_NOTE <- "IMD = Index of Multiple Deprivation (quintiles; 1 = most deprived)"

# Small grey line stating which stored object a view came from and what the
# app did to it. Written because "where does this number come from" was not
# answerable from the interface.
prov <- function(txt) div(
  style = paste("padding:2px 4px 8px 4px; color:#888; font-size:0.78em;",
                "font-style:italic;"),
  paste("Source:", txt))

# These are proportions of the population, not distributions over people, so
# quantiles are meaningless for them: report the mean as a prevalence and plot
# it as a bar rather than a box.
PREVALENCE_VARS <- c("Highly annoyed due to noise",
                     "Highly sleep disturbed due to noise")

# Scenario display names. DISPLAY ONLY -- the underlying values ("reference",
# "goDutch", ...) drive all the filtering and difference logic, so these are
# applied at plot/table time, never to the data used for computation.
SCEN_LABELS <- c(reference = "Reference", goDutch = "Go Dutch",
                 safeStreet = "Safe Streets", green = "Greening")
# Canonical display order, used everywhere: reference first, then scenarios.
SCEN_ORDER     <- unname(SCEN_LABELS)
SCEN_ORDER_RAW <- names(SCEN_LABELS)

# Returns an ORDERED FACTOR so plots and wide tables both follow SCEN_ORDER.
# Call sites that need a plain string (column names, all_of()) wrap this in
# as.character() -- assigning a factor into a character vector would otherwise
# insert the integer codes.
label_scen <- function(x) {
  x <- as.character(x); l <- unname(SCEN_LABELS[x])
  out <- ifelse(is.na(l), x, l)
  factor(out, levels = union(SCEN_ORDER, unique(out)))
}

# Raw scenario values in canonical order, for pickers and column sorting.
order_scen_raw <- function(x) x[order(match(x, SCEN_ORDER_RAW, nomatch = 99L))]

# "coronary_heart_disease" -> "Coronary heart disease"; keeps the death labels
# that are already human-readable untouched.
label_cause <- function(x) {
  x <- as.character(x)
  out <- gsub("_", " ", x)
  out <- paste0(toupper(substr(out, 1, 1)), substr(out, 2, nchar(out)))
  # acronyms that sentence-case would mangle
  out <- gsub("\\bCopd\\b", "COPD", out)
  out <- gsub("\\bNdvi\\b", "NDVI", out)
  # already-formatted labels such as "Death (car)" pass through unchanged
  ifelse(grepl("^Death \\(", x) | grepl("^[A-Z]", x), x, out)
}

# Exposure variable names -> readable labels. Unrecognised names fall through
# with the "exposure_" prefix stripped rather than being blanked.
label_exposure <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x == "total_PA"                       ~ "mMET hours per week",
    grepl("noise_HA$",  x, ignore.case = TRUE) ~ "Highly annoyed due to noise",
    grepl("noise_HSD$", x, ignore.case = TRUE) ~ "Highly sleep disturbed due to noise",
    grepl("lden",       x, ignore.case = TRUE) ~ "Lden",
    grepl("ndvi",       x, ignore.case = TRUE) ~ "NDVI",
    grepl("no2",        x, ignore.case = TRUE) ~ "NO2",
    grepl("pm25|pm2\\.5", x, ignore.case = TRUE) ~ "PM2.5",
    TRUE ~ sub("^exposure_", "", x)
  )
}

exp <- qs2::qs_read(data_path("exp_050826.qs2"))

trips <- qs2::qs_read(data_path("trips_200826.qs2"))

# ---- Distance and time tables ---------------------------------------------
# The file stores these at fine grain rather than one table per view, so the
# roll-up happens here. Every metric is expressed as a NUMERATOR and a
# DENOMINATOR; each view then sums both and divides. That way a view is always
# a properly weighted mean and never an average of averages, which would count
# a cell of 40 people the same as one of 40,000.
#
#   summary_distance / summary_duration carry the sums and the person count
#   (np) directly, so those roll up exactly.
#
#   avg_trip_dist / avg_trip_time carry only a mean, with no denominator, so
#   the trip counts are recovered from trips_percentage (summed over
#   t.purpose, which puts it at exactly the same grain) and used as weights.
#   This is a close approximation rather than an identity: the stored mean was
#   taken over trip RECORDS whereas trip_count sums t.factor, so a home-based
#   record counts twice. See the caveat in the provenance note.
local({
  need <- c("summary_distance", "summary_duration", "avg_trip_dist",
            "avg_trip_time", "trips_percentage")
  if (!all(need %in% names(trips))) return(invisible(NULL))
  
  trip_w <- trips$trips_percentage |>
    dplyr::group_by(scen, LAD_origin, imd_origin, gender, agegroup, mode) |>
    dplyr::summarise(den = sum(trip_count, na.rm = TRUE), .groups = "drop")
  
  trips$wdist_fine <<- trips$summary_distance |>
    dplyr::transmute(scen, gender, agegroup, imd5, LAD_group, mode,
                     num = sumDistance, den = np)
  
  # x60: summary_duration is stored in hours (process_trips.R divides the raw
  # times by 60), but weekly totals read better in minutes.
  trips$wtime_fine <<- trips$summary_duration |>
    dplyr::transmute(scen, gender, agegroup, imd5, LAD_group, mode,
                     num = sumDuration * 60, den = np)
  
  trips$tdist_fine <<- trips$avg_trip_dist |>
    dplyr::left_join(trip_w, by = c("scen", "LAD_origin", "imd_origin",
                                    "gender", "agegroup", "mode")) |>
    dplyr::transmute(scen, gender, agegroup, imd_origin, LAD_origin, mode,
                     num = avgDistance * den, den = den)
  
  # x60: stored durations are in hours, which reads badly for one trip.
  trips$ttime_fine <<- trips$avg_trip_time |>
    dplyr::left_join(trip_w, by = c("scen", "LAD_origin", "imd_origin",
                                    "gender", "agegroup", "mode")) |>
    dplyr::transmute(scen, gender, agegroup, imd_origin, LAD_origin, mode,
                     num = avgTime * 60 * den, den = den)
})

# ---- Travel ---------------------------------------------------------------
# The regenerated trips.qs2 stores one table per (metric x view), with the
# share already computed against the correct denominator. Nothing here
# recomputes a percentage: each view is a direct read of the stored column.
#
# `tables` maps a View-by level to its table, `gvars` to the column that level
# is keyed on. Where a view is absent from a metric, the data does not exist.
TRIP_SPEC <- list(
  "Mode share (%)" = list(
    tables = c(Overall = "mode_share_overall", Gender = "mode_share_gender",
               Agegroup = "mode_share_age",    LAD    = "mode_share_lad",
               IMD      = "mode_share_imd"),
    gvars  = c(Overall = NA,      Gender = "gender", Agegroup = "agegroup",
               LAD     = "LAD_origin", IMD = "imd_origin"),
    val = "percentage_of_trips", xvar = "mode", stacked = TRUE, agg = "direct",
    ylab = "Share of trips (%)"),
  
  # No LAD view: distance_mode_share_lad is not in the file. Listing it here
  # would drop the WHOLE metric, because the availability filter below
  # requires every named table to be present.
  "Mode share within distance band (%)" = list(
    tables = c(Overall = "distance_mode_share",
               Gender  = "distance_mode_share_gender",
               Agegroup= "distance_mode_share_age",
               IMD     = "distance_mode_share_imd"),
    gvars  = c(Overall = NA, Gender = "gender", Agegroup = "agegroup",
               IMD     = "imd5"),
    val = "percent", xvar = "mode", facet2 = "distance_bracket",
    stacked = TRUE, agg = "direct",
    ylab = "Share of trips in band (%)"),
  
  # Derived, because neither direction of this share is stored:
  # distance_mode_share_* normalises WITHIN a band (mode composition of short
  # trips), whereas this normalises within a MODE (how long are cycling trips?).
  # The latter is what shows where new cycling trips come from -- if they are
  # 1-3 km they replaced walking, if 5-10 km they replaced driving.
  # No LAD view: distance_counts has no LAD column.
  "Trip distance by mode (%)" = list(
    tables = c(Overall = "distance_counts", Gender = "distance_counts",
               Agegroup = "distance_counts", IMD = "distance_counts"),
    gvars  = c(Overall = NA, Gender = "gender", Agegroup = "agegroup",
               IMD     = "imd5"),
    val = "weighted_count", xvar = "distance_bracket", facet2 = "mode",
    agg = "share", ylab = "Share of that mode's trips (%)"),
  
  # ---- Distance and time ---------------------------------------------------
  # agg = "ratio": each view sums the numerator and denominator over the cells
  # it covers and divides, giving a correctly weighted mean at every level.
  #
  # Per-person metrics are keyed on the traveller's RESIDENCE (LAD_group,
  # imd5); per-trip metrics on the trip's ORIGIN (LAD_origin, imd_origin),
  # because that is the grain each source table was built at.
  "Weekly distance per person (km)" = list(
    tables = c(Overall = "wdist_fine", Gender = "wdist_fine",
               Agegroup = "wdist_fine", LAD = "wdist_fine", IMD = "wdist_fine"),
    gvars  = c(Overall = NA, Gender = "gender", Agegroup = "agegroup",
               LAD     = "LAD_group", IMD = "imd5"),
    xvar = "mode", agg = "ratio", num = "num", den = "den",
    ylab = "km per person per week"),
  
  "Weekly travel time per person (minutes)" = list(
    tables = c(Overall = "wtime_fine", Gender = "wtime_fine",
               Agegroup = "wtime_fine", LAD = "wtime_fine", IMD = "wtime_fine"),
    gvars  = c(Overall = NA, Gender = "gender", Agegroup = "agegroup",
               LAD     = "LAD_group", IMD = "imd5"),
    xvar = "mode", agg = "ratio", num = "num", den = "den",
    ylab = "minutes per person per week"),
  
  "Average trip distance (km)" = list(
    tables = c(Overall = "tdist_fine", Gender = "tdist_fine",
               Agegroup = "tdist_fine", LAD = "tdist_fine", IMD = "tdist_fine"),
    gvars  = c(Overall = NA, Gender = "gender", Agegroup = "agegroup",
               LAD     = "LAD_origin", IMD = "imd_origin"),
    xvar = "mode", agg = "ratio", num = "num", den = "den",
    ylab = "km per trip"),
  
  "Average trip duration (minutes)" = list(
    tables = c(Overall = "ttime_fine", Gender = "ttime_fine",
               Agegroup = "ttime_fine", LAD = "ttime_fine", IMD = "ttime_fine"),
    gvars  = c(Overall = NA, Gender = "gender", Agegroup = "agegroup",
               LAD     = "LAD_origin", IMD = "imd_origin"),
    xvar = "mode", agg = "ratio", num = "num", den = "den",
    ylab = "minutes per trip")
)

# Back-compat: older specs used direct = TRUE/FALSE rather than agg.
for (nm in names(TRIP_SPEC)) {
  if (is.null(TRIP_SPEC[[nm]]$agg))
    TRIP_SPEC[[nm]]$agg <- if (isTRUE(TRIP_SPEC[[nm]]$direct)) "direct" else "share"
}

# keep only metrics whose tables are present in the file
TRIP_SPEC <- TRIP_SPEC[vapply(TRIP_SPEC,
                              function(x) all(unique(x$tables) %in% names(trips)), logical(1))]
for (nm in names(TRIP_SPEC)) TRIP_SPEC[[nm]]$views <- names(TRIP_SPEC[[nm]]$tables)

# Distance bands sort alphabetically ("10-20" before "3-5") unless the factor
# levels are set. Enforced here rather than relying on the prep's levels
# surviving serialisation.
DIST_LEVELS <- c("0-1", "1-3", "3-5", "5-10", "10-20", "20-40", "40+")
order_dist <- function(x) factor(as.character(x),
                                 levels = union(DIST_LEVELS, sort(unique(as.character(x)))))

trip_group_var <- function(view, spec) {
  g <- spec$gvars[[view]]
  if (is.null(g) || is.na(g)) character(0) else g
}

# Relabel once at load so every downstream use (plot, table, CSV) agrees.
#
# 2026-08 FIX. The relabel below rewrites `grouping` into a display string:
#   "LADNM: City core" -> "City core (Manchester, Salford)"
#   "Gender: 1"        -> "Male"
# Downstream the views were selected with grepl() on that display string, so
# grepl("LAD", grouping) and grepl("Gender", grouping) matched NOTHING once
# the prefixes had been stripped -- the LAD and Gender exposure views came
# back empty and req(nrow(lexp) > 0) blanked the panel. IMD and Agegroup
# survived only because "IMD" and "Age" happen to remain in their labels.
#
# The fix is to carry the grouping TYPE and the raw area KEY as their own
# columns, so filtering never depends on how a label happens to read:
#   group_type -- "Overall" / "Gender" / "LAD" / "IMD" / "Agegroup", matching
#                 the values of input$view_level exactly
#   area_key   -- the bare area name ("City core"), for matching against
#                 input$lad_sel, whose choices come from pc$people_lad$ladnm
exp <- exp |>
  dplyr::mutate(
    variable = label_exposure(variable),
    group_type = dplyr::case_when(
      grepl("^LADNM:",    grouping) ~ "LAD",
      grepl("^IMD:",      grouping) ~ "IMD",
      grepl("^Agegroup:", grouping) ~ "Agegroup",
      grepl("^Gender:",   grouping) ~ "Gender",
      grepl("Overall",    grouping) ~ "Overall",
      TRUE                          ~ "Other"
    ),
    area_key = dplyr::if_else(
      group_type == "LAD",
      trimws(sub("^LADNM:", "", grouping)),
      NA_character_
    ),
    # display label, built last so the two columns above see the original value
    grouping = dplyr::case_when(
      group_type == "LAD"      ~ label_area(trimws(sub("^LADNM:",    "", grouping))),
      group_type == "IMD"      ~ paste("IMD", trimws(sub("^IMD:",    "", grouping))),
      group_type == "Agegroup" ~ paste("Age", trimws(sub("^Agegroup:", "", grouping))),
      grouping == "Gender: 1"  ~ "Male",
      grouping == "Gender: 2"  ~ "Female",
      TRUE ~ grouping
    )
  )




MIN_CYCLE <- 1
MAX_CYCLE <- max(pc$people_overall$cycle)

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
all_scenarios <- order_scen_raw(unique(pc$people_overall$scen))
pop_cycles    <- sort(unique(pc$people_overall$cycle))
trend_cycles  <- sort(unique(pc$asr_overall_all$cycle))
all_lads_nm   <- sort(unique(pc$people_lad$ladnm))

# Fail loudly at load rather than rendering a blank panel later. Placed HERE
# and not next to the exp relabel above, because it needs all_lads_nm, which
# is defined on the line above -- running it earlier threw
#   Error in eval(quote({ : object 'all_lads_nm' not found
local({
  if (!"group_type" %in% names(exp)) {
    warning("exp has no group_type column -- the relabel block did not run.")
    return(invisible(NULL))
  }
  if (sum(exp$group_type == "LAD") == 0)
    warning("exp has no LAD groupings -- the LAD exposure view will be empty. ",
            "Check the grouping prefixes written by process_exp.R.")
  unmatched <- setdiff(unique(stats::na.omit(exp$area_key)), all_lads_nm)
  if (length(unmatched))
    warning("exposure areas not in pc$people_lad$ladnm: ",
            paste(unmatched, collapse = ", "),
            " -- the Area(s) picker will not match these.")
})
all_genders   <- sort(unique(pc$people_gender$gender))
all_causes_asr <- pc$asr_overall_all |> distinct(cause) |> filter(!grepl("dead", cause)) |> pull() |> sort() #
all_causes_except_dead <- pc$asr_overall_all |> distinct(cause) |> filter(!grepl("dead", cause)) |> pull() |> sort()
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
      selectizeInput("lad_sel", "Area(s):",
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
      condition = "input.main_tabs == 'Differences vs reference' || input.main_tabs == 'Differences per 100,000'",
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
        condition = "input.main_tabs == 'Differences vs reference' || input.main_tabs == 'Differences per 100,000'",
        radioButtons("metric_kind", "Metric:",
                     choices = c(
                       "Deaths postponed"             = "deaths_av",
                       "Life years gained"          = "life_years",
                       "Diseases/injuries prevented"  = "dis_av"
                     ),
                     selected = "deaths_av"),
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
        selectInput(
          "asr_mode",
          "ASR view:",
          choices = setNames(
            c("bars", "avg"),
            c(paste0("Average 1-", MAX_CYCLE, " (bars)"),
              paste0("Average 1-", MAX_CYCLE, " (table)"))
          ),
          selected = "bars"
        ),
        conditionalPanel(
          condition = "input.asr_mode == 'bars'",
          checkboxInput("asr_pct", "Show % difference vs reference", value = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'Travel'",
        selectInput("trip_metric", "Travel metric:",
                    choices = names(TRIP_SPEC),
                    selected = names(TRIP_SPEC)[1]),
        checkboxInput("trip_show_table", "Show table", value = FALSE),
        checkboxInput("trip_pct", "Show change vs reference (points and %)", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'Exposures'",
        selectInput("exp_var",  "Exposure variable:", choices = NULL),
        selectInput("exp_year", "Year:",              choices = NULL),
        checkboxInput("exp_show_table", "Show full table", value = FALSE),
        checkboxInput("exp_pct", "Show % change vs reference", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'Age Standardised Rates' || ((input.main_tabs == 'Differences vs reference' || input.main_tabs == 'Differences per 100,000') && 
          input.metric_kind == 'dis_av')",
        shinyWidgets::pickerInput("asr_causes", "Causes:", choices =  append(all_causes_asr, death_values),
                                  selected = c("coronary_heart_disease","stroke"),
                                  multiple = TRUE,
                                  options = list(
                                    `actions-box` = TRUE,
                                    `deselect-all-text` = "None",
                                    `select-all-text` = "Select all",
                                    `none-selected-text` = "zero"
                                  ))#,
      )
      
    ),
    tags$hr(),
    downloadButton("download_csv", "Download current table (CSV)")
  ),
  navset_card_underline(
    id = "main_tabs",
    full_screen = TRUE,
    nav_panel("Travel",
              uiOutput("trip_key_note"),
              plotlyOutput("trip_plot", height = "480px"),
              gt_output("trip_table")
    ),
    nav_panel("Personal exposures",
              value = "Exposures",
              uiOutput("exp_area_note"),
              plotlyOutput("exp_plot", height = "420px"),
              gt_output("plot_exp")
    ),
    # The three health views sit under a "Health" heading.
    nav_menu(
      "Health",
      nav_panel("Differences vs reference",
                uiOutput("diff_metric_note"),
                uiOutput("group_key_note"),
                uiOutput("table_diff_summary", height = "100vh"),
      ),
      nav_panel("Differences per 100,000",
                uiOutput("diff_rate_note"),
                uiOutput("group_key_note2"),
                uiOutput("table_diff_rate", height = "100vh"),
      ),
      nav_panel("Age Standardised Rates",
                uiOutput("asr_key_note"),
                uiOutput("plot_asrly")#, height = "85vh"),
      )
    ),
    # ---- Hidden, not deleted -------------------------------------------
    # Commented out at the request of the user; all supporting server code
    # (get_onset_ages, pop_data, output$table_avg, output$plot_poply) is
    # untouched, so restoring these is a matter of uncommenting.
    #
    # ,nav_panel("Average onset ages",
    #           gt_output("table_avg")
    # )
    # ,nav_panel("Population",
    #           plotlyOutput("plot_poply")
    # )
  )
)


# ------------------- SERVER --------------------------------------------
server <- function(input, output, session) {
  
  get_normalized_table <- function(df){
    scen_cols <- all_scenarios
    
    if (length(input$scen_sel))
      scen_cols <- input$scen_sel
    
    # Canonical order (Reference first) regardless of pick order
    scen_cols <- order_scen_raw(intersect(scen_cols, names(df)))
    
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
    list(input$main_tabs, input$trip_metric)
  }, {
    req(input$main_tabs)
    
    current <- isolate(input$view_level)
    
    if (input$main_tabs %in% c("Differences vs reference", "Differences per 100,000", "Age Standardised Rates")) {
      new_choices <- c(selected_views, additional_selected_views)
    } else if (input$main_tabs %in% c("Population", "Average onset ages")) {
      new_choices <- selected_views
    } else if (input$main_tabs == "Travel") {
      # Only offer the views this metric's table can actually support: the
      # avg_trip_* tables have no gender/agegroup/IMD columns at all.
      sp <- TRIP_SPEC[[input$trip_metric %||% names(TRIP_SPEC)[1]]]
      new_choices <- if (is.null(sp)) selected_views else sp$views
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
      shinyWidgets::pickerInput("avg_cause", 
                                "Disease (onset):",
                                choices = all_causes_except_dead,
                                selected = "coronary_heart_disease",
                                multiple = TRUE,
                                options = list(
                                  `actions-box` = TRUE,
                                  `deselect-all-text` = "None",
                                  `select-all-text` = "Select all",
                                  `none-selected-text` = "zero"
                                ))
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
      labs(title = "Population by age group", x = "Age group", y = pd$y_lab) +
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
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  # Wording shown under the differences plots, so the sign convention and the
  # denominator are explicit rather than implied.
  metric_note <- reactive({
    base <- switch(input$metric_kind,
                   deaths_av  = "Number of deaths postponed in the selected scenario/s and for the simulation period compared with reference.",
                   life_years = paste(
                     "Number of life years and healthy life years gained in the selected scenario/s and for the simulation period compared with reference.",
                     "A life year is one person alive for one cycle, summed across everyone in the population -- so this is the total extra years of life lived across the whole cohort, not a gain per person.",
                     "Healthy life years count only the cycles spent in the healthy state, i.e. before any of the modelled conditions has occurred."),
                   dis_av     = "Number of diseases/injuries prevented in the selected scenario/s and for the simulation period compared with reference. Includes road deaths (car, cyclist, pedestrian); all-cause deaths are shown under the Deaths postponed metric.",
                   "")
    # State the cycle window and aggregation explicitly -- a chart pasted into
    # a slide otherwise gives no clue which cycles it covers.
    rng <- paste0("Cycles ", input$diff_min_cycle, "-", MAX_CYCLE, ", ",
                  if (isTRUE(input$diff_cumulative))
                    "summed over cycles (total for the period)."
                  else
                    "median difference in a single cycle (not a total).")
    base <- paste(base, rng)
    
    if (identical(input$main_tabs, "Differences per 100,000")) {
      unit <- if (isTRUE(input$diff_cumulative))
        " Expressed per 100,000 person-years of reference exposure (reference population summed over the selected cycles)."
      else
        " Expressed per 100,000 people per cycle (mean reference population over the selected cycles)."
      base <- paste0(base, unit)
      if (identical(input$metric_kind, "life_years"))
        base <- paste(base,
                      "Note that life years are themselves person-years, so this figure is close to a proportional change in years lived rather than a rate of events per person-year.")
    }
    base
  })
  
  # Explains the grouping in use: IMD spelled out, or the districts in each
  # area. Shown on all three health tabs.
  group_key <- reactive({
    if (identical(input$view_level, "IMD")) IMD_NOTE
    else if (identical(input$view_level, "LAD"))
      paste0("Areas: ",
             paste(paste0(names(AREA_DISTRICTS), " (", AREA_DISTRICTS, ")"),
                   collapse = "; "))
    else ""
  })
  key_ui <- function() {
    n <- group_key(); if (!nzchar(n)) return(NULL)
    div(style = "padding:2px 4px 8px 4px; color:#666; font-size:0.85em;", n)
  }
  output$group_key_note  <- renderUI({ key_ui() })
  output$group_key_note2 <- renderUI({ key_ui() })
  output$asr_key_note <- renderUI({
    tagList(key_ui(),
            prov(paste("pc$asr_* (ESP2013-standardised in process_all_data.R).",
                       "Rates are stored values; the % labels are computed here as",
                       "(scenario - reference) / reference x 100.")))
  })
  
  diff_source <- reactive({
    el <- switch(input$metric_kind,
                 deaths_av  = "pc$deaths_* (all-cause 'dead' only)",
                 life_years = "pc$lifey_* and pc$healthy_*",
                 dis_av     = "pc$diseases_* plus pc$deaths_injury_* (road deaths)",
                 "pc")
    paste0(el, ", column 'diff' from diff_vs_reference() in process_all_data.R. ",
           if (isTRUE(input$diff_cumulative)) "Summed over the selected cycles."
           else "Median across the selected cycles.",
           if (input$metric_kind %in% c("deaths_av", "dis_av"))
             " Sign reversed so a reduction reads as a positive count." else "",
           if (identical(input$main_tabs, "Differences per 100,000"))
             " Then divided by reference person-time from pc$people_*." else "")
  })
  output$diff_metric_note <- renderUI({
    n <- metric_note(); if (!nzchar(n)) return(NULL)
    tagList(div(style = "padding:8px 4px 2px 4px; color:#555; font-size:0.9em;", n),
            prov(diff_source()))
  })
  output$diff_rate_note <- renderUI({
    n <- metric_note(); if (!nzchar(n)) return(NULL)
    tagList(div(style = "padding:8px 4px 2px 4px; color:#555; font-size:0.9em;", n),
            prov(diff_source()))
  })
  
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
    
    # Road deaths (car / cyclist / pedestrian) belong with injuries, so they
    # are appended to the diseases series. They already carry a `cause` column
    # in the same shape. All-cause "dead" is NOT appended -- it has its own
    # metric, and including it here would double-count these three.
    dijl <- pc$deaths_injury_lad
    if (length(input$lad_sel)) dijl <- dijl |> filter(ladnm %in% input$lad_sel)
    add_inj <- function(dis, inj) if (is.null(inj)) dis else plyr::rbind.fill(dis, inj)
    do   <- add_inj(do,   pc$deaths_injury_overall)
    dg   <- add_inj(dg,   pc$deaths_injury_gender)
    dimd <- add_inj(dimd, pc$deaths_injury_imd)
    dil  <- add_inj(dil,  dijl)
    
    if (length(input$asr_causes)){
      do <- do |> filter(cause %in% input$asr_causes)
      dg <- dg |> filter(cause %in% input$asr_causes)
      dimd <- dimd |> filter(cause %in% input$asr_causes)
      dil <- dil |> filter(cause %in% input$asr_causes)
    }
    
    # Deaths and diseases are reported as postponed/prevented, so the raw difference
    # (scenario - reference, negative when the scenario is better) is
    # multiplied by -1. Life years are reported as GAINED and keep their sign.
    pick <- switch(input$metric_kind,
                   deaths_av = list(Overall = pc$deaths_overall,
                                    Gender  = pc$deaths_gender,
                                    LAD     = dl,
                                    IMD     = pc$deaths_imd,
                                    label   = "Deaths postponed", sign = -1),
                   dis_av    = list(Overall = do, Gender = dg, LAD = dil, IMD = dimd,
                                    label   = "Diseases/injuries prevented", sign = -1),
                   life_years = list(
                     Overall = plyr::rbind.fill(
                       pc$lifey_overall   |> mutate(factor = "Life years gained"),
                       pc$healthy_overall |> mutate(factor = "Healthy life years gained")),
                     Gender = plyr::rbind.fill(
                       pc$lifey_gender   |> mutate(factor = "Life years gained"),
                       pc$healthy_gender |> mutate(factor = "Healthy life years gained")),
                     LAD = plyr::rbind.fill(
                       ll |> mutate(factor = "Life years gained"),
                       hl |> mutate(factor = "Healthy life years gained")),
                     IMD = plyr::rbind.fill(
                       pc$lifey_imd   |> mutate(factor = "Life years gained"),
                       pc$healthy_imd |> mutate(factor = "Healthy life years gained")),
                     label = "Life years gained", sign = 1))
    
    base <- pick[[view]]
    # deaths_av is a single all-cause series with no split column;
    # dis_av splits by `cause`; life_years splits by `factor`.
    key <- switch(input$metric_kind,
                  dis_av     = "cause",
                  life_years = "factor",
                  character(0))
    by <- switch(view,
                 Overall = key,
                 Gender  = c(key, "gender"),
                 LAD     = c(key, "ladnm"),
                 IMD     = c(key, "imd10"))
    
    req(!is.null(base))
    
    df <- base |> filter(cycle >= minc, scen %in% scen_keep); grp <- c("scen", by)
    
    if ("gender" %in% names(df)) {
      df <- df |> 
        mutate(gender = case_when(gender == 1 ~ "Male",
                                  gender == 2 ~ "Female")) 
    }
    
    # Append baseline population to the grouping labels, so a difference can be
    # read against the population it came from. Derived from the people_*
    # objects ALREADY in the cache (no prep rerun needed): take the earliest
    # cycle of the reference scenario and sum over age groups.
    # ---- Person-time denominator (per-100,000 tab) -----------------------
    # Denominator is REFERENCE person-time, deliberately: the intervention
    # changes survival, so dividing each scenario by its own person-time would
    # put the effect in the denominator as well as the numerator and partly
    # cancel it. Reference person-time is a common yardstick across scenarios.
    # Summed over the SAME cycles as the numerator (cycle >= minc).
    persontime_of <- function(tbl, grp) {
      if (is.null(tbl) || !all(c("scen", "cycle", "pop") %in% names(tbl))) return(NULL)
      per_cycle <- tbl |>
        filter(scen == "reference", cycle >= minc) |>
        group_by(across(all_of(c(grp, "cycle")))) |>
        summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
      # cumulative numerator (a sum over cycles) -> person-YEARS
      # median numerator (a per-cycle value)     -> MEAN population per cycle
      per_cycle |>
        group_by(across(all_of(grp))) |>
        summarise(.pt = if (cumu) sum(pop, na.rm = TRUE) else mean(pop, na.rm = TRUE),
                  .groups = "drop")
    }
    
    if ("imd10" %in% names(df)) {
      pt <- persontime_of(pc$people_imd, "imd10")
      if (!is.null(pt)) df <- df |> left_join(pt, by = "imd10")
    } else if ("ladnm" %in% names(df)) {
      pt <- persontime_of(pc$people_lad, "ladnm")
      if (!is.null(pt)) df <- df |> left_join(pt, by = "ladnm")
    } else if ("gender" %in% names(df)) {
      pt <- persontime_of(pc$people_gender, "gender")
      if (!is.null(pt)) {
        pt <- pt |> mutate(gender = case_when(gender == 1 ~ "Male",
                                              gender == 2 ~ "Female"))
        df <- df |> left_join(pt, by = "gender")
      }
    }
    
    # TRUE when the per-100,000 tab is active. The same reactive serves both
    # tabs; only the final scaling differs, so all downstream plotting and
    # export code is shared.
    rate_mode <- identical(input$main_tabs, "Differences per 100,000")
    
    fmt_pop <- function(x) format(round(x), big.mark = ",", trim = TRUE)
    baseline_of <- function(tbl, grp) {
      if (is.null(tbl) || !all(c("scen", "cycle", "pop") %in% names(tbl))) return(NULL)
      tbl |>
        filter(scen == "reference", cycle == min(cycle, na.rm = TRUE)) |>
        group_by(across(all_of(grp))) |>
        summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")
    }
    bp_imd <- baseline_of(pc$people_imd, "imd10")
    if ("imd10" %in% names(df) && !is.null(bp_imd)) {
      df <- df |>
        left_join(bp_imd, by = "imd10") |>
        mutate(imd10 = as.character(imd10),
               imd10 = dplyr::if_else(is.na(pop), imd10,
                                      paste0("IMD ", imd10, " (n = ", fmt_pop(pop), ")"))) |>
        select(-pop)
    }
    bp_lad <- baseline_of(pc$people_lad, "ladnm")
    if ("ladnm" %in% names(df) && !is.null(bp_lad)) {
      df <- df |>
        left_join(bp_lad, by = "ladnm") |>
        mutate(ladnm = as.character(ladnm),
               ladnm = label_area(ladnm),
               ladnm = dplyr::if_else(is.na(pop), ladnm,
                                      paste0(ladnm, ", n = ", fmt_pop(pop)))) |>
        select(-pop)
    }
    bp_gen <- baseline_of(pc$people_gender, "gender")
    if ("gender" %in% names(df) && !is.null(bp_gen)) {
      gp <- bp_gen |>
        mutate(gender = case_when(gender == 1 ~ "Male", gender == 2 ~ "Female"))
      df <- df |>
        left_join(gp, by = "gender") |>
        mutate(gender = as.character(gender),
               gender = dplyr::if_else(is.na(pop), gender,
                                       paste0(gender, " (n = ", fmt_pop(pop), ")"))) |>
        select(-pop)
    }
    
    # Overall view has no grouping column, so none of the joins above supplied
    # person-time; fall back to the whole-cohort figure.
    if (!".pt" %in% names(df)) {
      pt_all <- persontime_of(pc$people_overall, character(0))
      df$.pt <- if (!is.null(pt_all) && nrow(pt_all)) pt_all$.pt[1] else NA_real_
    }
    
    out <- df |> group_by(across(all_of(c(grp)))) |>
      summarise(diff = (if (cumu) sum else median)(diff, na.rm = TRUE),
                .pt = dplyr::first(.pt), .groups = "drop") |>
      group_by(across(all_of(grp))) |>
      mutate(y = diff) |> #if (cumu) cumsum(diff) else diff) |>
      ungroup() |>
      mutate(metric = pick$label)
    
    # Report deaths and diseases as postponed/prevented rather than as a signed difference.
    sgn <- pick$sign %||% 1
    if (sgn != 1) out <- out |> mutate(diff = diff * sgn, y = y * sgn)
    
    if (rate_mode) {
      validate(need(any(!is.na(out$.pt) & out$.pt > 0),
                    "No person-time available for this view."))
      unit <- if (cumu) " per 100,000 person-years" else " per 100,000 people per cycle"
      out <- out |>
        mutate(diff = diff / .pt * 1e5,
               y    = y   / .pt * 1e5,
               metric = paste0(metric, unit))
    }
    out
    
  })
  
  build_diff_plot <- reactive({
    d <- diff_long(); req(nrow(d) > 0)
    ylab <- if (isTRUE(input$diff_cumulative)) "Cumulative Δ vs reference" else "Δ vs reference"
    bar_chart_func <- if (isTRUE(input$diff_cumulative)) "sum" else "median"
    
    ttl  <- d$metric[1]
    if ("gender" %in% names(d)) {
      ggplot(d, aes(x = cycle, y = y, colour = scen)) +
        geom_smooth(se = FALSE, method = "loess") + add_zero_line() +
        labs(title = ttl, x = "Cycle (year)", y = ylab) +
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
        labs(title = ttl, x = "Cycle (year)", y = ylab) +
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
          labs(title = ttl, x = "IMD quintile (1 = most deprived)", y = ylab)
        
      }else{
        
        ggplot(d, aes(x = cycle, y = y, colour = scen)) +
          geom_smooth(se = FALSE, method = "loess") + add_zero_line() +
          labs(title = ttl, x = "Cycle (year)", y = ylab) +
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
    
    if (!isTRUE(input$diff_cumulative)) {
      
      if ("cycle" %in% names(d)) {
        
        d <- d |> 
          group_by(across(all_of(c("scen", by)))) |>
          slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
          ungroup() |>
          transmute(
            metric = metric_lab, scen,
            !!!(if (length(by)) rlang::syms(by) else NULL),
            final_cycle = cycle,
            cumulative_value = y,
            !!!if (SCALING == 1) NULL else list(cumulative_value_scaled = y * SCALING)
          ) |>
          arrange(scen, across(all_of(by)))
      }else{
        
        d <- d |> 
          group_by(across(all_of(c("scen", by)))) |>
          ungroup() |>
          transmute(
            metric = metric_lab, scen,
            !!!(if (length(by)) rlang::syms(by) else NULL),
            cumulative_value = y,
            !!!if (SCALING == 1) NULL else list(cumulative_value_scaled = y * SCALING)
          ) |>
          arrange(scen, across(all_of(by)))
        
      }
      
    } else {
      
      if ("cycle" %in% names(d)) {
        d <- d |> 
          group_by(across(all_of(c("scen", by)))) |>
          summarise(
            final_cycle = max(cycle, na.rm = TRUE),
            cumulative_value = sum(diff, na.rm = TRUE), 
            .groups = "drop"
          ) |>
          mutate(
            metric = metric_lab, 
            !!!if (SCALING == 1) NULL else list(cumulative_value_scaled = y * SCALING),
            .before = 1
          ) |>
          arrange(scen, across(all_of(by)))
      }else {
        
        d <- d |> 
          group_by(across(all_of(c("scen", by)))) |>
          summarise(
            cumulative_value = sum(diff, na.rm = TRUE), 
            .groups = "drop"
          ) |>
          mutate(
            metric = metric_lab, 
            !!!if (SCALING == 1) NULL else list(cumulative_value_scaled = y * SCALING),
            .before = 1
          ) |>
          arrange(scen, across(all_of(by)))
      }
      
    }
    
    list(raw = d, by = by, metric_lab = metric_lab)
  }
  
  # Use the function in renderUI
  # Per-100,000 tab: same builders as the counts tab. diff_long() has already
  # divided by baseline population, so nothing else needs to change.
  output$table_diff_rate <- renderUI({
    if (isTRUE(input$diff_table)) gt_output("diff_rate_gt")
    else plotlyOutput("diff_rate_plot", height = "100vh")
  })
  
  output$diff_rate_gt <- render_gt({
    data <- get_processed_data()
    get_normalized_table(
      data$raw |>
        dplyr::select(-any_of(c("cumulative_value_scaled", "final_cycle"))) |>
        tidyr::pivot_wider(names_from = scen, values_from = cumulative_value)
    ) |>
      dplyr::select(-matches("min|max|norm")) |>
      gt::gt() |>
      gt::tab_options(table.font.size = "small") |>
      opt_interactive(use_filters = TRUE, use_sorting = FALSE, use_compact_mode = TRUE)
  })
  
  output$diff_rate_plot <- renderPlotly({ diff_plot_obj() })
  
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
  
  # Bar labels: whole numbers for counts, 1 dp for per-100,000 rates, and
  # thousands separators throughout. Previously printed at full double
  # precision (e.g. 514.1745011563221).
  fmt_val <- function(x) {
    d <- if (max(abs(x), na.rm = TRUE) >= 1000) 0 else 1
    formatC(round(x, d), format = "f", digits = d, big.mark = ",")
  }
  
  # Shared plot builder: used by both the counts tab and the per-100,000 tab.
  # diff_long() has already applied the scaling, so the build is identical.
  diff_plot_obj <- function() {
    data <- get_processed_data()
    cumdf <- data$raw
    by <- data$by
    
    # Display-only relabelling, applied here so every filter and difference
    # calculation upstream still sees the raw values.
    if ("scen"   %in% names(cumdf)) cumdf$scen   <- label_scen(cumdf$scen)
    if ("cause"  %in% names(cumdf)) cumdf$cause  <- label_cause(cumdf$cause)
    
    bar_chart_func <- if (isTRUE(input$diff_cumulative)) "sum" else "median"
    
    # Axis title reflects the active tab rather than the raw column name
    axis_lab <- if (identical(input$main_tabs, "Differences per 100,000")) {
      if (isTRUE(input$diff_cumulative)) "Per 100,000 person-years"
      else "Per 100,000 people per cycle"
    } else {
      paste0(data$metric_lab, if (isTRUE(input$diff_cumulative)) " (total)" else " (median per cycle)")
    }
    
    # Branch on the grouping column rather than on label text: the metric
    # labels are user-facing wording and were previously matched by grepl().
    if ("cause" %in% names(cumdf)){
      
      if (!"imd10" %in% names(cumdf)){
        p <- ggplot(cumdf) +
          aes(x = cause, y = cumulative_value, fill = scen) +
          geom_bar(
            stat = "summary",
            fun = bar_chart_func,
            position = "dodge2"
          ) +
          scale_fill_hue(direction = 1) +
          scale_y_continuous(labels = scales::label_comma()) +
          labs(
            fill = "Scenario",
            y = axis_lab,
            x = ""
          ) +
          
          geom_text(
            aes(label = fmt_val(cumulative_value), y = cumulative_value / 2),
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
          scale_color_hue(direction = 1) +
          theme_minimal() + 
          labs(
            x = "IMD quintile (1 = most deprived)",
            color = "Scenario",
            y = "Cumulative Δ"
          ) 
      }
    }
    else if ("factor" %in% names(cumdf)){
      
      if (!"imd10" %in% names(cumdf)){
        
        p <- ggplot(cumdf) +
          aes(x = scen, y = cumulative_value, fill = factor) +
          geom_bar(stat = "summary", fun = bar_chart_func, position = "dodge2") +
          scale_fill_hue(direction = 1) +
          scale_y_continuous(labels = scales::label_comma()) +
          labs(fill = "Metric") +
          geom_text(
            aes(label = fmt_val(cumulative_value), y = cumulative_value / 2),
            size = ifelse("gender" %in% names(cumdf), 2, 3),
            position = position_dodge(width = 1),
            color = "black"
          ) +
          coord_flip() +
          labs(x = "", y = axis_lab, fill = "Metric") +
          theme_minimal() 
      }else{
        
        p <- ggplot(cumdf) +
          aes(x = imd10, y = cumulative_value, fill = scen) +
          geom_col(position = position_dodge(width = 0.9)) +
          scale_color_hue(direction = 1) +
          theme_minimal() +
          labs(
            x = "IMD quintile (1 = most deprived)",
            color = "Scenario"
          ) +
          guides(color = "none")
        
        p <- ggplot(cumdf) +
          aes(x = cumulative_value, y = factor, fill = factor) +
          geom_bar(stat = "summary", fun = "sum") +
          scale_fill_hue(direction = 1) +
          scale_x_continuous(labels = scales::label_comma()) +
          labs(fill = "Metric", y = "Metric") +
          theme_minimal()
        
        
        
      }
      
    }else{
      
      if (!"imd10" %in% names(cumdf)){
        
        p <- ggplot(cumdf, aes(x = scen, y = cumulative_value, fill = scen)) +
          geom_col(position = "dodge") +
          scale_y_continuous(labels = scales::label_comma()) +
          geom_text(
            aes(label = fmt_val(cumulative_value), y = cumulative_value / 2),
            size = ifelse("gender" %in% names(cumdf), 2, 3),
            position = position_dodge(width = 1),
            color = "black"
          ) +
          labs(
            title = paste("Cumulative", data$metric_lab, "by Scenario"),
            x = "Scenario", 
            y = axis_lab,
            fill = "Scenario"
          ) +
          coord_flip() +
          theme_minimal()
      }else{
        p <- ggplot(cumdf) +
          aes(x = imd10, y = cumulative_value, colour = scen) +
          geom_col(position = position_dodge(width = 0.9), aes(fill = scen)) + 
          scale_color_hue(direction = 1) +
          theme_minimal() + 
          labs(
            x = "IMD quintile (1 = most deprived)",
            color = "Scenario"
          )
        
        
      }
      
    }
    
    if ("gender" %in% names(cumdf))  {
      p <- p + facet_wrap(~gender)
    }else if ("imd10" %in% names(cumdf) && "cause" %in% names(cumdf))  {
      p <- p + facet_wrap(~cause)
    }else if ("imd10" %in% names(cumdf) && "factor" %in% names(cumdf))  {
      p <- p + facet_wrap(vars(imd10, scen))#facet_wrap(~factor, scales = "free_y")
    }else if("ladnm" %in% names(cumdf))  {
      p <- p + facet_wrap(~ladnm)
    }
    
    p
  }
  
  output$diff_summary_plot <- renderPlotly({ diff_plot_obj() })
  
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
    if (input$asr_mode %in% c("avg", "bars")) {
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_avg_1_30, asr_healthy_years_overall_avg_1_30) |>
          filter(cause %in% causes, scen %in% scens) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause)))
        
        req(nrow(df) > 0)
        df <- df |>
          group_by(cause, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate)
        
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all_avg_1_30 |> 
          filter(cause %in% causes, scen %in% scens) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause)))
        
        req(nrow(df) > 0)
        df <- df |> 
          mutate(gender = case_when(
            gender == 1 ~ "Male",
            gender == 2 ~ "Female"
          )) |> 
          group_by(cause, gender, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate)
        
      } else if (input$view_level == "IMD") {
        df <- pc$asr_imd_all_avg_1_30 |> 
          filter(cause %in% causes, scen %in% scens) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause)))
        
        req(nrow(df) > 0)
        df <- df |>  
          group_by(cause, imd10, scen) |> 
          reframe(age_std_rate = mean(age_std_rate)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate)
        
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
        
        df <- df |> 
          group_by(cause, ladnm, scen) |> 
          reframe(age_std_rate = mean(age_std_rate, na.rm = TRUE)) |> 
          pivot_wider(names_from = scen, values_from = age_std_rate)
      }
    } else {
      # Non-average mode datasets
      if (input$view_level == "Overall") {
        df <- bind_rows(asr_overall_all, asr_healthy_years_overall) |>
          filter(cause %in% causes, scen %in% scens, cycle >= MIN_CYCLE) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause)))
      } else if (input$view_level == "Gender") {
        df <- asr_gender_all |> 
          filter(cause %in% causes, scen %in% scens, cycle >= MIN_CYCLE) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause))) |>
          mutate(gender = ifelse(gender == 1, "Male",
                                 ifelse(gender == 2, "Female", NA)))
      } else if (input$view_level == "IMD") {
        df <- pc$asr_imd_all |> 
          filter(cause %in% causes, scen %in% scens, cycle >= MIN_CYCLE) |> 
          mutate(cause = case_when(cause == "dead" ~ "Death (all causes)",
                                   cause == "dead_car" ~ "Death (car)",
                                   cause == "dead_bike" ~ "Death (cyclist)",
                                   cause == "dead_walk" ~ "Death (pedestrian)",
                                   .default = as.character(cause)))
        
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
        
      }
    }
    
    if ("scen" %in% names(df))
      df <- df |> rename(Scenario = scen)
    
    df
    
  })
  
  
  build_asr_plot <- reactive({
    df <- get_asr_data()
    req(df)
    
    if (input$asr_mode == "avg") {
      df  # wide table -> rendered by gt
      
    } else if (input$asr_mode == "bars") {
      # get_asr_data() returns one column per scenario; go back to long for ggplot
      long <- df |>
        tidyr::pivot_longer(
          cols = tidyr::any_of(input$scen_sel),
          names_to = "Scenario", values_to = "age_std_rate"
        ) |>
        filter(!is.na(age_std_rate))
      req(nrow(long) > 0)
      
      # % change vs reference, computed WITHIN each bar group (cause, or
      # cause x imd10 / gender / ladnm). Grouping columns are whatever is left
      # once Scenario and the rate are removed, so this works for every view.
      grp_cols <- setdiff(names(long), c("Scenario", "age_std_rate"))
      long <- long |>
        group_by(across(all_of(grp_cols))) |>
        mutate(
          ref_rate = {
            r <- age_std_rate[Scenario == "reference"]
            if (length(r)) r[1] else NA_real_
          },
          pct_change = 100 * (age_std_rate - ref_rate) / ref_rate,
          # reference is the baseline, so it gets no label; blank rather than
          # NA so geom_text doesn't warn about dropped rows
          lbl = dplyr::case_when(
            Scenario == "reference"  ~ "",
            is.na(pct_change)        ~ "",
            TRUE ~ sprintf("%+.1f%%", pct_change)
          ),
          # explicit y coordinate: ggplotly() drops vjust, which left the
          # labels sitting on the top-left corner of each bar
          lbl_y = age_std_rate * 1.02
        ) |>
        ungroup()
      
      # Relabel only now -- the pct_change block above matches on the raw
      # value "reference", so renaming earlier would silently blank every label.
      long <- long |> mutate(Scenario = label_scen(Scenario))
      if ("cause" %in% names(long)) long <- long |> mutate(cause = label_cause(cause))
      
      ttl <- paste0("Average ", MAX_CYCLE,
                    " years Age Standardised Rate per 100,000 people")
      
      show_pct <- isTRUE(input$asr_pct) && "Reference" %in% long$Scenario
      if (isTRUE(input$asr_pct) && !"Reference" %in% long$Scenario)
        ttl <- paste0(ttl, "  (select 'reference' to show % difference)")
      
      # Optional % labels. Always keep the headroom so the y-axis doesn't
      # jump when the labels are toggled on and off.
      pct_labels <- list(
        if (show_pct)
          geom_text(aes(y = lbl_y, label = lbl),
                    position = position_dodge(width = 0.9),
                    size = 3, show.legend = FALSE),
        scale_y_continuous(expand = expansion(mult = c(0, 0.10)))
      )
      
      if (input$view_level == "Overall") {
        ggplot(long, aes(x = cause, y = age_std_rate, fill = Scenario)) +
          geom_col(position = position_dodge(width = 0.9)) +
          pct_labels +
          labs(title = ttl, x = NULL, y = "ASR per 100,000") +
          theme_clean()
        
      } else if (input$view_level == "Gender") {
        ggplot(long, aes(x = cause, y = age_std_rate, fill = Scenario)) +
          geom_col(position = position_dodge(width = 0.9)) +
          pct_labels +
          facet_wrap(vars(gender), scales = "free_y") +
          labs(title = ttl, x = NULL, y = "ASR per 100,000") +
          theme_clean()
        
      } else if (input$view_level == "IMD") {
        ggplot(long, aes(x = factor(imd10), y = age_std_rate, fill = Scenario)) +
          geom_col(position = position_dodge(width = 0.9)) +
          pct_labels +
          facet_wrap(vars(cause), scales = "free_y") +
          labs(title = ttl, x = "IMD quintile (1 = most deprived)",
               y = "ASR per 100,000") +
          theme_clean()
        
      } else {
        ggplot(long |> mutate(ladnm = label_area(ladnm)),
               aes(x = ladnm, y = age_std_rate, fill = Scenario)) +
          geom_col(position = position_dodge(width = 0.9)) +
          pct_labels +
          facet_wrap(vars(cause), scales = "free_y") +
          labs(title = ttl, x = NULL, y = "ASR per 100,000") +
          theme_clean() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
    } else {
      if (input$view_level == "Overall") {
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = Scenario, group = Scenario)) +
          geom_col(position = position_dodge(width = 0.9), aes(fill = Scenario)) +
          facet_wrap(vars(cause), scales = "free_y", ncol = 4) +
          labs(title = paste0("ASR per cycle (summed over cycles 1-", MAX_CYCLE, ")\n\n"), 
               x = "Cycle (year)", y = "ASR per 100,000") +
          theme_clean()
        
      } else if (input$view_level == "Gender") {
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = Scenario)) +
          geom_col(position = position_dodge(width = 0.9), aes(fill = Scenario)) +
          facet_wrap(vars(cause, gender), scales = "free_y") +
          labs(title = paste0("ASR per cycle (summed over cycles 1-", MAX_CYCLE, ")\n\n"),
               x = "Cycle (year)", y = "ASR per 100,000") +
          theme_clean()
        
      } else if (input$view_level == "IMD") {
        ggplot(df, aes(x = factor(imd10), y = age_std_rate, colour = Scenario)) +
          geom_col(position = position_dodge(width = 0.9), aes(fill = Scenario)) + 
          facet_wrap(vars(cause), scales = "free_y") +
          labs(title = paste0("ASR per cycle (summed over cycles 1-", MAX_CYCLE, ")\n\n"),
               x = "IMD", y = "ASR per 100,000") +
          theme_clean()
        
      } else {
        ggplot(df, aes(x = cycle, y = age_std_rate, colour = Scenario)) +
          geom_col(position = position_dodge(width = 0.9), aes(fill = Scenario)) +
          facet_grid(ladnm ~ cause, scales = "free_y") +
          labs(title = paste0("ASR per cycle (summed over cycles 1-", MAX_CYCLE, ")\n\n"),
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
  
  current_table <- reactive({
    req(input$main_tabs)
    
    tab <- input$main_tabs   # no need for isolate or print
    
    if (tab == "Population") {
      
      pd <- pop_data()
      d  <- pd$data |>
        mutate(across(where(is.numeric), ~ round(.x, 6)))
      
    } else if (tab %in% c("Differences vs reference", "Differences per 100,000")) {
      
      d <- diff_long()
      
      if (isTRUE(input$diff_cumulative)) {
        
        # Grouping columns, derived rather than hardcoded: the old list was
        # c("gender","ladnm") only, so IMD quintile, cause and factor were
        # silently dropped from the export by the transmute() below.
        by <- intersect(c("gender", "ladnm", "imd10", "cause", "factor",
                          "agegroup_cycle"), names(d))
        
        if ("cycle" %in% names(d)) {
          d <- d |>
            group_by(across(all_of(c("scen", by)))) |>
            slice_max(order_by = cycle, n = 1, with_ties = FALSE) |>
            ungroup() |>
            transmute(
              scen,
              across(all_of(by)),
              final_cycle            = cycle,
              cumulative_value       = y,
              !!!if (SCALING == 1) NULL else list(cumulative_value_scaled = y * SCALING)
            )
        } else {
          d <- d |>
            group_by(across(all_of(c("scen", by)))) |>
            # no slice here; assume one row per group
            ungroup() |>
            transmute(
              scen,
              across(all_of(by)),
              cumulative_value       = y,
              !!!if (SCALING == 1) NULL else list(cumulative_value_scaled = y * SCALING)
            )
        }
      }
      
    } else if (tab == "Average onset ages") {
      
      req(input$avg_kind, input$view_level,
          input$scen_sel, input$avg_cause,
          input$avg_death_causes)
      d <- get_onset_ages()
      
    } else if (tab == "Age Standardised Rates") {
      
      d <- get_asr_data()
      
    } else if (tab == "Travel") {
      
      d <- trips_sel()$data
      
    } else if (tab == "Exposures") {
      
      view <- input$view_level
      
      # 2026-08 FIX. Was a switch() of grepl() calls against the DISPLAY
      # label, which the load-time relabel had already rewritten -- so the
      # LAD and Gender views matched no rows at all. group_type is written at
      # load and takes exactly the values input$view_level uses, so a plain
      # equality test does the job and cannot silently stop matching.
      lexp <- exp |> filter(group_type == view)
      
      # %in% on the raw area key, not grepl() on the decorated label: the old
      # version pasted the selection into a regex, so an area name containing
      # a metacharacter, or one that is a substring of another, matched the
      # wrong rows.
      if (view == "LAD" && length(input$lad_sel)) {
        lexp <- lexp |> filter(area_key %in% input$lad_sel)
      }
      
      if (length(input$scen_sel)) {
        lexp <- lexp |> filter(scen %in% input$scen_sel)
      }
      
      d <- lexp
    } else {
      
      d <- NULL
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
      validate(
        need(!is.null(dat), "No data to download")
      )
      dat <- dat |> dplyr::select(-dplyr::any_of(".pt"))
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
  
  # ---- Travel ------------------------------------------------------------
  # Aggregation depends on the metric: totals are summed, averages are
  # weighted by np (trip count) where available, and mode share is recomputed
  # as a share of the group total so it always sums to 100 within a bar.
  trips_sel <- reactive({
    req(input$trip_metric)
    spec <- TRIP_SPEC[[input$trip_metric]]
    req(!is.null(spec))
    
    view <- if (input$view_level %in% spec$views) input$view_level else "Overall"
    tbl  <- spec$tables[[view]]
    df   <- trips[[tbl]]
    req(!is.null(df), nrow(df) > 0)
    
    gv <- trip_group_var(view, spec)
    xv <- spec$xvar
    if (identical(gv, xv)) xv <- "mode"
    req(xv %in% names(df))
    if (identical(spec$agg, "ratio"))
      req(all(c(spec$num, spec$den) %in% names(df)))
    if (length(gv)) {
      req(gv %in% names(df))
      df <- df |> filter(!is.na(.data[[gv]]))
    }
    df <- df |> filter(scen %in% input$scen_sel)
    req(nrow(df) > 0)
    
    keys <- unique(c("scen", gv, spec$facet2, xv))
    keys <- keys[!is.na(keys)]
    req(all(keys %in% names(df)))
    
    out <- switch(
      spec$agg,
      
      # Stored share, read as-is. No aggregation: the table is already at the
      # grain of this view, so summing would double-count.
      direct = df |>
        select(all_of(c(keys, spec$val))) |>
        rename(value = all_of(spec$val)),
      
      # Weighted mean: sum the numerator and denominator over the cells this
      # view covers, then divide. Never sums or re-averages a stored mean --
      # doing either would weight a cell of 40 people the same as one of
      # 40,000. spec$val is unused for this mode.
      ratio = df |>
        group_by(across(all_of(keys))) |>
        summarise(.num = sum(.data[[spec$num]], na.rm = TRUE),
                  .den = sum(.data[[spec$den]], na.rm = TRUE),
                  .groups = "drop") |>
        mutate(value = if_else(.den > 0, .num / .den, NA_real_)) |>
        select(-.num, -.den),
      
      # Share ACROSS bands, which is not stored. Counts are summed over the
      # columns not being shown, then normalised within each group.
      share = df |>
        group_by(across(all_of(keys))) |>
        summarise(value = sum(.data[[spec$val]], na.rm = TRUE), .groups = "drop") |>
        group_by(across(all_of(setdiff(keys, xv)))) |>
        mutate(value = value / sum(value, na.rm = TRUE) * 100) |>
        ungroup(),
      
      stop("Unknown agg mode: ", spec$agg)
    )
    
    list(data = out, gv = gv, view = view, spec = spec, xv = xv,
         tbl = tbl, is_share = !identical(spec$agg, "mean"))
  })
  
  output$trip_key_note <- renderUI({
    req(identical(input$main_tabs, "Travel"))
    sp <- TRIP_SPEC[[input$trip_metric %||% names(TRIP_SPEC)[1]]]
    n  <- group_key()
    view <- if ((input$view_level %||% "") %in% sp$views) input$view_level else "Overall"
    tagList(
      div(style = "padding:6px 4px; color:#666; font-size:0.85em;",
          paste0("Views available for this metric: ",
                 paste(sp$views, collapse = ", "), ". ",
                 if (nzchar(n)) n else "")),
      prov(paste0(
        "trips$", sp$tables[[view]],
        if (!is.null(sp$val)) paste0(", column '", sp$val, "'. ") else ". ",
        switch(sp$agg,
               direct = "Shown exactly as stored - the share was computed against this view's own denominator in process_trips.R.",
               ratio  = paste0(
                 "Weighted mean: the numerator and denominator are summed over the cells ",
                 "this view covers and then divided, so every level is weighted by the ",
                 "people or trips behind it. ",
                 if (sp$tables[[view]] %in% c("tdist_fine", "ttime_fine"))
                   paste0("Per-trip figures are approximate: the source table stores only a ",
                          "mean, so trip counts from trips_percentage are used as weights. ",
                          "Those counts sum t.factor, which counts a home-based record twice, ",
                          "and the stored mean itself has t.factor applied to the value - so ",
                          "per-trip distances and durations are inflated for home-based ",
                          "purposes. Weekly per-person figures are not affected.")
                 else
                   "Rolled up from the stored sums and person counts, so this is exact."),
               share  = "Counts summed over the columns not shown, then normalised so the distance bands total 100% within each group; the share across bands is not stored.",
               "")))
    )
  })
  
  output$trip_plot <- renderPlotly({
    ts <- trips_sel(); d <- ts$data; sp <- ts$spec
    d <- d |> mutate(scen = label_scen(scen))
    if ("gender" %in% names(d))
      d <- d |> mutate(gender = case_when(gender == 1 ~ "Male",
                                          gender == 2 ~ "Female",
                                          TRUE ~ as.character(gender)))
    for (col in c("LAD_group", "LAD_origin"))
      if (col %in% names(d)) d[[col]] <- label_area(d[[col]])
    if ("distance_bracket" %in% names(d))
      d$distance_bracket <- order_dist(d$distance_bracket)
    lbl <- input$trip_metric
    
    if (isTRUE(sp$stacked)) {
      p <- ggplot(d, aes(x = scen, y = value, fill = .data[[ts$xv]])) +
        geom_col() +
        labs(title = lbl, x = NULL, y = sp$ylab, fill = "Mode")
    } else {
      p <- ggplot(d, aes(x = .data[[ts$xv]], y = value, fill = scen)) +
        geom_col(position = position_dodge(width = 0.85), width = 0.75) +
        labs(title = lbl,
             x = if (identical(ts$xv, "distance_bracket")) "Trip distance (km)" else NULL,
             y = sp$ylab, fill = "Scenario")
    }
    p <- p + scale_y_continuous(labels = scales::label_comma())
    
    fv <- c(if (length(ts$gv)) ts$gv else NULL, sp$facet2)
    fv <- fv[!is.na(fv)]
    fv <- fv[fv %in% names(d)]
    if (length(fv)) p <- p + facet_wrap(vars(!!!rlang::syms(fv)))
    
    p <- p + theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p)
  })
  
  output$trip_table <- render_gt({
    req(isTRUE(input$trip_show_table))
    ts <- trips_sel(); d <- ts$data
    if ("distance_bracket" %in% names(d))
      d <- d |> mutate(distance_bracket = order_dist(distance_bracket)) |>
      arrange(distance_bracket)
    keys <- setdiff(names(d), c("scen", "value"))
    
    wide <- d |> tidyr::pivot_wider(names_from = scen, values_from = value)
    scen_cols <- order_scen_raw(setdiff(names(wide), keys))
    wide <- wide |> dplyr::relocate(dplyr::all_of(scen_cols), .after = dplyr::last_col())
    
    if (isTRUE(input$trip_pct)) {
      validate(need("reference" %in% scen_cols,
                    "Select the Reference scenario to show % change."))
      refv <- wide[["reference"]]
      # Share metrics get BOTH differences: the percentage-point change and
      # the relative change, which can tell very different stories -- cycling
      # rising from 1.0% to 16.6% is +15.5 points but +1,491%, and the point
      # change is the one that reflects how many trips actually moved.
      # Distance and time metrics are not percentages, so the absolute
      # difference is labelled in the metric's own units instead of "pp".
      unit_lbl <- if (isTRUE(ts$is_share)) "\u0394pp" else "\u0394"
      for (sc in setdiff(scen_cols, "reference")) {
        lbl <- as.character(label_scen(sc))
        wide[[paste0(lbl, " ", unit_lbl)]] <- wide[[sc]] - refv
        wide[[paste0(lbl, " %\u0394")]]  <-
          ifelse(is.na(refv) | refv == 0, NA_real_,
                 (wide[[sc]] - refv) / refv * 100)
      }
    }
    names(wide)[match(scen_cols, names(wide))] <- as.character(label_scen(scen_cols))
    
    wide |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      gt::gt() |>
      gt::tab_options(table.font.size = "small") |>
      opt_interactive(use_filters = TRUE, use_sorting = FALSE,
                      use_compact_mode = TRUE)
  })
  
  # Populate the exposure pickers from the data itself
  observe({
    req(input$main_tabs == "Exposures")
    vars <- sort(unique(exp$variable))
    yrs  <- sort(unique(exp$year))
    updateSelectInput(session, "exp_var",  choices = vars,
                      selected = if (isTRUE(input$exp_var %in% vars)) input$exp_var else vars[1])
    updateSelectInput(session, "exp_year", choices = yrs,
                      selected = if (isTRUE(input$exp_year %in% yrs)) input$exp_year else max(yrs))
  })
  
  # Which districts sit in each area -- read from the lookup written by
  # process_exp.R, so the mapping is defined in one place only.
  output$exp_area_note <- renderUI({
    # NB: the panel's value is "Exposures" even though its label reads
    # "Personal exposures", so input$main_tabs returns the value.
    req(identical(input$main_tabs, "Exposures"),
        identical(input$view_level, "LAD"))
    lk <- attr(exp, "area_lookup")
    if (is.null(lk)) return(NULL)
    txt <- lk |>
      dplyr::group_by(ladnm) |>
      dplyr::summarise(d = paste(lad_district, collapse = ", "), .groups = "drop") |>
      dplyr::mutate(line = paste0(ladnm, ": ", d)) |>
      dplyr::pull(line)
    tagList(
      div(style = "padding:6px 4px; color:#555; font-size:0.9em;",
          HTML(paste(txt, collapse = " &nbsp;|&nbsp; "))),
      prov("exp (process_exp.R). Quantiles and means are stored values, shown as-is; only the % change columns are computed here.")
    )
  })
  
  # Exposure rows for the selected variable and year, in long form
  exp_sel <- reactive({
    req(input$exp_var, input$exp_year)
    d <- current_table()
    req(nrow(d) > 0)
    d |> filter(variable == input$exp_var, year == input$exp_year)
  })
  
  # Two plot shapes, depending on the variable:
  #
  #  * PREVALENCE_VARS are population proportions, so quantiles carry no
  #    information (they are 0/1). Plot the mean as a bar, labelled prevalence.
  #  * everything else is a distribution over people, so show the spread.
  #
  # The box is drawn with geom_crossbar + geom_linerange rather than
  # geom_boxplot(stat = "identity"): ggplotly() does not convert an
  # identity-stat boxplot and collapses every box to a flat line.
  output$exp_plot <- renderPlotly({
    d <- exp_sel()
    req(nrow(d) > 0, all(c("stat", "grouping", "scen", "value") %in% names(d)))
    d <- d |> filter(scen %in% input$scen_sel)
    req(nrow(d) > 0)
    
    # Display-only relabel, applied after all filtering on scen has happened
    d <- d |> mutate(scen = label_scen(scen))
    is_prev <- input$exp_var %in% PREVALENCE_VARS
    
    if (is_prev) {
      m <- d |> filter(stat == "mean")
      req(nrow(m) > 0)
      p <- ggplot(m, aes(x = grouping, y = value, fill = scen)) +
        geom_col(position = position_dodge(width = 0.85), width = 0.75) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(title = paste0(input$exp_var, " (", input$exp_year, ")"),
             subtitle = "Prevalence in the population",
             x = NULL, y = "Prevalence", fill = "Scenario") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
    } else {
      w <- d |>
        filter(stat %in% c("5%", "25%", "50%", "75%", "95%")) |>
        tidyr::pivot_wider(names_from = stat, values_from = value)
      req(nrow(w) > 0, all(c("5%", "25%", "50%", "75%", "95%") %in% names(w)))
      
      dodge <- position_dodge(width = 0.85)
      p <- ggplot(w, aes(x = grouping, fill = scen)) +
        geom_linerange(aes(ymin = `5%`, ymax = `95%`, group = scen),
                       position = dodge, colour = "grey40",
                       show.legend = FALSE) +
        geom_crossbar(aes(y = `50%`, ymin = `25%`, ymax = `75%`, group = scen),
                      position = dodge, width = 0.7,
                      colour = "grey30", fatten = 2) +
        scale_y_continuous(labels = scales::label_comma()) +
        labs(title = paste0(input$exp_var, " (", input$exp_year, ")"),
             subtitle = "Box = 25th-75th percentile, line = median, whiskers = 5th-95th",
             x = NULL, y = input$exp_var, fill = "Scenario") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
    ggplotly(p)
  })
  
  output$plot_exp <- render_gt({
    req(input$view_level)
    
    req(isTRUE(input$exp_show_table))
    lexp <- exp_sel()
    req(nrow(lexp) > 0)
    req(all(c("scen", "value") %in% names(lexp)))
    
    # Avoid printing in reactive contexts (expensive in large apps)
    # message(names(lexp)) # use message() only for debugging if really needed
    
    # For proportion variables the "mean" IS the prevalence, so label it that
    # way; the quantile rows for these are 0/1 and carry no information, so
    # they are dropped rather than shown as misleading spread.
    if (input$exp_var %in% PREVALENCE_VARS) {
      lexp <- lexp |>
        filter(stat == "mean") |>
        mutate(stat = "prevalence")
    }
    
    # Pivot wider once
    wide_df <- lexp |>
      tidyr::pivot_wider(
        names_from  = scen,
        values_from = value
      )
    
    # Identify scenario columns once, in canonical order (Reference first)
    scen_cols <- order_scen_raw(
      setdiff(names(wide_df), c("grouping", "variable", "stat", "year")))
    wide_df <- wide_df |> dplyr::relocate(dplyr::all_of(scen_cols), .after = dplyr::last_col())
    
    # Compute row-wise min/max in a fully vectorised way
    scen_mat <- as.matrix(wide_df[scen_cols])
    
    row_min <- matrixStats::rowMins(scen_mat, na.rm = TRUE)
    row_max <- matrixStats::rowMaxs(scen_mat, na.rm = TRUE)
    
    # Normalisation, handling constant rows
    range <- row_max - row_min
    # Avoid division by zero: constant rows -> 0.5
    denom <- ifelse(range == 0 | is.na(range), 1, range)
    
    norm_mat <- (scen_mat - row_min) / denom
    norm_mat[range == 0 | is.na(range), ] <- 0.5
    
    # Apply colour function in a vectorised way
    # (define col_fun once outside render_gt for extra speed)
    # col_fun <- scales::col_numeric(palette = c("lightpink", "lightgreen"), domain = c(0, 1))
    
    # Colours for each normalised value
    col_mat <- col_fun(as.numeric(norm_mat))
    col_mat <- matrix(col_mat, nrow = nrow(norm_mat), ncol = ncol(norm_mat))
    
    # Build HTML strings vectorised (no mapply in a loop)
    val_mat <- round(scen_mat, 2)
    # Build HTML strings as a vector
    html_vec <- sprintf(
      "<div style='background-color:%s; padding:2px;'>%s</div>",
      as.vector(col_mat),
      as.vector(val_mat)
    )
    
    # Reshape back to matrix with same dims as scen_mat
    html_mat <- matrix(
      html_vec,
      nrow = nrow(scen_mat),
      ncol = ncol(scen_mat),
      byrow = FALSE,
      dimnames = list(NULL, scen_cols)
    )
    
    # Replace original scen columns by HTML columns
    norm_df <- wide_df
    norm_df[scen_cols] <- as.data.frame(html_mat, stringsAsFactors = FALSE)
    html_cols <- scen_cols
    
    # Optional % change vs reference, ADDED as extra columns rather than
    # replacing the values, so absolute levels and relative change are visible
    # together. Computed per row, i.e. each stat against the SAME stat in
    # reference: (scenario - reference) / reference * 100.
    pct_cols <- character(0)
    if (isTRUE(input$exp_pct)) {
      validate(need("reference" %in% scen_cols,
                    "Select the Reference scenario to show % change."))
      refv <- wide_df[["reference"]]
      # Prevalence is itself a percentage, so a percentage-POINT change is
      # meaningful and is shown alongside the relative change. For the
      # continuous exposures (NO2, PM2.5, NDVI, Lden, mMET) a point change
      # would be a raw unit difference, so only the relative change is shown.
      is_prev <- input$exp_var %in% PREVALENCE_VARS
      for (sc in setdiff(scen_cols, "reference")) {
        lbl <- as.character(label_scen(sc))
        if (is_prev) {
          nm_pp <- paste0(lbl, " \u0394pp")
          dv <- wide_df[[sc]] - refv
          norm_df[[nm_pp]] <- ifelse(is.na(dv), "", sprintf("%+.2f", round(dv, 2)))
          pct_cols <- c(pct_cols, nm_pp)
        }
        nm <- paste0(lbl, " %\u0394")
        pv <- ifelse(is.na(refv) | refv == 0, NA_real_,
                     (wide_df[[sc]] - refv) / refv * 100)
        norm_df[[nm]] <- ifelse(is.na(pv), "",
                                sprintf("%+.1f%%", round(pv, 1)))
        pct_cols <- c(pct_cols, nm)
      }
    }
    
    # Scenario column headers use display names ("Go Dutch", not "goDutch").
    # Done by renaming after all the numeric work, so nothing upstream breaks.
    names(norm_df)[match(html_cols, names(norm_df))] <- as.character(label_scen(html_cols))
    html_cols <- as.character(label_scen(html_cols))
    
    gt_tbl <- norm_df |>
      dplyr::select(grouping, year, variable, stat,
                    dplyr::all_of(html_cols), dplyr::all_of(pct_cols)) |>
      gt::gt() |>
      gt::cols_label(!!!rlang::set_names(html_cols, html_cols)) |>
      gt::fmt_markdown(columns = dplyr::all_of(html_cols)) |>
      gt::tab_options(table.font.size = "small", ihtml.use_pagination = FALSE) |>
      opt_interactive(
        use_filters      = TRUE,
        use_sorting      = FALSE,
        use_compact_mode = TRUE
      )
    
    gt_tbl
  })
  
}

shinyApp(ui, server)