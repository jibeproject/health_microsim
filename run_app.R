#!/usr/bin/env Rscript
# run_app.R
# Simple runner for the Shiny app contained in ./app
# Usage:
#   Rscript run_app.R [port] [host]
# Examples:
#   Rscript run_app.R           # use default port 3838 and host 127.0.0.1
#   Rscript run_app.R 8000      # run on port 8000
#   Rscript run_app.R 8000 0.0.0.0

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[[1]]) else 3838
host <- if (length(args) >= 2) args[[2]] else "127.0.0.1"

app_dir <- file.path(getwd(), "app")
if (!dir.exists(app_dir)) {
  stop("Could not find 'app' directory in current working directory: ", getwd())
}

message("Starting Shiny app from: ", normalizePath(app_dir))
message("Host: ", host, "  Port: ", port)
message("Please select a data/[region] folder containing pre-processed all_data.parquet using the file explorer dialog.\n(You may need to minimise your IDE to see the dialog window!)\n")

# prefer a browser in interactive sessions; when run via Rscript launch.browser = TRUE opens the system browser
shiny_available <- requireNamespace("shiny", quietly = TRUE)
if (!shiny_available) {
  stop("Package 'shiny' is required. Install with install.packages('shiny')")
}

# Run the app
shiny::runApp(appDir = app_dir, host = host, port = port, launch.browser = TRUE)
