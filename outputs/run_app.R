#!/usr/bin/env Rscript
# outputs/run_app.R
# Runs the Shiny app in outputs/app/combined_app.R. Run from the project folder:
#   Rscript outputs/run_app.R [port] [host]
# Examples:
#   Rscript outputs/run_app.R           # port 3838, host 127.0.0.1
#   Rscript outputs/run_app.R 8000      # port 8000
#   Rscript outputs/run_app.R 8000 0.0.0.0
# The app reads its pre-processed .qs2 files from outputs/app/data/
# (written by outputs/process_all_data.R, process_exp.R and process_trips.R).

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[[1]]) else 3838
host <- if (length(args) >= 2) args[[2]] else "127.0.0.1"

for (p in c("shiny", "here"))
  if (!requireNamespace(p, quietly = TRUE)) stop("Package '", p, "' is required: install.packages('", p, "')")

app_file <- here::here("outputs", "app", "combined_app.R")
if (!file.exists(app_file)) stop("Cannot find ", app_file)

message("Starting Shiny app: ", app_file)
message("Host: ", host, "  Port: ", port)
shiny::runApp(app_file, host = host, port = port, launch.browser = TRUE)
