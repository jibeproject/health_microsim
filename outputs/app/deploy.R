###############################################################################
## outputs/app/deploy.R
##
## Deploys the app (health, exposures, trips and the Maps tab) to shinyapps.io.
## Run from the project folder: source("outputs/app/deploy.R").
##
## Uploads only what the app reads: combined_app.R, modules/, maps/ and, in
## data/, run_name.txt with the health and exposure files of that run and the
## trips file. Other runs' files in data/ are left out.
###############################################################################

APP_NAME  <- "manchester-health-model"   # shinyapps.io name (part of the URL): letters, numbers, - and _ only
APP_TITLE <- "Manchester health model"   # title shown on shinyapps.io

app_dir <- here::here("outputs/app")
run     <- trimws(readLines(file.path(app_dir, "data", "run_name.txt"), n = 1L, warn = FALSE))
files   <- c("combined_app.R",
             file.path("modules", list.files(file.path(app_dir, "modules"))),
             file.path("maps", list.files(file.path(app_dir, "maps"))),
             file.path("data", c("run_name.txt", paste0("health_", run, ".qs2"),
                                 paste0("exposures_", run, ".qs2"), "trips_200826.qs2")))
missing <- files[!file.exists(file.path(app_dir, files))]
if (length(missing)) stop("Missing before deploying: ", paste(missing, collapse = ", "))

message("Deploying run ", run, " as '", APP_NAME, "' (", APP_TITLE, ")")
rsconnect::deployApp(appDir = app_dir, appPrimaryDoc = "combined_app.R",
                     appFiles = files, appName = APP_NAME, appTitle = APP_TITLE)
