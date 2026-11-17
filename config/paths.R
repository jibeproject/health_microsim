###############################################################################
## config/paths.R
##
## Where the JIBE data live on this computer. Every script that reads from or
## writes to the shared drive builds its paths from these, so nobody has to
## edit drive letters inside the scripts.
##
## The default is the X: drive. If the share is somewhere else on your computer
## (another drive letter, or a mounted folder on Linux/macOS), set it once,
## in either of these ways, without editing this file:
##   1. in your ~/.Renviron (open it with usethis::edit_r_environ()), add
##        JIBE_ROOT=Z:/HealthImpact/Data/Country/UK/JIBE
##      and restart R; or
##   2. create config/paths_local.R (ignored by git) containing, e.g.,
##        JIBE_ROOT  <- "Z:/HealthImpact/Data/Country/UK/JIBE"
##        LOCAL_DATA <- "D:/jibe_data"
###############################################################################

library(here)

JIBE_ROOT  <- Sys.getenv("JIBE_ROOT",  "X:/HealthImpact/Data/Country/UK/JIBE")  # shared drive
LOCAL_DATA <- Sys.getenv("JIBE_LOCAL", "C:/jibe_data")   # local copies of large datasets (faster than the network)

if (file.exists(here("config/paths_local.R"))) source(here("config/paths_local.R"))

MANCHESTER_ROOT <- file.path(JIBE_ROOT, "manchester")

# Manchester zone system (oaID -> LSOA, local authority, IMD): the one file every
# script uses (data prep, PIFs, microsimulation, processing for the apps)
ZONES_FILE <- here("manchester/synPop/sp_2021/zoneSystem.csv")
