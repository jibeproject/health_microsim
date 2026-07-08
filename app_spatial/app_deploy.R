rsconnect::deployApp(
  appDir  = "app_spatial",
  appName = "healthmicrosim",   # reuse the existing slot
  appFiles = c(
    "app.R",
    "exposure_medians_by_lsoa.rds",
    "gm_lsoa_boundaries.rds",
    "lsoa_profile.rds"
  )
)