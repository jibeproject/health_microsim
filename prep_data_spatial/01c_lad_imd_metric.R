# =============================================================================
# STAGE 1c — Metric by IMD quintile, within each LAD
#
# Crosses the per-LSOA scenario metric values (from 01_preprocess_medians.R)
# with each LSOA's IMD quintile and LAD (from 01b_demographics.R), and computes
# a POPULATION-WEIGHTED MEAN of the metric for every
#     LAD x metric x stat x IMD quintile x scenario
# group. The app uses this to draw a "metric change by IMD" chart on LAD click.
#
# Why population-weighted: each (LAD x quintile) group is the set of *people*
# living in that LAD's LSOAs of that quintile, so larger LSOAs count more.
#
# Produces:  app_spatial/lad_imd_metric.rds
# =============================================================================

pkgs <- c("dplyr", "tidyr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# inputs produced by the earlier stages
med  <- readRDS("app_spatial/exposure_medians_by_lsoa.rds")   # list(wide, long)
lsoa <- readRDS("app_spatial/lsoa_profile.rds")               # has ladcd/ladnm/imd_quintile

long <- med$long   # lsoa21cd, lsoa21nm, n_people, scenario, metric, stat, value

# attach LAD + IMD quintile to each LSOA's metric rows
keyed <- long |>
  dplyr::select(lsoa21cd, scenario, metric, stat, value, n_people) |>
  dplyr::left_join(
    lsoa |> dplyr::select(lsoa21cd, ladcd, ladnm, imd_quintile),
    by = "lsoa21cd"
  ) |>
  dplyr::filter(!is.na(ladcd), !is.na(imd_quintile))

# population-weighted mean of the metric per LAD x metric x stat x quintile x scenario
lad_imd_metric <- keyed |>
  dplyr::group_by(ladcd, ladnm, metric, stat, imd_quintile, scenario) |>
  dplyr::summarise(
    value    = stats::weighted.mean(value, w = n_people, na.rm = TRUE),
    n_people = sum(n_people, na.rm = TRUE),
    .groups  = "drop"
  )

if (!dir.exists("app_spatial")) dir.create("app_spatial", recursive = TRUE)
saveRDS(lad_imd_metric, "app_spatial/lad_imd_metric.rds")
message("Saved app_spatial/lad_imd_metric.rds (", nrow(lad_imd_metric), " rows)")
print(utils::head(lad_imd_metric, 10))