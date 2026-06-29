# =============================================================================
# STAGE 1d — TRUE pooled LAD-level metrics, per scenario
#
# Recomputes percentiles/means from the INDIVIDUAL-level exposure data, pooling
# every person in a LAD together (person -> zone/OA -> LAD via zoneSystem.csv).
# This is the correct way to get a LAD median/percentile: you cannot average
# LSOA medians to get the LAD median. Mirrors 01_preprocess_medians.R but groups
# by LAD instead of LSOA.
#
# Produces:  app_spatial/lad_metrics_by_scenario.rds  (list(wide, long))
#            long cols: ladcd, ladnm, n_people, scenario, metric, stat, value
# =============================================================================

pkgs <- c("duckdb", "DBI", "dplyr", "readr", "tidyr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

zone_path <- "prep_data_spatial/data/zoneSystem.csv"

scenarios <- c(
  base       = "prep_data_spatial/data/pp_exposure_2021_base_220526.csv",
  goDutch    = "prep_data_spatial/data/pp_exposure_2021_goDutch_260526.csv",
  green      = "prep_data_spatial/data/pp_exposure_2021_green_260526.csv",
  safeStreet = "prep_data_spatial/data/pp_exposure_2021_safeStreet_260526.csv"
)

# which metrics get percentiles vs mean-only (MUST match 01_preprocess_medians.R)
pctile_metrics <- c(
  "exposure_normalised_pm25",
  "exposure_normalised_no2",
  "exposure_normalised_noise_Lden",
  "exposure_normalised_ndvi",
  "mmets"
)
mean_metrics   <- c("exposure_noise_HA", "exposure_noise_HSD")
mmets_sql <- "(e.mmetHr_walk + e.mmetHr_cycle + e.mmetHr_otherSport)"

con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA temp_directory='duckdb_tmp';")

# zone -> LAD lookup (one row per OA), registered for the per-person join
zones_lkp <- readr::read_csv(zone_path, show_col_types = FALSE) |>
  dplyr::select(oaID, ladcd, ladnm) |>
  dplyr::distinct()
duckdb::duckdb_register(con, "zones_lad", zones_lkp)

pctile_exprs <- unlist(lapply(pctile_metrics, function(m) {
  e <- if (m == "mmets") mmets_sql else paste0("e.", m)
  c(
    sprintf("quantile_cont(%s, 0.05) AS %s__p05", e, m),
    sprintf("quantile_cont(%s, 0.25) AS %s__p25", e, m),
    sprintf("quantile_cont(%s, 0.50) AS %s__p50", e, m),
    sprintf("quantile_cont(%s, 0.75) AS %s__p75", e, m),
    sprintf("avg(%s) AS %s__mean", e, m)   # mean too, for the GM/LAD tables
  )
}))
mean_exprs <- vapply(mean_metrics, function(m)
  sprintf("avg(e.%s) AS %s__mean", m, m), character(1))
agg_sql <- paste(c(pctile_exprs, mean_exprs), collapse = ",\n  ")

summarise_one <- function(label, path) {
  if (!file.exists(path)) {
    warning("Missing file for '", label, "': ", path, " - skipped."); return(NULL)
  }
  message("Processing ", label, " (pooled by LAD) ...")
  sql <- sprintf("
    SELECT z.ladcd, z.ladnm,
           count(*) AS n_people,
           %s
    FROM read_csv_auto('%s', header=true, sample_size=-1) e
    JOIN zones_lad z ON e.zone = z.oaID
    GROUP BY z.ladcd, z.ladnm",
                 agg_sql, path)
  res <- dbGetQuery(con, sql)
  res$scenario <- label
  res
}

# GM-wide: pool ALL individuals in the region (no geographic grouping)
summarise_gm <- function(label, path) {
  if (!file.exists(path)) return(NULL)
  message("Processing ", label, " (pooled GM-wide) ...")
  sql <- sprintf("
    SELECT count(*) AS n_people,
           %s
    FROM read_csv_auto('%s', header=true, sample_size=-1) e",
                 agg_sql, path)
  res <- dbGetQuery(con, sql)
  res$scenario <- label
  res
}

# GM-wide BY GENDER (group only by gender; matches process_exp.R by_gender)
summarise_gm_gender <- function(label, path) {
  if (!file.exists(path)) return(NULL)
  message("Processing ", label, " (GM-wide by gender) ...")
  sql <- sprintf("
    SELECT e.gender, count(*) AS n_people,
           %s
    FROM read_csv_auto('%s', header=true, sample_size=-1) e
    GROUP BY e.gender",
                 agg_sql, path)
  res <- dbGetQuery(con, sql)
  res$scenario <- label
  res
}

# LAD BY GENDER
summarise_lad_gender <- function(label, path) {
  if (!file.exists(path)) return(NULL)
  message("Processing ", label, " (by LAD x gender) ...")
  sql <- sprintf("
    SELECT z.ladcd, z.ladnm, e.gender, count(*) AS n_people,
           %s
    FROM read_csv_auto('%s', header=true, sample_size=-1) e
    JOIN zones_lad z ON e.zone = z.oaID
    GROUP BY z.ladcd, z.ladnm, e.gender",
                 agg_sql, path)
  res <- dbGetQuery(con, sql)
  res$scenario <- label
  res
}

all_summaries <- lapply(names(scenarios),
                        function(l) summarise_one(l, scenarios[[l]]))
summ <- dplyr::bind_rows(all_summaries)

value_cols <- setdiff(names(summ), c("ladcd", "ladnm", "n_people", "scenario"))
long <- summ |>
  tidyr::pivot_longer(dplyr::all_of(value_cols),
                      names_to  = c("metric", "stat"),
                      names_sep = "__",
                      values_to = "value")

if (!dir.exists("app_spatial")) dir.create("app_spatial", recursive = TRUE)
saveRDS(list(wide = summ, long = long),
        "app_spatial/lad_metrics_by_scenario.rds")
message("Saved app_spatial/lad_metrics_by_scenario.rds")
message("Rows (LAD x scenario): ", nrow(summ), " | long rows: ", nrow(long))
print(utils::head(long, 12))

# ---- GM-wide pooled summary (all individuals in the region) -----------------
gm_summaries <- lapply(names(scenarios),
                       function(l) summarise_gm(l, scenarios[[l]]))
gm_summ <- dplyr::bind_rows(gm_summaries)
gm_value_cols <- setdiff(names(gm_summ), c("n_people", "scenario"))
gm_long <- gm_summ |>
  tidyr::pivot_longer(dplyr::all_of(gm_value_cols),
                      names_to  = c("metric", "stat"),
                      names_sep = "__",
                      values_to = "value")
saveRDS(list(wide = gm_summ, long = gm_long),
        "app_spatial/gm_metrics_by_scenario.rds")
message("Saved app_spatial/gm_metrics_by_scenario.rds (", nrow(gm_long), " long rows)")
print(utils::head(gm_long, 12))

# ---- GM-wide BY GENDER ------------------------------------------------------
gmg <- dplyr::bind_rows(lapply(names(scenarios),
                               function(l) summarise_gm_gender(l, scenarios[[l]])))
gmg_value_cols <- setdiff(names(gmg), c("gender", "n_people", "scenario"))
gmg_long <- gmg |>
  tidyr::pivot_longer(dplyr::all_of(gmg_value_cols),
                      names_to  = c("metric", "stat"),
                      names_sep = "__",
                      values_to = "value")
saveRDS(list(wide = gmg, long = gmg_long),
        "app_spatial/gm_metrics_by_gender.rds")
message("Saved app_spatial/gm_metrics_by_gender.rds (", nrow(gmg_long), " long rows)")

# ---- LAD BY GENDER ----------------------------------------------------------
ladg <- dplyr::bind_rows(lapply(names(scenarios),
                                function(l) summarise_lad_gender(l, scenarios[[l]])))
ladg_value_cols <- setdiff(names(ladg), c("ladcd", "ladnm", "gender", "n_people", "scenario"))
ladg_long <- ladg |>
  tidyr::pivot_longer(dplyr::all_of(ladg_value_cols),
                      names_to  = c("metric", "stat"),
                      names_sep = "__",
                      values_to = "value")
saveRDS(list(wide = ladg, long = ladg_long),
        "app_spatial/lad_metrics_by_gender.rds")
message("Saved app_spatial/lad_metrics_by_gender.rds (", nrow(ladg_long), " long rows)")
print(utils::head(ladg_long, 12))