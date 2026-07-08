# =============================================================================
# STAGE 1 — Preprocess: per-LSOA exposure statistics, per scenario
#
# Streams the four ~560MB person-level CSVs with DuckDB and, grouped by LSOA,
# computes per metric:
#   - percentile metrics (exposures + mmets): p05, p25, p50, p75
#   - mean-only metrics  (noise HA, HSD):     mean
# mmets = mmetHr_walk + mmetHr_cycle + mmetHr_otherSport (per person).
#
# Percentiles are computed DIRECTLY at LSOA level (percentiles can't be
# aggregated from OA-level percentiles), by joining zone(oaID) -> lsoa21cd
# inside DuckDB before the quantile step.
#
# Produces:  data/exposure_medians_by_lsoa.rds  (used by the app)
# NOTE: summaries are computed on ALL AGES (no age filter), matching
#       scripts/process_exp.R which generated the reference export CSV.
# =============================================================================

pkgs <- c("duckdb", "DBI", "dplyr", "readr", "tidyr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

data_dir  <- "prep_data_spatial/data"
zone_path <- "prep_data_spatial/data/zoneSystem.csv"

scenarios <- c(
  base       = "prep_data_spatial/data/pp_exposure_2021_base_220526.csv",
  goDutch    = "prep_data_spatial/data/pp_exposure_2021_goDutch_260526.csv",
  green      = "prep_data_spatial/data/pp_exposure_2021_green_260526.csv",
  safeStreet = "prep_data_spatial/data/pp_exposure_2021_safeStreet_260526.csv"
)

pctile_metrics <- c(
  "exposure_normalised_pm25",
  "exposure_normalised_no2",
  "exposure_normalised_noise_Lden",
  "exposure_normalised_ndvi",
  "mmets"
)
mean_metrics <- c("exposure_noise_HA", "exposure_noise_HSD")
mmets_sql <- "(mmetHr_walk + mmetHr_cycle + mmetHr_otherSport)"

con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA temp_directory='duckdb_tmp';")

# Register the zone -> LSOA lookup as a DuckDB table so we can join per person.
zones_lkp <- readr::read_csv(zone_path, show_col_types = FALSE) |>
  dplyr::select(oaID, lsoa21cd, lsoa21nm)
duckdb::duckdb_register(con, "zones_lkp", zones_lkp)

expr_for <- function(m) if (m == "mmets") mmets_sql else paste0("e.", m)

pctile_exprs <- unlist(lapply(pctile_metrics, function(m) {
  e <- if (m == "mmets")
    "(e.mmetHr_walk + e.mmetHr_cycle + e.mmetHr_otherSport)"
  else paste0("e.", m)
  c(
    sprintf("quantile_cont(%s, 0.05) AS %s__p05", e, m),
    sprintf("quantile_cont(%s, 0.25) AS %s__p25", e, m),
    sprintf("quantile_cont(%s, 0.50) AS %s__p50", e, m),
    sprintf("quantile_cont(%s, 0.75) AS %s__p75", e, m)
  )
}))
mean_exprs <- vapply(mean_metrics, function(m)
  sprintf("avg(e.%s) AS %s__mean", m, m), character(1))
agg_sql <- paste(c(pctile_exprs, mean_exprs), collapse = ",\n  ")

summarise_one <- function(label, csv_file) {
  path <- csv_file   # scenarios already hold full paths under prep_data_spatial/data
  if (!file.exists(path)) {
    warning("Missing file for '", label, "': ", path, " — skipped.")
    return(NULL)
  }
  message("Processing ", label, " ...")
  sql <- sprintf("
    SELECT z.lsoa21cd,
           z.lsoa21nm,
           count(*) AS n_people,
           %s
    FROM read_csv_auto('%s', header=true, sample_size=-1) e
    JOIN zones_lkp z ON e.zone = z.oaID
    GROUP BY z.lsoa21cd, z.lsoa21nm",
                 agg_sql, path)
  res <- dbGetQuery(con, sql)
  res$scenario <- label
  res
}

all_summaries <- lapply(names(scenarios),
                        function(l) summarise_one(l, scenarios[[l]]))
summ <- dplyr::bind_rows(all_summaries)

value_cols <- setdiff(names(summ),
                      c("lsoa21cd", "lsoa21nm", "n_people", "scenario"))

long <- summ |>
  tidyr::pivot_longer(dplyr::all_of(value_cols),
                      names_to = c("metric", "stat"),
                      names_sep = "__",
                      values_to = "value")

if (!dir.exists("app_spatial")) dir.create("app_spatial")
saveRDS(list(wide = summ, long = long),
        "app_spatial/exposure_medians_by_lsoa.rds")

message("Done. Saved app_spatial/exposure_medians_by_lsoa.rds")
message("Rows (LSOA x scenario): ", nrow(summ))
message("Percentile metrics: ", paste(pctile_metrics, collapse = ", "))
message("Mean-only metrics:  ", paste(mean_metrics, collapse = ", "))