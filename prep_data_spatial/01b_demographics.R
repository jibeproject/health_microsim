# =============================================================================
# STAGE 1b — Demographic profile per LSOA and per LAD (synthetic population)
#
# Chains person -> household -> zone(OA) -> LSOA -> LAD, then summarises:
#   age:    mean, min, max
#   gender: counts/percent  (1 = male, 2 = female)
#   IMD:    the LSOA's own decile (imd10), plus a 5-group quintile
#           (decile 1-2 -> Q1, 3-4 -> Q2, 5-6 -> Q3, 7-8 -> Q4, 9-10 -> Q5)
#
# Produces:  data/lsoa_profile.rds   (per-LSOA, used by the app hover)
#            data/lad_profile.rds    (per-LAD, used by the app click popup;
#                                     includes % of population in each IMD quintile)
# These are scenario-independent (same population across scenarios).
# =============================================================================

pkgs <- c("duckdb", "DBI", "dplyr", "readr", "tidyr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

pp_path   <- "prep_data_spatial/data/pp_2021.csv"
hh_path   <- "prep_data_spatial/data/hh_2021.csv"
zone_path <- "prep_data_spatial/data/zoneSystem.csv"

con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA temp_directory='duckdb_tmp';")

# person -> household gives zone (oaID). Aggregate age/gender by zone first
# (DuckDB streams both CSVs; only the join keys + age/gender are needed).
# Age bands: <18, 18-24, 25-44, 45-65, 66+
sql <- sprintf("
  SELECT h.zone               AS oaID,
         count(*)             AS n_people,
         avg(p.age)           AS age_mean,
         min(p.age)           AS age_min,
         max(p.age)           AS age_max,
         sum(CASE WHEN p.gender = 1 THEN 1 ELSE 0 END) AS n_male,
         sum(CASE WHEN p.gender = 2 THEN 1 ELSE 0 END) AS n_female,
         sum(CASE WHEN p.age < 18                  THEN 1 ELSE 0 END) AS age_u18,
         sum(CASE WHEN p.age >= 18 AND p.age <= 24 THEN 1 ELSE 0 END) AS age_18_24,
         sum(CASE WHEN p.age >= 25 AND p.age <= 44 THEN 1 ELSE 0 END) AS age_25_44,
         sum(CASE WHEN p.age >= 45 AND p.age <= 65 THEN 1 ELSE 0 END) AS age_45_65,
         sum(CASE WHEN p.age >= 66                 THEN 1 ELSE 0 END) AS age_66plus
  FROM read_csv_auto('%s', header=true) p
  JOIN read_csv_auto('%s', header=true) h
    ON p.hhid = h.id
  GROUP BY h.zone",
               pp_path, hh_path)

by_oa <- dbGetQuery(con, sql)
message("Aggregated population for ", nrow(by_oa), " OAs.")

# ---- attach LSOA + LAD + IMD from the lookup, then roll OA -> LSOA -----------
zones <- readr::read_csv(zone_path, show_col_types = FALSE) |>
  dplyr::select(oaID, lsoa21cd, lsoa21nm, ladcd, ladnm, imd10)

prof <- by_oa |>
  dplyr::left_join(zones, by = "oaID")

# IMD quintile: collapse 10 deciles into 5 groups (1-2->1, 3-4->2, ... 9-10->5)
imd_to_quintile <- function(d) as.integer(ceiling(d / 2))

lsoa_profile <- prof |>
  dplyr::mutate(age_pop = age_mean * n_people) |>   # weight before grouping
  dplyr::group_by(lsoa21cd, lsoa21nm, ladcd, ladnm) |>
  dplyr::summarise(
    n_people = sum(n_people),
    # population-weighted mean age across the OAs in the LSOA
    age_mean = sum(age_pop) / sum(n_people),
    age_min  = min(age_min),
    age_max  = max(age_max),
    n_male   = sum(n_male),
    n_female = sum(n_female),
    age_u18    = sum(age_u18),
    age_18_24  = sum(age_18_24),
    age_25_44  = sum(age_25_44),
    age_45_65  = sum(age_45_65),
    age_66plus = sum(age_66plus),
    imd10    = dplyr::first(imd10),    # constant within LSOA (verified)
    .groups  = "drop"
  ) |>
  dplyr::mutate(
    imd_quintile = imd_to_quintile(imd10),
    pct_male     = 100 * n_male   / (n_male + n_female),
    pct_female   = 100 * n_female / (n_male + n_female),
    pct_u18      = 100 * age_u18    / n_people,
    pct_18_24    = 100 * age_18_24  / n_people,
    pct_25_44    = 100 * age_25_44  / n_people,
    pct_45_65    = 100 * age_45_65  / n_people,
    pct_66plus   = 100 * age_66plus / n_people
  )

if (!dir.exists("app_spatial")) dir.create("app_spatial", recursive = TRUE)
saveRDS(lsoa_profile, "app_spatial/lsoa_profile.rds")
message("Saved app_spatial/lsoa_profile.rds (", nrow(lsoa_profile), " LSOAs)")
print(utils::head(lsoa_profile))

# ---- roll LSOA -> LAD -------------------------------------------------------
# Population-weighted summaries per LAD. The IMD quintile breakdown is the
# share of *people* living in LSOAs of each quintile (each LSOA's quintile
# weighted by its population), expressed as a percentage of the LAD total.
lad_core <- lsoa_profile |>
  dplyr::mutate(age_pop = age_mean * n_people) |>   # weight before grouping
  dplyr::group_by(ladcd, ladnm) |>
  dplyr::summarise(
    n_people = sum(n_people),
    n_lsoas  = dplyr::n(),
    age_mean = sum(age_pop) / sum(n_people),         # population-weighted mean age
    age_min  = min(age_min),
    age_max  = max(age_max),
    n_male   = sum(n_male),
    n_female = sum(n_female),
    age_u18    = sum(age_u18),
    age_18_24  = sum(age_18_24),
    age_25_44  = sum(age_25_44),
    age_45_65  = sum(age_45_65),
    age_66plus = sum(age_66plus),
    .groups  = "drop"
  ) |>
  dplyr::mutate(
    pct_male   = 100 * n_male   / (n_male + n_female),
    pct_female = 100 * n_female / (n_male + n_female),
    pct_u18    = 100 * age_u18    / n_people,
    pct_18_24  = 100 * age_18_24  / n_people,
    pct_25_44  = 100 * age_25_44  / n_people,
    pct_45_65  = 100 * age_45_65  / n_people,
    pct_66plus = 100 * age_66plus / n_people
  )

# population in each IMD quintile, per LAD, as % of the LAD population
lad_imd <- lsoa_profile |>
  dplyr::group_by(ladcd, imd_quintile) |>
  dplyr::summarise(q_people = sum(n_people), .groups = "drop") |>
  dplyr::group_by(ladcd) |>
  dplyr::mutate(pct = 100 * q_people / sum(q_people)) |>
  dplyr::ungroup() |>
  dplyr::select(ladcd, imd_quintile, pct) |>
  tidyr::complete(ladcd, imd_quintile = 1:5, fill = list(pct = 0)) |>
  tidyr::pivot_wider(
    names_from   = imd_quintile,
    values_from  = pct,
    names_prefix = "imd_q",
    values_fill  = 0
  )

lad_profile <- lad_core |>
  dplyr::left_join(lad_imd, by = "ladcd")

saveRDS(lad_profile, "app_spatial/lad_profile.rds")
message("Saved app_spatial/lad_profile.rds (", nrow(lad_profile), " LADs)")
print(utils::head(lad_profile))