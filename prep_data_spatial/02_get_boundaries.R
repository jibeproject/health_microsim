# =============================================================================
# STAGE 2 — Boundaries: download GM LSOA21 + LAD21 polygons once, cache to disk
# Produces: data/gm_lsoa_boundaries.rds  (used by the Shiny app)
#           data/gm_lad_boundaries.rds   (LAD outlines, overlaid on the app map)
# =============================================================================

pkgs <- c("sf", "readr", "dplyr", "arcgislayers")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

zones    <- readr::read_csv("prep_data_spatial/data/zoneSystem.csv", show_col_types = FALSE)
gm_lsoas <- unique(zones$lsoa21cd)

# LSOA21 BGC (generalised, clipped) FeatureServer, layer 0.
# ONS bumps the version periodically and retires old names, so try the known
# versions newest-first and use the first that opens.
lsoa_layer_names <- c(
  "Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5",
  "Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V4",
  "Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V3",
  "Lower_layer_Super_Output_Areas_2021_EW_BGC_V3",
  "Lower_layer_Super_Output_Areas_2021_EW_BGC_V2"
)
base_rest <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/"

open_first <- function(names) {
  for (nm in names) {
    url <- paste0(base_rest, nm, "/FeatureServer/0")
    fl  <- tryCatch(arcgislayers::arc_open(url), error = function(e) NULL)
    if (!is.null(fl)) {
      message("Using LSOA layer: ", nm)
      return(fl)
    }
    message("  not available: ", nm)
  }
  stop("None of the candidate LSOA layers opened. Find the current 'LSOA 2021 ",
       "Boundaries EW BGC' name at https://geoportal.statistics.gov.uk and add it.")
}

flayer <- open_first(lsoa_layer_names)
message("Service fields: ",
        paste(arcgislayers::list_fields(flayer)$name, collapse = ", "))

where_clause <- paste0(
  "LSOA21CD IN (",
  paste0("'", gm_lsoas, "'", collapse = ","),
  ")"
)

message("Downloading ", length(gm_lsoas), " LSOA boundaries from ONS...")
lsoa_sf <- arcgislayers::arc_select(
  flayer,
  where  = where_clause,
  fields = "LSOA21CD",
  crs    = 27700
) |> sf::st_make_valid()

if (nrow(lsoa_sf) == 0)
  stop("0 LSOAs returned. Check LSOA21CD codes look like 'E01...' and match.")

# standardise the join column name to lowercase to match the exposure data
names(lsoa_sf)[names(lsoa_sf) == "LSOA21CD"] <- "lsoa21cd"

if (!dir.exists("app_spatial")) dir.create("app_spatial", recursive = TRUE)
saveRDS(lsoa_sf, "app_spatial/gm_lsoa_boundaries.rds")
message("Saved app_spatial/gm_lsoa_boundaries.rds (", nrow(lsoa_sf), " LSOAs)")

# =============================================================================
# LAD21 boundaries for the 10 Greater Manchester local authority districts.
# Same ONS Geoportal source, different layer. Fetched as full (non-clipped)
# generalised BGC outlines and overlaid on the app map as heavy borders.
# =============================================================================

# The 10 GM metropolitan boroughs (LAD21CD). These are stable; if your
# zoneSystem.csv carries a LAD column you could derive them instead.
gm_lad_codes <- c(
  "E08000001",  # Bolton
  "E08000002",  # Bury
  "E08000003",  # Manchester
  "E08000004",  # Oldham
  "E08000005",  # Rochdale
  "E08000006",  # Salford
  "E08000007",  # Stockport
  "E08000008",  # Tameside
  "E08000009",  # Trafford
  "E08000010"   # Wigan
)

# LAD 2021 BGC FeatureServer candidates, newest-first (ONS retires old names).
# Verified current name carries a "_2022" suffix and omits "Boundaries".
lad_layer_names <- c(
  "Local_Authority_Districts_December_2021_UK_BGC_2022",
  "Local_Authority_Districts_December_2021_UK_BGC",
  "Local_Authority_Districts_(December_2021)_UK_BGC",
  "LAD_DEC_2021_UK_BGC"
)

lad_flayer <- open_first(lad_layer_names)
message("LAD service fields: ",
        paste(arcgislayers::list_fields(lad_flayer)$name, collapse = ", "))

lad_where <- paste0(
  "LAD21CD IN (",
  paste0("'", gm_lad_codes, "'", collapse = ","),
  ")"
)

message("Downloading ", length(gm_lad_codes), " LAD boundaries from ONS...")
lad_sf <- arcgislayers::arc_select(
  lad_flayer,
  where  = lad_where,
  fields = c("LAD21CD", "LAD21NM"),
  crs    = 27700
) |> sf::st_make_valid()

if (nrow(lad_sf) == 0)
  stop("0 LADs returned. Check LAD21CD codes look like 'E08...' and that the ",
       "layer exposes a 'LAD21CD' field (list printed above).")

# standardise column names to lowercase for consistency with the rest of the app
names(lad_sf)[names(lad_sf) == "LAD21CD"] <- "lad21cd"
names(lad_sf)[names(lad_sf) == "LAD21NM"] <- "lad21nm"

saveRDS(lad_sf, "app_spatial/gm_lad_boundaries.rds")
message("Saved app_spatial/gm_lad_boundaries.rds (", nrow(lad_sf), " LADs)")