###############################################################################
## functions/microsim_engine.R
##
## Individual-level health microsimulation for the JIBE health model: a closed
## cohort (the 2021 synthetic population) followed for n_cycles annual cycles,
## for a reference scenario and any number of other scenarios.
##
## Ported from run_micro_opt.R in the UBD repo, keeping only its "with_mm"
## mortality structure:
##   mortality : all-cause rate x exposure RR x multimorbidity RR
##               (multimorbidity RR by the number of modelled diseases held at
##               the start of the cycle, Hanlon et al. 2022: 0, 1, ..., 6+)
##   incidence : disease rate x exposure RR (x diabetes RR for CHD and stroke)
## The all-cause exposure RR is a total effect, so the part of the exposure
## effect that acts through the modelled diseases is partly counted twice
## (it also raises disease incidence, and so the multimorbidity RR).
##
## Inputs (all read by run_microsim()):
##   rates      : rate_raw by lsoa21cd x sex x age (0-100) x cause, as written by
##                scripts/transitions_calibration.R. all_cause_mortality rows are
##                deaths; every other cause is incidence. rate_raw, not rate: the
##                engine does the PIF adjustment itself, every cycle (below).
##   zones      : zoneSystem.csv (oaID -> lsoa21cd, ladcd, ladnm, imd10)
##   RR files   : the Java model's pp_rr_2021.csv, one per scenario: id, age,
##                gender, zone and rr_<EXPOSURE>_<cause> columns. A person's RR
##                for a cause is the product of the selected exposures' columns.
##   prevalence : diseases held at the start, one row per person with at least
##                one disease: id, diseases (space-separated), as written by
##                scripts/assign_prevalence.R
##   remission  : optional, table of remission rates (see derive_remission())
##
## Each cycle: all-cause mortality -> disease onsets (alphabetical) -> remission.
## Risks use the state at the start of the cycle. No relative risks below
## rr_min_age (20). Everyone still alive at force_death_age (100) dies. Each
## person's 2021 exposures (so their RRs) are kept for life. Same random
## numbers in every scenario (same seed, one draw per person per step), so
## differences between scenarios are not simulation noise.
##
## Rate calibration, every cycle: input rates already reflect the population's
## exposures, so multiplying them by individual RRs would overshoot. Each
## person's rate is
##   rate_raw(LSOA) x RR x [sum rate_raw(LSOA) / sum rate_raw(LSOA) x RR]
## with the sums over the people at risk in the same age x sex x IMD decile
## cell (at risk = alive for mortality; alive without the disease for
## incidence). The expected number of events in each cell then equals the
## number implied by the input rates. With rates constant within a cell (UBD)
## this is UBD's "divide by the mean RR"; weighting by the LSOA rate makes it
## exact when rates vary by LSOA within the cell. The factors come from the
## reference run and are reused unchanged in the other scenarios
## (recalibrating there would cancel the scenario effect). Calibrating every
## cycle, not only in 2021, stops rates drifting as the cohort ages with its
## 2021 exposures.
##
## Outputs, per scenario (out_dir/scenario_<name>/), aggregated by cycle x age
## at the start of the cycle x sex x IMD decile x local authority:
##   agg_cycle : people at the start, deaths, person-years, people at the end
##   agg_inc   : per cause, people at risk, onsets, expected cases
##               (incidence rate x time without the disease) and person-years
##               free of the disease
##   agg_prev  : per disease, people with the disease at the end of the cycle
##   agg_mm    : people at the start and deaths, by number of diseases held
##   persons   : one row per person: death cycle, baseline diseases, first onset
##               cycle of each disease, episodes of remitting diseases
## and, for the reference scenario, calibration (the factors by cycle x cell x
## cause). Baseline age = age - cycle + 1.
###############################################################################

suppressPackageStartupMessages(library(data.table))

KNOWN_EXPOSURES <- c("AIR_POLLUTION_PM25", "AIR_POLLUTION_NO2", "PHYSICAL_ACTIVITY", "NOISE", "NDVI")

# Settings ======================================================================
engine_settings <- function(...) {
  s <- list(
    n_cycles        = 30L,
    seed            = 2L,
    sample_prop     = 1,        # < 1: random sample of people (for testing)
    sample_seed     = 99L,
    rr_min_age      = 20L,      # no relative risks (so no calibration) below this age
    force_death_age = 100L,     # everyone alive at the start of a cycle at this age dies
    exposures       = KNOWN_EXPOSURES,
    mm_rr           = c(1.00, 1.23, 1.62, 2.09, 2.77, 3.46, 5.14),  # 0, 1, ..., 6+ diseases
    dm_rr           = list(coronary_heart_disease = c(male = 2.16, female = 2.82),
                           stroke                 = c(male = 1.83, female = 2.28)),
    mortality_cause = "all_cause_mortality",
    output_format   = "parquet", # or "csv" (written as .csv.gz)
    write_persons   = TRUE)
  args <- list(...)
  bad <- setdiff(names(args), names(s))
  if (length(bad)) stop("Unknown setting(s): ", paste(bad, collapse = ", "), call. = FALSE)
  s[names(args)] <- args
  bad <- setdiff(s$exposures, KNOWN_EXPOSURES)
  if (length(bad)) stop("Unknown exposure(s): ", paste(bad, collapse = ", "), call. = FALSE)
  s
}

# Small helpers =================================================================
is_parquet <- function(path) grepl("\\.parquet$", path, ignore.case = TRUE)

need_arrow <- function(path) {
  if (!requireNamespace("arrow", quietly = TRUE))
    stop("The arrow package is needed for ", path, call. = FALSE)
}

read_any <- function(path, select = NULL) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  if (is_parquet(path)) {
    need_arrow(path)
    x <- as.data.table(arrow::read_parquet(path))
    if (!is.null(select)) x <- x[, intersect(select, names(x)), with = FALSE]
    return(x)
  }
  fread(path, select = select, showProgress = FALSE)
}

column_names <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  if (is_parquet(path)) { need_arrow(path); return(names(arrow::read_parquet(path, as_data_frame = FALSE)$schema)) }
  names(fread(path, nrows = 0L))
}

write_any <- function(x, path_base, format) {
  if (format == "parquet") { need_arrow(path_base); arrow::write_parquet(x, paste0(path_base, ".parquet")) }
  else fwrite(x, paste0(path_base, ".csv.gz"))
}

check_cols <- function(x, need, what) {
  miss <- setdiff(need, names(x))
  if (length(miss)) stop(what, " has no column(s): ", paste(miss, collapse = ", "), call. = FALSE)
}

# Sex as 1 (male) / 2 (female); accepts 1/2, m/f, male/female
sex_code <- function(x, what = "sex") {
  v <- trimws(tolower(as.character(x)))
  out <- ifelse(v %in% c("1", "1.0", "m", "male", "males"), 1L,
                ifelse(v %in% c("2", "2.0", "f", "female", "females"), 2L, NA_integer_))
  if (anyNA(out)) stop(what, ": ", sum(is.na(out)), " value(s) not coded as male (1) / female (2), e.g. ",
                       paste(head(unique(x[is.na(out)]), 5), collapse = ", "), call. = FALSE)
  out
}

# Cause names as in the rates file (vintages of the inputs spell some differently)
normalise_cause <- function(x) {
  x <- gsub("’", "'", x)
  x <- gsub("-", "_", x)
  x <- sub("^parkinson'?s_disease$", "parkinson", x)
  sub("^head_and_neck_cancer$", "head_neck_cancer", x)
}

# Counts (w = NULL) or sums of w by key (1..nb), over the people in mask
tab <- function(key, mask, nb, w = NULL) {
  k <- key[mask]
  if (is.null(w)) return(tabulate(k, nb))
  out <- numeric(nb)
  if (length(k)) { s <- rowsum(w[mask], k); out[as.integer(rownames(s))] <- s[, 1] }
  out
}

# Inputs ========================================================================
read_zones <- function(path) {
  z <- read_any(path)
  check_cols(z, c("oaID", "lsoa21cd", "ladcd", "imd10"), path)
  if (!"ladnm" %in% names(z)) z[, ladnm := ladcd]
  z <- z[, .(zone = as.character(oaID), lsoa21cd = as.character(lsoa21cd), ladcd = as.character(ladcd),
             ladnm = as.character(ladnm), imd_decile = suppressWarnings(as.integer(imd10)))]
  if (anyDuplicated(z$zone)) stop(path, ": duplicated oaID.", call. = FALSE)
  # IMD decile is set per LSOA; if a zone file ever disagrees within an LSOA, take the commonest
  n_dec <- z[!is.na(imd_decile), uniqueN(imd_decile), by = lsoa21cd][V1 > 1L]
  if (nrow(n_dec)) {
    warning(nrow(n_dec), " LSOA(s) have zones with different IMD deciles; using the commonest.", call. = FALSE)
    modal <- z[!is.na(imd_decile), .N, by = .(lsoa21cd, imd_decile)][order(-N)][!duplicated(lsoa21cd)]
    z[modal, imd_decile := i.imd_decile, on = "lsoa21cd"]
  }
  z
}

read_rates <- function(path) {
  r <- read_any(path, select = c("lsoa21cd", "sex", "age", "cause", "rate_raw"))
  check_cols(r, c("lsoa21cd", "sex", "age", "cause", "rate_raw"), path)
  r <- r[, .(lsoa21cd = as.character(lsoa21cd), sex = sex_code(sex, paste(path, "sex")), age = as.integer(age),
             cause = normalise_cause(as.character(cause)), rate_raw = as.numeric(rate_raw))]
  if (anyNA(r$rate_raw) || any(r$rate_raw < 0)) stop(path, ": missing or negative rate_raw.", call. = FALSE)
  if (anyDuplicated(r, by = c("lsoa21cd", "sex", "age", "cause")))
    stop(path, ": more than one rate per lsoa21cd x sex x age x cause.", call. = FALSE)
  r
}

# Population and relative risks from one pp_rr file. Returns the people (with
# their LSOA, local authority and IMD decile) and a matrix of RRs, people x causes.
read_rr_file <- function(path, zones, exposures, causes) {
  hdr <- column_names(path)
  miss <- setdiff(c("id", "age", "gender", "zone"), hdr)
  if (length(miss)) stop(path, " has no column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  rr_cols <- grep("^rr_", hdr, value = TRUE)
  pat <- sprintf("^rr_(%s)_(.+)$", paste(KNOWN_EXPOSURES, collapse = "|"))
  ok <- grepl(pat, rr_cols)
  if (any(!ok)) warning(path, ": ignoring RR column(s) with an unknown exposure: ",
                        paste(rr_cols[!ok], collapse = ", "), call. = FALSE)
  map <- data.table(col = rr_cols[ok], exposure = sub(pat, "\\1", rr_cols[ok]),
                    cause = normalise_cause(sub(pat, "\\2", rr_cols[ok])))[exposure %in% exposures]
  unused <- setdiff(map$cause, causes)
  if (length(unused)) warning(path, ": RR columns for causes not in the rates file (ignored): ",
                              paste(unused, collapse = ", "), call. = FALSE)
  map <- map[cause %in% causes]

  p <- read_any(path, select = c("id", "age", "gender", "zone", map$col))
  RR <- matrix(1, nrow(p), length(causes), dimnames = list(NULL, causes))
  n_na <- 0L
  for (cz in unique(map$cause)) for (cl in map[cause == cz, col]) {
    v <- as.numeric(p[[cl]]); n_na <- n_na + sum(is.na(v)); v[is.na(v)] <- 1
    RR[, cz] <- RR[, cz] * v
  }
  if (n_na) warning(path, ": ", n_na, " missing RR value(s) set to 1.", call. = FALSE)
  pop <- p[, .(id = as.character(id), age0 = as.integer(age), sex = sex_code(gender, paste(path, "gender")),
               zone = as.character(zone))]
  pop[zones, `:=`(lsoa21cd = i.lsoa21cd, ladcd = i.ladcd, ladnm = i.ladnm, imd_decile = i.imd_decile), on = "zone"]
  if (anyNA(pop$age0)) stop(path, ": people without age.", call. = FALSE)
  if (anyDuplicated(pop$id)) stop(path, ": duplicated id.", call. = FALSE)
  list(pop = pop, RR = RR, map = map)
}

# Diseases held at the start: logical matrix, people x diseases
read_baseline_prevalence <- function(path, ids, diseases) {
  H0 <- matrix(FALSE, length(ids), length(diseases), dimnames = list(NULL, diseases))
  if (is.null(path)) { warning("No baseline prevalence file: everyone starts without disease.", call. = FALSE); return(H0) }
  pv <- read_any(path, select = c("id", "diseases"))
  check_cols(pv, c("id", "diseases"), path)
  pv <- pv[as.character(id) %in% ids]
  lst <- strsplit(trimws(as.character(pv$diseases)), "\\s+")
  long <- data.table(id = rep(as.character(pv$id), lengths(lst)), d = normalise_cause(unlist(lst)))
  unknown <- setdiff(unique(long$d), c(diseases, "healthy", ""))
  if (length(unknown)) warning(path, ": disease(s) not in the rates file (ignored): ",
                               paste(unknown, collapse = ", "), call. = FALSE)
  long <- long[d %in% diseases]
  H0[cbind(match(long$id, ids), match(long$d, diseases))] <- TRUE
  H0
}

# Remission rates for a disease that remits (depression), set so that its
# incidence reproduces its prevalence in steady state, as in UBD
# (data_prep_mslt.R): rem = inc x (1 - prev) / prev, bounded to `bounds` per
# year, for ages >= min_age with prev > 0; younger ages take the value at
# min_age. inc is the incidence among people without the disease (rate_raw).
#   prevalence: data.frame with age, sex, prevalence (proportion) and,
#   optionally, lsoa21cd or imd_decile: remission is computed at that level,
#   with incidence averaged over the LSOAs in it.
# Returns age, sex, [lsoa21cd | imd_decile], cause, rate, for run_microsim().
derive_remission <- function(rates, prevalence, zones, cause = "depression", min_age = 15L, bounds = c(0.5, 2)) {
  cz <- cause
  pv <- as.data.table(prevalence)
  check_cols(pv, c("age", "sex", "prevalence"), "prevalence")
  keys <- intersect(c("lsoa21cd", "imd_decile"), names(pv))[1]
  keys <- if (is.na(keys)) character(0) else keys
  pv <- pv[, c("age", "sex", keys, "prevalence"), with = FALSE]
  pv[, `:=`(age = as.integer(age), sex = sex_code(sex))]
  inc <- rates[cause == cz]
  if (!nrow(inc)) stop("No incidence rates for ", cz, call. = FALSE)
  if (identical(keys, "imd_decile")) {
    pv[, imd_decile := as.integer(imd_decile)]
    inc[unique(zones[, .(lsoa21cd, imd_decile)]), imd_decile := i.imd_decile, on = "lsoa21cd"]
  }
  inc <- inc[, .(inc = mean(rate_raw)), by = c("age", "sex", keys)]
  x <- merge(inc, pv, by = c("age", "sex", keys), all.x = TRUE)
  setorderv(x, c("sex", keys, "age"))
  x[, rate := fifelse(age >= min_age & !is.na(prevalence) & prevalence > 0,
                      pmin(pmax(inc * (1 - pmin(prevalence, 0.999)) / prevalence, bounds[1]), bounds[2]), NA_real_)]
  x[, rate := nafill(nafill(rate, type = "nocb"), type = "locf"), by = c("sex", keys)]
  x[is.na(rate), rate := 0]
  x[, cause := cz]
  x[, c("age", "sex", keys, "cause", "rate"), with = FALSE][]
}

# Rates as vectors indexed by (age 0..100, sex, LSOA): (age + 1) + 101 (sex - 1) + 202 (lsoa - 1)
build_rate_arrays <- function(rates, remission, lsoas, lsoa_dec, causes) {
  L <- length(lsoas); NC <- 101L * 2L * L
  li_of <- function(age, sex, lsoa21cd) (pmin(pmax(age, 0L), 100L) + 1L) + 101L * (sex - 1L) + 202L * (match(lsoa21cd, lsoas) - 1L)
  r <- rates[lsoa21cd %in% lsoas & age >= 0L & age <= 100L]
  miss <- setdiff(lsoas, r$lsoa21cd)
  if (length(miss)) stop(length(miss), " LSOA(s) of the population have no rates, e.g. ",
                         paste(head(miss, 5), collapse = ", "), call. = FALSE)
  RATE <- sapply(causes, function(cz) {
    a <- numeric(NC); s <- r[cause == cz]; a[li_of(s$age, s$sex, s$lsoa21cd)] <- s$rate_raw; a
  }, simplify = FALSE)
  REM <- list()
  if (!is.null(remission) && nrow(remission)) {
    rm_dt <- as.data.table(remission)
    check_cols(rm_dt, c("age", "sex", "cause", "rate"), "remission")
    rm_dt[, `:=`(age = as.integer(age), sex = sex_code(sex, "remission sex"), cause = normalise_cause(cause))]
    for (cz in intersect(unique(rm_dt$cause), causes)) {
      s <- rm_dt[cause == cz]
      key <- intersect(c("lsoa21cd", "imd_decile"), names(s))[1]
      s <- if (is.na(key)) s[rep(seq_len(.N), each = L)][, lsoa21cd := rep(lsoas, nrow(s))]
           else merge(s, lsoa_dec, by = key, allow.cartesian = TRUE)
      s <- s[lsoa21cd %in% lsoas & age >= 0L & age <= 100L]
      a <- numeric(NC); a[li_of(s$age, s$sex, s$lsoa21cd)] <- s$rate
      if (any(a > 0)) REM[[cz]] <- a
    }
  }
  list(RATE = RATE, REM = REM)
}

# Probability of still having the disease at the end of the year, for a
# two-state process with incidence and remission (exact one-year solution)
remission_stay_prob <- function(inc, rem, prevalent_at_start) {
  inc[is.na(inc) | inc < 0] <- 0; rem[is.na(rem) | rem < 0] <- 0
  k <- inc + rem; ek <- exp(-k)
  piC <- fifelse(k > 0, inc / k, 0)
  p_prev <- piC + (1 - piC) * ek
  p_new  <- fifelse(inc > 1e-12, piC * (1 - ek) / (1 - exp(-inc)), fifelse(rem > 0, (1 - exp(-rem)) / rem, 1))
  p <- fifelse(prevalent_at_start, p_prev, p_new)
  p[rem <= 0] <- 1
  pmin(pmax(p, 0), 1)
}

# Engine ========================================================================
# inp: list(pop, H0, RATE, REM, diseases, lads) from run_microsim(). RR: people x
# causes. calib: NULL for the reference run (computes the calibration factors);
# for other scenarios, the reference run's factors.
NB_CAL <- 101L * 2L * 10L                         # calibration cells: age x sex x IMD decile

run_scenario <- function(inp, RR, settings, calib = NULL, label = "") {
  s <- settings; n.c <- s$n_cycles
  MORT <- s$mortality_cause; D <- inp$diseases; CAUSES <- c(MORT, D)
  N <- nrow(inp$pop); is_ref <- is.null(calib)
  CAL <- if (is_ref) sapply(CAUSES, function(cz) matrix(1, NB_CAL, n.c), simplify = FALSE) else calib
  CELL_AS <- (seq_len(NB_CAL) - 1L) %% 202L + 1L  # age x sex cell of each calibration cell (deciles pooled)
  NB  <- NB_CAL * length(inp$lads)                # output strata: age x sex x decile x local authority
  N_MM <- length(s$mm_rr)                         # disease counts 0 .. N_MM - 1 (last = that many or more)
  DM_RR <- s$dm_rr[intersect(names(s$dm_rr), D)]
  if (length(DM_RR) && !"diabetes" %in% D) DM_RR <- list()
  REMIT <- names(inp$REM)

  sex <- inp$pop$sex; dec <- inp$pop$imd_decile; age0 <- inp$pop$age0
  lsoa <- inp$pop$lsoa_idx; lad <- inp$pop$lad_idx
  sexlab <- c("male", "female")[sex]
  alive <- rep(TRUE, N); H <- inp$H0
  death_cycle <- rep(NA_integer_, N)
  onset <- matrix(NA_integer_, N, length(D), dimnames = list(NULL, D))
  episodes <- matrix(0L, N, length(REMIT), dimnames = list(NULL, REMIT))
  A_cyc <- A_inc <- A_prev <- A_mm <- vector("list", n.c)
  set.seed(s$seed)

  for (t in seq_len(n.c)) {
    age_t <- age0 + t - 1L
    a  <- pmin(pmax(age_t, 0L), 100L)
    li <- (a + 1L) + 101L * (sex - 1L) + 202L * (lsoa - 1L)   # rate cell
    kc <- (a + 1L) + 101L * (sex - 1L) + 202L * (dec - 1L)    # calibration cell
    ko <- kc + NB_CAL * (lad - 1L)                            # output stratum
    young <- age_t < s$rr_min_age
    rr_of <- function(cz) { r <- RR[, cz]; r[young] <- 1; r }
    # Calibration factor for each person: sum of base rates / sum of base rate x RR
    # over the people at risk in their cell, from the reference run. Cells with
    # nobody at risk in the reference run (the oldest ages; another scenario may
    # have survivors there) take the factor pooled over deciles at the same age and sex.
    calibrate <- function(cz, base, rr, mask) {
      if (is_ref) {
        num <- tab(kc, mask, NB_CAL, base); den <- tab(kc, mask, NB_CAL, base * rr)
        num_g <- rowsum(num, CELL_AS)[, 1]; den_g <- rowsum(den, CELL_AS)[, 1]
        CAL[[cz]][, t] <<- fifelse(den > 0, num / den,
                                   fifelse(den_g[CELL_AS] > 0, num_g[CELL_AS] / den_g[CELL_AS], 1))
      }
      CAL[[cz]][kc, t]
    }

    alive_start <- alive
    H_start <- H
    n_dis_start <- pmin(rowSums(H), N_MM - 1L)

    # 1. all-cause mortality: rate x exposure RR x multimorbidity RR
    mm <- s$mm_rr[n_dis_start + 1L]; mm[young] <- 1
    rr <- rr_of(MORT) * mm
    base <- inp$RATE[[MORT]][li]
    rate <- base * rr * calibrate(MORT, base, rr, alive_start)
    u <- runif(N)
    die <- alive & (age_t >= s$force_death_age | u < 1 - exp(-rate))
    alive[die] <- FALSE; death_cycle[die] <- t
    inc_rows <- list(data.table(k = seq_len(NB), cause = MORT, at_risk = tab(ko, alive_start, NB),
                                onsets = tab(ko, die, NB)))
    inc_rows[[1]][, `:=`(cases = as.numeric(onsets), py_free = at_risk - 0.5 * onsets)]

    # 2. disease onsets (people alive after step 1, without the disease)
    inc_rate <- new_onset <- list()
    for (d in D) {
      at_risk <- alive & !H[, d]
      rr <- rr_of(d)
      if (d %in% names(DM_RR)) {
        dm <- fifelse(H_start[, "diabetes"], unname(DM_RR[[d]][sexlab]), 1); dm[young] <- 1
        rr <- rr * dm
      }
      base <- inp$RATE[[d]][li]
      rate <- base * rr * calibrate(d, base, rr, at_risk)
      u <- runif(N)
      new <- at_risk & u < 1 - exp(-rate)
      H[new, d] <- TRUE
      first <- new & is.na(onset[, d]); onset[first, d] <- t
      if (d %in% REMIT) episodes[new, d] <- episodes[new, d] + 1L
      inc_rate[[d]] <- rate; new_onset[[d]] <- new
    }

    # 3. remission
    for (d in REMIT) {
      p_stay <- remission_stay_prob(inc_rate[[d]], inp$REM[[d]][li], H_start[, d])
      u <- runif(N)
      H[alive & H[, d] & u >= p_stay, d] <- FALSE
    }

    # Incident cases counted as in a life table: incidence rate x person-time
    # without the disease (1 for survivors free all year; 0.5 for those free at
    # the start who had an onset or died). For remitting diseases, the expected
    # time without the disease comes from the exact two-state solution (incidence
    # i, remission r, k = i + r, f = (1 - exp(-k)) / k): free at the start
    # 1 - i/k (1 - f); with the disease at the start r/k (1 - f).
    for (d in D) {
      free0 <- alive_start & !H_start[, d]
      t_free <- numeric(N)
      if (d %in% REMIT) {
        i <- inc_rate[[d]]; r <- inp$REM[[d]][li]; k <- i + r
        f <- fifelse(k > 1e-12, (1 - exp(-k)) / k, 1)
        e_free <- fifelse(k > 0, 1 - i / k * (1 - f), 1)
        e_prev <- fifelse(k > 0, r / k * (1 - f), 0)
        m <- free0 & alive;        t_free[m] <- e_free[m]
        m <- alive & H_start[, d]; t_free[m] <- e_prev[m]
        t_free[free0 & die] <- 0.5
      } else {
        t_free[free0 & alive] <- 1
        t_free[free0 & (die | new_onset[[d]])] <- 0.5
      }
      m <- t_free > 0
      inc_rows[[length(inc_rows) + 1L]] <- data.table(
        k = seq_len(NB), cause = d, at_risk = tab(ko, alive & !H_start[, d], NB),
        onsets = tab(ko, new_onset[[d]], NB), cases = tab(ko, m, NB, inc_rate[[d]] * t_free),
        py_free = tab(ko, m, NB, t_free))
    }

    # 4. record
    A_cyc[[t]] <- data.table(k = seq_len(NB), cycle = t, n_start = tab(ko, alive_start, NB),
                             deaths = tab(ko, die, NB), py = tab(ko, alive, NB) + 0.5 * tab(ko, die, NB),
                             n_end = tab(ko, alive, NB), n_end_healthy = tab(ko, alive & rowSums(H) == 0, NB))
    A_inc[[t]] <- rbindlist(inc_rows)[, cycle := t]
    A_mm[[t]] <- data.table(k_mm = seq_len(NB * N_MM), cycle = t,
                            n_start = tabulate(ko[alive_start] + NB * n_dis_start[alive_start], NB * N_MM),
                            deaths = tabulate(ko[die] + NB * n_dis_start[die], NB * N_MM))
    A_prev[[t]] <- rbindlist(lapply(D, function(d)
      data.table(k = seq_len(NB), cause = d, n_with = tab(ko, alive & H[, d], NB))))[, cycle := t]
    if (t %% 10L == 0L || t == 1L)
      message(sprintf("  [%s] cycle %d: alive %s", label, t, format(sum(alive), big.mark = ",")))
  }

  strata <- data.table(k = seq_len(NB))[, `:=`(
    age = (k - 1L) %% 101L, sex = c("male", "female")[((k - 1L) %/% 101L) %% 2L + 1L],
    imd_decile = ((k - 1L) %/% 202L) %% 10L + 1L, ladcd = inp$lads[(k - 1L) %/% NB_CAL + 1L])]
  fin <- function(lst, keep) {
    x <- rbindlist(lst)
    x <- x[rowSums(x[, keep, with = FALSE]) > 0]
    x <- strata[x, on = "k"][, k := NULL]
    setcolorder(x, intersect(c("cycle", "age", "sex", "imd_decile", "ladcd", "cause"), names(x)))[]
  }
  mmt <- rbindlist(A_mm)[n_start > 0]
  mmt[, `:=`(k = (k_mm - 1L) %% NB + 1L, n_dis = (k_mm - 1L) %/% NB)][, k_mm := NULL]
  mmt <- strata[mmt, on = "k"][, k := NULL]
  setcolorder(mmt, c("cycle", "age", "sex", "imd_decile", "ladcd", "n_dis"))

  persons <- NULL
  if (isTRUE(s$write_persons)) {
    persons <- inp$pop[, .(id, age0, sex = c("male", "female")[sex], lsoa21cd, ladcd, imd_decile)]
    persons[, death_cycle := death_cycle]
    for (d in D) {
      persons[, (paste0("prev0_", d)) := inp$H0[, d]]
      persons[, (paste0("onset_", d)) := onset[, d]]
    }
    for (d in REMIT) persons[, (paste0("episodes_", d)) := episodes[, d]]
  }
  list(agg_cycle = fin(A_cyc, "n_start"), agg_inc = fin(A_inc, c("at_risk", "py_free")),
       agg_prev = fin(A_prev, "n_with"), agg_mm = mmt[], persons = persons, calib = CAL)
}

calib_to_dt <- function(cal) {
  cells <- data.table(k = seq_len(NB_CAL))[, `:=`(age = (k - 1L) %% 101L,
                                                  sex = c("male", "female")[((k - 1L) %/% 101L) %% 2L + 1L],
                                                  imd_decile = (k - 1L) %/% 202L + 1L)]
  out <- rbindlist(lapply(names(cal), function(cz)
    data.table(k = seq_len(NB_CAL), cycle = rep(seq_len(ncol(cal[[cz]])), each = NB_CAL), cause = cz,
               factor = as.vector(cal[[cz]]))))
  cells[out, on = "k"][, k := NULL][]
}

calib_from_dt <- function(x, causes, n.c) {
  x <- as.data.table(x)
  x[, k := (age + 1L) + 101L * (sex_code(sex) - 1L) + 202L * (imd_decile - 1L)]
  sapply(causes, function(cz) { m <- matrix(1, NB_CAL, n.c); y <- x[cause == cz]; m[cbind(y$k, y$cycle)] <- y$factor; m },
         simplify = FALSE)
}

# Driver ========================================================================
# rr_file: a function of the scenario name returning its pp_rr file, or a named
# character vector (names = scenarios). scenarios[1] is the reference.
# remission: NULL, or a table from derive_remission() (or with the same columns).
run_microsim <- function(rates_file, zones_file, rr_file, prevalence_file, scenarios, out_dir,
                         remission = NULL, settings = engine_settings(), overwrite = TRUE) {
  s <- settings
  rr_path <- function(scen) if (is.function(rr_file)) rr_file(scen) else rr_file[[scen]]
  REF <- scenarios[1]

  message("Reading zones and rates")
  zones <- read_zones(zones_file)
  rates <- read_rates(rates_file)
  MORT <- s$mortality_cause
  if (!MORT %in% rates$cause) stop("The rates file has no ", MORT, " rows.", call. = FALSE)
  D <- sort(setdiff(unique(rates$cause), MORT))   # alphabetical = order of the onset steps
  CAUSES <- c(MORT, D)
  message("Causes: ", paste(CAUSES, collapse = ", "))

  message("Reading the reference population and RRs: ", rr_path(REF))
  ref <- read_rr_file(rr_path(REF), zones, s$exposures, CAUSES)
  keep <- !is.na(ref$pop$lsoa21cd) & !is.na(ref$pop$imd_decile)
  if (any(!keep)) message("Dropping ", format(sum(!keep), big.mark = ","), " people with no LSOA or IMD decile.")
  if (s$sample_prop < 1) {
    set.seed(s$sample_seed)
    smp <- logical(length(keep)); smp[sort(sample.int(length(keep), round(length(keep) * s$sample_prop)))] <- TRUE
    keep <- keep & smp
  }
  pop <- ref$pop[keep]; RR_ref <- ref$RR[keep, , drop = FALSE]; rm(ref)
  ord <- order(pop$id); pop <- pop[ord]; RR_ref <- RR_ref[ord, , drop = FALSE]   # fixes the order of the draws
  lsoas <- sort(unique(pop$lsoa21cd)); lads <- sort(unique(pop$ladcd))
  pop[, `:=`(lsoa_idx = match(lsoa21cd, lsoas), lad_idx = match(ladcd, lads))]
  message("Population: ", format(nrow(pop), big.mark = ","), " people in ", length(lsoas), " LSOAs, ",
          length(lads), " local authorities")

  H0 <- read_baseline_prevalence(prevalence_file, pop$id, D)
  message("Baseline prevalence (%): ", paste(sprintf("%s %.1f", D, 100 * colMeans(H0)), collapse = ", "))

  lsoa_dec <- unique(zones[lsoa21cd %in% lsoas, .(lsoa21cd, imd_decile)])
  arr <- build_rate_arrays(rates, remission, lsoas, lsoa_dec, CAUSES)
  if (length(arr$REM)) message("Remission: ", paste(names(arr$REM), collapse = ", "))
  else message("No remission: every disease is lifelong once it starts.")
  rm(rates)
  inp <- list(pop = pop[, .(id, age0, sex, lsoa21cd, ladcd, imd_decile, lsoa_idx, lad_idx)], H0 = H0,
              RATE = arr$RATE, REM = arr$REM, diseases = D, lads = lads)
  rm(arr)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(paste("date:", format(Sys.time())), paste("rates:", rates_file), paste("zones:", zones_file),
               paste("prevalence:", if (is.null(prevalence_file)) "none" else prevalence_file),
               paste0("rr[", scenarios, "]: ", vapply(scenarios, rr_path, "")),
               paste("scenarios:", paste(scenarios, collapse = ", ")), paste("N:", nrow(pop)),
               paste("causes:", paste(CAUSES, collapse = ", ")),
               paste("remission:", if (length(inp$REM)) paste(names(inp$REM), collapse = ", ") else "none"),
               paste0(names(s), ": ", vapply(s, function(v) paste(format(unlist(v)), collapse = ", "), "")),
               "mortality: with_mm (all-cause rate x exposure RR x multimorbidity RR)",
               "calibration: every cycle, rate-weighted, by single year of age x sex x IMD decile, from the reference run"),
             file.path(out_dir, "run_settings.txt"))

  out_names <- c("agg_cycle", "agg_inc", "agg_prev", "agg_mm", if (isTRUE(s$write_persons)) "persons")
  ext <- if (s$output_format == "parquet") ".parquet" else ".csv.gz"
  cal_base <- file.path(out_dir, paste0("scenario_", REF), "calibration")
  calib <- NULL
  for (scen in scenarios) {
    sdir <- file.path(out_dir, paste0("scenario_", scen))
    if (!overwrite && all(file.exists(paste0(file.path(sdir, out_names), ext)))) {
      message("Skipping ", scen, " (outputs on disk)"); next
    }
    if (scen == REF) RR <- RR_ref else {
      if (is.null(calib)) {
        if (!file.exists(paste0(cal_base, ext))) stop("No reference calibration on disk: run ", REF, " first.", call. = FALSE)
        calib <- calib_from_dt(read_any(paste0(cal_base, ext)), CAUSES, s$n_cycles)
      }
      message("Reading RRs for ", scen, ": ", rr_path(scen))
      sc <- read_rr_file(rr_path(scen), zones, s$exposures, CAUSES)
      idx <- match(pop$id, sc$pop$id)
      if (anyNA(idx)) stop(sum(is.na(idx)), " people of the reference population are not in the ", scen,
                           " RR file.", call. = FALSE)
      if (any(sc$pop$age0[idx] != pop$age0 | sc$pop$sex[idx] != pop$sex))
        stop("Age or sex differ between the reference and ", scen, " RR files.", call. = FALSE)
      RR <- sc$RR[idx, , drop = FALSE]; rm(sc)
    }
    t0 <- Sys.time()
    r <- run_scenario(inp, RR, s, calib = if (scen == REF) NULL else calib, label = scen)
    dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
    for (nm in out_names) write_any(r[[nm]], file.path(sdir, nm), s$output_format)
    if (scen == REF) { calib <- r$calib; write_any(calib_to_dt(calib), cal_base, s$output_format) }
    message(sprintf("%s done in %.1f min", scen, as.numeric(Sys.time() - t0, units = "mins")))
    rm(r, RR); gc(verbose = FALSE)
  }
  message("Done. Outputs under ", out_dir)
  invisible(out_dir)
}
