###############################################################################
## tests/test_microsim_engine.R
##
## Checks functions/microsim_engine.R on small synthetic inputs where the
## right answer is known. Run from the project folder:
##   Rscript tests/test_microsim_engine.R
## Needs only data.table (outputs are written as .csv.gz, so arrow is not needed).
###############################################################################

suppressPackageStartupMessages(library(data.table))
source("functions/microsim_engine.R")

n_fail <- 0L
check <- function(ok, what) {
  cat(if (isTRUE(ok)) "PASS" else "FAIL", "-", what, "\n")
  if (!isTRUE(ok)) n_fail <<- n_fail + 1L
}

# Synthetic inputs ===============================================================
set.seed(1)
tmp <- file.path(tempdir(), "microsim_test"); unlink(tmp, recursive = TRUE); dir.create(tmp)

# 8 LSOAs in 2 local authorities; 4 IMD deciles with 2 LSOAs each, whose rates
# differ (so the rate-weighted calibration matters)
lsoa <- data.table(lsoa21cd = sprintf("E0100%04d", 1:8), ladcd = rep(c("E08000001", "E08000002"), each = 4),
                   imd10 = rep(1:4, 2), mult = c(1.6, 1.3, 1.0, 0.8, 1.2, 0.9, 0.7, 0.6))
zones <- lsoa[rep(1:8, each = 5)][, oaID := .I][, ladnm := ladcd]
fwrite(zones[, .(oaID, lsoa21cd, ladcd, ladnm, imd10)], file.path(tmp, "zones.csv"))

base_rate <- function(cause, age, sex) switch(cause,
  all_cause_mortality    = pmin(0.0004 * exp(0.09 * pmax(age - 20, 0)) * (1 + 0.2 * (sex == 1)), 3),
  coronary_heart_disease = fifelse(age >= 30, 0.0005 * exp(0.06 * (age - 30)), 0),
  stroke                 = fifelse(age >= 30, 0.0003 * exp(0.07 * (age - 30)), 0),
  diabetes               = fifelse(age >= 20, 0.004, 0),
  depression             = fifelse(age >= 15, 0.10, 0))
CAUSES <- c("all_cause_mortality", "coronary_heart_disease", "depression", "diabetes", "stroke")
rates <- CJ(lsoa21cd = lsoa$lsoa21cd, sex = 1:2, age = 0:100, cause = CAUSES)
rates[lsoa, mult := i.mult, on = "lsoa21cd"]
rates[, rate_raw := base_rate(cause[1], age, sex) * mult, by = cause]
rates[, rate := rate_raw]
fwrite(rates[, .(lsoa21cd, sex, cause, age, imd_decile = 1L, rate, rate_raw)], file.path(tmp, "rates.csv"))

# Population with physical activity; RRs as the Java pp_rr file writes them
N <- 60000L
pop <- data.table(id = sample(1e6, N), age = sample(0:90, N, TRUE), gender = sample(1:2, N, TRUE),
                  zone = sample(zones$oaID, N, TRUE), mmet = rexp(N, 1 / 10))
write_rr <- function(mmet, file) {
  x <- pop[, .(id, hhid = 1L, age, gender, zone)]
  pa <- exp(-0.03 * pmin(mmet, 17.5))
  for (cz in CAUSES) x[, (paste0("rr_PHYSICAL_ACTIVITY_", cz)) := pa]
  x[, rr_NOISE_coronary_heart_disease := 1.05]
  x[, rr_AIR_POLLUTION_PM25_all_cause_mortality := 1 + 0.002 * pop$zone]
  fwrite(x, file)
}
write_rr(pop$mmet, file.path(tmp, "rr_reference.csv"))
write_rr(pop$mmet, file.path(tmp, "rr_same.csv"))
write_rr(pop$mmet + 5, file.path(tmp, "rr_active.csv"))

# Baseline diseases (one curly-apostrophe-free row per person with >= 1 disease)
dis <- data.table(id = pop$id, d1 = fifelse(runif(N) < 0.08 & pop$age >= 30, "diabetes", ""),
                  d2 = fifelse(runif(N) < 0.05 & pop$age >= 40, "coronary_heart_disease", ""),
                  d3 = fifelse(runif(N) < 0.15 & pop$age >= 15, "depression", ""))
dis[, diseases := trimws(gsub("\\s+", " ", paste(d1, d2, d3)))]
fwrite(dis[diseases != "", .(id, diseases)], file.path(tmp, "prevalence.csv"))

rr_files <- c(reference = file.path(tmp, "rr_reference.csv"), same = file.path(tmp, "rr_same.csv"),
              active = file.path(tmp, "rr_active.csv"))
S <- engine_settings(output_format = "csv")
rd <- function(dir, scen, nm) fread(file.path(dir, paste0("scenario_", scen), paste0(nm, ".csv.gz")))

# 1. Helpers =====================================================================
check(identical(normalise_cause(c("parkinson’s_disease", "head-and-neck-cancer", "head_and_neck_cancer", "stroke")),
                c("parkinson", "head_neck_cancer", "head_neck_cancer", "stroke")), "normalise_cause")
check(identical(sex_code(c(1, 2, "male", "F")), c(1L, 2L, 1L, 2L)), "sex_code")

# derive_remission: rem = inc x (1 - prev) / prev, bounded 0.5-2, age-15 value below 15
rr_tab <- read_rates(file.path(tmp, "rates.csv"))
rem <- derive_remission(rr_tab, data.table(age = 0:100, sex = rep(1:2, each = 101), prevalence = 0.15),
                        read_zones(file.path(tmp, "zones.csv")))
inc_gm <- 0.10 * mean(lsoa$mult)                 # incidence averaged over the LSOAs
check(all(abs(rem[age >= 15, rate] - inc_gm * 0.85 / 0.15) < 1e-12) && all(abs(rem[age < 15, rate] - rem[age == 15, rate][1]) < 1e-12),
      "derive_remission (GM-wide prevalence)")
rem_dec <- derive_remission(rr_tab, CJ(age = 0:100, sex = 1:2, imd_decile = 1:4)[, prevalence := 0.02],
                            read_zones(file.path(tmp, "zones.csv")))
check(all(rem_dec$rate == 2) && "imd_decile" %in% names(rem_dec), "derive_remission (by decile, upper bound)")

# 2. Full run =====================================================================
out1 <- file.path(tmp, "run1")
run_microsim(file.path(tmp, "rates.csv"), file.path(tmp, "zones.csv"), rr_files, file.path(tmp, "prevalence.csv"),
             scenarios = c("reference", "same", "active"), out_dir = out1, remission = rem, settings = S)

cyc <- lapply(setNames(nm = names(rr_files)), function(sc) rd(out1, sc, "agg_cycle"))
inc <- lapply(setNames(nm = names(rr_files)), function(sc) rd(out1, sc, "agg_inc"))
prs <- rd(out1, "reference", "persons")

# Calibration: in the reference run, simulated deaths match those implied by the
# input rates (uncalibrated, the PA RRs alone would give ~20% fewer deaths).
# Expected deaths: sum over the people alive at the start of each cycle of
# 1 - exp(-rate_raw) at their age, sex and LSOA.
prs[, sexn := sex_code(sex)]
rmort <- rates[cause == "all_cause_mortality"]
exp_d <- sum(vapply(1:30, function(t) {
  al <- prs[is.na(death_cycle) | death_cycle >= t][age0 + t - 1L < 100L]
  r <- rmort[al[, .(lsoa21cd, sex = sexn, age = age0 + t - 1L)], on = c("lsoa21cd", "sex", "age"), rate_raw]
  sum(1 - exp(-r))
}, 0))
sim_d <- cyc$reference[age < 100, sum(deaths)]
z <- (sim_d - exp_d) / sqrt(exp_d)
cat(sprintf("     deaths below age 100, 30 cycles: simulated %d, expected from input rates %.0f (z = %.2f)\n", sim_d, exp_d, z))
check(abs(z) < 4, "reference reproduces input mortality (calibration)")

# Same for CHD incidence. Expected onsets: people at risk (alive after the
# mortality step, without CHD) x (1 - exp(-input rate)). Each decile x local
# authority stratum holds one LSOA here, so the stratum's rate is that LSOA's.
chd <- inc$reference[cause == "coronary_heart_disease" & age >= 20]
ra <- rates[cause == "coronary_heart_disease"][lsoa, `:=`(imd_decile = i.imd10, ladcd = i.ladcd), on = "lsoa21cd"]
ra <- ra[, .(age, sex = c("male", "female")[sex], imd_decile, ladcd, r = rate_raw)]
chd <- ra[chd, on = .(age, sex, imd_decile, ladcd)]
chd_e <- chd[, sum(at_risk * (1 - exp(-r)))]
z_chd <- (chd[, sum(onsets)] - chd_e) / sqrt(chd_e)
cat(sprintf("     CHD onsets age 20+: simulated %d, expected %.0f (z = %.2f)\n", chd[, sum(onsets)], chd_e, z_chd))
check(abs(z_chd) < 4, "reference reproduces input CHD incidence")

# Common random numbers: identical RRs give identical results
check(isTRUE(all.equal(cyc$reference, cyc$same)) && isTRUE(all.equal(inc$reference, inc$same)),
      "same RRs -> identical results (common random numbers)")

# More activity -> fewer deaths and fewer CHD onsets
d_ref <- cyc$reference[, sum(deaths)]; d_act <- cyc$active[, sum(deaths)]
o_ref <- inc$reference[cause == "coronary_heart_disease", sum(onsets)]; o_act <- inc$active[cause == "coronary_heart_disease", sum(onsets)]
cat(sprintf("     deaths: reference %d, active %d; CHD onsets: reference %d, active %d\n", d_ref, d_act, o_ref, o_act))
check(d_act < d_ref && o_act < o_ref, "more activity -> fewer deaths and CHD onsets")

# Nobody survives past 100
check(prs[is.na(death_cycle), all(age0 + 29L < 100L)] && prs[!is.na(death_cycle), all(age0 + death_cycle - 1L <= 100L)],
      "everyone dies at 100")

# Remission keeps depression prevalence near its input (0.15) instead of rising
prv <- rd(out1, "reference", "agg_prev")[cause == "depression" & age >= 30 & age < 80]
alv <- cyc$reference[age >= 30 & age < 80]
p30 <- prv[cycle == 30, sum(n_with)] / alv[cycle == 30, sum(n_end)]
cat(sprintf("     depression prevalence at cycle 30, ages 30-79: %.3f\n", p30))
check(p30 > 0.11 && p30 < 0.19, "depression prevalence stays near its steady state with remission")

# Multimorbidity: death rates rise with the number of diseases held
mm <- rd(out1, "reference", "agg_mm")[age >= 60 & age < 80, .(r = sum(deaths) / sum(n_start)), keyby = n_dis]
check(nrow(mm) >= 3 && all(diff(mm$r[1:3]) > 0), "death rate rises with the number of diseases")

# 3. Calibration from disk ==========================================================
# Re-running only the 'active' scenario, reading the reference calibration
# from disk, reproduces the first run
out2 <- file.path(tmp, "run2")
dir.create(out2)
file.copy(file.path(out1, "scenario_reference"), out2, recursive = TRUE)
run_microsim(file.path(tmp, "rates.csv"), file.path(tmp, "zones.csv"), rr_files, file.path(tmp, "prevalence.csv"),
             scenarios = c("reference", "active"), out_dir = out2, remission = rem, settings = S, overwrite = FALSE)
check(isTRUE(all.equal(rd(out2, "active", "agg_cycle"), cyc$active)), "calibration read from disk gives the same results")

# 4. No remission ===================================================================
out3 <- file.path(tmp, "run3")
run_microsim(file.path(tmp, "rates.csv"), file.path(tmp, "zones.csv"), rr_files["reference"], file.path(tmp, "prevalence.csv"),
             scenarios = "reference", out_dir = out3, settings = engine_settings(output_format = "csv", n_cycles = 10L))
p10 <- rd(out3, "reference", "agg_prev")[cause == "depression" & cycle == 10 & age >= 30 & age < 80, sum(n_with)] /
  rd(out3, "reference", "agg_cycle")[cycle == 10 & age >= 30 & age < 80, sum(n_end)]
cat(sprintf("     without remission, depression prevalence at cycle 10, ages 30-79: %.3f\n", p10))
check(p10 > 0.5, "without remission depression accumulates")

cat(if (n_fail == 0L) "\nAll checks passed.\n" else sprintf("\n%d check(s) FAILED.\n", n_fail))
quit(status = if (n_fail == 0L) 0L else 1L)
