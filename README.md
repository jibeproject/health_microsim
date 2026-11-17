# Health microsimulation (JIBE)

Health data preparation, an individual-level health microsimulation, and the processing and apps for the outputs of the JIBE (SILO/MITO) Java model, for Greater Manchester and Melbourne. Originally inspired by the [Microsimulation tutorial](https://github.com/DARTH-git/Microsimulation-tutorial) (Sick-Sicker model with memory).

## Layout

| Folder | What it holds |
|---|---|
| `config/` | `paths.R`: where the shared drive is (default X:, changeable per user). `run_config.R`: which Java model run the scripts work on (`RUN_NAME`) |
| `data_prep/manchester/` | Disease rates and prevalence for Greater Manchester: `health_data_Manchester.Rmd`, then `run_pifs.R`, `transitions_calibration.R` and `assign_prevalence.R`; helpers in `functions/` |
| `data_prep/melbourne/` | The same for Melbourne (`health_data_Melbourne.Rmd`, `assign_prevalence_Melbourne.R`), from Australian sources |
| `engine/` | The microsimulation (`microsim_engine.R`), its run script (`run_microsim.R`) and checks (`tests/`) |
| `erf/` | Exposure-response functions for PM2.5, NO2, NDVI and noise (used by the Java model) |
| `outputs/` | Processing of the Java model outputs (`data_prep_shiny.R`, `process_all_data.R`, `process_exp.R`, `process_trips.R`) and the apps: `app/` (health, exposures, trips; `run_app.R` starts it) and `app_spatial/` (maps; built by `prep_data_spatial/`) |
| `archive/` | Earlier versions and one-off analyses, kept for reference |

Run every script from the project folder (open `health_microsim.Rproj`). Paths inside the project use `here()`; paths on the shared drive come from `config/paths.R`.

## Where the data are

`config/paths.R` sets `JIBE_ROOT`, the JIBE folder on the shared drive (default `X:/HealthImpact/Data/Country/UK/JIBE`). If it is elsewhere on your computer, do not edit the scripts: add `JIBE_ROOT=<your path>` to your `~/.Renviron`, or create `config/paths_local.R` (not committed) that sets `JIBE_ROOT` (and `LOCAL_DATA`, the folder for local copies of large datasets).

## Manchester: from health data to microsimulation

1. `data_prep/manchester/health_data_Manchester.Rmd`: GBD, ONS and NDRS data → `health_transitions_manchester_raw.csv` and `health_transitions_manchester_prevalence.csv` (in `manchester/health/processed/`).
2. Set `RUN_NAME` in `config/run_config.R` to the Java run, then:
   1. `data_prep/manchester/run_pifs.R`: PIFs from the run's reference RRs (`RR/reference/pp_rr_2021.csv`) → `pifs_reference_<RUN_NAME>.csv`
   2. `data_prep/manchester/transitions_calibration.R`: rates by LSOA, `rate` = `rate_raw` × (1 − PAF) → `health_transitions_manchester_<RUN_NAME>.csv` (stops if the PIFs come from another run)
   3. `data_prep/manchester/assign_prevalence.R`: diseases at the start, per person → `base_prevalence_id_clean_<date>.csv`
3. `engine/run_microsim.R`: the microsimulation, for every scenario of the run. Outputs go to `output/microsim/<RUN_NAME>/scenario_<name>/`.

File names carry the run's name, so a step fails (file not found) instead of silently using another run's file.

## The microsimulation engine

`engine/microsim_engine.R` is ported from the UBD model (`run_micro_opt.R`, "with_mm" mortality structure): each year, all-cause mortality (rate × exposure RR × multimorbidity RR), then disease onsets (rate × exposure RR, × diabetes RR for CHD and stroke), then remission (depression, derived from its incidence and prevalence). Exposure RRs are read from the Java model's `pp_rr_2021.csv` files. Rates are calibrated every cycle by age × sex × IMD decile so that the reference scenario reproduces the input rates (`rate_raw`); the other scenarios reuse the reference factors. The header of the file describes the method, inputs and outputs in full.

Checks on synthetic data: `Rscript engine/tests/test_microsim_engine.R` (needs only `data.table`).

## Apps

- `outputs/app/combined_app.R` reads the `.qs2` files in `outputs/app/data/`, written by `outputs/process_all_data.R`, `process_exp.R` and `process_trips.R`. Start it with `Rscript outputs/run_app.R`.
- `outputs/app_spatial/app.R` reads the `.rds` files next to it, built by the scripts in `outputs/prep_data_spatial/`. Deploy with `outputs/app_spatial/app_deploy.R`.
