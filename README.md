# Microsimulation model
A microsimulation model for health calculations. Inspired by the [Microsimulation-tutorial](https://github.com/DARTH-git/Microsimulation-tutorial). We are particularly interested in a model that remembers each individual's characteritics and their behaviour and exposure over time. Specifically we are utilising `Sick-Sicker` model with memory component.([Appendix B_online_supp-samplev](https://github.com/DARTH-git/Microsimulation-tutorial/blob/master/Appendix%20B_online_supp-samplev.R)). 

## Running the microsimulation

The engine is `functions/microsim_engine.R`, ported from the UBD model (`run_micro_opt.R`, "with_mm" mortality structure): each year, all-cause mortality (rate × exposure RR × multimorbidity RR), then disease onsets (rate × exposure RR, × diabetes RR for CHD and stroke), then remission. Rates are calibrated every cycle by age × sex × IMD decile so that the reference scenario reproduces the input rates; the other scenarios reuse the reference factors. The header of the file describes the method, inputs and outputs in full.

The Java model run is set once, as `RUN_NAME` in `config/run_config.R`. For a new run, change it there and run, from the project folder:

1. `scripts/run_pifs.R`: PIFs from the run's reference RRs (`RR/reference/pp_rr_2021.csv`) → `pifs_reference_<RUN_NAME>.csv`
2. `scripts/transitions_calibration.R`: transition rates by LSOA, `rate` = `rate_raw` × (1 − PAF) → `health_transitions_manchester_<RUN_NAME>.csv`. It stops if the PIFs were made from another run.
3. `scripts/run_microsim.R`: the microsimulation, for every scenario of the run (one `pp_rr_2021.csv` per scenario under `RR/<scenario>/`, with the `rr_<EXPOSURE>_<cause>` columns). Outputs go to `output/microsim/<RUN_NAME>/scenario_<name>/`.

The file names carry the run's name, so a step fails (file not found) instead of silently using another run's file. `run_microsim.R` also reads `zoneSystem.csv`, the baseline prevalence per person (`scripts/assign_prevalence.R`) and the depression prevalence used to derive depression remission; set those paths at its top. The engine uses `rate_raw` and calibrates it itself; `rate` is for the Java model.

Checks on synthetic data: `Rscript tests/test_microsim_engine.R` (needs only `data.table`).

The earlier versions of the model are in `archive/old_microsim/`.
