# Microsimulation model
A microsimulation model for health calculations. Inspired by the [Microsimulation-tutorial](https://github.com/DARTH-git/Microsimulation-tutorial). We are particularly interested in a model that remembers each individual's characteritics and their behaviour and exposure over time. Specifically we are utilising `Sick-Sicker` model with memory component.([Appendix B_online_supp-samplev](https://github.com/DARTH-git/Microsimulation-tutorial/blob/master/Appendix%20B_online_supp-samplev.R)). 

## Running the microsimulation

The engine is `functions/microsim_engine.R`, ported from the UBD model (`run_micro_opt.R`, "with_mm" mortality structure): each year, all-cause mortality (rate × exposure RR × multimorbidity RR), then disease onsets (rate × exposure RR, × diabetes RR for CHD and stroke), then remission. Rates are calibrated every cycle by age × sex × IMD decile so that the reference scenario reproduces the input rates; the other scenarios reuse the reference factors. The header of the file describes the method, inputs and outputs in full.

1. Set `RUN_NAME` (the Java model run under `scenOutput/`) and the input paths in `scripts/run_microsim.R`:
   - transition rates by LSOA (`scripts/transitions_calibration.R`; the engine uses `rate_raw`)
   - `zoneSystem.csv`
   - one `pp_rr_2021.csv` per scenario from the Java model (`RR/<scenario>/`), with the `rr_<EXPOSURE>_<cause>` columns
   - baseline prevalence per person (`scripts/assign_prevalence.R`)
   - depression prevalence, to derive depression remission
2. Run it from the project folder: `source("scripts/run_microsim.R")`. Outputs go to `output/microsim/<RUN_NAME>/scenario_<name>/`.

Checks on synthetic data: `Rscript tests/test_microsim_engine.R` (needs only `data.table`).

The earlier versions of the model are in `archive/old_microsim/`.
