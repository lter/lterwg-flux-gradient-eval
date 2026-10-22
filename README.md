# LTER Synthesis working group -- The Flux Gradient Project: Understanding the methane sink-source capacity of natural ecosystems


## PIs: 

- Sparkle L. Malone, Assistant Professor, Yale University
- Jackie H. Matthes, Senior Scientist, Harvard University

## Project Summary

- https://lternet.edu/working-groups/the-flux-gradient-project/

## Repository Structure

```
lterwg-flux-gradient-eval/
├── functions/               # Core analysis functions
│   └── calc.*.R             # Calculation functions
│   
├── workflows/               # Complete analysis workflows
│   └── flow.*.R             # Analysis workflows
│
├── exploratory/             # Preliminary analyses and development
│   └── flow.evaluation.*.R
│
└── aop/             # Preparation of AOP, canopy diversity, and caopy information data
    ├── flow.NEONAOP.EVI.Download.R
    ├── flow_AOP_FormatLayers.R
    ├── flow.AOP.viz.R
    └── flow.StructuralDiversity.R 
```

## Data Frame Organization
- Column name should use snake case include units, last "_" proceeds units (i.e. var_molperm3)
- no "- or /" in column names

## Getting Started

1. Clone this repository
   ```bash
   git clone https://github.com/lter/lterwg-flux-gradient.git
   cd lterwg-flux-gradient
   ```

2. Install required R packages
   ```r
   # Core packages
   install.packages(c("tidyverse", "neonUtilities", "rhdf5", "googledrive", 
                     "foreach", "doParallel", "lubridate", "ggplot2"))
   
   # Additional packages
   install.packages(c("gslnls", "terra", "sf", "ggh4x"))
   
   # If using BiocManager
   if (!requireNamespace("BiocManager", quietly = TRUE))
       install.packages("BiocManager")
   BiocManager::install("rhdf5")
   ```

## Evaluation Workflow
1. `flow.evaluation.batch` → Creates the data needed to evaluate gradient fluxes using the products of `lterwg-flux-gradient/workflows/flow.evaluation.dataframe.R`.
First data is filtered (`flow.evaluation.filter.R`) to produce: FilteredData_ALLSites.Rdata and FilterReport_ALLSites.Rdata. Next, the One2One analysis (`flow.evaluation.One2One.R`) is done on filtered data to produce: One2One_ALLSites.Rdata and FilteredData_ALLSites_BH.Rdata. BH stands for the best height, which is determined by the height levels with the highest R2. Next the diurnal analysis (`flow.evaluation.diurnal.R`) produces: DiurnalSummary_ALLSites_BH.Rdata. Finally, we fit carbon exchange parameters (`flow.evaluation.cparms.R`) to produce: CarbonParms.Rdata. 

2. `flow.evaluation.figures` → produces the figures and tables that evaluate the gradient flux. The products of `flow.evaluation.batch` are used. 

   ```
   SITE_Evaluation.RData → flow.evaluation.batch
                         ├─ flow.evaluation.filter.R → FilteredData_ALLSites.Rdata, FilterReport_ALLSites.Rdata
                         ├─ flow.evaluation.One2One.R → One2One_ALLSites.Rdata, FilteredData_ALLSites_BH.Rdata
                         ├─ flow.evaluation.diurnal.R → DiurnalSummary_ALLSites_BH.Rdata
                         └─ flow.evaluation.cparms.R → CarbonParms.Rdata
   
   FilteredData_ALLSites.Rdata, etc. → flow.evaluation.figures → Figures and tables
   ```

## Review analyses
`workflows/WF_Version1/flow.supplement_review.R` produces the data-based answers to the referee comments on essd-2026-413. Tables go to `Review_Supplement/REVIEW_R*` and figures to `Figures/WF_Version1/REVIEW_R*`. The analyses are:
- **R1 (RC1; RC2 Sect. 2.1):** heights and canopy-level category of every sampling-height pair.
- **R2 (RC2 [3]):** tower geometry and inlet tube delays.
- **R3 (RC2 [3]):** profile sampling schedule.
- **R4 (RC1; RC2 [4]):** tracer used by MBR for each gas.
- **R5 (RC2 [2], [7], [10]):** seasonal day/night CO2 and H2O concentration profiles and the sink/source layer.
- **R6 (RC2 [2], [7]):** seasonal change in aerodynamic canopy height.
- **R7 (RC1 Fig. A13):** flux ranges before and after the u*/SNR filter and despike.
- **R8 (RC1 Fig. 6F):** TOOL diagnostics.

Section 1 aggregates the stacked EDI tables into `Review_Supplement/ALL_*.csv`. It runs only when `make.aggregates <- TRUE` and takes about an hour. `Review_Supplement/review_agg.py` is an equivalent Python version, used for the 2021–2026 run.

## Ensemble data product (EDI)
`workflows/WF_Version1/flow.flux.ensemble._V1R.R` builds the 30-min gradient-flux ensemble with `functions/calc.ensemble.R`:
- **Inputs:** reliable sampling-height pairs (CCC ≥ 0.5 against EC), any approach.
- **Half-hours:** each 9-min value goes to the fixed half-hour [hh:00, hh:30) or [hh:30, hh+1:00) containing the midpoint between its two inlet samples, the time to which EC was interpolated (`ensemble.halfhour()`, `add.halfhour()`). Intervals are built in UTC; `time.rounded` is the interval start in site local time. (Before version 3: the end of the first sample rounded to the nearest 30 min.)
- **`FG_ENSEMBLE`:** inverse-RMSE weighted mean of the pairs' 9-min fluxes in the half-hour. RMSE is the direct GF − EC difference of each pair (before version 3: scatter around a fitted GF–EC line, issue #1).
- **`FG_ENSEMBLE_SD`:** spread between pairs, the SD of the per-pair (approach × sampling-height pair) 30-min means. NA when only one pair contributes.
- **Also recorded:** `FG_ENSEMBLE_NPAIRS`, `FG_ENSEMBLE_NVALUES` and `FG_ENSEMBLE_APPROACHES`.

For new runs (`run.tag` other than `_AA_AW`), the script writes the EDI data table:
- one CSV per site: `NEON_GradientFlux_EDI<period>/{SITE}/{SITE}_GF_ENSEMBLE_30min.csv`;
- a column description table: `Metadata_table_GF_ENSEMBLE.csv`.

The CSV lists **every half-hour** from the first to the last value of each site, with start and end times in UTC and local time (UTC is the unique key; local time repeats at the end of daylight saving time). `FG_ENSEMBLE_FLAG` is `ok` (two or more pairs), `single_pair`, or `no_reliable_GF` (flux NA, `FG_ENSEMBLE_NPAIRS` = 0). Nothing is gap filled. `EC_mean` is the EC interpolated to the contributing samples; `EC_30min` is NEON's own 30-min turbulent flux (quality flag 0), filled where the extracted `SITE_30min.Rdata` files exist (`ec30.dirs`; all sites for Jul 2024–Aug 2026, only ABBY for 2021–24 unless those are re-extracted). Set `write.edi.ensemble` / `edi.ensemble.dir` to override.

`FG_ENSEMBLE_SD` ranks uncertainty well: errors against EC grow with it, Spearman 0.43 (CO2) and 0.56 (H2O). It is not a calibrated interval. EC lies within ±1 SD in 42–48% and within ±2 SD in 66–70% of windows (published 2021–2026 run).

## Version 3 run (October 2026)

`flow.evaluation.batch_V1.R` is set to the version 3 run: `run.tag <- "_AA_AW_2021_2026_v3"`, `eval.data.dirs <- "NEON_GradientFlux_Data_2021_2026_v3"`, `edi.dir <- "NEON_GradientFlux_EDI_2021_2026_v3"` (built by `lterwg-flux-gradient/workflows/WorkFlow_rerun_2021_2026_v3.R`; see that README for the flux changes). All outputs carry the `_v3` tag, so the published `_AA_AW_2021_2026` results are kept for comparison. Changes in this repository:
- **Pair RMSE** is the direct GF − EC difference (`calc.One2One.CCC_testing.R`, `calc.One2One.CCC_V2.R`; issue #1, PR #3).
- **Diel curves** are in g m⁻² h⁻¹ (3600 s; was 1800 s, so daily totals and absolute diel values were half; correlations, slopes, relative biases and signs are unaffected) (`calc.diel.R`; issue #2, PR #4). CH4 diel conversion corrected to g C per hour (PR #7).
- **Half-hour assignment** by sampling midpoint everywhere half-hours are formed (ensemble, Table 5 comparison, RSHP threshold sensitivity, `calc_validation.R`, EC filter diagnostic).
- **Spike evaluation** bins stability by the pair's (z − d)/L (`flow.evaluation.spike_eval.R`).
- **Review aggregates** (`flow.supplement_review.R`) are tagged by run and rebuilt automatically when missing (~1 h on the first v3 run).
- **New checks:** `flow.evaluation.canopy_height_sensitivity.R`, `flow.evaluation.rmse_definition_check.R`, and `flow.ec.filter.selection.R` (EC representativeness under SNR filtering, issue #5, PR #6; run after the v3 rerun).

Known limitation: the stability corrections of the flux calculation (φh, Eqs. 4–5 of the paper) were derived for roughly −5 < ζ < 0 and 0 ≤ ζ < 1. That is their range of validity, not a filter; they are applied to all values (0.07 % of filtered AE and 0.13 % of WP values were outside it in the 2021–2026 run).

## Supplementary analyses
These scripts produce the figures, tables and numbers in the manuscript supplement (essd-2026-413, review revision):
- **Supplement S1. Gradient-flux spikes: frequency, conditions and causes** → `flow.evaluation.spike_eval.R` (Figs. S1–S5, Tables S1–S2)
- **Supplement S2. Choosing sampling-height pairs without an eddy-covariance reference** → `flow.evaluation.noEC_pair_selection.R` (Fig. S6, Tables S3–S5)

The supplement text is kept as a shared document: https://claude.ai/code/artifact/ced0bb70-8e35-4132-860b-67b0f39c4f21 (private until shared). Numbers in it come from the `_AA_AW_2021_2026` run (Aug 2021 – Aug 2026, corrected effective height, Eq. 7 WP, despike k = 6).

Both scripts are sourced at the end of `flow.evaluation.batch_V1.R` and use the same `run.tag` (e.g. `_AA_AW_2021_2026`).

1. `workflows/WF_Version1/flow.evaluation.spike_eval.R` → Supplement S1. Flags gradient-flux spikes with the same rule as `despike_fluxes()` (median ± 6·MAD per site × approach × gas × height pair × day/night × season) and relates them to approach, gas, canopy level (AA/AW) and conditions (SNR, tracer SNR, u*, stability, K, (z − d)/z0) with a case-control logistic model. Outputs `Spike_Eval/SPIKE_*<run.tag>` and `Figures/WF_Version1/SPIKE_*<run.tag>.png`.

2. `workflows/WF_Version1/flow.evaluation.noEC_pair_selection.R` → Supplement S2. Chooses sampling-height pairs without an EC reference, as needed for gases such as CH4:
   - reference = equal-weight 30-min mean of all above-canopy (AA) MBR pairs;
   - AE/WP pairs with CCC ≥ 0.5 against that reference are kept;
   - each kept pair is scaled to the reference by a reduced-major-axis fit (b = sign(r)·sd(F)/sd(REF), a = mean(F) − b·mean(REF), F′ = (F − a)/b);
   - ensemble = equal-weight mean of the AA MBR and scaled AE/WP fluxes.

   It also runs two pre-checks that say whether the rule is likely to work at a site, without EC for the target gas:
   - A: AA MBR works for the other gases at the site (AA MBR ensemble CCC ≥ 0.5 against their EC; CO2 and H2O have EC at every NEON site);
   - B: at least one AE/WP pair agrees with the AA MBR reference (CCC ≥ 0.5).

   A site passes if A or B holds. On CO2 and H2O, 83% of passing site–gas cases have ensemble CCC ≥ 0.5 against EC, and 86% of failing ones are below 0.5.

   It is tested on CO2 and H2O, using EC only to score the result. Outputs `Spike_Eval/noEC_*<run.tag>.csv` (including `noEC_prechecks_by_site` and `noEC_precheck_validation`) and `Figures/WF_Version1/S2_noEC_pair_selection<run.tag>.png`.

   ```
   SITES_One2One<run.tag>.Rdata + SITES_<MBR|AE|WP>_9min_FILTER<run.tag>.Rdata
        ├─ flow.evaluation.spike_eval.R          → Spike_Eval/SPIKE_*, Figures/WF_Version1/SPIKE_*.png
        └─ flow.evaluation.noEC_pair_selection.R → Spike_Eval/noEC_*, Figures/WF_Version1/S2_noEC_pair_selection*.png
   ```

## AOP Workflow

1. `flow.NEONAOP.Download.R` → Downloads and mosaics NEON AOP Data. Data is stored locally on the Malone Lab server.

2. `flow.neon.site.squarebuffers.R` → Creates square buffers for LTER-NEON co-located sites using this file: Ameriflux_NEON field-sites.csv to produce: NEONLTERsiteBuffers.Rdata.

3. `flow.neon.site.simplefeatures.R` → This script uses the NEONLTERsiteBuffers.Rdata and breaks each buffer into wedges to produce: FG_Site_Wdges.RDATA.

4. `flow_AOP_FormatLayers` → Extracts AOP information for each wedge to produce a summary file.

   ```
   NEON AOP Data → flow.NEONAOP.Download.R → Downloaded AOP data 
   
   Ameriflux_NEON field-sites.csv → flow.neon.site.squarebuffers.R → NEONLTERsiteBuffers.Rdata
   
   NEONLTERsiteBuffers.Rdata → flow.neon.site.simplefeatures.R → FG_Site_Wdges.RDATA
   
   FG_Site_Wdges.RDATA + Downloaded AOP data → flow_AOP_FormatLayers → Summary files
   ```
   
## Function Folder
- Use hierarchical naming with the active verb first, i.e. "flag.iqr.R"
- This is where all functions called by flow. scripts in Workflow Folder are stored

## Exploratory Folder
- Wild West, this is where preliminary functions and workflows are stored

## Related Repositories

- [lterwg-flux-gradient](https://github.com/lter/lterwg-flux-gradient): Main repo, contains code for filtering
- [lterwg-flux-gradient-methane](https://github.com/lter/lterwg-flux-gradient-methane): Code for the methane paper

## Supplementary Resources

LTER Scientific Computing Team [website](https://lter.github.io/scicomp/) & NCEAS' [Resources for Working Groups](https://www.nceas.ucsb.edu/working-group-resources)

## Re-running the paper figures (2021–2026 run)

All workflow scripts default to `run.tag <- "_AA_AW_2021_2026"` (the published run; `flow.evaluation.batch_V1.R` sets the version 3 tag), `localdir <- "/Volumes/MaloneLab/Research/FluxGradient"` and the Dropbox repo path, and write to `Figures/WF_Version1/<name>_AA_AW_2021_2026.png`. The data products (filtered fluxes, `SITES_One2One`, `SITE_DATA_FILTERED_CCC`) already exist, so only the figure steps need to run. Run in this order (later steps read files written by earlier ones):

| # | Script (`workflows/…`) | Figures / tables |
|---|---|---|
| 1 | `WF_Version1/flow.evaluation.GoodDrivers.V2.R` (with `refit.rf <- TRUE`) | Fig 5 `GoodData_plot`, `Model.plot.matrix`; canopy model used by 8–9 |
| 2 | `WF_Version1/flow.flux.ensemble._V1R.R` | Fig 7 `ENSEMBLE_Eval_Summary_V1`, Figs A15–A16 `ENSEMBLE_Linear_plots_*`; ensemble Rdata + EDI ensemble tables with spread |
| 3 | `WF_Version1/flow.evaluation.diel_V1.R` | Fig 8 parts (`DIEL_EcoAssessment`, `DIEL_PLOTS_SEASON*_SUBSET*`), Figs A17–A19; `DIEL_SUMMARY_ENSEMBLE_V1` |
| 4 | `WF_Version1/flow.diel.range_table_V1.R` | Table A2 |
| 5 | `WF_Version1/flow.CCC.VIZ.R` | Fig 3 `final.eval.ccc.plot`, Figs A1–A4 |
| 6 | `WF_Version1/flow.flux.counts.R` | Figs A5–A7, A8E, A9, A10, A12 |
| 7 | `WF_Version1/flow.evaluation.RSHP_threshold_sensitivity_V1.R` | Table A1 |
| 8 | `WF_Version1/flow.CCC.placement.recommendations.R` | Fig 9 |
| 9 | `WF_Version1/flow.CCC.drivers.R` | Fig 6, Fig A11 |
| 10 | `WF_Version1/flow.results.ecotype.R` | Fig A8 A–D |
| 11 | `flow.CCC.results.summary.R` | Fig 4 `CCC_results_summary` |
| 12 | `WF_Version1/flow.flux.ensemble.compare_V1.R` | Table 5 |
| 13 | `WF_Version1/flow.flux.distributions.site_density_V1.R` | Figs A13–A14 |
| 14 | `WF_Version1/flow.evaluation.spike_eval.R`, `flow.evaluation.noEC_pair_selection.R`, `flow.supplement_review.R` | Supplements S1, S2, S3; Figs A20–A22 |

Not produced by code: Fig 1 (`flow.attr.map.R`, site attributes only; unchanged), Fig 2 (workflow diagram), and the assembly of Fig 8 from the step-3 panels. Scripts marked `# LEGACY` at the top of `workflows/` read preprint-era files and are not part of this set.
