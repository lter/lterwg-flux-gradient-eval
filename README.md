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
├── workflows/WF_Version1/   # Evaluation steps sourced by workflows/flow.evaluation.batch_V1.R
├── workflows/manuscript/    # In-text numbers of essd-2026-413 (flow.manuscript.numbers.R + ms_*.R)
├── workflows/EDI_release/   # EDI v3b release tables (metadata, lidar canopy, concentration profiles), same-inlet cleanup
├── workflows/review_tests/  # Tests behind decisions in the revision (SE-based SNR, CH4 pair rule)
├── hpc/bouchet_v3b/         # Slurm/run scripts used for the v3b run on Yale's Bouchet cluster
├── tests/                   # Unit tests (Rscript tests/<file>.R)
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

## EDI v3b release: which script makes each file
The release folder is `NEON_GradientFlux_EDI_2021_2026_v3b/` on the MaloneLab server (`/Volumes/MaloneLab/Research/FluxGradient`). Scripts in `workflows/EDI_release/` are also kept, with logs and md5s, in `EDI_v3b_scripts/` on the server. "calc" = the flux-calculation repository, `lter/lterwg-flux-gradient`; "eval" = this repository.

| File(s) | Made by | Notes |
|---|---|---|
| `{SITE}/{SITE}_MBR_9min.df.final.csv`, `_AE_`, `_WP_` | calc: `workflows/flow.evaluation.dataframe_EDI.R`, sourced by `workflows/WorkFlow_rerun_2021_2026_v3.R` (one Slurm task per site; scripts in eval `hpc/bouchet_v3b/`) | Same-inlet `n_n` rows then removed with eval `workflows/EDI_release/remove_self_pairs_v3b.sh`; the writer patch `edi_drop_self.patch.R` drops them in future runs. |
| `{SITE}/{SITE}_GF_ENSEMBLE_30min.csv`, `Metadata_table_GF_ENSEMBLE.csv` | eval: `workflows/WF_Version1/flow.flux.ensemble._V1R.R` → `write.ensemble.edi()` in `functions/calc.ensemble.R` | Written when `write.edi.ensemble` is TRUE (default for new runs). None at ABBY, GUAN, OSBS, PUUM (no reliable pairs). |
| `Metadata_table_method_MBR.csv`, `_AE.csv`, `_WP.csv`, `Site_Canopy_Attributes.csv`, `Site_SamplingHeightPairs.csv`, `Metadata_table_site_canopy.csv` | eval: `workflows/EDI_release/flow.edi.metadata.v3b.R` | Run after the profile table. |
| `Site_ConcentrationProfiles.csv` | eval: `workflows/EDI_release/flow.edi.profiles.v3b.R` (`site` per site via `run_profiles_v3b.sh`, then `combine`) | Reads the AE 9-min files. |
| `{SITE}/{SITE}_STORAGE_FLUX_30min.csv` | calc: `workflows/flow.neon.storage.R` (two runs: Aug 2021–Jun 2024 → `NEON_Storage_Flux/`, Jul 2024–Aug 2026 → `NEON_Storage_Flux_202407_202608/`), merged by eval `workflows/EDI_release/flow.edi.storage.v3b.R` | The merge keeps the newer run at the 1 Jul 2024 00:00 overlap and corrects the CH4 unit labels (nmol → umol; values unchanged). Row counts and md5s: `EDI_v3b_scripts/storage_merge_v3b_summary.txt`. |
| `Metadata_table_STORAGE_FLUX.csv` | calc: `workflows/flow.neon.storage.metadata.R` (`NEON_storage_flux_metadata.csv`), with a `file_name_pattern` column and notes on the merge, gap filling and CH4 units added by hand (copy in `workflows/EDI_release/`) | |
| `canopy_combined.csv`, `Metadata_table_canopy_combined.csv` | eval: `aop/flow.canopy.R` (writes `canopy_commbined.csv` in the FluxGradient root; inputs from `aop/workflow_master_AOP.R`); column table `aop/canopy_commbined_metadata.csv`, written by hand | Renamed for the release; the code still reads `canopy_commbined.csv`. `CanopyHeight` is hc (= `hc_m` in Site_Canopy_Attributes.csv); `canopyHeight_m` is the NEON site-metadata height. |
| `UstarNeonSites.csv`, `Metadata_table_UstarNeonSites.csv` | eval: `exploratory/flow.AmeriFlux__UstarThresholdR.R` (AmeriFlux FLUXNET AUXNEE u* thresholds); same file as calc `metadata/UstarNeonSites.csv`. Column table written by hand for the release (copy in `workflows/EDI_release/`) | `Threshold.final` is the threshold applied. |
| `metadata_validation.csv`, `Metadata_table_metadata_validation.csv` | Compiled by hand for the four non-NEON validation sites (no script); column table by hand | Release copy has the FI-Hyy latitude without its leading tab. |

Methods text for ezEML: `EDI_v3b_methods_canopy_height.md`; build notes and md5s: `README_EDI_v3b_metadata.md` (both in `workflows/EDI_release/`). File list with server links: the EDI v3b inventory doc.

## Version 3b run used in the revised manuscript (October 2026)

`flow.evaluation.batch_V1.R` is set to the run behind every number, table and figure of the revised manuscript:
`run.tag <- "_AA_AW_2021_2026_v3b"`, `eval.data.dirs <- "NEON_GradientFlux_Data_2021_2026_v3b"`, `edi.dir <- "NEON_GradientFlux_EDI_2021_2026_v3b"`.
The fluxes were calculated with `lterwg-flux-gradient/workflows/WorkFlow_rerun_2021_2026_v3.R` (see that README for the flux
changes) on Bouchet, 26–27 Oct 2026 (`hpc/bouchet_v3b/`). "v3b" is the version 3 code run there; `_v3` was the first test run of
the same code. All outputs carry the tag, so the published `_AA_AW_2021_2026` results are kept for comparison. Changes in this repository:
- **Pair RMSE** is the direct GF − EC difference (`calc.One2One.CCC_testing.R`, `calc.One2One.CCC_V2.R`; issue #1, PR #3).
- **Diel curves** are in g m⁻² h⁻¹ (3600 s; was 1800 s, so daily totals and absolute diel values were half; correlations, slopes, relative biases and signs are unaffected) (`calc.diel.R`; issue #2, PR #4). CH4 diel conversion corrected to g C per hour (PR #7).
- **Half-hour assignment** by sampling midpoint everywhere half-hours are formed (ensemble, Table 5 comparison, RSHP threshold sensitivity, `calc_validation.R`, EC filter diagnostic). The v3b run fixed two bugs here: a `%>%` inside a comment in `calc_validation.R`, and `flow.evaluation.sitelist_FILTER.R` now keeps `timeEnd_A`/`timeBgn_B`, which the midpoint needs.
- **Canopy height** everywhere in the manuscript is the NEON AOP lidar canopy-height-model mean in the tower area (`CanopyHeight = coalesce(CHM.mean, canopyHeight_m)`; NEON site value at DCFS, KONA, LAJA and TREE, which have no lidar summary; 0.03–37.9 m). The canopy classes (AA/AW/WW) always used it; the plots and rankings that used the NEON metadata height now use it too: `flow.attr.map.R` (Fig. 1), `flow.flux.ensemble._V1R.R` (ensemble CCC vs canopy height), `flow.results.delta_z_site_slopes.R` and `flow.results.delta_z_site_rankings_all_canopy.R` (and the slope-vs-height plots that read their output). These reproduce the 29 Oct 2026 lidar comparison (`run_logs/lidar_height_20261029`: slope vs hc R² 0.18 CO2, 0.25 H2O).
- **Spike evaluation** bins stability by the pair's (z − d)/L (`flow.evaluation.spike_eval.R`).
- **Review aggregates** (`flow.supplement_review.R`) are tagged by run and rebuilt automatically when missing (~1 h on the first run).
- **Paths:** scripts use `DirRepo.eval` (this repository) and `localdir` (data folder) instead of hard-coded Dropbox and server paths. The batch takes them from the environment variables `FG_EVAL_REPO` and `FG_ROOT` (defaults: the Dropbox repo and `/Volumes/MaloneLab/Research/FluxGradient`), so the same code runs on the Mac and on Bouchet. All RData names use the `.RData` extension (Linux is case sensitive). `flow.flux.counts.R` now writes `flow.flux.counts<run.tag>.RData` (was untagged).
- **New checks:** `flow.evaluation.canopy_height_sensitivity.R`, `flow.evaluation.rmse_definition_check.R`, and `flow.ec.filter.selection.R` (EC representativeness under SNR filtering, issue #5, PR #6).

Known limitation: the stability corrections of the flux calculation (φh, Eqs. 4–5 of the paper) were derived for roughly −5 < ζ < 0 and 0 ≤ ζ < 1. That is their range of validity, not a filter; they are applied to all values (< 0.2 % of filtered AE and < 0.4 % of WP values were outside it in the v3b run).

## Reproducing the revised manuscript (essd-2026-413)

1. **Fluxes** (`lterwg-flux-gradient`): `workflows/WorkFlow_rerun_2021_2026_v3.R`, one Slurm task per site (`hpc/bouchet_v3b/run_array_v3b.sh` → `run_site_v3b.sh`; `validate_sites_v3b.R` checks each site). Writes `NEON_GradientFlux_Data_2021_2026_v3b/{SITE}/{SITE}_Evaluation.RData` and the EDI 9-min CSVs.
2. **Evaluation** (this repository): `workflows/flow.evaluation.batch_V1.R` (`hpc/bouchet_v3b/run_evaluation_v3b.sh`; 128 GB, ~1 day). Writes the filtered data, CCC tables, ensemble, diel curves, every figure and table, and the in-text numbers (step 5).
3. **EC representativeness of the filters** (issue #5; Sects. 2.5, 3.4): `Rscript hpc/bouchet_v3b/ec_filter_selection_v3b_run.R hpc/bouchet_v3b/ec_filter_config_v3b.R` from the repository root (`run_ecfilter_v3b.sh`). Writes `EC_filter_selection_v3b/{SITE}_{hourly,daily,unmatched}.csv`.
4. **Tests behind decisions in the revision** (`workflows/review_tests/SNR_SE_v3b/`): `run_snrse_v3b.sh` (SE-based vs SD-based SNR; kept the SD rule, Sect. 2.5 limitation), `run_ensemble_v3b.sh` (ensemble under both rules) and `run_ch4method_v3b.sh` (CH4 pair rule: CO2-reliable pairs, AA-MBR fallback; Appendix C.5, Supplement S2.5, Table S5b). Outputs in `SNR_SE_v3b/` (copied to `Results_v3b/SNR_SE_ch4method_tests/` on the server).
5. **In-text numbers**: `workflows/manuscript/flow.manuscript.numbers.R` (sourced at the end of the batch; run it again after step 3). Writes `Manuscript_numbers/ms_*<run.tag>.txt`; see the table below.
6. **EDI release tables** (`workflows/EDI_release/`, run from the FluxGradient root with the scripts in `EDI_v3b_scripts/`): `remove_self_pairs_v3b.sh` (drops same-inlet `n_n` rows), `run_profiles_v3b.sh` + `flow.edi.profiles.v3b.R combine` (Site_ConcentrationProfiles.csv), then `flow.edi.metadata.v3b.R` (method and site metadata tables, lidar canopy attributes, sampling-height pairs). Methods text for ezEML and md5s of every output: `EDI_v3b_methods_canopy_height.md`, `README_EDI_v3b_metadata.md`. The matching change to the EDI writer (`edi_drop_self.patch.R`) belongs in `lterwg-flux-gradient/workflows/flow.evaluation.dataframe_EDI.R`.

### In-text numbers (`workflows/manuscript/`)

| Manuscript | Script | Reads |
|---|---|---|
| Abstract, Sect. 3.1 | `ms_S3.1_reliable_pairs.R` | `SITES_One2One`, `flow.flux.counts` |
| Sect. 2.4.2 (Eqs. 4–5 validity range) | `ms_S2.4_stability_range.R` | per-site `{SITE}_FILTER` files (on Bouchet; 0.05 % AE, 0.10 % WP outside) |
| Sect. 2.5 (filters) | `ms_S2.5_filters.R` | `SITES_FILTER_REPORTS` |
| Sects. 2.5, 3.4 (issue #5) | `ms_S2.5_S3.4_ec_representativeness.R` | `EC_filter_selection_v3b/` |
| Sects. 2.7, 3.2 (random forest) | `ms_S3.2_random_forest.R` | `flow.flux.counts` |
| Sect. 3.2 (placement, lidar hc, structure, Δz) | `ms_S3.2_geometry_structure.R` | `SITES_One2One`, `delta_z_*` csv |
| Sects. 2.8, 3.3, 3.4 (ensemble, overlap, spread, CPER/CLBJ) | `ms_S3.3_ensemble.R` | `SITE_DATA_ENSEMBLE_V1`, ensemble csv |
| Sects. 3.3, 4.3 (1.6×/1.5× windows) | `ms_S3.3_window_gain.R` | `SITES_*_9min_FILTER` |
| Abstract, Sect. 3.4 (diel) | `ms_S3.4_diel.R` | `DIEL_SUMMARY_ENSEMBLE_V1` |
| Sect. 4.2, Fig. 9 | `ms_S4.2_placement.R` | `SITES_One2One_canopy_model` |
| Sect. 2.5, App. B, Supplement S1 | `ms_AppB_spikes.R` | `Spike_Eval/SPIKE_*` |
| App. C, Supplement S2 | `ms_AppC_noEC.R` | `Spike_Eval/noEC_*`, `SITES_*_9min_FILTER` |

Checked 2 Nov 2026 against the v3b outputs: every script reproduces (`ms_S2.4_stability_range.R` was run on Bouchet, where its inputs are) the numbers in the revised manuscript (e.g. 17.6/25.2 % reliable pairs at 42/43 sites; RF OOB 11.9 %, accuracy 89.6 % vs NIR 80.8 %; r(CCC, lower inlet − hc) 0.42/0.50; ensemble windows 1.57×/1.54×; spread ρ 0.43/0.58; EC retention 20/44 %; AA-MBR self-agreement AUC 0.59; top-3 RF CCC 0.49/0.60).

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

## Manuscript map: figures and tables (v3b run)

`flow.evaluation.batch_V1.R` runs all of these in order; figures go to `Figures/WF_Version1/<name><run.tag>.png`. Individual scripts default to `run.tag <- "_AA_AW_2021_2026"` (the published run) when sourced on their own, so set `run.tag <- "_AA_AW_2021_2026_v3b"`, `localdir` and `DirRepo.eval` first. Once the data products (filtered fluxes, `SITES_One2One`, `SITE_DATA_FILTERED_CCC`) exist, only the figure steps need to run. Run in this order (later steps read files written by earlier ones):

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
| 14 | `WF_Version1/flow.results.delta_z_*.R` (7 scripts, order in the batch) | Δz figures and csv used in Sect. 3.2 (lidar hc) |
| 15 | `WF_Version1/flow.evaluation.spike_eval.R`, `flow.evaluation.sampling_time.R`, `flow.evaluation.noEC_pair_selection.R`, `flow.supplement_review.R` | Supplements S1, S2, S3 (sampling time); Appendices B–C; Figs A20–A22 |
| 16 | `flow.attr.map.R` | Fig 1 `Map_plot` (lidar canopy height) |
| 17 | `manuscript/flow.manuscript.numbers.R` | In-text numbers (table above) |

Not produced by code: Fig 2 (workflow diagram) and the assembly of Fig 8 from the step-3 panels. Scripts marked `# LEGACY` at the top of `workflows/` read preprint-era files and are not part of this set.
