# EDI v3b release: metadata, canopy and profile tables

The method and canopy tables were made on 31 Oct 2026 and the concentration-profile table on 2 Nov 2026. They were made on Bouchet with R 4.4.2 by the scripts in this folder. Running the same scripts on R 4.3.3 gives byte-identical metadata tables.

## Files written into `NEON_GradientFlux_EDI_2021_2026_v3b/`

| File | Rows | md5 |
|---|---|---|
| Metadata_table_method_MBR.csv | 110 | 882fe748a365800cedffb31977b1d045 |
| Metadata_table_method_AE.csv | 111 | fd6561dea55f2b616b27a1cb6060867b |
| Metadata_table_method_WP.csv | 111 | 9b115385feb6421fb33dd6f840e85544 |
| Site_Canopy_Attributes.csv | 47 | 433c8507f2ea73bdcafab8add57f0781 |
| Site_SamplingHeightPairs.csv | 513 | e3d31e0b1564deffb25ffd68b9d64f1b |
| Site_ConcentrationProfiles.csv (new 2 Nov) | 17,856 | 83a66bc520bf260eb0fe9b3e1bde3fc4 |
| Metadata_table_site_canopy.csv (now also describes the profile file) | 87 | ac98da592618f8c7d680cd98d69a6dfe |
| Metadata_table_GF_ENSEMBLE.csv (copied from Bouchet, made 26 Oct) | 15 | 395f8c9601b389b81ccde884a8364145 |

## Scripts

| Script | md5 |
|---|---|
| flow.edi.metadata.v3b.R | 1f258b39e21ef9b7e7b997fe667f1657 |
| flow.edi.profiles.v3b.R | 5103aa70f16f20ed06204657e6822571 |
| run_profiles_v3b.sh (Slurm array for the profile step) | 3ab3ad1b0dc7055a01660fdfdd347d04 |

## Reproduce

Run from the FluxGradient root on Bouchet:

```
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a
export R_LIBS_USER=$PWD/R-library-eval-v3b:$PWD/R-library-v3b   # needs data.table
# 1. concentration profiles: one task per site, then combine
sbatch --array=1-47 EDI_v3b_scripts/run_profiles_v3b.sh
Rscript --vanilla EDI_v3b_scripts/flow.edi.profiles.v3b.R combine
# 2. method, canopy and site metadata tables (run after step 1)
Rscript --vanilla EDI_v3b_scripts/flow.edi.metadata.v3b.R
```

Other ways to run it:
- **One site at a time:** `Rscript flow.edi.profiles.v3b.R site KONZ` writes `EDI_v3b_scripts/profiles_tmp/KONZ.csv`. Then run `combine`.
- **On the MaloneLab server** (from a Mac with the share mounted): run both scripts with `Rscript`. The root is detected automatically.
- **Anywhere else:** set the root with `FG_ROOT=/path/to/FluxGradient`.

Inputs:
- `canopy_commbined.csv` and `Ameriflux_NEON field-sites.csv` in the FluxGradient root.
- The header line of each `{SITE}_{MBR,AE,WP}_9min.df.final.csv`.
- For the profiles, the A/B concentration columns of each `{SITE}_AE_9min.df.final.csv`.

Each script prints the md5 of its outputs; compare them with the table above. The Bouchet log is `run_bouchet_profiles.log`.

## Methods text for ezEML

`EDI_v3b_methods_canopy_height.md` covers:
- concentration sampling: 6-min samples per inlet; the two samples of a pair are 10–60 min apart;
- the MBR known flux and known gradient (tracer gases);
- the data sources for the eddy diffusivity (Eq. 6);
- d = 0.66 h and z0 = 0.1 h from `z_veg_aero`;
- lidar canopy height `hc_m` versus `z_veg_aero`;
- the concentration-profile table.

The column descriptions in the metadata tables say the same.

## Concentration profiles

`Site_ConcentrationProfiles.csv` has one row per site × gas × month × day/night × inlet.
- **Source:** the 6-min inlet samples in the AE files. Each sample is used once and only samples with `qfFinl` = 0 are kept.
- **Day/night:** day is 07:00–18:59 local standard time, taken from the sample midpoint.
- **Columns:** inlet height, height minus the lidar canopy height, number of samples and years, and the mean, SD and median concentration.
- **Units:** CO2 and CH4 in µmol mol-1; H2O in mmol mol-1.
- **Ranges on Bouchet:** CH4 1.86–2.98, CO2 408–680, H2O 0.55–32.5.
- **Coverage:** each gas has 5,952 rows covering all 47 sites.

## Canopy height

hc is the NEON AOP lidar CHM mean at 43 sites. At DCFS, KONA, LAJA and TREE it is the NEON site canopy height.
- Range: 0.03 m (BARR) to 37.9 m (SERC).
- 13 sites are ≥ 15 m and 25 are ≥ 5 m.
- Canopy_class (AA/AW/WW) matches Canopy_L1 used in the evaluation for every pair.

## Checks run on Bouchet (31 Oct)

- **Pair labels:** every inter-level pair label in the 47 AE files is in Site_SamplingHeightPairs.csv (pair_check.log).
- **Same-inlet rows removed (31 Oct):** rows with dLevelsAminusB n_n (e.g. 4_4, two consecutive samples of one inlet) were removed from all 141 flux files with `remove_self_pairs_v3b.sh`.
  - Each file was filtered with awk and checked in R (`data.table::fread`): the kept labels equal the original labels minus the n_n rows, in order.
  - Removed: MBR 1,017,520 rows (78,252,894 → 77,235,374; these carried non-physical fluxes); AE 854,422 and WP 854,422 (70,390,649 → 69,536,227; no flux). The total, 2,726,364, equals the count found before removal.
  - Per-file counts and md5s are in `remove_self_pairs_v3b_summary.txt`.
  - The unnamed row-index column keeps its original values, so it now has gaps.
- **Evaluation unchanged:** these labels were never in SITE_DATA_FILTERED_CCC_AA_AW_2021_2026_v3b (check_self_ccc.log), so the evaluation, ensemble files and manuscript numbers are unchanged.
- **EDI writer patched:** `lterwg-flux-gradient/workflows/flow.evaluation.dataframe_EDI.R` now drops these rows before writing. The patched copy is in this folder; the original is `flow.evaluation.dataframe_EDI.R.orig`.
  - Still to do: commit it to GitHub.
  - `flow.edi.metadata.v3b.R` with `FG_CHECK_PAIRS=TRUE` stops if any n_n row remains.

## Server copy

- **2 Nov 2026:** all 141 flux files on the MaloneLab server match Bouchet's post-cleanup md5s (`verify_server_md5.sh`, results in `verify_server_md5.txt`). The 43 ensemble files and the metadata tables are present.
- **Upload:** leave the hidden `.smbdelete*` files out of the upload.
