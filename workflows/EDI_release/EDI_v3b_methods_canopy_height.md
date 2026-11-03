# EDI v3b release — methods text (paste into ezEML "Methods")

**Concentration sampling.** Each NEON tower draws air from 4–8 inlets in turn through one gas analyser. A concentration value in the gradient-flux files (`mean_A`, `mean_B`) is the mean of one 6-min sample at one inlet (`timeBgn_*` to `timeEnd_*`). The two samples that make up a gradient pair are taken at different times. Their midpoints are 10–60 min apart, in 10-min steps that depend on how far apart the two inlets are in the sampling cycle. `dConc` is `mean_A − mean_B`, where A is the upper inlet and B the lower. Pairs of two consecutive samples from the same inlet (labels such as 4_4) are not included in the release.

**Modified Bowen ratio (MBR).** The known flux is the eddy-covariance flux of a tracer gas measured at the tower top. The known gradient is the tracer's concentration difference between the same two inlets (`dConc.tracer`, A − B). H2O is the tracer for CO2 and CO2 is the tracer for H2O. CH4 is computed twice, once with H2O and once with CO2 as tracer (`tracer` column). `FG_mean` is the median of 1000 bootstrap draws of F_tracer × dConc / dConc.tracer. Each draw samples both 6-min concentrations from their mean and within-sample variance.

**Data used in the eddy diffusivity (Eq. 6 in Malone et al., 2026).**
- Friction velocity, sensible heat flux and latent heat flux from the NEON eddy-covariance system at the tower top, interpolated to the gradient timestamp: `ustar_interp`, `H_turb_interp`, `LE_turb_interp`.
- Horizontal wind speed from the NEON tower profile: `ubar1`–`ubar4`.
- Air temperature from the NEON tower profile: `Tair1`–`Tair4`.
- Inlet heights from NEON sensor metadata: `TowerHeight_A`, `TowerHeight_B`.
- Air pressure from the single NEON barometer at mid-tower: `P_kPa`. It is used for all inlets.
- The Obukhov length (`L_obukhov`) and stability parameter (`zoL`) are computed from these values.

**Displacement height and roughness length.** The zero-plane displacement height is d = 0.66 h (`z_displ_calc`) and the roughness length is z0 = 0.1 h (`roughLength_calc`). Here h is `z_veg_aero`, the aerodynamic canopy height.

**Canopy height.** The site canopy height (hc) used in this dataset and in Malone et al. (2026) is the mean of the NEON Airborne Observation Platform (AOP) lidar canopy height model (DP3.30015.001) over the tower area, averaged across available flight years. Four sites have no lidar summary (DCFS, KONA, LAJA, TREE); there, the canopy height in the NEON site metadata is used instead. Site values range from 0.03 m (BARR) to 37.9 m (SERC).
- hc is reported as `hc_m` (with `hc_source`) in Site_Canopy_Attributes.csv, together with the lidar summary statistics, AOP vegetation indices and lidar canopy-structure metrics.
- canopy_combined.csv (one row per site and inlet pair) gives the same hc as `CanopyHeight`. Its `canopyHeight_m` column is the NEON site-metadata canopy height, which is used for hc only at DCFS, KONA, LAJA and TREE; `CHM.mean` is the lidar value.
- Site_SamplingHeightPairs.csv gives each inlet's height relative to hc and the class of each sampling-height pair:
  - AA: both inlets at or above the canopy.
  - AW: upper inlet above the canopy, lower inlet within it.
  - WW: both inlets within the canopy.

**Aerodynamic canopy height (`z_veg_aero`).** The 9-min gradient-flux files also contain `z_veg_aero`. It is estimated from friction velocity and wind speed at the top inlet (Pennypacker and Baldocchi, 2016), using daily medians averaged over a centred 20-day window. It is used only to set d and z0. It is not the lidar canopy height and should not be used to classify inlets relative to the canopy; use `hc_m` for that.

**Concentration profiles (Site_ConcentrationProfiles.csv).** This file summarises the vertical CO2, H2O and CH4 profile at each site. It is built from the 6-min inlet samples in the AE files.
- Each sample is used once, and only samples with `qfFinl` = 0 are kept.
- Samples are grouped by calendar month, day or night and inlet. Day is 07:00–18:59 local standard time and night is the rest. The month and hour come from the sample midpoint in local time.
- For each group the file gives the inlet height (`z_m`), the height relative to the lidar canopy height (`z_minus_hc_m`), the number of samples and years, and the mean, standard deviation and median concentration.
- Units: CO2 and CH4 in µmol mol-1, as in the flux files; H2O in mmol mol-1.
- Months are pooled across 2021–2026.
- Made by `flow.edi.profiles.v3b.R`.
