## ------------------------------------------------------------------ ##
##  EDI v3b release: column tables for the 9-min method files and
##  site / sampling-height-pair canopy tables built from lidar heights
## ------------------------------------------------------------------ ##
# Script: flow.edi.metadata.v3b.R
# Writes into the v3b EDI folder (NEON_GradientFlux_EDI_2021_2026_v3b):
#   Metadata_table_method_MBR.csv, _AE.csv, _WP.csv
#       one row per column of {SITE}_{METHOD}_9min.df.final.csv (union over
#       all sites; n_sites = number of site files that have the column)
#   Site_Canopy_Attributes.csv        one row per site
#   Site_SamplingHeightPairs.csv      one row per site x sampling-height pair
#   Metadata_table_site_canopy.csv    column descriptions for the two tables and for
#                                     Site_ConcentrationProfiles.csv (flow.edi.profiles.v3b.R)
#
# Canopy height hc = NEON AOP lidar canopy height model mean (CHM.mean);
# where no lidar summary exists (DCFS, KONA, LAJA, TREE) the NEON site
# canopy height (DistZaxsCnpy) is used, as in the manuscript (Sect. 2.1).
#
# Inputs (identical on Bouchet and the MaloneLab server):
#   <root>/canopy_commbined.csv
#   <root>/Ameriflux_NEON field-sites.csv
#   <root>/NEON_GradientFlux_EDI_2021_2026_v3b/<SITE>/*_9min.df.final.csv (header line only)
# Base R only. Run from anywhere:
#   Rscript flow.edi.metadata.v3b.R            (root detected automatically)
#   FG_ROOT=/path/to/FluxGradient Rscript flow.edi.metadata.v3b.R
# Output is deterministic: the same inputs give byte-identical files.

options(stringsAsFactors = FALSE, scipen = 0)

root <- Sys.getenv("FG_ROOT", "")
if (root == "") {
  cand <- c("/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient",   # Bouchet
            "/Volumes/MaloneLab/Research/FluxGradient")              # MaloneLab server (Mac)
  root <- cand[dir.exists(cand)][1]
}
if (is.na(root) || !dir.exists(root)) stop("FluxGradient root not found; set FG_ROOT")
edi.dir <- file.path(root, Sys.getenv("FG_EDI_DIR", "NEON_GradientFlux_EDI_2021_2026_v3b"))
if (!dir.exists(edi.dir)) stop("EDI folder not found: ", edi.dir)
message("root: ", root, "\nEDI folder: ", edi.dir)

wcsv <- function(x, f) {
  write.csv(x, file.path(edi.dir, f), row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
  message("wrote ", f, " (", nrow(x), " rows)")
}
rd <- function(x, n = 4) ifelse(is.na(x), NA, round(as.numeric(x), n))

sites <- sort(basename(list.dirs(edi.dir, recursive = FALSE)))
sites <- sites[grepl("^[A-Z]{4}$", sites)]
message(length(sites), " site folders")

## ---------------------------------------------------------------- ##
## 1. Method column tables
## ---------------------------------------------------------------- ##
dict <- read.csv(text = r"---(
"applies_to","column_name","data_type","units","description","source_or_derivation"
"ALL","(row index)","integer","unitless","Unnamed row index written by R write.csv; not a variable.","Created by write.csv (row.names = TRUE)"
"MBR","match_time.x","datetime/character","date-time","Original matched timestamp retained from the MBR gradient-flux table before joins.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","timeEndA","datetime/character","date-time","End time for tower level A in the MBR common time fields.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","timeEndB","datetime/character","date-time","End time for tower level B in the MBR common time fields.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","dlvl","character","unitless","Tower-level pair identifier used in the MBR source table.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","match_time.y","datetime/character","date-time","Second matched timestamp retained during MBR joins; duplicates match_time.x.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","match_time","datetime/character","date-time","Timestamp used to align concentration gradients, meteorology, and eddy-covariance fluxes.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","site","character","unitless","NEON site code.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","zeta","numeric","unitless","Stability parameter of the pair, (z - d)/L, with z = sqrt(zA zB), d = z_displ_calc and z - d floored at 0.1 m.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","L","numeric","m","Obukhov length from EC u*, H and LE with von Karman constant k = 0.4.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","Stability_100","character","unitless","Stability class using |L| > 100 m as neutral, positive L as stable, and negative L as unstable.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","Stability_500","character","unitless","Stability class using |L| > 500 m as neutral, positive L as stable, and negative L as unstable.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","Stability_Exteme","character","unitless","Extreme-stability flag from |zeta| > 1; spelling follows source column name.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","FG_mean","numeric","umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O); nmol CH4 m-2 s-1 (CH4)","MBR gradient flux: median of 1000 bootstrap draws of F_tracer x dConc/dConc.tracer, where each draw samples both 6-min concentrations from their mean and within-sample variance (v3: median replaces the mean of the ratio, which is unstable when dConc.tracer is near zero).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","FG_mean","numeric","umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O); nmol CH4 m-2 s-1 (CH4)","AE gradient flux, -rho_mol x K x dConc / dHeight, with K the aerodynamic (AE) eddy diffusivity in EddyDiff.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","FG_mean","numeric","umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O); nmol CH4 m-2 s-1 (CH4)","WP gradient flux, -rho_mol x K x dConc / dHeight, with K the wind-profile (WP) eddy diffusivity in EddyDiff.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","FG_sd","numeric","umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O); nmol CH4 m-2 s-1 (CH4)","Standard deviation of the 1000 MBR bootstrap draws.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","FG_sd","numeric","umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O); nmol CH4 m-2 s-1 (CH4)","Standard deviation of the AE flux across bootstrap draws of dConc; NA where no bootstrap was run.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","FG_sd","numeric","umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O); nmol CH4 m-2 s-1 (CH4)","Standard deviation of the WP flux across bootstrap draws of dConc; NA where no bootstrap was run.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","gas","character","unitless","Gas for this row: CO2, H2O, or CH4.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","tracer","character","unitless","Tracer gas used by the MBR calculation for the target gas flux.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","dConc","numeric","gas-specific concentration units","Mean concentration difference between tower levels A and B for the row gas.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","dConc_pvalue","numeric","gas-specific concentration units","P-value associated with the tower-level concentration difference.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","dConc_sd","numeric","gas-specific concentration units","Standard deviation of the tower-level concentration difference.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","dConc_bin","integer/character","gas-specific concentration units","Binned concentration-difference indicator retained from concentration processing.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","dConc.tracer","character","gas-specific concentration units","Mean concentration difference for the tracer gas used by the MBR calculation.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","dConc.tracer_pvalue","character","gas-specific concentration units","P-value for the tracer-gas concentration difference.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","dConc.tracer_sd","character","gas-specific concentration units","Standard deviation for the tracer-gas concentration difference.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","dConc.tracer_bin","character","gas-specific concentration units","Binned tracer concentration-difference indicator.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","timeEnd_A","datetime/character","date-time","End time of the averaging window for tower level A.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","timeBgn_A","datetime/character","date-time","Beginning time of the averaging window for tower level A.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","TowerPosition_A","numeric","unitless","NEON tower position number for level A.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","mean_A","numeric","gas-specific concentration units","Mean gas concentration at tower level A during the averaging window.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","qfFinl_A","integer/character","unitless","Final NEON quality flag for tower level A concentration data.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","min_A","numeric","gas-specific concentration units","Minimum gas concentration at tower level A during the averaging window.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","max_A","numeric","gas-specific concentration units","Maximum gas concentration at tower level A during the averaging window.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","vari_A","numeric","unitless","Variance of gas concentration at tower level A during the averaging window.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","numSamp_A","integer/character","unitless","Number of samples contributing to tower level A statistics.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","timeEnd_B","datetime/character","date-time","End time of the averaging window for tower level B.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","timeBgn_B","datetime/character","date-time","Beginning time of the averaging window for tower level B.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","TowerPosition_B","numeric","unitless","NEON tower position number for level B.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","mean_B","numeric","gas-specific concentration units","Mean gas concentration at tower level B during the averaging window.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","qfFinl_B","integer/character","unitless","Final NEON quality flag for tower level B concentration data.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","min_B","numeric","gas-specific concentration units","Minimum gas concentration at tower level B during the averaging window.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","max_B","numeric","gas-specific concentration units","Maximum gas concentration at tower level B during the averaging window.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","vari_B","numeric","unitless","Variance of gas concentration at tower level B during the averaging window.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","numSamp_B","integer/character","unitless","Number of samples contributing to tower level B statistics.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","diffTowerPosition","numeric","unitless","Difference in NEON tower position numbers between levels A and B.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","dLevelsAminusB","character","unitless","Sampling-height pair label ""A_B"" from NEON tower positions; A is the upper inlet and B the lower inlet (e.g. 6_1). Same-inlet rows (n_n, e.g. 4_4: two consecutive samples of one inlet; not a gradient) were removed from the release files (remove_self_pairs_v3b.sh).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","timeMid","datetime","YYYY-MM-DD hh:mm:ss (UTC)","Midpoint between the centres of the paired A and B 6-min samples; EC fluxes are interpolated to this time.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","TowerHeight_A","numeric","m","Inlet height above ground of level A (upper inlet).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","TowerHeight_B","numeric","m","Inlet height above ground of level B (lower inlet).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","FC_turb_interp","numeric","umol CO2 m-2 s-1","Interpolated eddy-covariance turbulent CO2 flux aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","FC_stor_interp","numeric","umol CO2 m-2 s-1","Interpolated CO2 storage flux aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","FC_nee_interp","numeric","umol CO2 m-2 s-1","Interpolated net ecosystem exchange CO2 flux aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","LE_turb_interp","numeric","W m-2","Interpolated turbulent latent heat flux aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","LE_stor_interp","numeric","W m-2","Interpolated latent heat storage flux aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","LE_nsae_interp","numeric","W m-2","Interpolated latent heat flux including storage/non-storage terms as stored in source data.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","H_turb_interp","numeric","W m-2","Interpolated turbulent sensible heat flux aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","H_stor_interp","numeric","W m-2","Interpolated sensible heat storage flux aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","H_nsae_interp","numeric","W m-2","Interpolated sensible heat flux including storage/non-storage terms as stored in source data.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","ustar_interp","numeric","m s-1","Interpolated friction velocity aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","roughLength_interp","numeric","m","Interpolated roughness length aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","RH","numeric","unitless","Relative humidity aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","P_kPa","numeric","kPa","Air pressure from the single NEON barometer at mid-tower, aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","PAR","numeric","umol photons m-2 s-1","Photosynthetically active radiation aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","z_veg_aero","numeric","m","Aerodynamic canopy height h (Pennypacker and Baldocchi, 2016) from u*, wind speed and height of the top inlet in near-neutral conditions; daily medians averaged over a centred 20-day window and interpolated to each sample. Used only for d and z0 in the flux calculation; it is not the lidar canopy height (hc_m in Site_Canopy_Attributes.csv), which defines the AA/AW classes and is the canopy height used in the paper.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","z_displ_calc","numeric","m","Zero-plane displacement height d = 0.66 h (z_veg_aero).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","roughLength_calc","numeric","m","Aerodynamic roughness length z0 = 0.1 h (z_veg_aero).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","Tair_K","numeric","K","Air temperature converted to Kelvin.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","esat_Pa","numeric","Pa","Saturation vapor pressure.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","e_Pa","numeric","Pa","Actual vapor pressure.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","VPD","numeric","kPa","Vapor pressure deficit.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","rhoa_kgm3","numeric","kg m-3","Dry-air density.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","rhov_kgm3","numeric","kg m-3","Water-vapor density.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","rho_kgm3","numeric","kg m-3","Moist-air density.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","specificHumidity_pct","numeric","unitless","Specific humidity as stored by the processing workflow.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","Cp_moist","numeric","J kg-1 K-1","Specific heat capacity of moist air.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","Ftemp","numeric","unitless","Temperature-related flux or correction term used in aerodynamic calculations.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","lambda","numeric","J kg-1","Latent heat of vaporization.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","FH2O_interp","numeric","mmol H2O m-2 s-1","Interpolated water-vapor flux aligned to the gradient timestamp.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","L_obukhov","numeric","m","Obukhov length used for stability; equal to L (k = 0.4).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","zoL","numeric","unitless","NEON tower-top stability parameter z/L rescaled to the Obukhov length L (k = 0.4).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","cross_grad_flag","integer/character","unitless","Flag equal to 1 where Kgas is negative, indicating cross-gradient flux; 0 otherwise.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","ustar_threshold","numeric","m s-1","Site-level friction velocity threshold merged from UstarNeonSites.csv.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","timeEndA.local","datetime","YYYY-MM-DD hh:mm:ss (site local time)","timeEndA (MBR) or timeEnd_A (AE, WP) converted from UTC to the site local time zone.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","Month.local","integer/character","month number","Month extracted from local end time.","Added in flow.evaluation.dataframe_EDI.R"
"ALL","time.local","datetime/character","date-time","Clock time extracted from local end time.","Added in flow.evaluation.dataframe_EDI.R"
"ALL","hour.local","integer/character","hour of day","Hour extracted from local end time.","Added in flow.evaluation.dataframe_EDI.R"
"ALL","rho","numeric","g m-3","Dry-air density from pressure and air temperature used in the Obukhov-length calculation.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","vpotflux","numeric","K m s-1","Virtual potential temperature flux from H and LE used in the Obukhov-length calculation.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","mean_TowerH","numeric","m","Arithmetic mean of TowerHeight_A and TowerHeight_B.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","effective_h","numeric","m","z - d for the pair, with z = sqrt(zA zB) and d = z_displ_calc; floored at 0.1 m.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","MO.param","numeric","unitless","effective_h / L; same quantity as zeta, used for phih and phim.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","canopy.flag","integer","unitless","1 if the lower inlet is at or above the displacement height d (z_displ_calc); 0 if below.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","phih","numeric","unitless","Stability function for scalars: 1 + 5 zeta (stable), (1 - 16 zeta)^-0.5 (unstable) (Lee, eqs. 4.36-4.37).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","phim","numeric","unitless","Stability function for momentum: 1 + 5 zeta (stable), (1 - 16 zeta)^-0.25 (unstable) (Lee, eqs. 4.35, 4.37).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","EddyDiff","numeric","m2 s-1","AE eddy diffusivity used for FG_mean (K_AE).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","EddyDiff","numeric","m2 s-1","WP eddy diffusivity used for FG_mean, from the measured wind speed at z = sqrt(zA zB) with the Monin-Obukhov stability correction (k = 0.4); NA when z - d <= z0.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","EddyDiff_WP","numeric","m2 s-1","Wind-profile eddy diffusivity K_WP (in the AE file, for comparison only).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","EddyDiff_WP","numeric","m2 s-1","Wind-profile eddy diffusivity K_WP (in the AE file, for comparison only).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","dHeight","numeric","m","Vertical distance between tower levels A and B.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","dHeight","numeric","m","Vertical distance between tower levels A and B.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","EC_mean","numeric","umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O); nmol CH4 m-2 s-1 (CH4)","EC turbulent flux of the row gas interpolated to the sample midpoint (FC_turb_interp or FH2O_interp); reference for evaluation.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","dz1","character","m","Tower height of level A copied for cross-gradient calculations.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","dz1","character","m","Tower height of level A copied for cross-gradient calculations.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","dz2","character","m","Tower height of level B copied for cross-gradient calculations.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","dz2","character","m","Tower height of level B copied for cross-gradient calculations.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","dx","numeric","gas-specific concentration units","Concentration difference copied from dConc for cross-gradient calculations.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","dx","numeric","gas-specific concentration units","Concentration difference copied from dConc for cross-gradient calculations.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","dz","numeric","m","Vertical separation calculated as dz1 minus dz2.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","dz","numeric","m","Vertical separation calculated as dz1 minus dz2.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","cp","numeric","J kg-1 K-1","Specific heat capacity constant used in cross-gradient calculations.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","cp","numeric","J kg-1 K-1","Specific heat capacity constant used in cross-gradient calculations.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","mol_air","numeric","unitless","Molar density of air calculated from air density.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","mol_air","numeric","unitless","Molar density of air calculated from air density.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","flux","numeric","gas-specific flux units","Reference EC flux transformed as needed for Kgas calculation.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","flux","numeric","gas-specific flux units","Reference EC flux transformed as needed for Kgas calculation.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","Kgas","character","m2 s-1","Eddy diffusivity back-calculated from EC flux and concentration gradient.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","Kgas","character","m2 s-1","Eddy diffusivity back-calculated from EC flux and concentration gradient.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"AE","Eddy_outlier","numeric","unitless","Flag equal to 1 where Kgas is outside acceptable eddy-diffusivity bounds for the method; 0 otherwise.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","Eddy_outlier","numeric","unitless","Flag equal to 1 where Kgas is outside acceptable eddy-diffusivity bounds for the method; 0 otherwise.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","GeometricMean_AB","numeric","unitless","Geometric mean concentration of levels A and B used in wind-profile calculations.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"WP","EddyDiff_AE","numeric","m2 s-1","Aerodynamic eddy diffusivity K_AE carried over from the AE calculation (for comparison only).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"MBR","L_gas","numeric","m","Obukhov length carried from the target-gas table (renamed from L during the MBR join; same as L).","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","Tair{n}","numeric","deg C","Air temperature at NEON tower level n (1 = lowest); present only where the tower has level n.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
"ALL","ubar{n}","numeric","m s-1","Mean horizontal wind speed at NEON tower level n (1 = lowest); present only where the tower has level n.","From *_Evaluation.RData (v3b run, flow.evaluation.dataframe.R)"
)---", check.names = FALSE)

method.desc <- c(
  MBR = "Modified Bowen ratio (MBR) gradient flux: the EC flux of a tracer gas (H2O for CO2, CO2 for H2O) scaled by the ratio of the target- to tracer-gas concentration differences between inlets A and B.",
  AE  = "Aerodynamic (AE) gradient flux: -rho_mol K dC/dz with the eddy diffusivity K from EC friction velocity and Monin-Obukhov stability functions.",
  WP  = "Wind-profile (WP) gradient flux: -rho_mol K dC/dz with the eddy diffusivity K from the measured wind speed profile and Monin-Obukhov stability functions.")

read.header <- function(f) {
  h <- readLines(f, n = 1, warn = FALSE)
  h <- scan(text = h, what = "", sep = ",", quote = "\"", quiet = TRUE, na.strings = character(0))
  h[h == ""] <- "(row index)"
  h
}

for (m in names(method.desc)) {
  files <- file.path(edi.dir, sites, paste0(sites, "_", m, "_9min.df.final.csv"))
  files <- files[file.exists(files)]
  if (!length(files)) stop("no ", m, " files")
  hdrs <- lapply(files, read.header)
  cols <- character(0)
  for (h in hdrs) cols <- c(cols, setdiff(h, cols))          # union, first-seen order
  # tower-level columns: sort each family by level, keep at the position of the first one
  lev <- grepl("^(Tair|ubar)[0-9]+$", cols)
  if (any(lev)) {
    lv <- cols[lev]
    fam <- sub("[0-9]+$", "", lv); num <- as.integer(sub("^[A-Za-z]+", "", lv))
    cols[lev] <- lv[order(match(fam, unique(fam)), num)]
  }
  n.sites <- vapply(cols, function(cc) sum(vapply(hdrs, function(h) cc %in% h, TRUE)), 1L)
  key <- ifelse(grepl("^(Tair|ubar)[0-9]+$", cols), sub("[0-9]+$", "{n}", cols), cols)
  out <- lapply(seq_along(cols), function(i) {
    d <- dict[dict$column_name == key[i] & dict$applies_to == m, ]
    if (!nrow(d)) d <- dict[dict$column_name == key[i] & dict$applies_to == "ALL", ]
    if (nrow(d) != 1) stop(m, ": no unique description for column ", cols[i])
    desc <- d$description
    if (key[i] != cols[i]) desc <- gsub("level n", paste("level", sub("^[A-Za-z]+", "", cols[i])), desc, fixed = TRUE)
    data.frame(method = m,
               file_name_pattern = paste0("{SITE}_", m, "_9min.df.final.csv"),
               method_description = method.desc[[m]],
               column_order = i,
               column_name = cols[i],
               data_type = d$data_type, units = d$units,
               description = desc,
               source_or_derivation = d$source_or_derivation,
               n_sites = n.sites[[i]], n_site_files = length(files),
               check.names = FALSE)
  })
  wcsv(do.call(rbind, out), paste0("Metadata_table_method_", m, ".csv"))
}

## ---------------------------------------------------------------- ##
## 2. Site and sampling-height-pair canopy tables (lidar hc)
## ---------------------------------------------------------------- ##
cc <- read.csv(file.path(root, "canopy_commbined.csv"), check.names = FALSE)
cc <- cc[cc$Site %in% sites, ]
num <- function(x) suppressWarnings(as.numeric(x))

# site-level values are repeated on every pair row; take the first row per site
first <- cc[!duplicated(cc$Site), ]
first <- first[order(first$Site), ]
chm <- num(first$CHM.mean); neon.h <- num(first$canopyHeight_m)
hc <- ifelse(is.na(chm), neon.h, chm)
hc.src <- ifelse(is.na(chm), "NEON site canopy height (DistZaxsCnpy)", "NEON AOP lidar CHM mean")

af <- read.csv(file.path(root, "Ameriflux_NEON field-sites.csv"), check.names = FALSE,
               fileEncoding = "latin1")
af <- af[match(first$Site, af[["Site_Id NEON"]]), ]

# inlet heights per site from all pair rows (upper and lower inlets)
lev <- unique(rbind(
  data.frame(Site = cc$Site, pos = num(cc$TowerPosition_A), z = num(cc$MeasurementHeight_m_A)),
  data.frame(Site = cc$Site, pos = num(cc$TowerPosition_B), z = num(cc$MeasurementHeight_m_B))))
n.lev <- tapply(lev$pos, lev$Site, function(p) length(unique(p)))
z.top <- tapply(lev$z, lev$Site, max, na.rm = TRUE)
z.bot <- tapply(lev$z, lev$Site, min, na.rm = TRUE)

aop.dict <- read.csv(text = r"---(
"EVI.mean","numeric","unitless","Mean Enhanced Vegetation Index for the site from NEON AOP raster summaries.","canopy_commbined.csv: Calculated in flow_AOP_FormatLayers.R and averaged across available raster years."
"EVI.sd","numeric","unitless","Mean within-site standard deviation of Enhanced Vegetation Index.","canopy_commbined.csv: Calculated from NEON AOP EVI raster summaries."
"EVI.years","numeric","count","Number of unique EVI raster years contributing to the site summary.","canopy_commbined.csv: Count of unique EVI.year values."
"NDVI.mean","numeric","unitless","Mean Normalized Difference Vegetation Index for the site from NEON AOP raster summaries.","canopy_commbined.csv: Calculated in flow_AOP_FormatLayers.R and averaged across available raster years."
"NDVI.sd","numeric","unitless","Mean within-site standard deviation of Normalized Difference Vegetation Index.","canopy_commbined.csv: Calculated from NEON AOP NDVI raster summaries."
"NDVI.years","numeric","count","Number of unique NDVI raster years contributing to the site summary.","canopy_commbined.csv: Count of unique NDVI.year values."
"LAI.mean","numeric","unitless","Mean Leaf Area Index for the site from NEON AOP raster summaries.","canopy_commbined.csv: Calculated in flow_AOP_FormatLayers.R and averaged across available raster years."
"LAI.sd","numeric","unitless","Mean within-site standard deviation of Leaf Area Index.","canopy_commbined.csv: Calculated from NEON AOP LAI raster summaries."
"LAI.years","numeric","count","Number of unique LAI raster years contributing to the site summary.","canopy_commbined.csv: Count of unique LAI.year values."
"PRI.mean","numeric","unitless","Mean Photochemical Reflectance Index for the site from NEON AOP raster summaries.","canopy_commbined.csv: Calculated in flow_AOP_FormatLayers.R and averaged across available raster years."
"PRI.sd","numeric","unitless","Mean within-site standard deviation of Photochemical Reflectance Index.","canopy_commbined.csv: Calculated from NEON AOP PRI raster summaries."
"PRI.years","numeric","count","Number of unique PRI raster years contributing to the site summary.","canopy_commbined.csv: Count of unique PRI.year values."
"SAVI.mean","numeric","unitless","Mean Soil-Adjusted Vegetation Index for the site from NEON AOP raster summaries.","canopy_commbined.csv: Calculated in flow_AOP_FormatLayers.R and averaged across available raster years."
"SAVI.sd","numeric","unitless","Mean within-site standard deviation of Soil-Adjusted Vegetation Index.","canopy_commbined.csv: Calculated from NEON AOP SAVI raster summaries."
"SAVI.years","numeric","count","Number of unique SAVI raster years contributing to the site summary.","canopy_commbined.csv: Count of unique SAVI.year values."
"Cutoff05-CVH","numeric","unitless","Canopy volume heterogeneity metric from the structural-diversity raster product using a 5 percent height cutoff.","canopy_commbined.csv: Summarized by site in flow.StructuralDiversity.R; source field name CVH."
"Cutoff05-DGF","numeric","fraction","Direct gap fraction from the structural-diversity raster product using a 5 percent height cutoff.","canopy_commbined.csv: Mapped in workflow code as Direct gap fraction; summarized by site."
"Cutoff05-GFP","numeric","fraction","Gap fraction profile from the structural-diversity raster product using a 5 percent height cutoff.","canopy_commbined.csv: Mapped in workflow code as Gap fraction profile; summarized by site."
"Cutoff05-GiniCoeff","numeric","unitless","Gini coefficient for canopy-height or canopy-structure distribution using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Grain01-FHD","numeric","unitless","Foliage Height Diversity at grain size 01 using a 5 percent height cutoff.","canopy_commbined.csv: Mapped in workflow code as Foliage Height Diversity."
"Cutoff05-Grain01-VAI","numeric","m2 m-2","Vegetation Area Index at grain size 01 using a 5 percent height cutoff.","canopy_commbined.csv: Mapped in workflow code as Vegetation Area Index."
"Cutoff05-Grain10-FHD","numeric","unitless","Foliage Height Diversity at grain size 10 using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Grain10-VAI","numeric","m2 m-2","Vegetation Area Index at grain size 10 using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Grain30-FHD","numeric","unitless","Foliage Height Diversity at grain size 30 using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Grain30-VAI","numeric","m2 m-2","Vegetation Area Index at grain size 30 using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-MCH","numeric","m","Mean canopy-height structural-diversity metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site; source field name MCH."
"Cutoff05-MH","numeric","m","Mean height structural-diversity metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site; source field name MH."
"Cutoff05-MSDH","numeric","m","Mean standard deviation of height structural-diversity metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site; source field name MSDH."
"Cutoff05-PointDens","numeric","points m-2","Point density from the structural-diversity raster product using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Q0","numeric","m","0th percentile height metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Q1","numeric","m","1st percentile height metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Q25","numeric","m","25th percentile height metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Q5","numeric","m","5th percentile height metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Q50","numeric","m","50th percentile or median height metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Q75","numeric","m","75th percentile height metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Q95","numeric","m","95th percentile height metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-Q99","numeric","m","99th percentile height metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-RumpleIndex","numeric","unitless","Rumple index from the structural-diversity raster product using a 5 percent height cutoff.","canopy_commbined.csv: Metric describing canopy surface roughness/complexity; summarized by site."
"Cutoff05-SDH","numeric","m","Standard deviation of height metric using a 5 percent height cutoff.","canopy_commbined.csv: Structural-diversity raster metric summarized by site."
"Cutoff05-SDSDH","numeric","m","Standard deviation of the standard deviation of height metric using a 5 percent height cutoff.","canopy_commbined.csv: Used elsewhere in the repo as a canopy height-variance metric."
"Cutoff05-TopRugosity","numeric","m","Top rugosity metric from the structural-diversity raster product using a 5 percent height cutoff.","canopy_commbined.csv: Mapped in workflow code as Top Rugosity; summarized by site."
"Cutoff05-VCI","numeric","unitless","Vertical Complexity Index using a 5 percent height cutoff.","canopy_commbined.csv: Mapped in workflow code as Vertical Complexity Index; summarized by site."
"PointDens","numeric","points m-2","Point density structural-diversity metric without the Cutoff05 prefix.","canopy_commbined.csv: Selected from the structural-diversity site summary in flow.canopy.R."
)---", header = FALSE, check.names = FALSE,
                     col.names = c("column_name", "data_type", "units", "description", "source_or_derivation"))

site.tab <- data.frame(
  Site = first$Site,
  Site_Id_AmeriFlux = af[["Site Id AF"]],
  Site_Name = af[["Name"]],
  Latitude_deg = rd(af[["Latitude (degrees)"]], 5),
  Longitude_deg = rd(af[["Longitude (degrees)"]], 5),
  Elevation_m = rd(af[["Elevation (m)"]], 1),
  IGBP = af[["Vegetation Abbreviation (IGBP)"]],
  Koeppen = af[["Climate Class Abbreviation (Koeppen)"]],
  N_inlet_levels = as.integer(n.lev[first$Site]),
  Inlet_height_lowest_m = rd(z.bot[first$Site], 2),
  Inlet_height_highest_m = rd(z.top[first$Site], 2),
  hc_m = rd(hc), hc_source = hc.src,
  CHM_mean_m = rd(chm), CHM_sd_m = rd(first$CHM.sd), CHM_years = as.integer(num(first$CHM.years)),
  NEON_canopy_height_m = rd(neon.h),
  check.names = FALSE)
for (k in aop.dict$column_name) site.tab[[k]] <- rd(first[[k]], 6)
if (anyNA(site.tab$hc_m)) stop("missing hc for ", paste(site.tab$Site[is.na(site.tab$hc_m)], collapse = ", "))
# consistency with the evaluation canopy height (CanopyHeight = coalesce(CHM.mean, canopyHeight_m))
if (!is.null(first$CanopyHeight) && any(abs(num(first$CanopyHeight) - hc) > 1e-6, na.rm = TRUE))
  stop("hc differs from canopy_commbined CanopyHeight")
wcsv(site.tab, "Site_Canopy_Attributes.csv")

# pairs: canopy_commbined uses A = lower inlet and label "B_A"; the EDI flux files use
# A = upper inlet and label "A_B". Both labels read "upper_lower", e.g. 6_1.
pr <- data.frame(Site = cc$Site, dLevelsAminusB = cc$dLevelsAminusB,
                 TowerPosition_upper = as.integer(num(cc$TowerPosition_B)),
                 TowerPosition_lower = as.integer(num(cc$TowerPosition_A)),
                 z_upper_m = num(cc$MeasurementHeight_m_B),
                 z_lower_m = num(cc$MeasurementHeight_m_A))
stopifnot(all(pr$dLevelsAminusB == paste0(pr$TowerPosition_upper, "_", pr$TowerPosition_lower)),
          all(pr$z_upper_m > pr$z_lower_m))
pr$hc_m <- hc[match(pr$Site, first$Site)]
pr$hc_source <- hc.src[match(pr$Site, first$Site)]
pr$z_geomean_m <- sqrt(pr$z_upper_m * pr$z_lower_m)
pr$dz_m <- pr$z_upper_m - pr$z_lower_m
pr$z_upper_minus_hc_m <- pr$z_upper_m - pr$hc_m
pr$z_lower_minus_hc_m <- pr$z_lower_m - pr$hc_m
n.within <- (pr$hc_m - pr$z_upper_m > 0) + (pr$hc_m - pr$z_lower_m > 0)
pr$Canopy_class <- c("AA", "AW", "WW")[n.within + 1]
for (k in c("hc_m", "z_upper_m", "z_lower_m", "z_geomean_m", "dz_m", "z_upper_minus_hc_m", "z_lower_minus_hc_m"))
  pr[[k]] <- rd(pr[[k]])
pr <- pr[order(pr$Site, pr$TowerPosition_upper, pr$TowerPosition_lower), ]
if (!is.null(cc$Canopy_L1)) {
  ref <- cc$Canopy_L1[match(paste(pr$Site, pr$dLevelsAminusB), paste(cc$Site, cc$dLevelsAminusB))]
  if (any(ref != pr$Canopy_class, na.rm = TRUE)) stop("Canopy_class differs from canopy_commbined Canopy_L1")
}
wcsv(pr, "Site_SamplingHeightPairs.csv")

site.meta <- rbind(
  c("Site_Canopy_Attributes.csv", "Site", "character", "unitless", "NEON site code.", "canopy_commbined.csv"),
  c("Site_Canopy_Attributes.csv", "Site_Id_AmeriFlux", "character", "unitless", "AmeriFlux site ID of the NEON tower.", "Ameriflux_NEON field-sites.csv"),
  c("Site_Canopy_Attributes.csv", "Site_Name", "character", "unitless", "Site name.", "Ameriflux_NEON field-sites.csv"),
  c("Site_Canopy_Attributes.csv", "Latitude_deg", "numeric", "decimal degrees (WGS84)", "Tower latitude.", "Ameriflux_NEON field-sites.csv"),
  c("Site_Canopy_Attributes.csv", "Longitude_deg", "numeric", "decimal degrees (WGS84)", "Tower longitude.", "Ameriflux_NEON field-sites.csv"),
  c("Site_Canopy_Attributes.csv", "Elevation_m", "numeric", "m a.s.l.", "Site elevation.", "Ameriflux_NEON field-sites.csv"),
  c("Site_Canopy_Attributes.csv", "IGBP", "character", "unitless", "IGBP vegetation class.", "Ameriflux_NEON field-sites.csv"),
  c("Site_Canopy_Attributes.csv", "Koeppen", "character", "unitless", "Koeppen climate class.", "Ameriflux_NEON field-sites.csv"),
  c("Site_Canopy_Attributes.csv", "N_inlet_levels", "integer", "count", "Number of NEON tower inlet levels.", "canopy_commbined.csv (NEON sensor positions)"),
  c("Site_Canopy_Attributes.csv", "Inlet_height_lowest_m", "numeric", "m", "Height above ground of the lowest inlet.", "canopy_commbined.csv (NEON DistZaxsLvlMeasTow)"),
  c("Site_Canopy_Attributes.csv", "Inlet_height_highest_m", "numeric", "m", "Height above ground of the highest inlet.", "canopy_commbined.csv (NEON DistZaxsLvlMeasTow)"),
  c("Site_Canopy_Attributes.csv", "hc_m", "numeric", "m", "Canopy height hc used in the paper and release: NEON AOP lidar CHM mean (CHM_mean_m); NEON site canopy height where no lidar summary exists. Use this, not z_veg_aero in the flux files, as the site canopy height.", "flow.edi.metadata.v3b.R"),
  c("Site_Canopy_Attributes.csv", "hc_source", "character", "unitless", "Source of hc.", "flow.edi.metadata.v3b.R"),
  c("Site_Canopy_Attributes.csv", "CHM_mean_m", "numeric", "m", "Mean of the NEON AOP lidar canopy height model (DP3.30015.001) over the tower area, averaged across flight years.", "canopy_commbined.csv CHM.mean (flow_AOP_FormatLayers.R)"),
  c("Site_Canopy_Attributes.csv", "CHM_sd_m", "numeric", "m", "Mean within-area standard deviation of the lidar canopy height model.", "canopy_commbined.csv CHM.sd"),
  c("Site_Canopy_Attributes.csv", "CHM_years", "integer", "count", "Number of AOP flight years in the CHM summary.", "canopy_commbined.csv CHM.years"),
  c("Site_Canopy_Attributes.csv", "NEON_canopy_height_m", "numeric", "m", "Canopy height in the NEON site metadata (DistZaxsCnpy); for reference.", "canopy_commbined.csv canopyHeight_m"),
  cbind("Site_Canopy_Attributes.csv", as.matrix(aop.dict)),
  c("Site_SamplingHeightPairs.csv", "Site", "character", "unitless", "NEON site code.", "canopy_commbined.csv"),
  c("Site_SamplingHeightPairs.csv", "dLevelsAminusB", "character", "unitless", "Sampling-height pair label as in the flux files: 'upper_lower' NEON tower positions (A = upper inlet). Same-inlet labels (n_n) are not pairs and were removed from the flux files.", "canopy_commbined.csv"),
  c("Site_SamplingHeightPairs.csv", "TowerPosition_upper", "integer", "unitless", "NEON tower position of the upper inlet (A in the flux files).", "canopy_commbined.csv"),
  c("Site_SamplingHeightPairs.csv", "TowerPosition_lower", "integer", "unitless", "NEON tower position of the lower inlet (B in the flux files).", "canopy_commbined.csv"),
  c("Site_SamplingHeightPairs.csv", "z_upper_m", "numeric", "m", "Height above ground of the upper inlet.", "canopy_commbined.csv"),
  c("Site_SamplingHeightPairs.csv", "z_lower_m", "numeric", "m", "Height above ground of the lower inlet.", "canopy_commbined.csv"),
  c("Site_SamplingHeightPairs.csv", "hc_m", "numeric", "m", "Site canopy height hc (see Site_Canopy_Attributes.csv).", "flow.edi.metadata.v3b.R"),
  c("Site_SamplingHeightPairs.csv", "hc_source", "character", "unitless", "Source of hc.", "flow.edi.metadata.v3b.R"),
  c("Site_SamplingHeightPairs.csv", "z_geomean_m", "numeric", "m", "Geometric mean inlet height sqrt(z_upper z_lower), the z used for the pair's stability parameter.", "flow.edi.metadata.v3b.R"),
  c("Site_SamplingHeightPairs.csv", "dz_m", "numeric", "m", "Inlet separation z_upper - z_lower.", "flow.edi.metadata.v3b.R"),
  c("Site_SamplingHeightPairs.csv", "z_upper_minus_hc_m", "numeric", "m", "Upper inlet height relative to hc (negative = within the canopy).", "flow.edi.metadata.v3b.R"),
  c("Site_SamplingHeightPairs.csv", "z_lower_minus_hc_m", "numeric", "m", "Lower inlet height relative to hc (negative = within the canopy).", "flow.edi.metadata.v3b.R"),
  c("Site_SamplingHeightPairs.csv", "Canopy_class", "character", "unitless", "AA: both inlets at or above hc; AW: upper above, lower within the canopy; WW: both within the canopy.", "flow.edi.metadata.v3b.R"))
prof.meta <- rbind(
  c("Site_ConcentrationProfiles.csv", "Site", "character", "unitless", "NEON site code.", "flow.edi.profiles.v3b.R"),
  c("Site_ConcentrationProfiles.csv", "gas", "character", "unitless", "Gas: CO2, H2O or CH4.", "flow.edi.profiles.v3b.R"),
  c("Site_ConcentrationProfiles.csv", "units", "character", "unitless", "Units of the concentration columns: umol mol-1 (CO2, CH4), mmol mol-1 (H2O), as reported by NEON.", "flow.edi.profiles.v3b.R"),
  c("Site_ConcentrationProfiles.csv", "month", "integer", "month number", "Calendar month in site local time, pooled over all years (2021-2026).", "flow.edi.profiles.v3b.R"),
  c("Site_ConcentrationProfiles.csv", "tod", "character", "unitless", "Time of day of the sample midpoint in site local time: day (07:00-18:59) or night.", "flow.edi.profiles.v3b.R"),
  c("Site_ConcentrationProfiles.csv", "TowerPosition", "integer", "unitless", "NEON tower inlet level (1 = lowest).", "{SITE}_AE_9min.df.final.csv"),
  c("Site_ConcentrationProfiles.csv", "z_m", "numeric", "m", "Inlet height above ground.", "{SITE}_AE_9min.df.final.csv"),
  c("Site_ConcentrationProfiles.csv", "hc_m", "numeric", "m", "Site canopy height hc (lidar; see Site_Canopy_Attributes.csv).", "Site_Canopy_Attributes.csv"),
  c("Site_ConcentrationProfiles.csv", "z_minus_hc_m", "numeric", "m", "Inlet height relative to hc (negative = within the canopy).", "flow.edi.profiles.v3b.R"),
  c("Site_ConcentrationProfiles.csv", "n_samples", "integer", "count", "Number of 6-min inlet samples averaged (each sample counted once; samples with a NEON final quality flag excluded).", "flow.edi.profiles.v3b.R"),
  c("Site_ConcentrationProfiles.csv", "n_years", "integer", "count", "Number of years contributing samples.", "flow.edi.profiles.v3b.R"),
  c("Site_ConcentrationProfiles.csv", "conc_mean", "numeric", "see units", "Mean of the 6-min inlet mean concentrations.", "flow.edi.profiles.v3b.R"),
  c("Site_ConcentrationProfiles.csv", "conc_sd", "numeric", "see units", "Standard deviation of the 6-min inlet mean concentrations (NA when n_samples = 1).", "flow.edi.profiles.v3b.R"),
  c("Site_ConcentrationProfiles.csv", "conc_median", "numeric", "see units", "Median of the 6-min inlet mean concentrations.", "flow.edi.profiles.v3b.R"))
site.meta <- rbind(site.meta, prof.meta)
site.meta <- as.data.frame(site.meta)
names(site.meta) <- c("file_name", "column_name", "data_type", "units", "description", "source_or_derivation")
stopifnot(identical(site.meta$column_name[site.meta$file_name == "Site_Canopy_Attributes.csv"], names(site.tab)),
          identical(site.meta$column_name[site.meta$file_name == "Site_SamplingHeightPairs.csv"], names(pr)))
pf <- file.path(edi.dir, "Site_ConcentrationProfiles.csv")
if (file.exists(pf)) {
  ph <- read.header(pf)
  if (!identical(ph, site.meta$column_name[site.meta$file_name == "Site_ConcentrationProfiles.csv"]))
    stop("Site_ConcentrationProfiles.csv columns differ from their descriptions")
} else message("note: Site_ConcentrationProfiles.csv not found yet (run flow.edi.profiles.v3b.R)")
wcsv(site.meta, "Metadata_table_site_canopy.csv")

## ---------------------------------------------------------------- ##
## 3. Checks and checksums
## ---------------------------------------------------------------- ##
# pair labels in the flux files (read from the dLevelsAminusB column of the AE files when
# FG_CHECK_PAIRS=TRUE; slow on the full 251 GB release, so off by default)
if (toupper(Sys.getenv("FG_CHECK_PAIRS", "FALSE")) == "TRUE") {
  for (s in sites) {
    f <- file.path(edi.dir, s, paste0(s, "_AE_9min.df.final.csv"))
    h <- read.header(f); j <- which(h == "dLevelsAminusB")
    x <- unique(read.csv(f, colClasses = ifelse(seq_along(h) == j, "character", "NULL"))[[1]])
    if (any(vapply(strsplit(x, "_", fixed = TRUE), function(v) length(v) == 2 && v[1] == v[2], TRUE))) stop(s, ": same-inlet (n_n) rows present; run remove_self_pairs_v3b.sh")
    miss <- setdiff(x, pr$dLevelsAminusB[pr$Site == s])
    if (length(miss)) stop(s, ": pairs not in Site_SamplingHeightPairs.csv: ", paste(miss, collapse = " "))
  }
  message("pair labels checked for ", length(sites), " sites")
}
outs <- c(paste0("Metadata_table_method_", names(method.desc), ".csv"),
          "Site_Canopy_Attributes.csv", "Site_SamplingHeightPairs.csv", "Metadata_table_site_canopy.csv")
md5 <- tools::md5sum(file.path(edi.dir, outs))
print(data.frame(file = outs, md5 = unname(md5)), row.names = FALSE)
