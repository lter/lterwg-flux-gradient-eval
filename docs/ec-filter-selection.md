# EC representativeness under GF filtering

Run from the repository root:

```sh
Rscript workflows/flow.ec.filter.selection.R config.R
```

Example config (an R script defining these variables):

```r
localdir <- "/path/to/data"
eval.data.dirs <- "NEON_GradientFlux_Data_2021_2026"
site.timezones <- c(HARV = "America/New_York")
canopy.file <- file.path(localdir, "canopy_commbined.csv")
output.dir <- file.path(localdir, "EC_filter_selection")
thresholds <- c(2, 3, 4)
min_per_hour <- 5L
# Optional, recommended for assessing the entire EC record:
# ec.reference.rds <- "/path/to/ec_reference.rds"
# Optional actual final-ensemble availability (not regenerated at each threshold):
# ensemble.times.rds <- "/path/to/ensemble_times.rds"
```

Input evaluation files are the existing pre-filter SITE_Evaluation.Rdata/RDATA,
with MBR_9min.df.final, AE_9min.df.final and WP_9min.df.final.
The script selects AA/AW pairs using the existing canopy table.
Periods must not contain conflicting versions of the same observations.

An optional EC reference RDS must be a data frame with Site, gas, time (POSIXct),
and EC: one quality-controlled value per gas and half-hour. CO2 is in umol/m2/s
and H2O in mmol/m2/s. Align the time labels to the ensemble's nearest-half-hour
convention before supplying the table; do not silently join interval midpoints
to interval endpoints. Preserve physical instants when assigning site timezones.
The project's preparation code already replaces NEON turbulent-flux values with
qfFinl == 1 with NA before interpolation ([source, lines 223–271](https://github.com/lter/lterwg-flux-gradient/blob/c7fac402691da57f16309788dc5a2a9d8afce60f/workflows/flow.neon.data.format.conc.diffs.R#L223-L271)).
NEON processes EC with eddy4R and supplies flags incorporating sensor checks,
stationarity and integral-turbulence tests ([NEON documentation](https://www.neonscience.org/resources/learning-hub/tutorials/eddy-diel-cycle)).
Apply equivalent EC quality screening to a supplied reference and document its
release and exclusions. Periods already excluded from EC cannot be evaluated.

Without this table the baseline is explicitly labelled profile_aligned_EC_proxy:
the mean of distinct finite interpolated EC values in each profile half-hour.
It does NOT assess periods lacking profile observations. Both reference modes
hold the EC value fixed while changing the mask, avoiding method/pair replication
and mask-dependent reaveraging of the EC reference.

The optional ensemble mask has Site, gas, time (POSIXct), one row for every
available final ensemble half-hour. For example, build it from finite
FG_ENSEMBLE rows in SITE_DATA_ENSEMBLE, using time.rounded as time.
This mask includes the actual despiking and CCC selection of that run.
It is reported separately and is not recomputed for SNR thresholds 2/3/4.

## Outputs

Each site gets hourly.csv, daily.csv and unmatched.csv.

- Hourly: EC mean, mean absolute EC, count, reference count and retention for
  each season/hour, approach/pair and threshold. ANY means any pair or method
  survives. A half-hour counts once, regardless of how many pairs survive.
- Stages: all reference EC; profile availability; turbulence; turbulence plus
  target SNR; turbulence plus tracer SNR; turbulence plus both; and finite GF
  availability after both. Tracer screening is a no-op for AE/WP.
- Daily: sum of the 24 hourly EC means times 3600 and molar-mass conversion,
  yielding g CO2 or g H2O/m2/day. These are representative seasonal daily
  totals, not observed individual-day or annual budgets. No loess or gap fill
  is applied. All 24 hours must meet min_per_hour; otherwise the total is NA.
  daily_difference_g_m2 compares with the fixed all-reference baseline.
- Unmatched: profile half-hours without a finite supplied/reference EC value.
  Inspect this before interpreting retention rates.

The first-stage masks reproduce filter_fluxes comparisons and reject missing
filter inputs. Despiking/CCC are included only if the actual ensemble mask is
supplied. This is a diagnostic, not a replacement production filter.

Interpret target and tracer effects separately. Inspect hourly means and
absolute magnitudes as well as signed daily totals, which can cancel. Compare
turbulence_target and turbulence_tracer with turbulence, then both together.
Report selection effects rather than treating the reference as error-free truth.
Check coverage across years and conditions; five observations/hour is a reporting
guard, not proof of representativeness. Uncertainty estimation and full data
reruns remain separate tasks.

## Interpretation

The main question is whether gradient-SNR filters change the hourly patterns and
representative daily totals of available, quality-screened EC observations.
Hold the project's turbulence eligibility fixed: compare turbulence_target,
turbulence_tracer and turbulence_target_tracer with turbulence. The exported
daily_difference_g_m2 uses all_reference, so subtract the turbulence-stage daily
total from each SNR-stage total for this comparison.

Target SNR screens gradient detectability, which depends on exchange and mixing.
Tracer SNR also protects against an unstable MBR denominator. Report hourly
coverage, signed and absolute EC means, and signed daily totals separately.
This is a representativeness check with no assumed direction of bias; it does
not establish exchange during periods excluded by NEON QC or turbulence screening.
