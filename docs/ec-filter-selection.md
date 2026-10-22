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
Only supply EC values that pass the intended independent reference QC.
Document that QC, including any turbulence, stationarity or detection screening.
Do not remove otherwise valid EC solely for being small when constructing this
diagnostic reference. The diagnostic cannot assess periods already absent from
the EC reference; a shared exclusion can hide a selection effect.

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

Target SNR screens gradient detectability; tracer SNR also protects the MBR
denominator. Turbulence screening addresses transport assumptions. These purposes
are distinct, and a failed flag alone does not establish that the underlying
exchange was unreal. This diagnostic does not recommend keeping every rejected
measurement or applying a chamber-style retention rule to all tower filters.

For the target/tracer detection question, compare masks within the same
turbulence-eligible reference periods. A difference across the turbulence screen
is descriptive: EC under inadequate turbulence may itself miss exchange, so it
cannot establish the true budget of those excluded periods.

Changes in hourly patterns, absolute magnitudes and signed daily totals are
different outcomes. Missing peaks need not produce a systematic net-budget bias.
Report each outcome separately. The script diagnoses availability selection; it
does not validate GF errors or later gap filling. Any reconstruction needs
separate validation under the hours, seasons and conditions actually excluded.

