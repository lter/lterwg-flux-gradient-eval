# RSHP Threshold Methods

To identify an operational concordance threshold for defining reliable sampling-height pairs (RSHP), we performed a sensitivity analysis across three candidate cutoffs for flux-gradient versus eddy covariance (EC) agreement: `CCC >= 0.3`, `CCC >= 0.5`, and `CCC >= 0.7`. For each threshold, we reran the AA_AW RSHP workflow and summarized both the fraction of candidate sampling-height pairs retained and the resulting agreement between retained gradient fluxes and EC. Daily sign agreement was quantified as the fraction of retained half-hourly intervals for which `sign(FG_mean) == sign(EC_mean)`. Mean flux bias was quantified as the average difference between retained gradient and EC fluxes, `mean(FG_mean - EC_mean)`, and mean absolute flux bias as `mean(abs(FG_mean - EC_mean))`. We interpreted the lowest threshold (`CCC >= 0.3`) as a permissive screen emphasizing directional consistency, the highest threshold (`CCC >= 0.7`) as a conservative screen emphasizing low mean flux bias, and the intermediate threshold (`CCC >= 0.5`) as a candidate balance point. We selected the working threshold based on the strongest tradeoff between retaining enough RSHP for analysis and improving sign agreement, mean flux bias, and mean absolute flux bias relative to EC.

# RSHP Threshold Table

| Gas | Threshold | RSHP retained (%) | FG-EC sign agreement | Mean flux bias (FG - EC) | Mean absolute flux bias |
| --- | --- | --- | --- | --- | --- |
| CO2 | CCC >= 0.3 | 69.2 | 0.837 |  0.074 | 3.996 |
| CO2 | CCC >= 0.5 | 33.8 | 0.858 | -0.146 | 3.046 |
| CO2 | CCC >= 0.7 | 12.7 | 0.876 | -0.203 | 2.569 |
| H2O | CCC >= 0.3 | 81.9 | 0.854 | -0.007 | 0.858 |
| H2O | CCC >= 0.5 | 57.3 | 0.860 | -0.046 | 0.670 |
| H2O | CCC >= 0.7 | 23.8 | 0.883 | -0.039 | 0.513 |
