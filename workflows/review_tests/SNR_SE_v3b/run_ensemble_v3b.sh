#!/bin/bash
# Filter -> despike -> pair CCC -> 30-min ensembles (CO2/H2O RSHP ensemble; no-EC S2 ensemble for all gases)
# under the previous SD-based SNR rule and the SE-based rule, for the v3b gradient fluxes.
# Usage (Bouchet login node):  bash run_ensemble_v3b.sh      Optional: PART=<partition>
# Output: $FG/SNR_SE_v3b/ensemble/<SITE>/ and $FG/SNR_SE_v3b/ENSEMBLE_SUMMARY_SE_vs_SD.txt
set -eo pipefail
FG=${FG:-/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient}
EDI=$FG/NEON_GradientFlux_EDI_2021_2026_v3b
OUT=$FG/SNR_SE_v3b
CODE=$OUT/code
PART=${PART:-day}
HERE=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$CODE" "$OUT/logs"
exec > >(tee -a "$OUT/logs/launch_ensemble.log") 2>&1
echo "== launch $(date -Is) on $(hostname)"
cp -f "$HERE"/calc.filter_FG.R "$HERE"/ensemble_site.R "$HERE"/aggregate_ens.R "$CODE"/
chmod u+w "$CODE"/*
V3=$FG/runtime_v3b/lterwg-flux-gradient-eval/functions/calc.ensemble.R
[ -f "$V3" ] || { echo "missing $V3"; exit 1; }
cp -f "$V3" "$CODE"/calc.ensemble.R; chmod u+w "$CODE"/calc.ensemble.R; echo "calc.ensemble.R from $V3"
[ -s "$CODE/canopy_L1.csv" ] && [ -s "$OUT/sites.txt" ] || { echo "Run run_snrse_v3b.sh first (canopy table and site list)"; exit 1; }
RMODS="R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a"
module load $RMODS
export R_LIBS_USER="$FG/R-library-eval-v3b:$FG/R-library-v3b"
N=$(wc -l < "$OUT/sites.txt"); echo "sites: $N"
cat > "$CODE/ens_site.sbatch" <<EOT
#!/bin/bash
#SBATCH -J snrse_ens
#SBATCH -t 03:00:00
#SBATCH --mem=48G
#SBATCH -c 1
#SBATCH -o $OUT/logs/ens_%a.out
module load $RMODS
export R_LIBS_USER="$R_LIBS_USER" OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
SITE=\$(sed -n "\${SLURM_ARRAY_TASK_ID}p" $OUT/sites.txt)
Rscript --vanilla $CODE/ensemble_site.R \$SITE $EDI $OUT $CODE
EOT
cat > "$CODE/ens_aggregate.sbatch" <<EOT
#!/bin/bash
#SBATCH -J snrse_enssum
#SBATCH -t 00:30:00
#SBATCH --mem=8G
#SBATCH -o $OUT/logs/ens_aggregate.out
module load $RMODS
export R_LIBS_USER="$R_LIBS_USER"
Rscript --vanilla $CODE/aggregate_ens.R $OUT
EOT
J1=$(sbatch --parsable -p "$PART" --array=1-"$N" "$CODE/ens_site.sbatch")
J2=$(sbatch --parsable -p "$PART" --dependency=afterany:"$J1" "$CODE/ens_aggregate.sbatch")
echo "submitted: sites = job $J1 (array 1-$N), summary = job $J2"
echo "results:   $OUT/ENSEMBLE_SUMMARY_SE_vs_SD.txt"
