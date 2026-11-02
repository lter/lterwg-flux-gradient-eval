#!/bin/bash
# EC-free pair selection test (CH4 method): previous SD-based SNR rule; rules scored on CO2/H2O vs EC, applied to CH4.
# Usage (Bouchet login node): bash run_ch4method_v3b.sh     Output: $FG/SNR_SE_v3b/CH4METHOD_SUMMARY.txt
set -eo pipefail
FG=${FG:-/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient}
EDI=$FG/NEON_GradientFlux_EDI_2021_2026_v3b
OUT=$FG/SNR_SE_v3b
CODE=$OUT/code
PART=${PART:-day}
HERE=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$CODE" "$OUT/logs"
exec > >(tee -a "$OUT/logs/launch_ch4method.log") 2>&1
echo "== launch $(date -Is) on $(hostname)"
cp -f "$HERE"/ch4method_site.R "$HERE"/aggregate_ch4method.R "$CODE"/
chmod u+w "$CODE"/*
[ -s "$CODE/calc.ensemble.R" ] && [ -s "$CODE/calc.filter_FG.R" ] && [ -s "$CODE/canopy_L1.csv" ] && [ -s "$OUT/sites.txt" ] || { echo "missing inputs from earlier runs"; exit 1; }
RMODS="R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a"
module load $RMODS
export R_LIBS_USER="$FG/R-library-eval-v3b:$FG/R-library-v3b"
N=$(wc -l < "$OUT/sites.txt"); echo "sites: $N"
cat > "$CODE/ch4m_site.sbatch" <<EOT
#!/bin/bash
#SBATCH -J ch4method
#SBATCH -t 02:00:00
#SBATCH --mem=48G
#SBATCH -c 1
#SBATCH -o $OUT/logs/ch4m_%a.out
module load $RMODS
export R_LIBS_USER="$R_LIBS_USER" OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
SITE=\$(sed -n "\${SLURM_ARRAY_TASK_ID}p" $OUT/sites.txt)
Rscript --vanilla $CODE/ch4method_site.R \$SITE $EDI $OUT $CODE
EOT
cat > "$CODE/ch4m_aggregate.sbatch" <<EOT
#!/bin/bash
#SBATCH -J ch4method_sum
#SBATCH -t 00:30:00
#SBATCH --mem=8G
#SBATCH -o $OUT/logs/ch4m_aggregate.out
module load $RMODS
export R_LIBS_USER="$R_LIBS_USER"
Rscript --vanilla $CODE/aggregate_ch4method.R $OUT
EOT
J1=$(sbatch --parsable -p "$PART" --array=1-"$N" "$CODE/ch4m_site.sbatch")
J2=$(sbatch --parsable -p "$PART" --dependency=afterany:"$J1" "$CODE/ch4m_aggregate.sbatch")
echo "submitted: sites = job $J1 (array 1-$N), summary = job $J2"
echo "results:   $OUT/CH4METHOD_SUMMARY.txt"
