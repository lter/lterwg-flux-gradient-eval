#!/bin/bash
# Run the SE-based SNR filter (MBR 30, AE/WP 3) and the previous SD rule on the v3b gradient fluxes.
# Usage (Bouchet login node, from the folder holding this script and its R files):
#   bash run_snrse_v3b.sh
# Optional: PART=<partition> bash run_snrse_v3b.sh   (default: day)
# Log of this script: $FG/SNR_SE_v3b/logs/launch.log
# Writes everything to $FG/SNR_SE_v3b; nothing in the v3b folder or your repos is changed.
set -eo pipefail
FG=${FG:-/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient}
EDI=$FG/NEON_GradientFlux_EDI_2021_2026_v3b
OUT=$FG/SNR_SE_v3b
CODE=$OUT/code
PART=${PART:-day}
HERE=$(cd "$(dirname "$0")" && pwd)

[ -d "$EDI" ] || { echo "v3b folder not found: $EDI"; exit 1; }
mkdir -p "$CODE" "$OUT/logs"
exec > >(tee -a "$OUT/logs/launch.log") 2>&1
echo "== launch $(date -Is) on $(hostname)"
cp -f "$HERE"/calc.filter_FG.R "$HERE"/run_site.R "$HERE"/aggregate.R "$HERE"/canopy_L1.csv "$CODE"/
chmod u+w "$CODE"/*

# R: same modules and libraries as the v3b pipeline (run_evaluation_v3b.sh)
RMODS="R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a"
module load $RMODS
export R_LIBS_USER="$FG/R-library-eval-v3b:$FG/R-library-v3b"
Rscript --vanilla -e 'suppressPackageStartupMessages({library(data.table); library(dplyr)}); cat("R", R.version$major, R.version$minor, "| data.table", as.character(packageVersion("data.table")), "| dplyr", as.character(packageVersion("dplyr")), "\n")'

# Use the canopy table on Bouchet if there is one (v3 canopy heights); otherwise the bundled copy
for c in "$FG/canopy_commbined.csv" "$FG/NEON_GradientFlux_EDI_2021_2026_v3b/canopy_commbined.csv"; do
  if [ -f "$c" ]; then
    Rscript --vanilla -e "x <- unique(read.csv('$c')[, c('Site','dLevelsAminusB','Canopy_L1')]); write.csv(x, '$CODE/canopy_L1.csv', row.names = FALSE); cat('canopy table:', '$c', nrow(x), 'rows\n')"
    break
  fi
done

# Sites = folders in v3b with an MBR, AE or WP file
ls -d "$EDI"/*/ | xargs -n1 basename | while read s; do ls "$EDI/$s/${s}"_*_9min.df.final.csv >/dev/null 2>&1 && echo "$s"; done > "$OUT/sites.txt"
N=$(wc -l < "$OUT/sites.txt"); echo "sites: $N"; [ "$N" -gt 0 ] || { echo "No site files found in $EDI"; exit 1; }

cat > "$CODE/site.sbatch" <<EOF
#!/bin/bash
#SBATCH -J snrse_site
#SBATCH -t 02:00:00
#SBATCH --mem=32G
#SBATCH -c 1
#SBATCH -o $OUT/logs/site_%a.out
module load $RMODS
export R_LIBS_USER="$R_LIBS_USER" OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
SITE=\$(sed -n "\${SLURM_ARRAY_TASK_ID}p" $OUT/sites.txt)
Rscript --vanilla $CODE/run_site.R \$SITE $EDI $OUT $CODE
EOF
cat > "$CODE/aggregate.sbatch" <<EOF
#!/bin/bash
#SBATCH -J snrse_sum
#SBATCH -t 00:30:00
#SBATCH --mem=8G
#SBATCH -o $OUT/logs/aggregate.out
module load $RMODS
export R_LIBS_USER="$R_LIBS_USER"
Rscript --vanilla $CODE/aggregate.R $OUT
EOF

J1=$(sbatch --parsable -p "$PART" --array=1-"$N" "$CODE/site.sbatch")
J2=$(sbatch --parsable -p "$PART" --dependency=afterany:"$J1" "$CODE/aggregate.sbatch")
echo "submitted: sites = job $J1 (array 1-$N), summary = job $J2"
echo "watch:     squeue -u \$USER"
echo "results:   $OUT/SUMMARY_SE_vs_SD.txt (plus ALL_*.csv, RSHP_*.csv, sites/<SITE>/<SITE>_FILTER_SE.Rdata)"
