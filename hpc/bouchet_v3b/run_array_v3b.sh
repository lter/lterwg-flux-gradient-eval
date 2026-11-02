#!/bin/bash
#SBATCH --job-name=GF-v3b-array
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=12:00:00
#SBATCH --output=logs_v3b/array-%A-%a.log
set -euo pipefail
cd /nfs/roberts/project/pi_sm3466/sm3466/FluxGradient
test -s logs_v3b/INPUTS_VERIFIED
test -s logs_v3b/PILOTS_VALIDATED
site=$(sed -n "$((SLURM_ARRAY_TASK_ID+1))p" remaining_sites_v3b.txt)
test -n "$site"
bash run_site_v3b.sh "$site"
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a
export R_LIBS_USER="$PWD/R-library-v3b"
Rscript --vanilla validate_sites_v3b.R "$site"
