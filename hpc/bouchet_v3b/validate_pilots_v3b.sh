#!/bin/bash
#SBATCH --job-name=GF-pilot-check
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=01:00:00
#SBATCH --output=logs_v3b/pilot-validation-%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_sm3466/sm3466/FluxGradient
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a
export R_LIBS_USER="$PWD/R-library-v3b"
Rscript --vanilla validate_sites_v3b.R KONZ HARV
 date -Is > logs_v3b/PILOTS_VALIDATED
