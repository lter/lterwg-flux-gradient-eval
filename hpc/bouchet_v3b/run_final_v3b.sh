#!/bin/bash
#SBATCH --job-name=GF-v3b-final
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=02:00:00
#SBATCH --output=logs_v3b/final-%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_sm3466/sm3466/FluxGradient
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a
export R_LIBS_USER="$PWD/R-library-eval-v3b:$PWD/R-library-v3b"
Rscript --vanilla final_v3b.R
date -Is > logs_v3b/PIPELINE_COMPLETE
