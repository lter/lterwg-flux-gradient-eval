#!/bin/bash
#SBATCH --job-name=GF-v3b
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=32G
#SBATCH --time=12:00:00
#SBATCH --output=logs_v3b/%x-%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_sm3466/sm3466/FluxGradient
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export R_LIBS_USER="$PWD/R-library-v3b"
export GF_SITE="$1"
cd runtime_v3b/lterwg-flux-gradient
Rscript --vanilla -e 'source("workflows/WorkFlow_resume_exports_v3b.R")'
