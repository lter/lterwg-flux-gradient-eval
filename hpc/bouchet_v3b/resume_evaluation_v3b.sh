#!/bin/bash
#SBATCH --job-name=GF-v3b-evaluation
#SBATCH --partition=day
#SBATCH --cpus-per-task=4
#SBATCH --mem=128G
#SBATCH --time=1-00:00:00
#SBATCH --output=logs_v3b/evaluation-%j.log
set -euo pipefail
cd /nfs/roberts/project/pi_sm3466/sm3466/FluxGradient
test -s logs_v3b/INPUTS_VERIFIED
test -s logs_v3b/SUPPORT_VERIFIED
test -s logs_v3b/PILOTS_VALIDATED
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
export R_LIBS_USER="$PWD/R-library-eval-v3b:$PWD/R-library-v3b"
Rscript --vanilla validate_sites_v3b.R
Rscript --vanilla -e 'writeLines(capture.output(sessionInfo()),"logs_v3b/evaluation_session.txt"); source("runtime_v3b/lterwg-flux-gradient-eval/workflows/flow.evaluation.resume_v3b.R")'
date -Is > /nfs/roberts/project/pi_sm3466/sm3466/FluxGradient/logs_v3b/PIPELINE_COMPLETE
