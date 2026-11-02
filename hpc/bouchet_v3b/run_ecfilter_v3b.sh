#!/bin/bash
#SBATCH --job-name=GF-v3b-ecfilter
#SBATCH --partition=day
#SBATCH --cpus-per-task=1
#SBATCH --mem=48G
#SBATCH --time=08:00:00
#SBATCH --output=logs_v3b/ecfilter-%j.log
set -euo pipefail
FG=/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a
export R_LIBS_USER="$FG/R-library-eval-v3b:$FG/R-library-v3b"
cd $FG/runtime_v3b/lterwg-flux-gradient-eval
Rscript --vanilla $FG/ec_filter_selection_v3b_run.R $FG/ec_filter_config_v3b.R
date -Is > $FG/EC_filter_selection_v3b/DONE
