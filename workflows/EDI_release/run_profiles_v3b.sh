#!/bin/bash
# Per-site concentration profiles for the v3b EDI release (flow.edi.profiles.v3b.R).
# Usage (Bouchet, from the FluxGradient folder):
#   sbatch --array=1-47 EDI_v3b_scripts/run_profiles_v3b.sh
#   then: Rscript --vanilla EDI_v3b_scripts/flow.edi.profiles.v3b.R combine
#         Rscript --vanilla EDI_v3b_scripts/flow.edi.metadata.v3b.R
#SBATCH -J ediprof
#SBATCH -p day
#SBATCH -t 02:00:00
#SBATCH --mem=48G
#SBATCH -o EDI_v3b_scripts/logs/profiles_%a.out
set -eo pipefail
cd ${FG_ROOT:-/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient}
E=${FG_EDI_DIR:-NEON_GradientFlux_EDI_2021_2026_v3b}
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a >/dev/null 2>&1 || true
export R_LIBS_USER="$PWD/R-library-eval-v3b:$PWD/R-library-v3b" OMP_NUM_THREADS=1
s=$(ls -d $E/*/ | xargs -n1 basename | sed -n "${SLURM_ARRAY_TASK_ID:-1}p")
Rscript --vanilla EDI_v3b_scripts/flow.edi.profiles.v3b.R site "$s"
