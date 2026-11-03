#!/bin/bash
# Bouchet: sbatch --array=1-47%24 EDI_split_v3b/run_split_v3b.sh  (from the FluxGradient folder)
#SBATCH -J edisplit
#SBATCH -p day
#SBATCH -t 04:00:00
#SBATCH --mem=4G
#SBATCH -o EDI_split_v3b/logs/split_%a.out
set -eo pipefail
cd ${FG_ROOT:-/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient}
s=$(ls -d NEON_GradientFlux_EDI_2021_2026_v3b/*/ | xargs -n1 basename | sed -n "${SLURM_ARRAY_TASK_ID:-1}p")
python3 EDI_split_v3b/split_period_v3b.py "$s"
