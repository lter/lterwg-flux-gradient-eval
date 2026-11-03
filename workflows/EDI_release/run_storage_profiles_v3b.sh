#!/bin/bash
# Bouchet: sbatch --array=1-47%24 EDI_split_v3b/run_storage_profiles_v3b.sh  (after run_split_v3b.sh;
# needs NEON_Storage_Flux/ and NEON_Storage_Flux_202407_202608/ in the FluxGradient folder)
#SBATCH -J edistorprof
#SBATCH -p day
#SBATCH -t 04:00:00
#SBATCH --mem=48G
#SBATCH -o EDI_split_v3b/logs/storprof_%a.out
set -eo pipefail
cd ${FG_ROOT:-/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient}
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a >/dev/null 2>&1 || true
export R_LIBS_USER="$PWD/R-library-eval-v3b:$PWD/R-library-v3b" OMP_NUM_THREADS=1 FG_ROOT=$PWD
s=$(ls -d NEON_GradientFlux_EDI_2021_2026_v3b/*/ | xargs -n1 basename | sed -n "${SLURM_ARRAY_TASK_ID:-1}p")
python3 EDI_split_v3b/split_storage_v3b.py "$s"
for p in 202108_202406 202407_202608; do
  mkdir -p EDI_split_v3b/profiles_tmp_$p
  FG_EDI_DIR=NEON_GradientFlux_EDI_${p}_v3b FG_PROFILE_TMP=$PWD/EDI_split_v3b/profiles_tmp_$p Rscript --vanilla EDI_split_v3b/flow.edi.profiles.v3b.R site "$s"
done
