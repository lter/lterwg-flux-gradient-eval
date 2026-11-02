#!/bin/bash
set -u
cd /nfs/roberts/project/pi_sm3466/sm3466/FluxGradient || exit 1
date -Is
squeue -u "$USER" -o '%.20i %.24j %.10T %.10M %R'
for marker in INPUTS_VERIFIED SUPPORT_VERIFIED EVALUATION_DEPENDENCIES_VERIFIED PILOTS_VALIDATED PIPELINE_COMPLETE; do
  if test -s "logs_v3b/$marker"; then printf '%s: ' "$marker"; cat "logs_v3b/$marker"; else printf '%s: pending\n' "$marker"; fi
done
find NEON_GradientFlux_Data_2021_2026_v3b -name '*_Evaluation.RData' -type f | wc -l
for log in logs_v3b/GF-v3b-KONZ-30269409.log logs_v3b/GF-v3b-HARV-30269410.log; do
  printf '\n%s\n' "$log"
  tail -4 "$log"
done
