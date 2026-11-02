#!/bin/bash
#SBATCH -J selfrows
#SBATCH -p day
#SBATCH -t 01:00:00
#SBATCH --mem=2G
#SBATCH -o EDI_v3b_scripts/self_rows_%a.out
cd /nfs/roberts/project/pi_sm3466/sm3466/FluxGradient
E=NEON_GradientFlux_EDI_2021_2026_v3b
s=$(ls -d $E/*/ | xargs -n1 basename | sed -n "${SLURM_ARRAY_TASK_ID}p")
for m in MBR AE WP; do f=$E/$s/${s}_${m}_9min.df.final.csv; [ -f $f ] || continue
awk -F, -v s=$s -v m=$m 'NR==1{for(i=1;i<=NF;i++){g=$i;gsub(/"/,"",g);c[g]=i};next}{l=$c["dLevelsAminusB"];gsub(/"/,"",l);split(l,a,"_");k=(a[1]==a[2]?"self":"pair");n[k]++;if($c["FG_mean"]!="NA"&&$c["FG_mean"]!="")f[k]++}END{print s,m,"self",n["self"]+0,"self_FG",f["self"]+0,"pair",n["pair"]+0}' $f
done
