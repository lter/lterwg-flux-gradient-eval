#!/bin/bash
# Remove same-inlet rows (dLevelsAminusB "n_n", e.g. 4_4) from the v3b EDI 9-min flux files.
# These rows pair two consecutive samples of one inlet: AE/WP have no flux, MBR has a
# non-physical flux; they are not in the evaluation. Idempotent (a clean file loses 0 rows).
# Each file is filtered with awk and checked in R (data.table::fread) before it replaces
# the original: the kept labels must equal the original labels minus the n_n rows.
# Usage (Bouchet):  sbatch --array=1-47 EDI_v3b_scripts/remove_self_pairs_v3b.sh
#SBATCH -J rmself
#SBATCH -p day
#SBATCH -t 04:00:00
#SBATCH --mem=24G
#SBATCH -o EDI_v3b_scripts/logs/rmself_%a.out
set -eo pipefail
cd ${FG_ROOT:-/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient}
E=${FG_EDI_DIR:-NEON_GradientFlux_EDI_2021_2026_v3b}
module load R/4.4.2-gfbf-2024a R-bundle-CRAN/2024.11-foss-2024a >/dev/null 2>&1 || true
export R_LIBS_USER="$PWD/R-library-eval-v3b:$PWD/R-library-v3b"
s=$(ls -d $E/*/ | xargs -n1 basename | sed -n "${SLURM_ARRAY_TASK_ID:-1}p")
for m in MBR AE WP; do
  f=$E/$s/${s}_${m}_9min.df.final.csv; [ -f "$f" ] || continue
  t=$f.tmp_noself
  n0=$(( $(wc -l < "$f") - 1 ))
  awk -F, 'NR==1{for(i=1;i<=NF;i++){g=$i;gsub(/"/,"",g);if(g=="dLevelsAminusB")c=i}; if(!c){print "no dLevelsAminusB" > "/dev/stderr"; exit 2}; print; next}
           {l=$c; gsub(/"/,"",l); n=split(l,a,"_"); if(n==2 && a[1]==a[2]) next; print}' "$f" > "$t"
  n1=$(( $(wc -l < "$t") - 1 ))
  chk=$(Rscript --vanilla -e 'suppressMessages(library(data.table)); a <- commandArgs(TRUE)
    rd <- function(f) fread(f, select = "dLevelsAminusB", colClasses = "character", showProgress = FALSE)[[1]]
    self <- function(x) vapply(strsplit(x, "_", fixed = TRUE), function(v) length(v) == 2 && v[1] == v[2], TRUE)
    x <- rd(a[1]); y <- rd(a[2])
    stopifnot(!any(self(y)), identical(y, x[!self(x)]))
    cat("R_ok", length(x), sum(self(x)), length(y))' "$f" "$t")
  mv "$t" "$f"
  echo "$s $m rows_before $n0 removed $((n0 - n1)) rows_after $n1 $chk md5 $(md5sum < "$f" | cut -c1-32)"
done
