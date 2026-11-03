# Storage fluxes for the two v3b EDI release periods (cut 2024-07-01 00:00 UTC on time_halfhour).
# Period 1 (Aug 2021 - Jun 2024): NEON_Storage_Flux/{SITE}_storage_flux.csv, rows before the cut
#   (drops the 2024-07-01 00:00 CO2/H2O row that the newer run also has).
# Period 2 (Jul 2024 - Aug 2026): NEON_Storage_Flux_202407_202608/{SITE}_storage_flux.csv, all rows (checked >= cut).
# CH4 unit labels are corrected from nmol to umol ("nmol m-" -> "umol m-"); both runs predate the 21 Oct 2026 label
# fix in flow.neon.storage.R and the values were always umol. No values change.
# Output: {period folder}/{SITE}/{SITE}_STORAGE_FLUX_30min.csv. Usage: python3 split_storage_v3b.py SITE
import sys, os, csv, hashlib
site = sys.argv[1]; CUT = "2024-07-01"
SRC = {1: "NEON_Storage_Flux", 2: "NEON_Storage_Flux_202407_202608"}
OUT = {1: "NEON_GradientFlux_EDI_202108_202406_v3b", 2: "NEON_GradientFlux_EDI_202407_202608_v3b"}
log = open(os.path.join("EDI_split_v3b", "logs", site + "_storage.tsv"), "w")
for k in (1, 2):
    f = open(os.path.join(SRC[k], site + "_storage_flux.csv"), newline="")
    header = f.readline(); h = next(csv.reader([header])); i = h.index("time_halfhour")
    os.makedirs(os.path.join(OUT[k], site), exist_ok=True)
    out = os.path.join(OUT[k], site, site + "_STORAGE_FLUX_30min.csv")
    w = open(out + ".tmp", "w", newline=""); md5 = hashlib.md5(); w.write(header); md5.update(header.encode())
    tot = kept = relab = 0
    for line in f:
        tot += 1
        t = line.split(",", i + 1)[i].strip().strip('"')
        if k == 1 and t[:10] >= CUT: continue
        if k == 2 and t[:10] < CUT: raise SystemExit("period-2 row before cut: " + t)
        if '"nmol m-' in line: line = line.replace('"nmol m-', '"umol m-'); relab += 1
        w.write(line); md5.update(line.encode()); kept += 1
    w.close(); os.replace(out + ".tmp", out)
    log.write("\t".join(map(str, [site, k, tot, kept, tot - kept, relab, md5.hexdigest()])) + "\n")
log.close(); print(site, "storage done")
