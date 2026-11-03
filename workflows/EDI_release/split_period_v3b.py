# Split one site's v3b EDI flux files into two release periods (EDI file-size limit).
# Period 1: Aug 2021 - Jun 2024; period 2: Jul 2024 - Aug 2026. Cut: 2024-07-01 00:00 UTC.
# 9-min MBR/AE/WP files: by timeMid (UTC midpoint of the paired samples; EC is interpolated to it).
# Ensemble files: by time_start_utc (the half-hour that contains timeMid).
# Rows are copied unchanged. Usage: python3 split_period_v3b.py SITE  (from the FluxGradient folder)
import sys, os, csv, hashlib
site = sys.argv[1]
SRC = "NEON_GradientFlux_EDI_2021_2026_v3b"
OUT = {1: "NEON_GradientFlux_EDI_202108_202406_v3b", 2: "NEON_GradientFlux_EDI_202407_202608_v3b"}
CUT = "2024-07-01"
src = os.path.join(SRC, site)
for d in OUT.values(): os.makedirs(os.path.join(d, site), exist_ok=True)
log = open(os.path.join("EDI_split_v3b", "logs", site + ".tsv"), "w")
for fn in sorted(os.listdir(src)):
    if not fn.endswith(".csv"): continue
    cols = ["time_start_utc"] if "GF_ENSEMBLE" in fn else ["timeMid", "match_time", "timeEnd_A", "timeEndA"]
    f = open(os.path.join(src, fn), newline="")
    header = f.readline(); h = next(csv.reader([header])); nh = len(h)
    idx = [h.index(c) for c in cols if c in h]
    tmp = {k: os.path.join(OUT[k], site, fn + ".tmp") for k in OUT}
    fo = {k: open(tmp[k], "w", newline="") for k in OUT}
    md5 = {k: hashlib.md5() for k in OUT}; n = {1: 0, 2: 0}; tot = 0; fallback = 0
    for k in OUT: fo[k].write(header); md5[k].update(header.encode())
    for line in f:
        tot += 1
        p = line.split(",")
        if len(p) != nh: p = next(csv.reader([line]))
        t = ""
        for j, i in enumerate(idx):
            v = p[i].strip().strip('"')
            if v and v != "NA":
                t = v; fallback += j > 0; break
        if not t: raise SystemExit("no time in %s row %d" % (fn, tot))
        k = 1 if t[:10] < CUT else 2
        fo[k].write(line); md5[k].update(line.encode()); n[k] += 1
    for k in OUT: fo[k].close(); os.replace(tmp[k], tmp[k][:-4])
    assert n[1] + n[2] == tot
    log.write("\t".join(map(str, [site, fn, tot, n[1], n[2], fallback, md5[1].hexdigest(), md5[2].hexdigest()])) + "\n")
log.close(); print(site, "done")
