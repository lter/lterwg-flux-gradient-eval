from pathlib import Path
import shutil, hashlib, json, re
root=Path('/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient')
snapshot=root/'code_snapshot'
runtime=root/'runtime_v3b'
assert not runtime.exists(), 'Runtime already exists; inspect before replacing'
runtime.mkdir()
changes=json.loads((snapshot/'workflow-fixes-20261022/changes.json').read_text())
for repo in ['lterwg-flux-gradient','lterwg-flux-gradient-eval']:
 shutil.copytree(snapshot/repo,runtime/repo)
for item in changes:
 dest=runtime/item['repo']/item['path']
 current=hashlib.sha256(dest.read_bytes()).hexdigest() if dest.exists() else None
 assert current in [item['old_sha256'],item['new_sha256']], str(dest)
 src=snapshot/'workflow-fixes-20261022'/item['repo']/item['path']
 assert hashlib.sha256(src.read_bytes()).hexdigest()==item['new_sha256']
 dest.parent.mkdir(parents=True,exist_ok=True)
 shutil.copy2(src,dest)
local='/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient'
for f in runtime.rglob('*.R'):
 text=f.read_text()
 text=text.replace(local+'/lterwg-flux-gradient-eval',str(runtime/'lterwg-flux-gradient-eval'))
 text=text.replace(local+'/lterwg-flux-gradient',str(runtime/'lterwg-flux-gradient'))
 text=text.replace('/Volumes/MaloneLab/Research/FluxGradient',str(root))
 text=text.replace('_2021_2026_v3','_2021_2026_v3b')
 text=re.sub(r'\.R(?:data|DATA)\b','.RData',text)
 f.write_text(text)
workflow=runtime/'lterwg-flux-gradient/workflows/WorkFlow_rerun_2021_2026_v3.R'
text=workflow.read_text()
needle='all.sites <- metadata$Site_Id.NEON %>% unique()'
assert text.count(needle)==1
text=text.replace(needle,needle+'\nrequested.site <- Sys.getenv("GF_SITE")\nstopifnot(nzchar(requested.site), requested.site %in% all.sites)\nall.sites <- requested.site')
workflow.write_text(text)
print('Runtime prepared:',runtime)
