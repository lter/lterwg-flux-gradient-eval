"""Apply reviewed source changes without overwriting edits or altering a running stage."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import stat
import tempfile

root = Path(__file__).resolve().parent
base = root.parent
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--repo', choices=['calculation', 'evaluation', 'both'], default='both')
args = parser.parse_args()
state = json.loads((base / 'run_logs/20261021_v3_overnight/status.json').read_text())
repos = {'calculation': 'lterwg-flux-gradient', 'evaluation': 'lterwg-flux-gradient-eval'}
selected = list(repos.values()) if args.repo == 'both' else [repos[args.repo]]
for stage, repo in repos.items():
    history = [s for s in state['steps'] if s['name'] == stage]
    if repo not in selected:
        continue
    if stage == 'calculation' and (not history or history[-1]['status'] != 'complete'):
        raise SystemExit('Calculation is incomplete; preserve its source until it finishes.')
    if stage == 'evaluation' and history and history[-1]['status'] == 'running':
        raise SystemExit('Evaluation is running; preserve its source until it finishes.')

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest() if path.exists() else None

manifest = json.loads((root / 'changes.json').read_text())
pending = []
for entry in manifest:
    if entry['repo'] not in selected:
        continue
    dest = base / entry['repo'] / entry['path']
    src = root / entry['repo'] / entry['path']
    if digest(src) != entry['new_sha256']:
        raise SystemExit('Staged source changed since verification: ' + str(src))
    current = digest(dest)
    if current == entry['new_sha256']:
        continue
    if current != entry['old_sha256']:
        raise SystemExit('Source has other edits; reconcile before applying: ' + str(dest))
    pending.append((src, dest))
for src, dest in pending:
    dest.parent.mkdir(parents=True, exist_ok=True)
    mode = stat.S_IMODE(dest.stat().st_mode) if dest.exists() else 0o644
    fd, name = tempfile.mkstemp(prefix='.' + dest.name, dir=dest.parent)
    try:
        with os.fdopen(fd, 'wb') as out:
            out.write(src.read_bytes())
        os.chmod(name, mode)
        os.replace(name, dest)
    finally:
        if os.path.exists(name): os.unlink(name)
    print('Applied:', dest)
print('Applied', len(pending), 'files; already applied files were preserved.')
