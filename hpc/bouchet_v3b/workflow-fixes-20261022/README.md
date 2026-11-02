# Workflow fixes — October 22, 2026

Evaluation fixes are installed in the evaluation repository. Calculation fixes are staged here until the active calculation stage finishes, because that stage reloads functions between sites. The existing heartbeat is responsible for applying them at that safe boundary. No production calculations were restarted for these changes.

The fixes cover atomic verified RData/archive publication and per-site completion manifests; unavailable canopy-height estimates; direct turbulent-flux RMSE in SNR diagnostics; shared rejection flags plus separate spike-removal counts; method-specific facet labels; and reproducible bootstrap streams keyed by run/site/method/gas.

## Verification

12 regression checks passed. All 153 R files in these working copies and the test script parsed successfully. Tests used temporary files and synthetic data; they did not load or recalculate production datasets.

Run the tests from this directory with:

```sh
Rscript --vanilla tests/regression.R .
```

## Applying calculation fixes

After the last calculation stage is recorded as complete:

```sh
python3 apply_fixes.py --repo calculation
```

The script refuses to alter source used by an incomplete calculation. It checks SHA-256 hashes before replacing any source and refuses to overwrite intervening changes. Applying already-installed changes is a no-op. Baseline copies, a manifest, and separate repository patches are included here.

Deterministic seeding applies to future calculations. The current production run retains its original RNG behavior and existing flux outputs. For a fully deterministic dataset, rerun every site consistently after these calculation fixes are installed; do not mix regenerated sites with old bootstrap results. The manifest's `validate_flux_checkpoint()` helper loads the three method files and checks their required objects before a checkpoint is trusted.

RData validation reads saved objects back into memory, and archive verification reads compressed data; these integrity checks add disk I/O and temporary memory use. The new per-site `*_filter_stage_counts<run.tag>.csv` reconciles input, SNR/ustar rejection, spike rejection, and retained rows. Existing filter-report columns remain available.
