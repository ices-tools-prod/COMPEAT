# Rerunning an assessment

## Recalculate through the UI

1. Select the assessment period on the **Assessment** tab and wait for loading to finish.
2. Click **Recalculate assessment...**.
3. Check the period in the confirmation dialog, then click **Recalculate**.
4. Wait for the background job to finish. The maps, indicators and stations refresh,
   including when the period has not changed.

The button works in **both local and S3 modes**. It bypasses completed local
results, S3 restoration and pending-publication reuse. No manual deletion is needed.
Canceling the dialog does not start a job. The button is disabled when no period
is selected or the session is loading/running an assessment.

If another session already has a job running for that period, the forced request
is rejected with a warning. Wait for that job to finish, then request recalculation
again. Ordinary assessment selection can still join the existing job.

## Input data

Forced recalculation downloads **all configured input files again**, replacing
existing downloads and re-extracting ZIP archives before recomputing results.
This includes configured assessment units, configuration workbooks, station
samples and any indicator inputs listed for the selected period.
Locally supplied files without a configured download URL are not refreshed.
Normal assessment selection still reuses cached inputs.

Each download replaces its cached file only after a successful transfer. A
download failure aborts recalculation and preserves previous completed results;
inputs successfully refreshed before the failure remain updated. Retrying S3
publication alone does not download inputs or recalculate.

## Preserving previous results

The calculation writes to a separate temporary output directory. Required outputs
and their manifest are validated before replacing the current local output
directory, without keeping a local backup. A calculation failure leaves the previous output unchanged; the app
reports the failure and displays previous completed results when available.

In S3 mode, successful calculations are uploaded under a new `runs/<run_id>/`
prefix. `current.json` is updated only after all outputs and the manifest have
uploaded successfully. Previous S3 run objects are not deleted.

If S3 publication fails:

- The new, validated local results remain available with a publication-pending warning.
- The previous S3 pointer remains current if it was not successfully updated.
- Select another period (or the empty option), then select this period again to
  retry publication **without recalculating**. This also works after an app restart
  if the local working data survives.
- The previous local output has already been replaced; no local backup is kept.
  Previous published S3 runs remain archived.

If installing the validated staged output fails after removing the old output,
the job reports the failure and retains the staged results at the path shown in
the error for operator recovery.

Other sessions in the same Shiny process viewing the period detect the new job
on their next five-second poll and reload its results. Result-version keys prevent
the Stations map from reusing cached data from the previous calculation.

## Normal selection is still cache-first

Selecting a period normally does not force recalculation:

1. Reuse a completed local cache when valid.
2. In S3 mode, retry validated pending publication or restore the current S3 run.
3. Calculate when no published S3 run exists (or when local mode needs generation).

Deleting the local completion marker or the entire local output directory does
**not** force recalculation when S3 has a published run. S3 access errors, corrupt
objects and invalid manifests are reported, not treated as missing results.
Do not delete S3 archives to request a rerun; use the explicit UI action instead.

## Running from R

From the repository root, with the same storage environment as the application:

```r
source("app_storage.R")
app_root <- normalizePath(getwd())
storage <- assessment_storage_config(app_root)
assessment_run_job(
  app_root, "COMP 5 (2021-2026)", storage,
  force_recalculate = TRUE
)
```

Only run this when no app session or other process is preparing the same period.
The standalone helper does not participate in the Shiny shared-job registry.

## Logs and deployment

Forced-job logs are written to `data/<period>/assessment-stdout.log` and
`assessment-stderr.log`, outside `output`, so replacing that directory does not
move open log files. Normal preparation logs remain inside `output`.
`COMPEAT_DATA_DIR` changes the base directory for both.

Recalculation is a shared, potentially expensive operation. COMPEAT does not
provide application-level user authentication or operator roles. Protect access
to this UI at deployment level; do not expose it to untrusted users.
Job exclusion applies within one Shiny R process, not across multiple app workers
or instances. Use a single worker for a shared writable data directory, or provide
external coordination before enabling concurrent instances.

See [README.md](./README.md) for storage configuration and test commands.
