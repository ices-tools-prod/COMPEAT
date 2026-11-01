# S3 upload failure recovery

An error stating "Calculation completed, but S3 persistence failed" means that
publication did not finish. It does not establish whether credentials, permissions,
connectivity, or the S3 service caused the failure.

The app retains `assessment.generated.json` and local output files. Waiting
sessions display them only after validating every file against the manifest,
with a warning and a status identifying S3 publication as pending.
No successful persistence receipt or completion marker is created for this run.
Local files reside in the deployed app's working cache, not necessarily in this
repository on your computer.

## Retry in the UI

1. Wait for local results to finish loading.
2. Select the empty option or another assessment, then select the affected period.
3. The worker retries the pending publication without recalculating.
4. If publication succeeds, the warning state clears. If it fails again, validated
   local results remain available with a warning.

This also works after an app restart if its local working directory survives.
Unpublished results do not survive loss of ephemeral container storage.

## Check the deployment

- Check endpoint, bucket, prefix, and URL style against your service configuration.
- Check credential validity without printing or sharing keys or session tokens.
- Check write permissions, including multipart upload support. On AWS,
  verification using HEAD requires `s3:GetObject`, not an `s3:HeadObject` action.
- Check connectivity and the reported error in the deployed worker logs.
- A corrupt or incomplete pending manifest is not safe to display or publish.
  See [README.md](README.md) for cache recovery and deployment limits.

Do not run manual publication against a cache concurrently with the app.
The supported retry path is the UI, which coordinates jobs within one Shiny process.
