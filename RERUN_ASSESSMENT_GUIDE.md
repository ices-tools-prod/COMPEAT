# Guide: Rerunning an Archived Assessment

This guide explains how to rerun an assessment for a given assessment period that has already been run and has results archived in S3 storage.

## How COMPEAT Handles Assessment Caching

COMPEAT uses a **completion marker** file called `assessment.complete` to track whether an assessment has been successfully run. This marker is stored in:

```
data/<ASSESSMENT_PERIOD>/output/assessment.complete
```

When you select an assessment period in the UI:
1. COMPEAT checks if `assessment.complete` exists
2. If it exists AND all required output files are present, the cached results are loaded (fast)
3. If it doesn't exist, a new assessment calculation is triggered

## Two Approaches to Rerun an Assessment

### Approach 1: Via the Web UI (Recommended for Users)

#### Local Storage Mode (No S3)
1. **Delete the completion marker file**:
   - Navigate to: `data/<ASSESSMENT_PERIOD>/output/`
   - Delete: `assessment.complete`
   
2. **Select the assessment period** from the COMPEAT web application
   - System detects missing marker and recalculates from input data

#### S3 Storage Mode
**There is no UI-based method to force recalculation.** The system is designed to:
1. Reuse local files if complete
2. Restore from S3 cache if local files are missing
3. Only recalculate if NEITHER local files nor S3 cache exist

This means if an assessment is already cached in S3, deleting only the local files will just re-download from S3 without recalculating.

To force a true recalculation in S3 mode, you must delete the cached results from S3 storage itself (see Approach 2 below).

### Approach 2: Via Script/Command Line

If you need to programmatically rerun assessments or do this via automation:

#### For Local Storage Mode: R Script

```R
# Load the configuration
source("./app_storage.R", local = TRUE)
assessment_storage <- assessment_storage_config(getwd())

# For the assessment you want to rerun
assessment <- "COMP 5 (2021-2026)"  # Replace with your assessment period

# Delete the completion marker only (sufficient for local mode)
marker <- file.path(assessment_storage$data_dir, assessment, "output", "assessment.complete")
if (file.exists(marker)) {
  file.remove(marker)
  cat("Removed completion marker:", marker, "\n")
}
```

#### For S3 Storage Mode: Delete from S3 Using AWS CLI

The only way to force a true recalculation in S3 mode is to delete the cached results from S3:

```bash
# Delete the entire assessment results from S3
aws s3 rm s3://your-bucket/your-prefix/COMP\ 5\ \(2021-2026\)/ --recursive

# Verify deletion
aws s3 ls s3://your-bucket/your-prefix/COMP\ 5\ \(2021-2026\)/
```

Alternatively using boto3 (Python):

```python
import boto3

s3 = boto3.client('s3')
bucket = 'your-bucket'
prefix = 'your-prefix/COMP 5 (2021-2026)/'

# List all objects with this prefix
response = s3.list_objects_v2(Bucket=bucket, Prefix=prefix)

# Delete all objects
if 'Contents' in response:
    for obj in response['Contents']:
        s3.delete_object(Bucket=bucket, Key=obj['Key'])
        print(f"Deleted: {obj['Key']}")
```

Or using the aws.s3 R package:

```R
library(aws.s3)

# Configure S3 access (uses environment variables)
assessment <- "COMP 5 (2021-2026)"
s3_prefix <- "your-prefix"
bucket <- "your-bucket"

# List and delete all objects for this assessment
key_prefix <- paste0(s3_prefix, "/", assessment, "/")
objects <- aws.s3::get_bucket(bucket, prefix = key_prefix)

if (length(objects) > 0) {
  for (obj in objects) {
    aws.s3::delete_object(object = obj$Key, bucket = bucket)
    cat("Deleted from S3:", obj$Key, "\n")
  }
} else {
  cat("No objects found at:", key_prefix, "\n")
}

# Also delete local cache so it will recalculate
output_dir <- file.path("data", assessment, "output")
unlink(output_dir, recursive = TRUE)
```

**After deleting from S3:**
1. The local output directory will be empty (or delete it manually)
2. Select the assessment in COMPEAT
3. `assessment_restore()` will fail to find S3 cache
4. Recalculation will be triggered

## Understanding the Assessment Run Process

When you trigger assessment selection in COMPEAT, the system follows these steps:

1. **Check S3 configuration** (if applicable)
2. **For S3 mode:**
   - Try to **restore from S3 cache** → If successful, use cached results
   - If S3 restore fails → **Calculate fresh** from input data
   - Publish to S3 for future restores
3. **For local mode:**
   - Check if completion marker exists
   - If missing → **Calculate fresh** from input data
4. **Execute in background** (separate R process)
   - Logs written to: `assessment-stdout.log` and `assessment-stderr.log`
5. **Create completion marker** once done

**Key Point:** In S3 mode, the system ALWAYS tries to use the S3 cache first. There's no built-in flag to skip S3 and recalculate. To force recalculation, you must make S3 restore fail by deleting the S3 cache.

## Storage Modes

### Local Storage Mode
- Results are stored in: `data/<ASSESSMENT_PERIOD>/output/`
- **To force recalculation:** Delete only `assessment.complete`
- **What happens:** System recalculates from input data
- No S3 dependencies

### S3 Storage Mode
- Local cache is maintained in: `data/<ASSESSMENT_PERIOD>/output/`
- Results are archived in S3: `s3://bucket/prefix/<ASSESSMENT_PERIOD>/`
- Additional S3-specific files:
  - `assessment.generated.json` (pending publication marker)
  - `assessment.persisted.json` (storage identity receipt)

**Design Philosophy:**
The system prioritizes performance and reusability:
1. **First, check local cache** → Use if complete
2. **Then, check S3 cache** → Restore and use if available  
3. **Finally, recalculate** → Only if neither cache exists

**To force recalculation in S3 mode:**
- You MUST delete the cached results from S3 itself
- Local deletion alone won't trigger recalculation (S3 restore will still succeed)
- See "Approach 2" above for AWS CLI and R examples

## What Gets Recalculated

When you rerun an assessment, the following happens:

- **Input data** is re-read from:
  - Configuration files (e.g., `Configuration2021-2026.xlsx`)
  - Station samples data (CTD, BOT, PMP files)
  - Assessment unit definitions
  - Any other data sources configured in `data.R`

- **Indicators** are recalculated:
  - Annual indicators
  - Assessment indicators
  - Nutrient levels (Nitrogen/Phosphorus)
  - Direct and indirect effects

- **Output files** are regenerated:
  - `Assessment.csv.gz` - Main assessment results
  - `Assessment_Indicator.csv.gz` - Indicator-level results
  - `Annual_Indicator.csv.gz` - Annual aggregates
  - Shapefiles (Units.shp, .shx, .dbf, .prj) - Geographic data
  - Station files (StationsBOT.csv.gz, StationsCTD.csv.gz, StationsPMP.csv.gz)

## Troubleshooting

### Assessment not rerunning after deleting completion marker

**Solution:**
1. Check that the file was actually deleted: `ls -la data/<PERIOD>/output/assessment.complete`
2. Refresh the web browser
3. Check browser console for errors
4. Review the logs: `data/<PERIOD>/output/assessment-stderr.log`

### S3 credentials or permissions errors

**Solution:**
1. Verify S3 credentials are configured: `echo $AWS_ACCESS_KEY_ID` (should not be empty)
2. Check bucket and prefix permissions
3. Ensure COMPEAT_S3_BUCKET environment variable is set correctly
4. Review error message from `assessment-stderr.log`

### Assessment calculation fails with data errors

**Solution:**
1. Check input data directory: `data/<PERIOD>/input/`
2. Verify all required input files are present
3. Review data.R for download/extraction issues
4. Check `assessment-stderr.log` for specific error messages

## Environment Variables (S3 Configuration)

When using S3 storage, ensure these are set:

```bash
# Required for S3
export AWS_S3_ENDPOINT="s3.provider.com"
export AWS_ACCESS_KEY_ID="your-access-key"
export AWS_SECRET_ACCESS_KEY="your-secret-key"
export COMPEAT_S3_BUCKET="my-bucket"

# Optional
export COMPEAT_S3_PREFIX="compeat"          # Default: compeat
export COMPEAT_DATA_DIR="/path/to/data"     # Default: ./data
export COMPEAT_STORAGE="s3"                 # Default: auto (detects based on endpoint)
export AWS_DEFAULT_REGION="us-east-1"       # For AWS (not needed for custom endpoints)
```

## Related Files

- **[app.R](./app.R)** - Main Shiny application with `run_assessment()` function (lines 162-332)
- **[app_storage.R](./app_storage.R)** - Storage configuration and `assessment_run_job()` function
- **[data.R](./data.R)** - Assessment calculation and data loading logic
- **[retry_s3_publication.R](./retry_s3_publication.R)** - Utility for retrying failed S3 publications

## Performance Tips

- **First-time assessment runs** may take 5-30 minutes depending on data size
- **Subsequent loads** are fast (seconds) when cached results are available
- **S3 transfers** depend on network speed and file sizes (typically 100-500 MB)
- **Run assessments during off-peak hours** if possible to avoid impacting other users
