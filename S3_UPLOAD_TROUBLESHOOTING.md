# COMPEAT S3 Upload Failure - Troubleshooting & Resolution

## Issue Summary
Assessment preparation for **COMP 5 (2021-2026)** failed with:
```
S3 upload did not succeed: compeat/COMP 5 (2021-2026)/runs/20261101T215015-30-run-1e4389ef96/output/Annual_Indicator.csv.gz
```

## What Happened
✅ **Local calculation succeeded** - All assessment files were generated locally:
- Annual_Indicator.csv.gz
- Assessment.csv.gz  
- Assessment_Indicator.csv.gz
- StationSamplesBOT/CTD/PMP.csv.gz
- StationsBOT/CTD/PMP.csv.gz
- Units.* (shapefile components)

❌ **S3 upload failed** - The upload to the S3 bucket was not successful

✅ **Results preserved** - Local files remain in: `data/COMP 5 (2021-2026)/output/`

## Root Causes (Most Likely)

### 1. **AWS Credentials Expired** (Most Common)
The container/application was granted temporary AWS credentials that have expired.

**Solution:**
- Contact your AWS/platform administrator to refresh credentials
- Restart the COMPEAT container with new credentials
- Retry the assessment by selecting COMP 5 (2021-2026) again

### 2. **Network Connectivity Issue**
The S3 endpoint was temporarily unreachable during upload.

**Solution:**
- Verify network connectivity to the S3 endpoint
- Check if `AWS_S3_ENDPOINT` is accessible
- Retry the assessment (automatic retry mechanism is in place)

### 3. **S3 Bucket Permissions**
The credentials lack the required permissions to write to the bucket/prefix.

**Solution:**
- Verify the IAM policy includes:
  - `s3:PutObject` 
  - `s3:HeadObject` (for verification after upload)
- Verify the bucket/prefix path is correct
- Check that the path doesn't have extra spaces or encoding issues

### 4. **Disk Space or Timeout**
Upload was cut off due to time limits or disk issues.

**Solution:**
- Check container resources
- Increase timeout settings if needed
- Retry the assessment

## How to Retry

The application is **designed to handle this scenario**. The local results are preserved, and you can retry publication:

### Option A: Manual Retry (Recommended)
1. Open COMPEAT in your browser
2. Go to the **Assessment** tab
3. Select **"COMP 5 (2021-2026)"** from the dropdown
4. The system will:
   - Detect the local results are complete
   - Load the local files into the application
   - Attempt to publish to S3 if properly configured
   - If S3 fails again, local results are still available

### Option B: Force Retry from CLI
If you want to force the system to retry S3 publication:

```r
# In R console:
setwd("C:/Users/hjalte/GitHub/COMPEAT")
source("app_storage.R")

# Set up storage config (must match deployment environment)
storage <- assessment_storage_config(getwd())

# Attempt publication
tryCatch({
  client <- assessment_s3_client(storage)
  assessment <- "COMP 5 (2021-2026)"
  directory <- file.path(storage$data_dir, assessment, "output")
  manifest <- assessment_make_manifest(assessment, directory)
  assessment_publish(storage, assessment, directory, manifest, client)
  print("✓ S3 publication succeeded")
}, error = function(e) {
  print(paste("✗ S3 publication failed:", conditionMessage(e)))
})
```

## Configuration Check

To verify your S3 configuration is correct, check these environment variables:

**Required:**
- `AWS_S3_ENDPOINT` - S3 endpoint hostname (e.g., `s3.example.com`)
- `COMPEAT_S3_BUCKET` - S3 bucket name
- `AWS_ACCESS_KEY_ID` - AWS access key
- `AWS_SECRET_ACCESS_KEY` - AWS secret key

**Optional:**
- `COMPEAT_S3_PREFIX` - Object prefix (default: `compeat`)
- `AWS_SESSION_TOKEN` - For temporary credentials
- `AWS_DEFAULT_REGION` - AWS region (default: `us-east-1` for AWS)
- `COMPEAT_STORAGE` - Force mode: `local`, `s3`, or `auto` (default)

**To disable S3 and work with local storage only:**
```
COMPEAT_STORAGE=local
```

## File Status

### ✓ Files Present (Ready for S3 Upload)
All required assessment files are locally available and can be manually uploaded if needed:

| File | Size | Location |
|------|------|----------|
| Annual_Indicator.csv.gz | 84.7 KB | `data/COMP 5 (2021-2026)/output/` |
| Assessment.csv.gz | 4.8 KB | `data/COMP 5 (2021-2026)/output/` |
| Assessment_Indicator.csv.gz | 23.6 KB | `data/COMP 5 (2021-2026)/output/` |
| StationSamplesBOT.csv.gz | 532.5 KB | `data/COMP 5 (2021-2026)/output/` |
| StationSamplesCTD.csv.gz | 17.4 MB | `data/COMP 5 (2021-2026)/output/` |
| StationSamplesPMP.csv.gz | 300.6 KB | `data/COMP 5 (2021-2026)/output/` |
| StationsBOT.csv.gz | 145.7 KB | `data/COMP 5 (2021-2026)/output/` |
| StationsCTD.csv.gz | 166.7 KB | `data/COMP 5 (2021-2026)/output/` |
| StationsPMP.csv.gz | 185.1 KB | `data/COMP 5 (2021-2026)/output/` |
| Units.shp/.shx/.dbf/.prj | 4.7 MB | `data/COMP 5 (2021-2026)/output/` |

### ✓ Local Cache Complete
The `assessment.complete` marker file exists, indicating local calculation finished successfully.

### ✗ S3 Persistence Receipt Missing
The `assessment.persisted.json` file is missing, indicating S3 upload never completed.

## Next Steps

1. **Verify AWS Credentials:**
   - Are credentials still valid?
   - Have temporary credentials expired?
   - Do credentials have S3 upload permissions?

2. **Check Network:**
   - Can the container reach the S3 endpoint?
   - Are there any firewall rules blocking access?

3. **Retry the Assessment:**
   - Use the COMPEAT UI to reselect the assessment
   - Or use the CLI approach above

4. **If Persistent Failures:**
   - Check S3 bucket policies and IAM permissions
   - Review CloudWatch logs or S3 access logs
   - Contact your infrastructure team

## Related Code References

- **Storage configuration:** [app_storage.R](./app_storage.R) - `assessment_storage_config()` function
- **Upload handler:** [app_storage.R](./app_storage.R) - `assessment_publish()` and `assessment_s3_client()` functions
- **Retry logic:** [app.R](./app.R) - Assessment job monitoring and error handling
- **Tests:** [tests/test-assessment-storage.R](./tests/test-assessment-storage.R) - Integration tests

## For Developers

If you need to debug S3 connectivity, set these environment variables:

```bash
# Enable verbose output in aws.s3 calls
export AWS_EC2_METADATA_DISABLED=true  # Disable EC2 metadata lookup
```

The error occurs in `app_storage.R` at line ~205 in the `assessment_publish()` function:
```r
if (!isTRUE(client$put(key, file))) stop("S3 upload did not succeed: ", key)
```

The `client$put()` function uses `aws.s3::put_object()` with multipart upload and includes HEAD verification.
