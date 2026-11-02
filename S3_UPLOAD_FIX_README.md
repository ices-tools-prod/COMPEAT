# COMPEAT S3 Upload Fix

## Overview

This fix resolves S3 upload failures for COMPEAT assessments with special characters in their names (spaces, parentheses, etc.).

## Problem Statement

**Symptom:** Assessments complete successfully and display locally, but only the first file (Annual_Indicator.csv.gz) uploads to S3. All other files fail, and the assessment shows "pending publication".

**Error Message:**
```
Displaying validated local results; S3 publication is still pending.
! in callr subprocess. Caused by error in `value[[3L]](cond)`:
! Calculation completed, but S3 persistence failed. Local results retained...
S3 upload did not succeed: compeat/COMP 5 (2021-2026)/runs/...
```

**Root Causes:**
1. HTTP paths with spaces and special characters were not URL-encoded, causing aws.s3 API calls to fail
2. Error messages leaked AWS credentials from internal aws_error objects

## Solution

Modified `app_storage.R` to:
1. URL-encode HTTP paths for `aws.s3::s3HTTP()` calls (for GET/HEAD requests)
2. Suppress output from `aws.s3::put_object()` to prevent credential leakage
3. Keep S3 object keys unencoded (S3 accepts literal spaces and special characters)

## Key Changes

### File: app_storage.R

**`get()` function (lines 104-121):**
- Added URL encoding: `encoded_path <- paste0("/", utils::URLencode(key, reserved = TRUE))`
- Pass encoded path to s3HTTP() for GET requests

**`put()` function (lines 123-172):**
- Suppress stdout/stderr during put_object() to prevent aws_error printing
- Pass unencoded object key to put_object()
- Use encoded path for HEAD verification
- Safe error handling without exposing sensitive attributes

## Testing

All tests pass:
```bash
cd /srv/shiny-server
Rscript tests/test-assessment-storage.R
```

Output:
```
Storage configuration tests passed.
Real environment storage configuration tests passed.
Assessment storage lifecycle tests passed.
S3 HTTP, shared restoration and disconnected publication tests passed.
```

## Deployment

1. Backup current app_storage.R
2. Replace with fixed version
3. Run tests to verify
4. Restart Shiny application
5. Test with an assessment that has special characters in its name

## Verification

After deployment, verify:
1. Assessment completes without errors
2. Status shows "published" (not "pending")
3. All 13+ files appear in S3 bucket
4. manifest.json and current.json exist in S3

Example S3 check:
```bash
aws s3 ls s3://your-bucket/compeat/COMP\ 5\ \(2021-2026\)/runs/ --recursive
```

## Documentation

- **S3_UPLOAD_FIX_SUMMARY.md** - Executive summary of changes
- **S3_FIX_TECHNICAL_ANALYSIS.md** - Detailed technical explanation
- **S3_DEPLOYMENT_GUIDE.md** - Step-by-step deployment instructions

## No Breaking Changes

- All public APIs remain unchanged
- All existing functionality preserved
- Backward compatible with existing assessments
- All unit tests pass

## Success Criteria

✅ Assessments with spaces/parentheses in names upload successfully  
✅ All 13+ required files upload to S3 (not just the first one)  
✅ No credential leakage in error messages  
✅ Assessment status shows "published" after successful upload  
✅ All unit tests pass
