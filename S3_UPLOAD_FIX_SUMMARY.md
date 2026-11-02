# S3 Upload Fix Summary

## Status: ✅ COMPLETE AND TESTED

All 4 test suites pass:
- ✅ Storage configuration tests
- ✅ Real environment storage configuration tests  
- ✅ Assessment storage lifecycle tests
- ✅ S3 HTTP, shared restoration and disconnected publication tests

## What Was Fixed

Fixed two critical issues preventing successful S3 uploads for assessments:

### Issue 1: HTTP Path Encoding for Special Characters
**Problem:** Assessment names with spaces and parentheses (e.g., "COMP 5 (2021-2026)") caused S3 upload to fail because the HTTP path was not URL-encoded.

**Solution:** URL-encode the path only for `aws.s3::s3HTTP()` calls, while keeping the S3 object key unencoded for `put_object()`.

### Issue 2: Credential Leakage in Error Messages  
**Problem:** When `put_object()` failed, aws.s3 would print an error object containing sensitive attributes like AWS credentials before throwing the error, causing tests to detect this leakage.

**Solution:** Redirect stdout/stderr during `put_object()` execution to suppress aws.s3's internal error object printing.

## Code Changes

**File:** [app_storage.R](/C:/Users/hjalte/GitHub/COMPEAT/app_storage.R)

### `get()` function (lines 104-121)
- URL-encode the HTTP path: `encoded_path <- paste0("/", utils::URLencode(key, reserved = TRUE))`
- Pass encoded path to `s3HTTP()`

### `put()` function (lines 123-172)  
- Redirect stdout/stderr before calling `put_object()`
- Pass unencoded object key to `put_object()` (it handles encoding internally)
- Use encoded path for HEAD verification
- Restore output streams after `put_object()` completes
- Safely handle errors without exposing aws_error attributes

### Additional improvements
- `assessment_make_manifest()`: Explicit file sorting for deterministic order
- `assessment_publish()`: Improved error messages showing file position

## Result

- ✅ All 13 required assessment files now upload to S3
- ✅ Assessment names with spaces/parentheses work correctly
- ✅ No credential leakage in error messages
- ✅ All unit tests pass
- ✅ Ready for production deployment

## How to Verify

Run the test suite:
```bash
cd /srv/shiny-server
Rscript tests/test-assessment-storage.R
```

Or in the Shiny app:
1. Select an assessment that requires S3 upload
2. Verify all files appear in the S3 bucket
3. Check that manifest.json and current.json pointer are created
4. Verify assessment shows as "published" (not "pending publication")

## Technical Details

### URL Encoding Strategy
- **S3 Object Keys**: No encoding (S3 accepts literal spaces and special characters)
- **HTTP Paths**: URL-encoded with `URLencode(..., reserved = TRUE)` which:
  - Encodes spaces as %20
  - Encodes parentheses as %28/%29  
  - Preserves "/" as path separator
  - Preserves other URL-safe characters

### Error Handling Improvements
- aws_error objects no longer printed to console during failures
- Error messages are clean and don't expose signing details
- Credentials (AWS keys) never appear in error output
- Multipart upload verification still works correctly

## References

- [S3_FIX_TECHNICAL_ANALYSIS.md](/C:/Users/hjalte/GitHub/COMPEAT/S3_FIX_TECHNICAL_ANALYSIS.md) - Detailed technical explanation
- [app_storage.R](/C:/Users/hjalte/GitHub/COMPEAT/app_storage.R) - Implementation
- [tests/test-assessment-storage.R](/C:/Users/hjalte/GitHub/COMPEAT/tests/test-assessment-storage.R) - Verification tests
