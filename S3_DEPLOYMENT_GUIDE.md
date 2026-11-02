# S3 Upload Fix - Deployment Guide

## What Was Changed

Modified `app_storage.R` to fix S3 upload failures for assessments with special characters in their names.

### Files Changed
- `app_storage.R` - Fixed HTTP path encoding and credential leakage in S3 operations

### No Breaking Changes
- All public APIs remain the same
- All existing functionality preserved
- All tests pass

## Before Deployment

1. **Backup** the current `app_storage.R`:
   ```bash
   cp /srv/shiny-server/app_storage.R /srv/shiny-server/app_storage.R.backup
   ```

2. **Verify** the test suite passes:
   ```bash
   cd /srv/shiny-server
   Rscript tests/test-assessment-storage.R
   ```
   Expected output: All 4 test suites should pass.

## Deployment

Replace `app_storage.R` in the Shiny server directory with the fixed version.

## After Deployment

### Immediate Testing
1. Restart the Shiny application
2. Select an assessment in the UI (any assessment that requires S3 publication)
3. Verify the assessment completes without errors
4. Check that all files were uploaded to S3:
   ```bash
   aws s3 ls s3://your-bucket/compeat/COMP\ 5\ \(2021-2026\)/runs/ --recursive
   ```

### Expected Result
- ✅ Assessment status shows "published" (not "pending publication")
- ✅ All 13+ required files appear in S3 bucket
- ✅ manifest.json file exists
- ✅ current.json pointer file exists
- ✅ No errors in application logs

### Rollback (if needed)
If issues occur, restore the backup:
```bash
cp /srv/shiny-server/app_storage.R.backup /srv/shiny-server/app_storage.R
systemctl restart shiny-server
```

## Key Improvements

### Issue 1: Special Characters in Assessment Names
- Assessment names like "COMP 5 (2021-2026)" with spaces and parentheses now work correctly
- HTTP paths are properly URL-encoded for the AWS API
- S3 object keys preserve the original names with spaces and special characters

### Issue 2: Credential Security
- Error messages no longer leak AWS credentials
- aws_error objects with signing details are suppressed
- Safe for logging and user-facing error messages

## Testing Verification

The fix includes comprehensive tests for:
- URL encoding with various special characters
- Error handling without credential leakage
- Multipart upload verification
- Assessment lifecycle with S3 publication

All tests pass successfully:
```
Storage configuration tests passed.
Real environment storage configuration tests passed.
Assessment storage lifecycle tests passed.
S3 HTTP, shared restoration and disconnected publication tests passed.
```

## Support

If issues occur after deployment:
1. Check application logs for errors
2. Verify S3 bucket permissions and credentials
3. Review [S3_FIX_TECHNICAL_ANALYSIS.md](S3_FIX_TECHNICAL_ANALYSIS.md) for technical details
4. Check [S3_UPLOAD_FIX_SUMMARY.md](S3_UPLOAD_FIX_SUMMARY.md) for reference information

## Questions?

See the technical documentation in [S3_FIX_TECHNICAL_ANALYSIS.md](S3_FIX_TECHNICAL_ANALYSIS.md) for detailed explanations of the problems and solutions.
