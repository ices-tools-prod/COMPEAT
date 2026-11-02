# S3 Upload Fix - Final Solution

## The Real Problem

Two distinct issues were causing S3 upload failures:

### Issue 1: HTTP Path Encoding

The S3 object key was being passed unencoded to `aws.s3::s3HTTP()` for verification (GET/HEAD requests):
- **S3 Object Keys** can contain spaces and special characters directly
- **HTTP Paths** must be URL-encoded for HTTP transmission
- When calling `aws.s3::s3HTTP()` with unencoded paths containing spaces, the HTTP layer would reject the request

### Issue 2: Credential Leakage in Error Messages

When `aws.s3::put_object()` failed (e.g., due to permission errors or network issues), it would:
1. Print the aws_error object to console before throwing
2. The aws_error object contains sensitive attributes like `request_signature` and `request_canonical` that include AWS credentials ("fixture-key") and request signing details
3. Tests that captured output from error handling would find this sensitive data

## Root Causes Identified

### Issue 1 Root Cause
When calling `aws.s3::s3HTTP()` for HEAD verification with unencoded paths:
```r
# WRONG: path has literal spaces
confirmation <- aws.s3::s3HTTP(verb = "HEAD", path = "/compeat/COMP 5 (2021-2026)/runs/123/...")
# HTTP layer rejects: Invalid path with spaces
```

### Issue 2 Root Cause
aws.s3::put_object internally calls parse_aws_s3_response which prints the aws_error object before raising an exception:
```
 'aws_error' chr "AccessDenied"
 - attr(*, "request_signature")= chr "AWS4-HMAC-SHA256 Credential=fixture-key/..."
 ...
```

## The Solution

### For Issue 1: URL-Encode HTTP Paths

URL-encode the path only when calling `aws.s3::s3HTTP()`, but keep the S3 object key unencoded for put_object:

**For GET/Download Operations:**
```r
get = function(key, file, missing_ok = FALSE) {
  # URL-encode the path for HTTP transmission
  encoded_path <- paste0("/", utils::URLencode(key, reserved = TRUE))
  response <- do.call(aws.s3::s3HTTP, c(list(
    verb = "GET", path = encoded_path,  # ← Encoded
    parse_response = FALSE,
    write_disk = httr::write_disk(file, overwrite = TRUE)
  ), arguments))
  # ... rest of function
}
```

**For PUT/Upload Operations:**
```r
put = function(key, file) {
  # Pass the unencoded key to put_object - it handles encoding internally
  result <- do.call(aws.s3::put_object, c(list(
    object = key,  # ← Unencoded, put_object handles it
    file = file,
    multipart = TRUE,
    partsize = 10 * 1024^2
  ), arguments))
  
  # But encode the path for HEAD verification
  encoded_path <- paste0("/", utils::URLencode(key, reserved = TRUE))
  confirmation <- do.call(aws.s3::s3HTTP, c(list(
    verb = "HEAD", path = encoded_path  # ← Encoded for HTTP
  ), arguments))
  # ... rest of function
}
```

### For Issue 2: Suppress Credential Leakage

Redirect stdout and stderr during put_object execution to suppress aws.s3's internal error object printing:

```r
put = function(key, file) {
  # Suppress output that may contain aws_error attributes with sensitive data
  temp_file <- tempfile()
  stdout_conn <- file(temp_file, open = "wt")
  stderr_conn <- file(temp_file, open = "wt")
  
  on.exit({
    suppressWarnings(tryCatch({
      sink(file = NULL, type = "output")
      sink(file = NULL, type = "message")
    }, error = function(e) NULL))
    close(stdout_conn)
    close(stderr_conn)
    unlink(temp_file, force = TRUE)
  }, add = TRUE)
  
  # Redirect output before calling put_object
  sink(file = stdout_conn, type = "output")
  sink(file = stderr_conn, type = "message")
  
  result <- tryCatch(
    do.call(aws.s3::put_object, c(list(
      object = key, file = file, multipart = TRUE, partsize = 10 * 1024^2
    ), arguments)),
    error = identity
  )
  
  # Restore output immediately
  suppressWarnings(tryCatch({
    sink(file = NULL, type = "output")
    sink(file = NULL, type = "message")
  }, error = function(e) NULL))
  
  # Now handle errors safely without aws_error object being printed
  if (inherits(result, "error")) {
    stop(conditionMessage(result))
  }
  # ... rest of function
}
```

## Why This Works

### For Issue 1
1. **`aws.s3::put_object()`** internally handles URL encoding when communicating with S3
2. **`aws.s3::s3HTTP()`** expects an already-formed HTTP path and doesn't add additional encoding
3. By encoding only for s3HTTP(), we ensure:
   - PUT operations work correctly with the object key
   - HEAD/GET verification works with proper HTTP paths
   - S3 stores objects with actual spaces in the key name

### For Issue 2
1. By redirecting stdout and stderr during put_object execution, aws.s3's internal error printing is suppressed
2. When we catch and re-throw the error with `stop(conditionMessage(error))`, we only get the cleaned error message
3. The aws_error object with its sensitive attributes is never printed to console or captured

## Testing

The fix was verified with:
- Unit tests for error handling with sensitive credentials (✓ PASSED)
- Tests for URL encoding with assessment names containing spaces and parentheses (✓ PASSED)
- Tests for HTTP path handling with special characters (✓ PASSED)
- All 4 test suites in test-assessment-storage.R (✓ PASSED)

## Changes Made

**File:** [app_storage.R](/C:/Users/hjalte/GitHub/COMPEAT/app_storage.R)

### In `assessment_s3_client()`:

1. **get() function (lines 104-121)**
   - Add URL encoding for the HTTP path before calling s3HTTP
   - Preserves the unencoded key for S3 operations

2. **put() function (lines 123-172)**
   - Redirect stdout/stderr to suppress aws.s3's error object printing
   - Pass unencoded object key to put_object (it handles encoding)
   - Add URL encoding only for HEAD verification path
   - Properly handle errors and restore output streams

### Other improvements:
- **assessment_make_manifest()** (line 185-203): Added explicit file sorting with order(basename(files))
- **assessment_publish()** (lines 211-242): Improved error reporting showing file position (e.g., "file 1 of 13")

## Impact

✅ Fixes S3 upload failures for assessment names with spaces/special characters
✅ Fixes credential leakage in error messages
✅ Maintains S3 object key compatibility
✅ Works with all assessment periods
✅ No changes to public APIs
✅ All existing tests pass
✅ All 13 required files successfully upload to S3 (not just the first one)

## How to Test

1. Run the test suite:
   ```bash
   cd /srv/shiny-server
   Rscript tests/test-assessment-storage.R
   ```

2. Select an assessment in the UI that requires S3 upload
3. Verify all 13 files appear in S3 bucket under the assessment path
4. Check that manifest.json and current.json are created

## Cleanup

The following diagnostic files can be removed (they were created for debugging):
- `test_s3_url_encoding.R` (optional)
- `diagnose_s3_upload.R` (optional)
- `test_aws_s3_direct.R` (optional)
- `test_encoding_strategy.R` (optional)
