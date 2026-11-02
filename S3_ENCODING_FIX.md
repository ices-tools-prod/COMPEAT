# S3 SignatureDoesNotMatch Fix

## Problem

When running assessments with names containing spaces or parentheses (e.g., "COMP 5 (2021-2026)"), 
the application would fail during S3 download with:

```
Assessment preparation failed: S3 download failed (HTTP 403, SignatureDoesNotMatch): 
compeat/COMP 5 (2021-2026)/current.json
```

## Root Cause

The `app_storage.R` file was using `utils::URLencode(key, reserved = TRUE)` when preparing 
HTTP paths for S3 requests. The `reserved = TRUE` parameter treats forward slashes (`/`) as 
reserved characters and encodes them as `%2F`.

This caused paths like:
- Input: `compeat/COMP 5 (2021-2026)/current.json`
- Output: `/compeat%2FCOMP%205%20%282021-2026%29%2Fcurrent.json` ❌

The AWS S3 service then calculated the signature based on the correct path, but the request
contained an invalid path with encoded slashes, causing a `SignatureDoesNotMatch` error.

## Solution

Changed `utils::URLencode(key, reserved = FALSE)` in two locations:
1. Line 107: In the `get()` function for downloading files
2. Line 173: In the `put()` function for verification after upload

With `reserved = FALSE`, forward slashes are preserved while spaces and special characters 
are properly encoded:
- Input: `compeat/COMP 5 (2021-2026)/current.json`  
- Output: `/compeat/COMP%205%20(2021-2026)/current.json` ✓

## Testing

All existing tests in `tests/test-assessment-storage.R` pass with the fix, including:
- Storage configuration validation
- Assessment lifecycle operations
- Shared restoration and publication

The fix handles assessment names with:
- Spaces: `COMP 5 (2021-2026)`
- Parentheses: `(2021-2026)`
- Multiple path segments: `runs/123/output/file.csv`

## Impact

This fix allows assessments with spaces and special characters in their names to be 
downloaded from S3 without signature errors.
