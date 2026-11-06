# S3 assessment path encoding

## Problem and root cause

Assessment names such as `COMP 5 (2021-2026)` contain spaces and parentheses.
Pre-encoding the whole key with `utils::URLencode(key, reserved = FALSE)`
encodes spaces but leaves parentheses intact:

```text
/compeat/COMP%205%20(2021-2026)/current.json
```

`aws.s3::s3HTTP()` already encodes each path segment with `reserved = TRUE`.
However, R's `URLencode()` normally leaves a segment unchanged when it contains
an existing percent escape. Pre-encoded spaces therefore prevent the remaining
parentheses from being encoded. The signed URI does not match S3's canonical URI.

## Fix

Pass the raw key as `path = paste0("/", key)` for GET and upload-verification
HEAD requests. Continue passing raw keys to `put_object()` as well. The package
preserves separators and encodes the segments before signing and transmission:

```text
/compeat/COMP%205%20%282021-2026%29/current.json
```

This supersedes earlier guidance to pre-encode `s3HTTP()` paths, including
examples in [S3_FIX_TECHNICAL_ANALYSIS.md](S3_FIX_TECHNICAL_ANALYSIS.md).

## Validation

Run `Rscript tests/test-assessment-storage.R` from the repository root.
The loopback S3 fixture verifies GET/HEAD signatures against a canonical URI
with fully encoded segments. Tests cover missing and existing assessment
pointers, upload verification, restoration, and background publication.
No production credentials or external network access are used.

## HTTP 403 without a recognized S3 error code

This response alone does not establish a signature mismatch. It can also
indicate a credential, permission, endpoint or gateway problem. The application
retains the failure and provides safe troubleshooting hints without displaying
raw response bodies or signed requests.

After deploying the updated [app_storage.R](app_storage.R), restart the Shiny
application and retry. If 403 persists, check the running service's credentials
and S3 API endpoint, and verify access to `s3://project-compeat/compeat`.
The service needs object read/write permissions for the configured prefix and
sufficient bucket-list permission to distinguish a missing `current.json` from
denied access. Only HTTP 404 with `NoSuchKey` permits initial calculation;
HTTP 403 must not be bypassed or treated as a missing assessment.
