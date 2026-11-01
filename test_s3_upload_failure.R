# Test for user's S3 upload failure scenario
# Verifies that local results are served even when S3 upload fails

source("app_storage.R")

cat("Testing S3 Upload Failure Scenario\n")
cat("==================================\n\n")

root <- tempfile("s3-failure-test-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE))

storage <- list(
  mode = "s3",
  data_dir = file.path(root, "cache"),
  bucket = "fixture-bucket",
  prefix = "project/compeat",
  base_url = "s3.example.invalid",
  use_https = TRUE,
  url_style = "path",
  region = ""
)

assessment <- "TEST_ASSESSMENT"
directory <- file.path(storage$data_dir, assessment, "output")
dir.create(directory, recursive = TRUE)

# Mock S3 client that fails on upload
objects <- new.env(parent = emptyenv())
uploads <- character()
client <- list(
  get = function(key, file, missing_ok = FALSE) {
    if (!exists(key, objects, inherits = FALSE)) {
      if (missing_ok) return(FALSE)
      stop("S3 object not found: ", key)
    }
    writeBin(objects[[key]], file)
    TRUE
  },
  put = function(key, file) {
    # Simulate upload failure
    stop("Simulated S3 upload failure (network timeout)")
  }
)

key <- function(...) assessment_s3_key(storage, assessment, ...)

# Simulate a successful local calculation
calculate <- function() {
  for (name in assessment_required_files) {
    writeLines(paste("Test data for", name), file.path(directory, name))
  }
}

# Run initial assessment with S3 upload failure
cat("Step 1: Run assessment with S3 upload failure\n")
result <- tryCatch({
  assessment_run_job(root, assessment, storage, reuse_local = FALSE, client = client, calculate = calculate)
  "SUCCESS"
}, error = function(e) {
  conditionMessage(e)
})
cat("  Result:", result, "\n")

if (!grepl("S3 persistence failed", result)) {
  cat("ERROR: Expected S3 persistence failure message\n")
  quit(status = 1)
}

# Check cache state after S3 upload failure
marker <- file.path(directory, "assessment.complete")
receipt <- file.path(directory, "assessment.persisted.json")
pending <- file.path(directory, "assessment.generated.json")

cat("\nStep 2: Check file states after failure\n")
cat("  Marker exists:", file.exists(marker), "(should be TRUE)\n")
cat("  Receipt exists:", file.exists(receipt), "(should be FALSE)\n")
cat("  Pending exists:", file.exists(pending), "(should be TRUE)\n")

if (!file.exists(marker)) {
  cat("ERROR: Marker should exist after calculation succeeds\n")
  quit(status = 1)
}

if (file.exists(receipt)) {
  cat("ERROR: Receipt should not exist (S3 upload failed)\n")
  quit(status = 1)
}

if (!file.exists(pending)) {
  cat("ERROR: Pending should exist (publication retrying)\n")
  quit(status = 1)
}

# Most importantly: check that cache is considered complete
cat("\nStep 3: Check if cached results can be served\n")
cache_complete <- assessment_cache_complete(storage, assessment)
cat("  Cache complete:", cache_complete, "(should be TRUE)\n")

if (!cache_complete) {
  cat("ERROR: Cache should be complete! Local results should be served to user.\n")
  quit(status = 1)
}

cat("\n✓ All checks passed!\n")
cat("\nSummary:\n")
cat("  - Local calculation succeeded\n")
cat("  - S3 upload failed (as expected)\n")
cat("  - Marker was written (tracks local calculation)\n")
cat("  - Pending manifest exists (ready for retry)\n")
cat("  - Cache is considered complete (local results can be served)\n")
cat("  - On retry, S3 publication will be attempted again\n")
