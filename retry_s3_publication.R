#!/usr/bin/env Rscript
#
# COMPEAT S3 Manual Recovery Script
# Purpose: Manually retry publishing locally-completed assessments to S3
# Usage: Rscript retry_s3_publication.R [assessment_name]
# Example: Rscript retry_s3_publication.R "COMP 5 (2021-2026)"
#

args <- commandArgs(trailingOnly = TRUE)

cat("COMPEAT S3 Publication Retry\n")
cat("============================\n\n")

# Get assessment to retry
if (length(args) > 0) {
  assessment <- args[1]
} else {
  cat("Usage: Rscript retry_s3_publication.R <assessment_name>\n\n")
  cat("Available assessments:\n")
  cat("  - COMP 4 (2015-2020)\n")
  cat("  - COMP 5 (1990-2000)\n")
  cat("  - COMP 5 (2001-2006)\n")
  cat("  - COMP 5 (2006-2014)\n")
  cat("  - COMP 5 (2015-2020)\n")
  cat("  - COMP 5 (2021-2026)\n\n")
  cat("Example:\n")
  cat("  Rscript retry_s3_publication.R \"COMP 5 (2021-2026)\"\n")
  quit(status = 1)
}

cat("Target Assessment: ", assessment, "\n\n")

# Load storage module
tryCatch({
  source("app_storage.R")
  storage <- assessment_storage_config(".")
}, error = function(e) {
  cat("ERROR: Could not load storage configuration\n")
  cat("  ", conditionMessage(e), "\n")
  quit(status = 1)
})

# Check storage mode
if (storage$mode == "local") {
  cat("WARNING: Storage mode is LOCAL - S3 publishing is disabled\n")
  cat("Set COMPEAT_STORAGE=s3 to enable S3 publishing\n")
  quit(status = 1)
}

cat("Storage Mode: ", storage$mode, "\n")
cat("S3 Bucket: ", storage$bucket, "\n")
cat("S3 Prefix: ", storage$prefix, "\n\n")

# Check if assessment output exists
output_dir <- file.path(storage$data_dir, assessment, "output")
cat("Output Directory: ", output_dir, "\n")

if (!dir.exists(output_dir)) {
  cat("ERROR: Output directory does not exist\n")
  quit(status = 1)
}

# Check if all required files exist
cat("\nChecking required files:\n")
required_files <- c(
  "Assessment.csv.gz", "Assessment_Indicator.csv.gz", "Annual_Indicator.csv.gz",
  "Units.shp", "Units.shx", "Units.dbf", "Units.prj",
  "StationsBOT.csv.gz", "StationsCTD.csv.gz", "StationsPMP.csv.gz"
)

all_present <- TRUE
for (file in required_files) {
  file_path <- file.path(output_dir, file)
  if (file.exists(file_path)) {
    file_size <- file.info(file_path)$size
    cat(sprintf("  ✓ %-30s (%s)\n", file, format(file_size, units = "auto")))
  } else {
    cat(sprintf("  ✗ %-30s (MISSING)\n", file))
    all_present <- FALSE
  }
}

if (!all_present) {
  cat("\nERROR: Not all required files are present\n")
  quit(status = 1)
}

# Create manifest
cat("\nCreating publication manifest...\n")
tryCatch({
  manifest <- assessment_make_manifest(assessment, output_dir)
  cat(sprintf("  Run ID: %s\n", manifest$run_id))
  cat(sprintf("  Files: %d\n", length(manifest$files$name)))
}, error = function(e) {
  cat("ERROR: Could not create manifest\n")
  cat("  ", conditionMessage(e), "\n")
  quit(status = 1)
})

# Create S3 client
cat("\nInitializing S3 client...\n")
tryCatch({
  client <- assessment_s3_client(storage)
  cat("  ✓ Client created\n")
}, error = function(e) {
  cat("ERROR: Could not create S3 client\n")
  cat("  ", conditionMessage(e), "\n")
  quit(status = 1)
})

# Attempt publication
cat("\nPublishing to S3...\n")
tryCatch({
  assessment_publish(storage, assessment, output_dir, manifest, client)
  cat("\n✓ Publication SUCCEEDED\n")
  cat("\nResults are now available on S3:\n")
  cat(sprintf("  Bucket: s3://%s\n", storage$bucket))
  cat(sprintf("  Prefix: %s/%s/runs/%s/\n", storage$prefix, assessment, manifest$run_id))
}, error = function(e) {
  cat("\n✗ Publication FAILED\n")
  cat("Error: ", conditionMessage(e), "\n")
  cat("\nTroubleshooting steps:\n")
  cat("  1. Check AWS credentials are still valid\n")
  cat("  2. Verify S3 bucket exists and is accessible\n")
  cat("  3. Check IAM policy has s3:PutObject and s3:HeadObject permissions\n")
  cat("  4. Test network connectivity to the S3 endpoint\n")
  cat("  5. Review container logs for detailed error information\n")
  quit(status = 1)
})
