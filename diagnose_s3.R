#!/usr/bin/env Rscript
#
# COMPEAT S3 Connectivity Diagnostic Script
# Purpose: Test S3 configuration and connectivity
# Usage: Rscript diagnose_s3.R
#

cat("COMPEAT S3 Diagnostic\n")
cat("====================\n\n")

# Check environment variables
cat("1. Checking Environment Variables\n")
cat("-" , rep("", 40), "\n", sep = "")

env_vars <- c(
  "COMPEAT_STORAGE", "COMPEAT_S3_BUCKET", "COMPEAT_S3_PREFIX",
  "AWS_S3_ENDPOINT", "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY",
  "AWS_SESSION_TOKEN", "AWS_DEFAULT_REGION", "COMPEAT_DATA_DIR"
)

for (var in env_vars) {
  value <- Sys.getenv(var, unset = "[NOT SET]")
  # Redact secrets
  if (var %in% c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN")) {
    if (value != "[NOT SET]") {
      value <- paste0("[", nchar(value), " chars]")
    }
  }
  cat(sprintf("  %-30s : %s\n", var, value))
}

cat("\n2. Testing Storage Configuration\n")
cat("-" , rep("", 40), "\n", sep = "")

# Source the storage module
tryCatch({
  source("app_storage.R")
  storage <- assessment_storage_config(".")
  
  cat(sprintf("  Storage Mode              : %s\n", storage$mode))
  cat(sprintf("  Data Directory            : %s\n", storage$data_dir))
  
  if (storage$mode == "s3") {
    cat(sprintf("  S3 Bucket                 : %s\n", storage$bucket))
    cat(sprintf("  S3 Prefix                 : %s\n", storage$prefix))
    cat(sprintf("  S3 Base URL               : %s\n", storage$base_url))
    cat(sprintf("  Use HTTPS                 : %s\n", storage$use_https))
    cat(sprintf("  URL Style                 : %s\n", storage$url_style))
    cat(sprintf("  Region                    : %s\n", if (nzchar(storage$region)) storage$region else "[empty]"))
    
    cat("\n3. Testing S3 Connectivity\n")
    cat("-" , rep("", 40), "\n", sep = "")
    
    # Try to create a client and test basic connectivity
    tryCatch({
      client <- assessment_s3_client(storage)
      
      # Create a small test file
      test_file <- tempfile(pattern = "compeat-test-", fileext = ".txt")
      writeLines(paste("COMPEAT S3 Test", Sys.time()), test_file)
      test_key <- paste(storage$prefix, "diagnostics", "test.txt", sep = "/")
      
      cat(sprintf("  Testing upload to: %s\n", test_key))
      
      tryCatch({
        result <- client$put(test_key, test_file)
        cat("  ✓ S3 Upload Test PASSED\n")
        
        # Try to download it back
        download_file <- tempfile(pattern = "compeat-download-", fileext = ".txt")
        tryCatch({
          result <- client$get(test_key, download_file)
          content <- readLines(download_file)
          cat("  ✓ S3 Download Test PASSED\n")
          cat(sprintf("    Retrieved: %s\n", content[1]))
          unlink(download_file)
        }, error = function(e) {
          cat("  ✗ S3 Download Test FAILED\n")
          cat(sprintf("    Error: %s\n", conditionMessage(e)))
          unlink(download_file)
        })
        
      }, error = function(e) {
        cat("  ✗ S3 Upload Test FAILED\n")
        cat(sprintf("    Error: %s\n", conditionMessage(e)))
      })
      
      unlink(test_file)
      
    }, error = function(e) {
      cat("  ✗ Could not create S3 client\n")
      cat(sprintf("    Error: %s\n", conditionMessage(e)))
    })
    
  } else {
    cat("  Storage Mode is LOCAL - S3 tests skipped\n")
  }
  
}, error = function(e) {
  cat("✗ Storage Configuration Failed\n")
  cat(sprintf("  Error: %s\n", conditionMessage(e)))
})

cat("\n4. Checking Assessment Output Files\n")
cat("-" , rep("", 40), "\n", sep = "")

assessments <- c(
  "COMP 4 (2015-2020)",
  "COMP 5 (1990-2000)",
  "COMP 5 (2001-2006)",
  "COMP 5 (2006-2014)",
  "COMP 5 (2015-2020)",
  "COMP 5 (2021-2026)"
)

for (assessment in assessments) {
  output_dir <- file.path(storage$data_dir, assessment, "output")
  
  if (dir.exists(output_dir)) {
    marker_file <- file.path(output_dir, "assessment.complete")
    persist_file <- file.path(output_dir, "assessment.persisted.json")
    
    status <- if (file.exists(marker_file)) "✓ Complete" else "✗ Incomplete"
    s3_status <- if (file.exists(persist_file)) "✓ S3 Published" else "✗ S3 Not Published"
    
    cat(sprintf("  %-28s : %s | %s\n", assessment, status, s3_status))
  } else {
    cat(sprintf("  %-28s : [directory not found]\n", assessment))
  }
}

cat("\n5. Troubleshooting Recommendations\n")
cat("-" , rep("", 40), "\n", sep = "")

if (storage$mode == "local") {
  cat("  ℹ Storage is in LOCAL mode\n")
  cat("  → S3 upload is disabled\n")
  cat("  → To enable S3: set COMPEAT_STORAGE=s3 (or auto) and AWS credentials\n")
} else {
  cat("  ℹ Storage is in S3 mode\n")
  
  # Check for specific issues
  if (!nzchar(Sys.getenv("AWS_ACCESS_KEY_ID"))) {
    cat("  ✗ AWS_ACCESS_KEY_ID is not set\n")
  }
  if (!nzchar(Sys.getenv("AWS_SECRET_ACCESS_KEY"))) {
    cat("  ✗ AWS_SECRET_ACCESS_KEY is not set\n")
  }
  if (is.na(Sys.getenv("AWS_SESSION_TOKEN", unset = NA))) {
    cat("  ℹ AWS_SESSION_TOKEN not set (OK for permanent credentials)\n")
  }
  
  cat("\n  Next steps to diagnose:\n")
  cat("  1. Verify AWS credentials are valid and not expired\n")
  cat("  2. Check S3 bucket exists and is accessible\n")
  cat("  3. Verify IAM policy includes s3:PutObject and s3:HeadObject\n")
  cat("  4. Test network connectivity to AWS_S3_ENDPOINT\n")
  cat("  5. Check container logs for detailed error messages\n")
}

cat("\nDiagnostic Complete\n")
