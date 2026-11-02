#!/usr/bin/env Rscript
#
# Test S3 URL Encoding Fix
# Verifies that assessment names with special characters are properly encoded
# in S3 key paths.
#

source("app_storage.R")

cat("S3 URL Encoding Test Suite\n")
cat(strrep("=", 70), "\n\n")

# Test 1: Verify URL encoding for assessment names with spaces and parentheses
cat("Test 1: URL Encoding of Assessment Names\n")
cat("-" , rep("", 60), "\n", sep = "")

test_cases <- list(
  list(assessment = "COMP 4 (2015-2020)", 
       expected_contains = "COMP%204%20%282015-2020%29"),
  list(assessment = "COMP 5 (2021-2026)", 
       expected_contains = "COMP%205%20%282021-2026%29"),
  list(assessment = "COMP 5 (1990-2000)", 
       expected_contains = "COMP%205%20%281990-2000%29")
)

storage <- list(prefix = "compeat", bucket = "test")

all_pass <- TRUE
for (test in test_cases) {
  key <- assessment_s3_key(storage, test$assessment, "runs", "123", "output", "test.csv")
  
  if (grepl(test$expected_contains, key)) {
    cat(sprintf("  ✓ %s\n", test$assessment))
  } else {
    cat(sprintf("  ✗ %s\n", test$assessment))
    cat(sprintf("    Expected: %s\n", test$expected_contains))
    cat(sprintf("    Got: %s\n", key))
    all_pass <- FALSE
  }
}

# Test 2: Verify no literal spaces or unencoded characters
cat("\nTest 2: No Literal Special Characters in Keys\n")
cat("-" , rep("", 60), "\n", sep = "")

for (test in test_cases) {
  key <- assessment_s3_key(storage, test$assessment, "test")
  
  has_literal_spaces <- grepl(" ", key, fixed = TRUE)
  has_literal_parens <- grepl("[()]", key)
  
  if (!has_literal_spaces && !has_literal_parens) {
    cat(sprintf("  ✓ %s (no literal special chars)\n", test$assessment))
  } else {
    cat(sprintf("  ✗ %s (found literal special chars)\n", test$assessment))
    cat(sprintf("    Key: %s\n", key))
    all_pass <- FALSE
  }
}

# Test 3: Verify manifest file ordering is deterministic
cat("\nTest 3: Deterministic Manifest Ordering\n")
cat("-" , rep("", 60), "\n", sep = "")

test_dir <- tempfile("s3-encoding-test-")
dir.create(test_dir)
on.exit(unlink(test_dir, recursive = TRUE))

# Create required assessment files
required_files <- c(
  "Assessment.csv.gz", "Assessment_Indicator.csv.gz", "Annual_Indicator.csv.gz",
  "Units.shp", "Units.shx", "Units.dbf", "Units.prj",
  "StationsBOT.csv.gz", "StationsCTD.csv.gz", "StationsPMP.csv.gz"
)

for (file in required_files) {
  writeLines(paste("test content", file), file.path(test_dir, file))
}

# Generate multiple manifests
manifests <- list()
for (i in 1:3) {
  manifests[[i]] <- assessment_make_manifest("COMP 5 (2021-2026)", test_dir)
}

# Compare all manifests
files_match <- TRUE
for (i in 2:3) {
  if (!identical(manifests[[1]]$files$name, manifests[[i]]$files$name)) {
    files_match <- FALSE
    break
  }
}

if (files_match) {
  cat("  ✓ All manifests have identical file ordering\n")
  cat(sprintf("  Files are in order: %s\n", paste(manifests[[1]]$files$name[1:3], collapse = ", ")))
} else {
  cat("  ✗ Manifests have different file ordering\n")
  all_pass <- FALSE
}

# Test 4: Verify files are sorted alphabetically
cat("\nTest 4: Alphabetical File Sorting\n")
cat("-" , rep("", 60), "\n", sep = "")

sorted <- sort(manifests[[1]]$files$name)
if (identical(manifests[[1]]$files$name, sorted)) {
  cat("  ✓ Files are sorted in alphabetical order\n")
  cat(sprintf("    First: %s\n", manifests[[1]]$files$name[1]))
  cat(sprintf("    Last:  %s\n", manifests[[1]]$files$name[length(manifests[[1]]$files$name)]))
} else {
  cat("  ✗ Files are NOT in alphabetical order\n")
  all_pass <- FALSE
}

# Test 5: Verify URL encoding doesn't break path parsing
cat("\nTest 5: URL Encoding Path Integrity\n")
cat("-" , rep("", 60), "\n", sep = "")

key <- assessment_s3_key(storage, "COMP 5 (2021-2026)", "runs", "20261102T054421-1", "output", "Assessment.csv.gz")
# Split by / and check structure
parts <- unlist(strsplit(key, "/", fixed = TRUE))

expected_structure <- c("compeat", "COMP%205%20%282021-2026%29", "runs", "20261102T054421-1", "output", "Assessment.csv.gz")
if (identical(parts, expected_structure)) {
  cat("  ✓ Path structure maintained after URL encoding\n")
  cat(sprintf("    Full path: %s\n", key))
} else {
  cat("  ✗ Path structure broken by URL encoding\n")
  cat(sprintf("    Expected parts: %s\n", paste(expected_structure, collapse = " / ")))
  cat(sprintf("    Got parts: %s\n", paste(parts, collapse = " / ")))
  all_pass <- FALSE
}

# Summary
cat("\n", strrep("=", 70), "\n", sep = "")
if (all_pass) {
  cat("✓ All S3 URL encoding tests PASSED\n")
  cat("\nThe fix correctly:\n")
  cat("  1. Encodes spaces as %20 and parentheses as %28/%29\n")
  cat("  2. Prevents literal special characters in S3 paths\n")
  cat("  3. Maintains deterministic manifest file ordering\n")
  cat("  4. Sorts files alphabetically for consistency\n")
  cat("  5. Preserves path structure after encoding\n")
} else {
  cat("✗ Some S3 URL encoding tests FAILED\n")
  quit(status = 1)
}
