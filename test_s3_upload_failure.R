source("app_storage.R")

local({
  root <- tempfile("s3-failure-test-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  storage <- list(mode = "s3", data_dir = root)
  assessment <- "TEST_ASSESSMENT"
  directory <- file.path(root, assessment, "output")
  client <- list(
    get = function(key, file, missing_ok = FALSE) FALSE,
    put = function(key, file) stop("Simulated S3 upload failure")
  )
  result <- tryCatch(
    assessment_run_job(root, assessment, storage, client = client,
      calculate = function() {
        for (name in assessment_required_files) {
          writeLines(name, file.path(directory, name))
        }
      }),
    error = identity
  )
  stopifnot(inherits(result, "error"),
            grepl("S3 persistence failed", conditionMessage(result), fixed = TRUE),
            !file.exists(file.path(directory, "assessment.complete")),
            !file.exists(file.path(directory, "assessment.persisted.json")),
            !assessment_cache_complete(storage, assessment),
            assessment_pending_results_available(storage, assessment))
  writeLines("corrupt", file.path(directory, assessment_required_files[1]))
  result <- tryCatch(assessment_pending_results_available(storage, assessment),
                     error = identity)
  stopifnot(inherits(result, "error"))
  cat("S3 upload failure validation tests passed.\n")
})
