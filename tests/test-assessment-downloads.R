local({
  environment <- new.env(parent = globalenv())
  expressions <- parse("data.R")
  helper <- Filter(function(expression) {
    is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      identical(expression[[2]], as.name("download.file.unzip.maybe"))
  }, expressions)
  stopifnot(length(helper) == 1L)
  eval(helper[[1]], environment)

  path <- tempfile("assessment-download-tests-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  path_expressions <- Filter(function(expression) {
    is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      is.symbol(expression[[2]]) &&
      as.character(expression[[2]]) %in% c("inputPath", "outputPath")
  }, expressions)
  environment$assessmentDataDir <- path
  environment$assessmentPeriod <- "period"
  for (expression in path_expressions) eval(expression, environment)
  stopifnot(environment$outputPath == file.path(path, "period", "output"))
  environment$assessmentOutputDir <- file.path(path, "staged-output")
  for (expression in path_expressions) eval(expression, environment)
  stopifnot(environment$outputPath == environment$assessmentOutputDir,
            environment$inputPath == file.path(path, "period", "input"))
  url <- "https://example.invalid/input.csv?version=1"
  dest <- file.path(path, "input.csv")
  calls <- 0L
  behavior <- "error"
  environment$download.file <- function(url, destfile, mode, quiet) {
    calls <<- calls + 1L
    stopifnot(mode == "wb", quiet,
              normalizePath(dirname(destfile)) == normalizePath(path),
              destfile != dest)
    writeLines("partial", destfile)
    if (behavior == "error") stop("Fixture transfer interrupted")
    if (behavior == "status") return(1L)
    writeLines("complete", destfile)
    0L
  }
  download <- environment$download.file.unzip.maybe
  expect_error <- function(expression, pattern) {
    error <- tryCatch(expression, error = identity)
    stopifnot(inherits(error, "error"))
    if (!grepl(pattern, conditionMessage(error), fixed = TRUE)) {
      stop("Unexpected error: ", conditionMessage(error))
    }
  }

  expect_error(download(url, path = path), "Fixture transfer interrupted")
  stopifnot(!file.exists(dest), length(list.files(path)) == 0L)
  behavior <- "status"
  expect_error(download(url, path = path), "Download failed")
  stopifnot(!file.exists(dest), length(list.files(path)) == 0L)

  behavior <- "success"
  download(url, path = path)
  stopifnot(identical(readLines(dest), "complete"), calls == 3L,
            identical(list.files(path), "input.csv"))
  download(url, path = path)
  stopifnot(calls == 3L)

  behavior <- "error"
  expect_error(download(url, refetch = TRUE, path = path),
               "Fixture transfer interrupted")
  stopifnot(identical(readLines(dest), "complete"),
            identical(list.files(path), "input.csv"))
  behavior <- "success"
  download(url, refetch = TRUE, path = path)
  stopifnot(calls == 5L, identical(readLines(dest), "complete"))

  archive <- file.path(path, "units.zip")
  file.create(archive)
  extracted <- file.path(path, "units.csv")
  extractions <- 0L
  environment$unzip <- function(zipfile, exdir) {
    stopifnot(zipfile == archive, exdir == path)
    extractions <<- extractions + 1L
    writeLines("units", extracted)
    extracted
  }
  download("https://example.invalid/units.zip", path = path)
  stopifnot(calls == 5L, extractions == 1L, file.exists(extracted))
  unlink(extracted)
  download("https://example.invalid/units.zip", path = path)
  stopifnot(calls == 5L, extractions == 2L, file.exists(extracted))

  # Exercise the production download invocation, not only the helper's refetch argument.
  download_flag <- Filter(function(expression) {
    is.call(expression) && identical(expression[[1]], as.name("if")) &&
      identical(expression[[2]], quote(!exists("assessmentForceDownload", inherits = FALSE)))
  }, expressions)
  download_inputs <- Filter(function(expression) {
    is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      identical(expression[[2]], as.name("files"))
  }, expressions)
  stopifnot(length(download_flag) == 1L, length(download_inputs) == 1L)
  eval(download_flag[[1]], environment)
  stopifnot(identical(environment$assessmentForceDownload, FALSE))
  environment$urls <- c(url, "https://example.invalid/units.zip")
  environment$inputPath <- path
  eval(download_inputs[[1]], environment)
  stopifnot(calls == 5L, extractions == 3L)

  environment$assessmentForceDownload <- TRUE
  eval(download_flag[[1]], environment)
  stopifnot(identical(environment$assessmentForceDownload, TRUE))
  writeLines("old input", dest)
  eval(download_inputs[[1]], environment)
  stopifnot(calls == 7L, extractions == 4L,
            identical(readLines(dest), "complete"),
            identical(readLines(archive), "complete"),
            identical(readLines(extracted), "units"))

  behavior <- "error"
  expect_error(eval(download_inputs[[1]], environment), "Fixture transfer interrupted")
  stopifnot(calls == 8L, extractions == 4L,
            identical(readLines(dest), "complete"),
            !any(grepl("^assessment-download-", list.files(path))))

  cat("Assessment download tests passed.\n")
})
