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

  cat("Assessment download tests passed.\n")
})
