assessment_required_files <- c(
  "Assessment.csv.gz", "Assessment_Indicator.csv.gz", "Annual_Indicator.csv.gz",
  "Units.shp", "Units.shx", "Units.dbf", "Units.prj",
  "StationsBOT.csv.gz", "StationsCTD.csv.gz", "StationsPMP.csv.gz"
)

assessment_storage_config <- function(app_root, getenv = Sys.getenv) {
  mode <- tolower(trimws(getenv("COMPEAT_STORAGE", "auto")))
  if (!mode %in% c("auto", "local", "s3")) {
    stop("COMPEAT_STORAGE must be auto, local or s3.")
  }
  endpoint <- trimws(getenv("AWS_S3_ENDPOINT", ""))
  if (mode == "auto") mode <- if (nzchar(endpoint)) "s3" else "local"
  data_dir <- path.expand(getenv("COMPEAT_DATA_DIR", file.path(app_root, "data")))
  if (!nzchar(data_dir)) stop("COMPEAT_DATA_DIR must not be empty.")
  if (!grepl("^([A-Za-z]:[/\\\\]|[/\\\\])", data_dir)) {
    data_dir <- file.path(app_root, data_dir)
  }
  config <- list(mode = mode, data_dir = data_dir)
  if (mode == "local") return(config)

  bucket <- trimws(getenv("COMPEAT_S3_BUCKET", ""))
  if (!grepl("^[a-z0-9][a-z0-9.-]{1,61}[a-z0-9]$", bucket)) {
    stop("S3 storage requires COMPEAT_S3_BUCKET to contain a valid bucket name.")
  }
  prefix <- gsub("^/+|/+$", "", trimws(getenv("COMPEAT_S3_PREFIX", "compeat")))
  if (grepl("\\\\|[[:cntrl:]]", prefix) ||
      any(strsplit(prefix, "/", fixed = TRUE)[[1]] %in% c("", ".", ".."))) {
    stop("COMPEAT_S3_PREFIX must be a relative S3 prefix without empty, . or .. segments.")
  }
  if (!nzchar(endpoint)) stop("S3 storage requires AWS_S3_ENDPOINT.")
  if (!grepl("^https?://", endpoint)) endpoint <- paste0("https://", endpoint)
  url <- httr::parse_url(endpoint)
  if (is.null(url$hostname) || !nzchar(url$hostname) ||
      !url$scheme %in% c("http", "https") ||
      (!is.null(url$path) && !url$path %in% c("", "/")) ||
      !is.null(url$query) || !is.null(url$fragment) ||
      !is.null(url$username) || !is.null(url$password)) {
    stop("AWS_S3_ENDPOINT must be an HTTP(S) hostname, optionally with a port, without a path or credentials.")
  }
  missing <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
  missing <- missing[!nzchar(vapply(missing, getenv, character(1), default = ""))]
  if (length(missing)) stop("S3 storage requires: ", paste(missing, collapse = ", "), ".")
  if (!requireNamespace("aws.s3", quietly = TRUE)) {
    stop("S3 storage requires aws.s3. Restore dependencies from renv.lock.")
  }
  config$bucket <- bucket
  config$prefix <- prefix
  config$base_url <- paste0(url$hostname, if (!is.null(url$port)) paste0(":", url$port))
  config$use_https <- identical(url$scheme, "https")
  config$url_style <- getenv("COMPEAT_S3_URL_STYLE", "path")
  if (!config$url_style %in% c("path", "virtual")) {
    stop("COMPEAT_S3_URL_STYLE must be path or virtual.")
  }
  # aws.s3 prepends nonempty regions to custom endpoint hostnames.
  config$region <- if (identical(config$base_url, "s3.amazonaws.com")) {
    getenv("AWS_DEFAULT_REGION", "us-east-1")
  } else ""
  config
}

assessment_storage_identity <- function(storage) {
  storage[c("mode", "bucket", "prefix", "base_url", "use_https", "url_style", "region")]
}

assessment_s3_key <- function(storage, assessment, ...) {
  paste(c(if (nzchar(storage$prefix)) storage$prefix, assessment, ...), collapse = "/")
}

assessment_s3_client <- function(storage) {
  arguments <- list(
    bucket = storage$bucket, base_url = storage$base_url, use_https = storage$use_https,
    region = storage$region, url_style = storage$url_style,
    verbose = FALSE, show_progress = FALSE,
    config = httr::config(timeout = 3600, connecttimeout = 30)
  )
  list(
    get = function(key, file, missing_ok = FALSE) {
      response <- do.call(aws.s3::s3HTTP, c(list(
        verb = "GET", path = paste0("/", key), parse_response = FALSE,
        write_disk = httr::write_disk(file, overwrite = TRUE)
      ), arguments))
      status <- httr::status_code(response)
      if (status == 404L && missing_ok) {
        error <- xml2::read_xml(file)
        code <- xml2::xml_text(xml2::xml_find_first(error, "//*[local-name()='Code']"))
        if (identical(code, "NoSuchKey")) return(FALSE)
      }
      if (status != 200L) stop("S3 download failed (HTTP ", status, "): ", key)
      TRUE
    },
    put = function(key, file) {
      # aws.s3 prints signed request details on errors; propagate errors without logging credentials.
      result <- NULL
      invisible(utils::capture.output(result <- do.call(aws.s3::put_object, c(list(
        object = key, file = file, multipart = TRUE, partsize = 10 * 1024^2
      ), arguments))))
      if (!isTRUE(result)) stop("S3 upload did not succeed: ", key)
      confirmation <- do.call(aws.s3::s3HTTP, c(list(
        verb = "HEAD", path = paste0("/", key)
      ), arguments))
      size <- attr(confirmation, "content-length", exact = TRUE)
      if (!isTRUE(confirmation) || length(size) != 1L ||
          !identical(as.numeric(size), unname(file.info(file)$size))) {
        stop("S3 upload verification failed: ", key)
      }
      TRUE
    }
  )
}

assessment_write_json <- function(value, file) {
  temporary <- tempfile("assessment-json-", tmpdir = dirname(file))
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(value, temporary, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  if (!file.rename(temporary, file)) stop("Could not publish metadata: ", file)
}

assessment_validate_manifest <- function(manifest, assessment) {
  scalar_text <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value)
  }
  files <- manifest$files
  if (!identical(manifest$version, 1L) || !identical(manifest$assessment, assessment) ||
      !scalar_text(manifest$run_id) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", manifest$run_id) ||
      !is.data.frame(files) || !all(c("name", "size", "md5") %in% names(files)) ||
      !is.character(files$name) || anyNA(files$name) ||
      anyDuplicated(tolower(files$name)) ||
      any(!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", files$name)) ||
      !all(assessment_required_files %in% files$name) ||
      any(grepl("^assessment-", files$name, ignore.case = TRUE)) ||
      any(tolower(files$name) %in% c("assessment.complete", "assessment.generated.json",
                                    "assessment.persisted.json")) ||
      !is.numeric(files$size) || any(!is.finite(files$size) | files$size < 0) ||
      !is.character(files$md5) || anyNA(files$md5) ||
      any(!grepl("^[a-f0-9]{32}$", files$md5))) {
    stop("Invalid assessment manifest for ", assessment, ".")
  }
  invisible(TRUE)
}

assessment_check_files <- function(manifest, directory) {
  files <- file.path(directory, manifest$files$name)
  info <- file.info(files)
  if (anyNA(info$size) || any(info$isdir) ||
      !all(info$size == manifest$files$size) ||
      !identical(unname(tools::md5sum(files)), manifest$files$md5)) {
    stop("Assessment files do not match the manifest (missing, truncated or changed).")
  }
  invisible(TRUE)
}

assessment_make_manifest <- function(assessment, directory) {
  files <- list.files(directory, full.names = TRUE)
  files <- files[!file.info(files)$isdir &
                   !grepl("^assessment[.-]", basename(files))]
  manifest <- list(
    version = 1L, assessment = assessment,
    run_id = paste0(format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-",
                    Sys.getpid(), "-", basename(tempfile("run-"))),
    files = data.frame(name = basename(files), size = unname(file.info(files)$size),
                       md5 = unname(tools::md5sum(files)), stringsAsFactors = FALSE)
  )
  assessment_validate_manifest(manifest, assessment)
  manifest
}

assessment_publish <- function(storage, assessment, directory, manifest, client) {
  assessment_validate_manifest(manifest, assessment)
  assessment_check_files(manifest, directory)
  key <- function(...) assessment_s3_key(storage, assessment, ...)
  put <- function(key, file) {
    if (!isTRUE(client$put(key, file))) stop("S3 upload did not succeed: ", key)
  }
  for (name in manifest$files$name) {
    put(key("runs", manifest$run_id, "output", name), file.path(directory, name))
  }
  metadata <- tempfile("assessment-manifest-")
  on.exit(unlink(metadata), add = TRUE)
  assessment_write_json(manifest, metadata)
  put(key("runs", manifest$run_id, "manifest.json"), metadata)
  pointer <- list(version = 1L, assessment = assessment, run_id = manifest$run_id,
                  manifest_md5 = unname(tools::md5sum(metadata)))
  assessment_write_json(pointer, metadata)
  put(key("current.json"), metadata)
  invisible(TRUE)
}

assessment_restore <- function(storage, assessment, directory, client) {
  staging <- tempfile("assessment-restore-", tmpdir = dirname(directory))
  if (!dir.create(staging)) stop("Could not create assessment staging directory.")
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)
  key <- function(...) assessment_s3_key(storage, assessment, ...)
  pointer_file <- file.path(staging, "current.json")
  found <- client$get(key("current.json"), pointer_file, missing_ok = TRUE)
  if (identical(found, FALSE)) return(FALSE)
  if (!isTRUE(found)) stop("S3 pointer download did not succeed.")
  pointer <- jsonlite::read_json(pointer_file, simplifyVector = TRUE)
  if (!identical(pointer$version, 1L) || !identical(pointer$assessment, assessment) ||
      !is.character(pointer$run_id) || length(pointer$run_id) != 1L ||
      is.na(pointer$run_id) || !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", pointer$run_id) ||
      !is.character(pointer$manifest_md5) || length(pointer$manifest_md5) != 1L ||
      is.na(pointer$manifest_md5) || !grepl("^[a-f0-9]{32}$", pointer$manifest_md5)) {
    stop("Invalid S3 assessment pointer.")
  }
  manifest_file <- file.path(staging, "manifest.json")
  get <- function(key, file) {
    if (!isTRUE(client$get(key, file))) stop("S3 download did not succeed: ", key)
  }
  get(key("runs", pointer$run_id, "manifest.json"), manifest_file)
  if (!identical(unname(tools::md5sum(manifest_file)), pointer$manifest_md5)) {
    stop("S3 manifest checksum mismatch.")
  }
  manifest <- jsonlite::read_json(manifest_file, simplifyVector = TRUE)
  assessment_validate_manifest(manifest, assessment)
  if (!identical(manifest$run_id, pointer$run_id)) stop("S3 manifest run ID mismatch.")
  for (name in manifest$files$name) {
    get(key("runs", manifest$run_id, "output", name), file.path(staging, name))
  }
  assessment_check_files(manifest, staging)
  for (name in manifest$files$name) {
    if (!file.copy(file.path(staging, name), file.path(directory, name), overwrite = TRUE)) {
      stop("Could not install restored assessment file: ", name)
    }
  }
  manifest
}

assessment_cache_complete <- function(storage, assessment) {
  directory <- file.path(storage$data_dir, assessment, "output")
  if (!all(file.exists(file.path(directory, c("assessment.complete", assessment_required_files))))) {
    return(FALSE)
  }
  if (storage$mode == "local") return(TRUE)
  receipt <- file.path(directory, "assessment.persisted.json")
  if (!file.exists(receipt)) return(FALSE)
  receipt <- jsonlite::read_json(receipt, simplifyVector = FALSE)
  identical(receipt, assessment_storage_identity(storage))
}

assessment_run_job <- function(app_root, assessment, storage, reuse_local = FALSE,
                               client = NULL, calculate = NULL) {
  directory <- file.path(storage$data_dir, assessment, "output")
  marker <- file.path(directory, "assessment.complete")
  pending <- file.path(directory, "assessment.generated.json")
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
    stop("Could not create assessment output directory.")
  }
  if (file.exists(marker) && !file.remove(marker)) {
    stop("Could not invalidate the assessment completion marker.")
  }
  if (is.null(calculate)) {
    calculate <- function() {
      library(data.table)
      library(ncdf4)
      library(R.utils)
      library(readxl)
      library(sf)
      library(tidyverse)
      source(file.path(app_root, "data.R"), local = list2env(list(
        assessmentPeriod = assessment, assessmentDataDir = storage$data_dir
      )))
    }
  }
  if (storage$mode == "s3") {
    if (is.null(client)) client <- assessment_s3_client(storage)
    if (file.exists(pending)) {
      manifest <- jsonlite::read_json(pending, simplifyVector = TRUE)
      assessment_validate_manifest(manifest, assessment)
      assessment_check_files(manifest, directory)
    } else {
      manifest <- assessment_restore(storage, assessment, directory, client)
      if (identical(manifest, FALSE)) {
        if (!reuse_local) calculate()
        manifest <- assessment_make_manifest(assessment, directory)
        assessment_write_json(manifest, pending)
      }
    }
    # A pending manifest means calculation succeeded but publication still needs retrying.
    if (file.exists(pending)) {
      tryCatch(
        assessment_publish(storage, assessment, directory, manifest, client),
        error = function(error) stop(
          "Calculation completed, but S3 persistence failed. Local results retained; ",
          "select the assessment again to retry publication. ", conditionMessage(error)
        )
      )
    }
    assessment_write_json(assessment_storage_identity(storage),
                          file.path(directory, "assessment.persisted.json"))
    if (file.exists(pending) && !file.remove(pending)) {
      stop("Could not remove the pending publication manifest.")
    }
  } else {
    calculate()
  }
  if (!all(file.exists(file.path(directory, assessment_required_files)))) {
    stop("Assessment generation completed without creating all result files.")
  }
  temporary_marker <- tempfile("assessment-complete-", tmpdir = directory)
  on.exit(unlink(temporary_marker), add = TRUE)
  writeLines(as.character(Sys.time()), temporary_marker)
  if (!file.rename(temporary_marker, marker)) {
    stop("Could not publish the assessment completion marker.")
  }
  TRUE
}
