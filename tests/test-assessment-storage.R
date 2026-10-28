source("app_storage.R")

expect_error <- function(expression, pattern) {
  error <- tryCatch(expression, error = identity)
  stopifnot(inherits(error, "error"))
  if (!grepl(pattern, conditionMessage(error), fixed = TRUE)) {
    stop("Unexpected error: ", conditionMessage(error))
  }
}

local({
  root <- tempfile("storage-config-")
  values <- character()
  getenv <- function(name, default = "") {
    if (name %in% names(values)) values[[name]] else default
  }
  config <- function() assessment_storage_config(root, getenv)
  stopifnot(config()$mode == "local", config()$data_dir == file.path(root, "data"))
  values <- c(COMPEAT_STORAGE = "invalid")
  expect_error(config(), "COMPEAT_STORAGE must")
  values <- c(AWS_S3_ENDPOINT = "https://s3.example.invalid")
  expect_error(config(), "COMPEAT_S3_BUCKET")
  values <- c(values, COMPEAT_S3_BUCKET = "project-bucket")
  expect_error(config(), "AWS_ACCESS_KEY_ID")
  values <- c(values, AWS_ACCESS_KEY_ID = "fixture-key", AWS_SECRET_ACCESS_KEY = "fixture-secret",
              AWS_DEFAULT_REGION = "injected-region", COMPEAT_DATA_DIR = "cache")
  storage <- config()
  stopifnot(storage$mode == "s3", storage$base_url == "s3.example.invalid",
            storage$region == "", storage$use_https, storage$prefix == "compeat",
            storage$data_dir == file.path(root, "cache"))
  values["AWS_S3_ENDPOINT"] <- "http://127.0.0.1:9000/"
  stopifnot(config()$base_url == "127.0.0.1:9000", !config()$use_https)
  values["AWS_S3_ENDPOINT"] <- "s3.example.invalid"
  stopifnot(config()$use_https)
  values["AWS_S3_ENDPOINT"] <- "https://s3.example.invalid/path"
  expect_error(config(), "without a path")
  values["AWS_S3_ENDPOINT"] <- "https://user:password@s3.example.invalid"
  expect_error(config(), "without a path or credentials")
  values["COMPEAT_STORAGE"] <- "local"
  stopifnot(config()$mode == "local")
  values["COMPEAT_STORAGE"] <- "s3"
  values["AWS_S3_ENDPOINT"] <- ""
  expect_error(config(), "requires AWS_S3_ENDPOINT")
  values["AWS_S3_ENDPOINT"] <- "s3.example.invalid"
  values["COMPEAT_S3_PREFIX"] <- "../outside"
  expect_error(config(), "COMPEAT_S3_PREFIX")
  values["COMPEAT_S3_PREFIX"] <- "project//compeat"
  expect_error(config(), "COMPEAT_S3_PREFIX")
  values["COMPEAT_S3_PREFIX"] <- "/project/compeat/"
  stopifnot(config()$prefix == "project/compeat")
  values["COMPEAT_S3_PREFIX"] <- ""
  stopifnot(assessment_s3_key(config(), "period", "current.json") == "period/current.json")
  cat("Storage configuration tests passed.\n")
})

local({
  root <- tempfile("assessment-storage-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  storage <- list(mode = "s3", data_dir = file.path(root, "cache"), bucket = "fixture-bucket",
                  prefix = "project/compeat", base_url = "s3.example.invalid",
                  use_https = TRUE, url_style = "path", region = "")
  assessment <- "COMP 5 (2021-2026)"
  directory <- file.path(storage$data_dir, assessment, "output")
  dir.create(directory, recursive = TRUE)
  objects <- new.env(parent = emptyenv())
  uploads <- character()
  downloads <- character()
  failure <- NULL
  client <- list(
    get = function(key, file, missing_ok = FALSE) {
      downloads <<- c(downloads, key)
      if (identical(failure, "download")) stop("Fixture access denied")
      if (!exists(key, objects, inherits = FALSE)) {
        if (missing_ok) return(FALSE)
        stop("Fixture object missing: ", key)
      }
      writeBin(objects[[key]], file)
      TRUE
    },
    put = function(key, file) {
      uploads <<- c(uploads, key)
      if (identical(failure, "upload")) return(FALSE)
      objects[[key]] <- readBin(file, "raw", n = file.info(file)$size)
      TRUE
    }
  )
  key <- function(...) assessment_s3_key(storage, assessment, ...)
  calculation_count <- 0L
  calculate <- function() {
    calculation_count <<- calculation_count + 1L
    for (name in c(assessment_required_files, "StationSamplesBOT.csv.gz", "Units.cpg")) {
      writeLines(paste("Fixture", name, calculation_count), file.path(directory, name))
    }
    writeLines("not an artifact", file.path(directory, "assessment-stdout.log"))
  }
  run <- function(...) assessment_run_job(root, assessment, storage, client = client,
                                          calculate = calculate, ...)
  marker <- file.path(directory, "assessment.complete")
  pending <- file.path(directory, "assessment.generated.json")
  stopifnot(!assessment_cache_complete(storage, assessment))
  stopifnot(isTRUE(run()), calculation_count == 1L,
            assessment_cache_complete(storage, assessment), !file.exists(pending))
  stopifnot(tail(uploads, 1) == key("current.json"),
            !any(grepl("assessment-stdout.log", uploads, fixed = TRUE)))
  pointer <- jsonlite::fromJSON(rawToChar(objects[[key("current.json")]]))
  manifest <- jsonlite::fromJSON(rawToChar(objects[[
    key("runs", pointer$run_id, "manifest.json")
  ]]))
  stopifnot(all(c(assessment_required_files, "StationSamplesBOT.csv.gz", "Units.cpg") %in%
                  manifest$files$name))
  assessment_check_files(manifest, directory)
  alternate <- storage
  alternate$bucket <- "another-bucket"
  stopifnot(!assessment_cache_complete(alternate, assessment))

  # A fresh container restores all result artifacts without recalculation.
  unlink(c(file.path(directory, manifest$files$name), marker,
           file.path(directory, "assessment.persisted.json")))
  upload_count <- length(uploads)
  stopifnot(isTRUE(run()), calculation_count == 1L, length(uploads) == upload_count,
            assessment_cache_complete(storage, assessment))
  assessment_check_files(manifest, directory)
  stopifnot(!any(grepl("^assessment-restore-", list.files(dirname(directory)))))

  # Missing required objects or corrupt bytes must not expose a complete cache.
  artifact_key <- key("runs", manifest$run_id, "output", manifest$files$name[1])
  original <- objects[[artifact_key]]
  objects[[artifact_key]] <- charToRaw("truncated")
  expect_error(run(), "do not match the manifest")
  stopifnot(!file.exists(marker), calculation_count == 1L)
  objects[[artifact_key]] <- original
  rm(list = artifact_key, envir = objects)
  expect_error(run(), "Fixture object missing")
  stopifnot(!file.exists(marker))
  objects[[artifact_key]] <- original
  failure <- "download"
  expect_error(run(), "Fixture access denied")
  stopifnot(!file.exists(marker), calculation_count == 1L)
  failure <- NULL
  stopifnot(isTRUE(run()))

  # A failed replacement leaves current.json unchanged and can retry without calculating.
  original_pointer <- objects[[key("current.json")]]
  calculate()
  replacement <- assessment_make_manifest(assessment, directory)
  assessment_write_json(replacement, pending)
  failure <- "upload"
  expect_error(run(), "S3 persistence failed")
  stopifnot(!file.exists(marker), file.exists(pending), calculation_count == 2L,
            identical(objects[[key("current.json")]], original_pointer))
  failure <- NULL
  stopifnot(isTRUE(run()), calculation_count == 2L, !file.exists(pending),
            assessment_cache_complete(storage, assessment),
            !identical(objects[[key("current.json")]], original_pointer))

  # A failure publishing current.json also retains the old published run.
  original_pointer <- objects[[key("current.json")]]
  calculate()
  replacement <- assessment_make_manifest(assessment, directory)
  assessment_write_json(replacement, pending)
  pointer_failure <- client
  pointer_failure$put <- function(key, file) {
    if (grepl("/current.json$", key)) stop("Fixture pointer publication failed")
    client$put(key, file)
  }
  expect_error(
    assessment_run_job(root, assessment, storage, client = pointer_failure, calculate = calculate),
    "S3 persistence failed"
  )
  stopifnot(identical(objects[[key("current.json")]], original_pointer), !file.exists(marker))
  stopifnot(isTRUE(run()), calculation_count == 3L)

  # A generated manifest with changed files cannot be retried or published.
  replacement <- assessment_make_manifest(assessment, directory)
  assessment_write_json(replacement, pending)
  writeLines("changed", file.path(directory, replacement$files$name[1]))
  expect_error(run(), "do not match the manifest")
  stopifnot(!file.exists(marker))
  unlink(pending)
  stopifnot(isTRUE(run()), calculation_count == 3L)

  bad <- manifest
  bad$files$name[1] <- "../outside"
  expect_error(assessment_validate_manifest(bad, assessment), "Invalid assessment manifest")
  bad <- manifest
  bad$files <- bad$files[-1, ]
  expect_error(assessment_validate_manifest(bad, assessment), "Invalid assessment manifest")
  bad <- manifest
  bad$files$name[1] <- "assessment.persisted.json"
  expect_error(assessment_validate_manifest(bad, assessment), "Invalid assessment manifest")

  # Existing local completed outputs can be published without a new calculation.
  rm(list = ls(objects), envir = objects)
  unlink(file.path(directory, "assessment.persisted.json"))
  stopifnot(!assessment_cache_complete(storage, assessment),
            isTRUE(run(reuse_local = TRUE)), calculation_count == 3L)

  # Calculation failure cannot publish or create completion metadata.
  rm(list = ls(objects), envir = objects)
  expect_error(assessment_run_job(root, assessment, storage, client = client,
                                  calculate = function() stop("Fixture calculation failed")),
               "Fixture calculation failed")
  stopifnot(!file.exists(marker), !file.exists(pending),
            !exists(key("current.json"), objects, inherits = FALSE))
  cat("Assessment storage lifecycle tests passed.\n")
})

local({
  root <- tempfile("s3-http-tests-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  credential_names <- c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN")
  previous <- Sys.getenv(credential_names, unset = NA_character_)
  on.exit({
    Sys.unsetenv(names(previous)[is.na(previous)])
    if (any(!is.na(previous))) do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
  }, add = TRUE)
  Sys.setenv(AWS_ACCESS_KEY_ID = "fixture-key", AWS_SECRET_ACCESS_KEY = "fixture-secret",
             AWS_SESSION_TOKEN = "fixture-token")
  port <- httpuv::randomPort()
  service <- callr::r_bg(function(root, port) {
    httpuv::runServer("127.0.0.1", port, list(call = function(request) {
      path <- utils::URLdecode(request$PATH_INFO)
      response <- function(status, body, type = "application/xml") {
        list(status = status, headers = list("Content-Type" = type, ETag = '"fixture"'),
             body = body)
      }
      error <- function(status, code) response(status, paste0("<Error><Code>", code, "</Code></Error>"))
      if (path == "/health") return(response(200L, "ready"))
      if (!identical(request$HTTP_X_AMZ_SECURITY_TOKEN, "fixture-token") ||
          !grepl("Credential=fixture-key/", request$HTTP_AUTHORIZATION, fixed = TRUE)) {
        return(error(403L, "InvalidToken"))
      }
      if (grepl("^/missing-bucket/", path)) return(error(404L, "NoSuchBucket"))
      if (grepl("/denied$", path)) return(error(403L, "AccessDenied"))
      if (grepl("/current.json$", path)) {
        while (file.exists(file.path(root, "hold-download"))) Sys.sleep(0.05)
      }
      file <- file.path(root, sub("^/", "", path))
      query <- shiny::parseQueryString(request$QUERY_STRING)
      if (request$REQUEST_METHOD == "POST" && "uploads" %in% names(query)) {
        return(response(200L, "<InitiateMultipartUploadResult><Bucket>fixture-bucket</Bucket><Key>multipart.bin</Key><UploadId>fixture-upload</UploadId></InitiateMultipartUploadResult>"))
      }
      if (request$REQUEST_METHOD == "POST" && "uploadId" %in% names(query)) {
        if (grepl("/bad-multipart.bin$", path)) return(error(200L, "InternalError"))
        parts <- list.files(paste0(file, "-parts"), full.names = TRUE)
        connection <- base::file(file, "wb")
        for (part in parts) {
          writeBin(readBin(part, "raw", n = file.info(part)$size), connection)
        }
        close(connection)
        return(response(200L, "<CompleteMultipartUploadResult><ETag>fixture</ETag></CompleteMultipartUploadResult>"))
      }
      if (request$REQUEST_METHOD == "PUT") {
        if ("partNumber" %in% names(query)) {
          file <- file.path(paste0(file, "-parts"), sprintf("%05d", as.integer(query$partNumber)))
        }
        dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
        writeBin(request$rook.input$read(), file)
        return(response(200L, ""))
      }
      if (!file.exists(file)) return(error(404L, "NoSuchKey"))
      if (request$REQUEST_METHOD == "HEAD") {
        return(list(status = 200L, headers = list(
          "Content-Length" = as.character(file.info(file)$size), "Content-Encoding" = "identity"),
                    body = ""))
      }
      response(200L, readBin(file, "raw", n = file.info(file)$size), "application/octet-stream")
    }))
  }, args = list(root = root, port = port))
  on.exit(if (service$is_alive()) service$kill(), add = TRUE, after = FALSE)
  ready <- FALSE
  for (attempt in seq_len(100)) {
    response <- tryCatch(curl::curl_fetch_memory(paste0("http://127.0.0.1:", port, "/health")),
                         error = identity)
    if (!inherits(response, "error") && response$status_code == 200L) {
      ready <- TRUE
      break
    }
    if (!service$is_alive()) stop("S3 fixture server exited: ", service$get_result())
    Sys.sleep(0.05)
  }
  if (!ready) stop("S3 fixture server did not become responsive.")
  storage <- list(mode = "s3", data_dir = file.path(root, "cache"), bucket = "fixture-bucket",
                  prefix = "compeat", base_url = paste0("127.0.0.1:", port),
                  use_https = FALSE, url_style = "path", region = "")
  client <- assessment_s3_client(storage)
  file <- file.path(root, "transfer")
  stopifnot(identical(client$get("missing", file, missing_ok = TRUE), FALSE))
  expect_error(client$get("missing", file), "HTTP 404")
  expect_error(client$get("denied", file, missing_ok = TRUE), "HTTP 403")
  error_output <- capture.output(expect_error(client$put("denied", file), "Forbidden"))
  stopifnot(!any(grepl("fixture-key|fixture-token|request_signature", error_output)))
  missing_bucket <- storage
  missing_bucket$bucket <- "missing-bucket"
  expect_error(assessment_s3_client(missing_bucket)$get("missing", file, missing_ok = TRUE),
               "HTTP 404")
  writeLines("fixture transfer", file)
  stopifnot(isTRUE(client$put("directory with spaces/object.txt", file)))
  writeLines("replace me", file)
  stopifnot(isTRUE(client$get("directory with spaces/object.txt", file)),
            identical(readLines(file), "fixture transfer"))
  writeBin(as.raw(rep(1L, 10 * 1024^2 + 1L)), file)
  checksum <- unname(tools::md5sum(file))
  stopifnot(isTRUE(client$put("multipart.bin", file)))
  stopifnot(isTRUE(client$get("multipart.bin", file)),
            identical(unname(tools::md5sum(file)), checksum))
  expect_error(client$put("bad-multipart.bin", file), "S3 upload verification failed")

  assessment <- "COMP 5 (2021-2026)"
  directory <- file.path(storage$data_dir, assessment, "output")
  dir.create(directory, recursive = TRUE)
  stopifnot(isTRUE(assessment_run_job(root, assessment, storage, client = client,
    calculate = function() {
      for (name in assessment_required_files) writeLines(name, file.path(directory, name))
    })))
  stopifnot(assessment_cache_complete(storage, assessment))

  # Production Shiny runner: sessions share the same background S3 restoration.
  app <- new.env(parent = globalenv())
  app$app_root <- file.path(root, "app")
  dir.create(app$app_root)
  file.copy("app_storage.R", file.path(app$app_root, "app_storage.R"))
  sys.source("app_storage.R", envir = app)
  app$assessment_storage <- storage
  app$assessment_storage$data_dir <- file.path(root, "fresh-cache")
  app$assessment_periods <- c(assessment, "background")
  app$global_active_jobs <- list()
  app$glossary <- NULL
  app$station_configuration <- NULL
  app$assessment_data_path <- function(assessment, file_name) {
    file.path(app$assessment_storage$data_dir, assessment, "output", file_name)
  }
  for (name in c("moduleAssessmentServer", "moduleAssessmentIndicatorsServer",
                 "moduleAnnualIndicatorsServer", "moduleStationsServer")) {
    app[[name]] <- function(...) invisible(NULL)
  }
  expressions <- parse("app.R")
  server <- Filter(function(expression) {
    is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      identical(expression[[2]], as.name("server"))
  }, expressions)
  eval(server[[1]], app)
  hold <- file.path(root, "hold-download")
  file.create(hold)
  on.exit({
    unlink(hold)
    for (job in app$global_active_jobs) if (job$is_alive()) job$kill()
  }, add = TRUE, after = FALSE)
  job_for <- function(assessment) {
    app$global_active_jobs[[file.path(app$assessment_storage$data_dir, assessment, "output")]]
  }
  shiny::testServer(app$server, {
    session$flushReact()
    run_assessment(assessment)
    stopifnot(shared_state$assessment_running, is.null(shared_state$assessment))
  })
  job <- job_for(assessment)
  stopifnot(job$is_alive())
  shiny::testServer(app$server, {
    session$flushReact()
    run_assessment(assessment)
    stopifnot(identical(job_for(assessment), job),
              length(app$global_active_jobs) == 1L,
              shared_state$assessment_running, is.null(shared_state$assessment))
    unlink(hold)
    job$wait(timeout = 60000)
    stopifnot(!job$is_alive(), isTRUE(job$get_result()))
    session$elapse(5000)
    session$flushReact()
    stopifnot(identical(shared_state$assessment, assessment))
  })

  # Generation and S3 publication finish even when no session is waiting.
  writeLines(c(
    'while (!file.exists(file.path(app_root, "release"))) Sys.sleep(0.05)',
    'output <- file.path(assessmentDataDir, assessmentPeriod, "output")',
    'for (name in assessment_required_files) writeLines(name, file.path(output, name))'
  ), file.path(app$app_root, "data.R"))
  shiny::testServer(app$server, {
    session$flushReact()
    run_assessment("background")
    stopifnot(shared_state$assessment_running)
  })
  job <- job_for("background")
  stopifnot(job$is_alive())
  file.create(file.path(app$app_root, "release"))
  job$wait(timeout = 60000)
  stopifnot(!job$is_alive(), isTRUE(job$get_result()),
            assessment_cache_complete(app$assessment_storage, "background"),
            isTRUE(client$get("compeat/background/current.json", file)))
  cat("S3 HTTP, shared restoration and disconnected publication tests passed.\n")
})
