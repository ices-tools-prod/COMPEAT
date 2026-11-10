library(shiny)

local({
  app_environment <- new.env(parent = globalenv())
  app_environment$app_root <- tempfile("shared-assessments-")
  dir.create(app_environment$app_root)
  sys.source("app_storage.R", envir = app_environment)
  file.copy("app_storage.R", file.path(app_environment$app_root, "app_storage.R"))
  app_environment$assessment_storage <- list(
    mode = "local", data_dir = file.path(app_environment$app_root, "working-data")
  )
  app_environment$assessment_data_path <- function(assessment, file_name) {
    file.path(app_environment$assessment_storage$data_dir, assessment, "output", file_name)
  }
  app_environment$global_active_jobs <- list()
  app_environment$assessment_periods <- c("shared", "independent", "failed")
  app_environment$glossary <- NULL
  app_environment$station_configuration <- NULL

  # Exercise the runner without loading the production UI or assessment data.
  module_stub <- function(...) invisible(NULL)
  for (name in c(
    "moduleAssessmentServer", "moduleAssessmentIndicatorsServer",
    "moduleAnnualIndicatorsServer", "moduleStationsServer"
  )) {
    app_environment[[name]] <- module_stub
  }
  expressions <- parse("app.R")
  server_expression <- Filter(function(expression) {
    is.call(expression) && identical(expression[[1]], as.name("<-")) &&
      identical(expression[[2]], as.name("server"))
  }, expressions)
  stopifnot(length(server_expression) == 1L)
  eval(server_expression[[1]], app_environment)

  writeLines(c(
    'while (!file.exists(file.path(app_root, paste0(assessmentPeriod, ".release")))) Sys.sleep(0.05)',
    'for (i in seq_len(2048)) {',
    '  cat(strrep("o", 1024), "\\n")',
    '  cat(strrep("e", 1024), "\\n", file = stderr())',
    '}',
    'output_path <- assessmentOutputDir',
    'writeLines(as.character(assessmentForceDownload), file.path(app_root, paste0(assessmentPeriod, ".download-forced")))',
    'dir.create(output_path, recursive = TRUE, showWarnings = FALSE)',
    'file.create(file.path(output_path, c("Assessment.csv.gz", "Assessment_Indicator.csv.gz", "Annual_Indicator.csv.gz", "Units.shp", "Units.shx", "Units.dbf", "Units.prj", "StationsBOT.csv.gz", "StationsCTD.csv.gz", "StationsPMP.csv.gz")))',
    'if (file.exists(file.path(app_root, "fail"))) stop("Fixture assessment failure")'
  ), file.path(app_environment$app_root, "data.R"))

  on.exit({
    for (job in app_environment$global_active_jobs) {
      if (job$is_alive()) job$kill()
    }
    unlink(app_environment$app_root, recursive = TRUE)
  })

  job_for <- function(assessment) {
    app_environment$global_active_jobs[[
      file.path(app_environment$assessment_storage$data_dir, assessment, "output")
    ]]
  }
  release <- function(assessment) {
    file.create(file.path(app_environment$app_root, paste0(assessment, ".release")))
  }
  wait_for <- function(job) {
    job$wait(timeout = 60000)
    stopifnot(!job$is_alive())
  }

  testServer(app_environment$server, {
    session$flushReact()
    run_assessment("independent")
    stopifnot(shared_state$assessment_running, job_for("independent")$is_alive())
  })
  independent_job <- job_for("independent")
  stopifnot(independent_job$is_alive())

  testServer(app_environment$server, {
    session$flushReact()
    run_assessment("shared")
    stopifnot(shared_state$assessment_running, job_for("shared")$is_alive())
  })
  shared_job <- job_for("shared")
  stopifnot(shared_job$is_alive(), independent_job$is_alive())

  testServer(app_environment$server, {
    session$flushReact()
    dir.create(dirname(assessment_output_files("shared")[1]), recursive = TRUE,
               showWarnings = FALSE)
    file.create(assessment_output_files("shared"))
    run_assessment("shared")
    stopifnot(
      identical(job_for("shared"), shared_job),
      length(app_environment$global_active_jobs) == 2L,
      shared_state$assessment_running,
      identical(shared_state$requested_assessment, "shared"),
      is.null(shared_state$assessment)
    )
    session$flushReact()
    release("shared")
    wait_for(shared_job)
    session$elapse(5000)
    session$flushReact()
    stopifnot(
      !shared_state$assessment_running,
      identical(shared_state$assessment, "shared")
    )
    stopifnot(file.exists(completion_marker("shared")))
    logs <- file.path(dirname(completion_marker("shared")),
                      c("assessment-stdout.log", "assessment-stderr.log"))
    stopifnot(all(file.info(logs)$size > 2 * 1024^2))
  })

  release("independent")
  wait_for(independent_job)
  stopifnot(identical(independent_job$get_result(), TRUE))
  stopifnot(identical(readLines(file.path(app_environment$app_root,
                                        "independent.download-forced")), "FALSE"))

  testServer(app_environment$server, {
    session$flushReact()
    run_assessment("shared")
    stopifnot(
      !shared_state$assessment_running,
      shared_state$assessment_loading,
      identical(shared_state$assessment, "shared"),
      identical(job_for("shared"), shared_job)
    )
    stopifnot(identical(run_assessment("independent"), FALSE))
    session$setInputs(assessment_loaded = "independent")
    stopifnot(shared_state$assessment_loading)
    session$setInputs(assessment_loaded = "shared")
    stopifnot(!shared_state$assessment_loading)
    stopifnot(identical(shared_state$requested_assessment, "shared"))
  })

  # A process restart loses job history, but a completed cache remains usable.
  app_environment$global_active_jobs <- list()
  testServer(app_environment$server, {
    session$flushReact()
    run_assessment("shared")
    stopifnot(
      identical(shared_state$assessment, "shared"),
      !shared_state$assessment_running,
      length(app_environment$global_active_jobs) == 0L
    )
  })

  file.create(file.path(app_environment$app_root, "fail"))
  testServer(app_environment$server, {
    session$flushReact()
    run_assessment("shared")
    session$setInputs(assessment_loaded = "shared")
    run_assessment("failed")
    stopifnot(is.null(shared_state$assessment))
    failed_job <- job_for("failed")
    session$flushReact()
    release("failed")
    wait_for(failed_job)
    session$elapse(5000)
    session$flushReact()
    stopifnot(
      !shared_state$assessment_running,
      is.null(shared_state$assessment),
      identical(shared_state$requested_assessment, "failed"),
      all(file.exists(assessment_output_files("failed"))),
      !file.exists(completion_marker("failed"))
    )
    unlink(file.path(app_environment$app_root, "fail"))
    app_environment$global_active_jobs <- list()
    run_assessment("failed")
    retry_job <- job_for("failed")
    stopifnot(
      !identical(retry_job, failed_job),
      shared_state$assessment_running,
      is.null(shared_state$assessment)
    )
    session$flushReact()
    wait_for(retry_job)
    session$elapse(5000)
    session$flushReact()
    stopifnot(
      !shared_state$assessment_running,
      identical(shared_state$assessment, "failed")
    )
    stopifnot(file.exists(completion_marker("failed")))
    stopifnot(shared_state$assessment_loading)
    session$setInputs(assessment_loaded = "failed")
    run_assessment("")
    stopifnot(is.null(shared_state$requested_assessment), is.null(shared_state$assessment))
  })

  testServer(app_environment$server, {
    session$flushReact()
    unlink(assessment_output_files("shared")[4])
    run_assessment("shared")
    stopifnot(
      shared_state$assessment_running,
      is.null(shared_state$assessment),
      !file.exists(completion_marker("shared"))
    )
    session$flushReact()
    wait_for(job_for("shared"))
    session$elapse(5000)
    session$flushReact()
    stopifnot(assessment_complete("shared"), identical(shared_state$assessment, "shared"))
  })

  # A forced request cannot silently join a normal in-flight job.
  unlink(file.path(app_environment$app_root, "shared.release"))
  old_version <- app_environment$assessment_result_version(app_environment$assessment_storage, "shared")
  testServer(app_environment$server, {
    session$flushReact()
    run_assessment("shared")
    session$setInputs(assessment_loaded = "shared")
    stopifnot(isTRUE(run_assessment("shared", force_recalculate = TRUE)))
    stopifnot(job_for("shared")$is_alive(), assessment_complete("shared"))
  })
  forced_job <- job_for("shared")
  testServer(app_environment$server, {
    session$flushReact()
    stopifnot(identical(run_assessment("shared", force_recalculate = TRUE), FALSE),
              identical(job_for("shared"), forced_job),
              is.null(shared_state$requested_assessment),
              !shared_state$assessment_running)
    run_assessment("shared")
    session$flushReact()
    release("shared")
    wait_for(forced_job)
    session$elapse(5000)
    session$flushReact()
    stopifnot(assessment_complete("shared"), identical(shared_state$assessment, "shared"),
              !identical(shared_state$results_version, old_version))
    stopifnot(identical(readLines(file.path(app_environment$app_root,
                                          "shared.download-forced")), "TRUE"))
    logs <- file.path(app_environment$assessment_storage$data_dir, "shared",
                      c("assessment-stdout.log", "assessment-stderr.log"))
    stopifnot(all(file.info(logs)$size > 2 * 1024^2))
    session$setInputs(assessment_loaded = "shared")

    # A failed forced calculation keeps the previous results and reports failure.
    old_version <- shared_state$results_version
    file.create(file.path(app_environment$app_root, "fail"))
    run_assessment("shared", force_recalculate = TRUE)
    session$flushReact()
    wait_for(job_for("shared"))
    session$elapse(5000)
    session$flushReact()
    stopifnot(assessment_complete("shared"), identical(shared_state$assessment, "shared"),
              identical(shared_state$results_version, old_version),
              !shared_state$assessment_running)
    unlink(file.path(app_environment$app_root, "fail"))
  })

  # A viewer notices a job completed by another session, even between polls.
  testServer(app_environment$server, {
    session$flushReact()
    run_assessment("shared")
    session$setInputs(assessment_loaded = "shared")
    shared_state$publication_pending <- TRUE
    old_version <- shared_state$results_version
    app_environment$assessment_run_job(
      app_environment$app_root, "shared", app_environment$assessment_storage,
      force_recalculate = TRUE,
      calculate = function(output_dir) {
        for (name in app_environment$assessment_required_files) {
          writeLines("another session's results", file.path(output_dir, name))
        }
      }
    )
    completed_job <- new.env()
    completed_job$is_alive <- function() FALSE
    completed_job$get_result <- function() TRUE
    target <- file.path(app_environment$assessment_storage$data_dir, "shared", "output")
    app_environment$global_active_jobs[[target]] <- completed_job
    session$elapse(5000)
    session$flushReact()
    stopifnot(identical(shared_state$assessment, "shared"),
              shared_state$assessment_loading,
              !shared_state$publication_pending,
              !identical(shared_state$results_version, old_version))
  })

  cat("Shared assessment tests passed.\n")
})

local({
  library(dplyr)
  library(leaflet)
  library(DT)
  source("app_assessment.R", local = TRUE)
  source("app_assessment_indicators.R", local = TRUE)
  source("app_annual_indicators.R", local = TRUE)
  source("app_stations.R", local = TRUE)
  app_root <- tempfile("module-result-versions-")
  dir.create(app_root)
  on.exit(unlink(app_root, recursive = TRUE))
  assessment_data_path <- function(assessment, file_name) {
    file.path(app_root, "working-data", assessment, "output", file_name)
  }

  for (module in list(
    moduleAssessmentServer, moduleAssessmentIndicatorsServer,
    moduleAnnualIndicatorsServer, moduleStationsServer
  )) {
    state <- reactiveValues(
      assessment = NULL, requested_assessment = NULL, assessment_running = FALSE,
      assessment_loading = FALSE
    )
    requests <- character()
    forced_requests <- character()
    request <- function(assessment, force_recalculate = FALSE) {
      requests <<- c(requests, assessment)
      if (force_recalculate) forced_requests <<- c(forced_requests, assessment)
      state$requested_assessment <- if (identical(assessment, "")) NULL else assessment
    }
    arguments <- list(shared_state = state, run_assessment = request)
    if (identical(module, moduleStationsServer)) {
      arguments$station_configuration <- list(
        displayed = list(CTD = "displayed-ctd.csv"),
        requested = list(BOT = "requested-bot.csv")
      )
    } else {
      arguments$glossary <- data.frame()
    }

    testServer(module, args = arguments, {
      messages <- new.env()
      root_session <- session$rootScope()
      root_session$sendCustomMessage <- function(type, message) {
        if (identical(type, "shinyjs-toggleState") &&
            identical(message$id, session$ns("assessmentSelect"))) {
          messages$selector_enabled <- message$condition
        }
        if (identical(type, "shinyjs-toggleState") &&
            identical(message$id, session$ns("recalculate"))) {
          messages$recalculate_enabled <- message$condition
        }
      }
      root_session$sendInputMessage <- function(inputId, message) {
        if (grepl("assessmentSelect$", inputId)) {
          messages$assessmentSelect <- message
        }
      }
      session$flushReact()
      stopifnot(is.null(output$main_panel))
      stopifnot(isTRUE(messages$selector_enabled))
      if (identical(module, moduleAssessmentServer)) {
        stopifnot(identical(messages$recalculate_enabled, FALSE))
      }
      session$setInputs(assessmentSelect = "requested")
      stopifnot(identical(requests, "requested"))
      state$requested_assessment <- "from-another-tab"
      session$flushReact()
      stopifnot(identical(messages$assessmentSelect$value, "from-another-tab"))
      session$setInputs(assessmentSelect = "from-another-tab")
      stopifnot(length(requests) == 1L)
      state$assessment_running <- TRUE
      session$setInputs(assessmentSelect = "ignored-while-running")
      stopifnot(length(requests) == 1L, identical(messages$selector_enabled, FALSE))
      state$assessment_running <- FALSE
      state$assessment_loading <- TRUE
      session$setInputs(assessmentSelect = "ignored-while-loading")
      stopifnot(
        length(requests) == 1L,
        identical(messages$selector_enabled, FALSE),
        identical(messages$assessmentSelect$value, "from-another-tab")
      )
      state$assessment_loading <- FALSE
      session$setInputs(assessmentSelect = "")
      stopifnot(is.null(state$requested_assessment), isTRUE(messages$selector_enabled))

      if (identical(module, moduleAssessmentServer)) {
        root_session$sendModal <- function(type, message) {
          messages$modal <- list(type = type, message = message)
        }
        state$requested_assessment <- "requested"
        session$flushReact()
        stopifnot(isTRUE(messages$recalculate_enabled))
        request_count <- length(requests)
        session$setInputs(recalculate = 1)
        stopifnot(length(requests) == request_count,
                  identical(messages$modal$type, "show"),
                  grepl("requested", messages$modal$message$html, fixed = TRUE),
                  grepl("download all configured input files again",
                        messages$modal$message$html, fixed = TRUE))
        state$requested_assessment <- "different-period"
        session$setInputs(confirmRecalculate = 1)
        stopifnot(length(requests) == request_count)
        session$setInputs(recalculate = 2)
        stopifnot(length(requests) == request_count)
        session$setInputs(confirmRecalculate = 2)
        stopifnot(length(requests) == request_count + 1L,
                  identical(forced_requests, "different-period"))
        state$assessment_running <- TRUE
        session$setInputs(recalculate = 3)
        stopifnot(identical(messages$recalculate_enabled, FALSE),
                  length(requests) == request_count + 1L)
      }

      if (identical(module, moduleStationsServer)) {
        state$assessment <- "displayed"
        state$requested_assessment <- "requested"
        session$setInputs(assessmentSelect = "requested", stationType = "CTD")
        stopifnot(
          identical(unname(types()$required_station_types), "CTD"),
          identical(req_file(), assessment_data_path("displayed", "StationsCTD.csv.gz"))
        )
      }
    })
  }
  cat("Assessment module state tests passed.\n")

  fread <- data.table::fread
  assessment <- "versioned"
  directory <- file.path(app_root, "working-data", assessment, "output")
  dir.create(directory, recursive = TRUE)
  write_results <- function(value) {
    for (name in c("Assessment.csv.gz", "Assessment_Indicator.csv.gz", "Annual_Indicator.csv.gz")) {
      data.table::fwrite(data.frame(UnitID = 1L, Name = "indicator", Description = "unit",
                                   Value = value), file.path(directory, name))
    }
    data.table::fwrite(data.frame(Longitude..degrees_east. = value,
                                 Latitude..degrees_north. = value),
                      file.path(directory, "StationsCTD.csv.gz"))
    units <- sf::st_as_sf(data.frame(UnitID = 1L, x = value, y = value),
                          coords = c("x", "y"), crs = 4326)
    sf::st_write(units, file.path(directory, "Units.shp"),
                 delete_layer = TRUE, quiet = TRUE)
  }
  for (module in list(
    moduleAssessmentServer, moduleAssessmentIndicatorsServer,
    moduleAnnualIndicatorsServer, moduleStationsServer
  )) {
    write_results(1)
    state <- reactiveValues(assessment = assessment, requested_assessment = assessment,
                            assessment_running = FALSE, assessment_loading = FALSE,
                            results_version = "first-run")
    arguments <- list(shared_state = state, run_assessment = function(...) NULL)
    if (identical(module, moduleStationsServer)) {
      arguments$station_configuration <- list(versioned = list(CTD = "fixture"))
    } else arguments$glossary <- data.frame()
    testServer(module, args = arguments, {
      session$setInputs(stationType = "CTD")
      read_value <- function() {
        if (identical(module, moduleAssessmentServer)) return(assessment_data()$Value)
        if (identical(module, moduleStationsServer)) {
          return(station_data()$Longitude..degrees_east.)
        }
        indicator_data()$Value
      }
      stopifnot(read_value() == 1)
      has_units <- identical(module, moduleAssessmentServer) ||
        identical(module, moduleAssessmentIndicatorsServer)
      if (has_units) stopifnot(sf::st_coordinates(units())[1, 1] == 1)
      write_results(2)
      state$results_version <- "second-run"
      session$flushReact()
      stopifnot(read_value() == 2, identical(state$assessment, assessment))
      if (has_units) stopifnot(sf::st_coordinates(units())[1, 1] == 2)
    })
  }
  cat("Same-period result refresh tests passed.\n")
})
