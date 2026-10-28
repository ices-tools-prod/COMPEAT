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
    'output_path <- file.path(assessmentDataDir, assessmentPeriod, "output")',
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
  app_root <- tempdir()
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
    request <- function(assessment) {
      requests <<- c(requests, assessment)
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
      }
      root_session$sendInputMessage <- function(inputId, message) {
        if (grepl("assessmentSelect$", inputId)) {
          messages$assessmentSelect <- message
        }
      }
      session$flushReact()
      stopifnot(is.null(output$main_panel))
      stopifnot(isTRUE(messages$selector_enabled))
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
})
