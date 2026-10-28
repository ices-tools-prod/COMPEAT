library(bslib)
library(data.table)
library(DT)
library(htmltools)
library(httr)
library(leaflet)
library(leaflet.extras)
library(ncdf4)
library(callr)
library(R.utils)
library(readxl)
library(sf)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(tidyverse)
library(yaml)

app_root <- normalizePath(getwd())

source("./app_storage.R", local = TRUE)
assessment_storage <- assessment_storage_config(app_root)

global_active_jobs <- list()

assessment_data_path <- function(assessment, file_name) {
  file.path(assessment_storage$data_dir, assessment, "output", file_name)
}

source("./app_helpers.R", local = TRUE)
source("./app_stations.R", local = TRUE)
source("./app_annual_indicators.R", local = TRUE)
source("./app_assessment_indicators.R", local = TRUE)
source("./app_assessment.R", local = TRUE)

glossary <- read.csv(file.path(app_root, "app_glossary.csv"))
station_configuration <- read_yaml(file.path(app_root, "app_stations_config.yml"))

ui <- tagList(
  shinyjs::useShinyjs(),
  textOutput("storage_status"),
  textOutput("assessment_status"),
  tags$script(
    HTML(
      "
    $(document).ready(function() {
      $('[data-toggle=\"tooltip\"]').tooltip();
    });
    (function() {
      var pendingAssessment = null;
      var pendingOutputs = new Set();
      var deliveredOutputs = new Set();
      var busy = false;
      $(document).on('shiny:inputchanged', function(event) {
        if (/-assessmentSelect$/.test(event.name) && event.value) {
          $('select[id$=\"-assessmentSelect\"]').each(function() {
            if (this.selectize) this.selectize.disable();
            else $(this).prop('disabled', true);
          });
        }
      });
      $(document).on('shiny:busy', function() { busy = true; });
      $(document).on('shiny:bound', function(event) {
        if (event.bindingType === 'output' && !deliveredOutputs.has(event.target.id)) {
          pendingOutputs.add(event.target.id);
        }
      });
      $(document).on('shiny:value shiny:error', function(event) {
        deliveredOutputs.add(event.target.id);
        pendingOutputs.delete(event.target.id);
      });
      $(document).on('shiny:unbound', function(event) {
        pendingOutputs.delete(event.target.id);
      });
      Shiny.addCustomMessageHandler('assessmentResultsLoading', function(message) {
        pendingAssessment = message.assessment;
      });
      $(document).on('shiny:idle', function() {
        busy = false;
        requestAnimationFrame(function() {
          requestAnimationFrame(function() {
            var waitingForOutput = Array.from(pendingOutputs).some(function(id) {
              return $(document.getElementById(id)).is(':visible');
            });
            if (pendingAssessment !== null && !busy && !waitingForOutput &&
                $('.shiny-bound-output.recalculating:visible').length === 0) {
              var assessment = pendingAssessment;
              pendingAssessment = null;
              Shiny.setInputValue('assessment_loaded', assessment, {priority: 'event'});
            }
          });
        });
      });
    })();
  "
    )
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "./styles.css")
  ),
  navbarPage(
    position = "static-top",
    collapsible = TRUE,
    windowTitle = "COMPEAT",
    id = "tabset",
    fluid = TRUE,
    theme = bslib::bs_theme(bootswatch = "flatly"),
    title = span("Commom Procedure Eutrophication Assessment Tool (COMPEAT)"),
    tabPanel("Assessment", moduleAssessmentUI("Assessment")),
    tabPanel(
      "Indicators",
      tabsetPanel(
        moduleAssessmentIndicatorsUI("AssessmentIndicator"),
        moduleAnnualIndicatorsUI("AnnualIndicator")
      )
    ),
    tabPanel("Stations", moduleStationsUI("Stations")),
  )
)

server <- function(input, output, session) {
  shared_state <- reactiveValues(
    assessment = NULL,
    requested_assessment = NULL,
    data_ready = FALSE,
    assessment_loading = FALSE,
    assessment_running = FALSE
  )

  assessment_process_name <- NULL

  assessment_output_files <- function(assessment) {
    assessment_data_path(assessment, assessment_required_files)
  }

  completion_marker <- function(assessment) {
    assessment_data_path(assessment, "assessment.complete")
  }

  assessment_complete <- function(assessment) {
    assessment_cache_complete(assessment_storage, assessment)
  }

  output$storage_status <- renderText({
    if (assessment_storage$mode == "s3") {
      paste0("Storage: S3 (s3://", assessment_storage$bucket, "/",
             assessment_storage$prefix, "); local working cache: ", assessment_storage$data_dir)
    } else {
      paste("Storage: local files at", assessment_storage$data_dir)
    }
  })

  output$assessment_status <- renderText({
    requested <- shared_state$requested_assessment
    displayed <- shared_state$assessment
    paste(
      if (isTRUE(shared_state$assessment_loading)) {
        paste("Loading:", requested)
      } else if (isTRUE(shared_state$assessment_running)) {
        paste("Running:", requested)
      } else if (!is.null(requested) && !identical(requested, displayed)) {
        paste("Requested assessment is not available:", requested)
      } else {
        "Ready."
      },
      if (is.null(displayed)) "No results displayed." else paste("Displaying:", displayed)
    )
  })

  observeEvent(input$assessment_loaded, {
    if (!isTRUE(shared_state$assessment_running) &&
        identical(input$assessment_loaded, shared_state$requested_assessment) &&
        identical(shared_state$assessment, shared_state$requested_assessment)) {
      shared_state$assessment_loading <- FALSE
    }
  })

  run_assessment <- function(assessment) {
    if (isTRUE(shared_state$assessment_running) ||
        isTRUE(shared_state$assessment_loading)) return(invisible(FALSE))
    if (is.null(assessment) || identical(assessment, "")) {
      shared_state$requested_assessment <- NULL
      shared_state$assessment <- NULL
      return(invisible(TRUE))
    }
    req(assessment %in% assessment_periods)
    shared_state$requested_assessment <- assessment
    shared_state$assessment <- NULL
    shared_state$assessment_loading <- TRUE

    target_dir <- file.path(assessment_storage$data_dir, assessment, "output")
    job <- global_active_jobs[[target_dir]]
    job_running <- !is.null(job) && job$is_alive()
    previous_result <- if (!is.null(job) && !job_running) {
      tryCatch(job$get_result(), error = function(error) error)
    }

    # CASE A: Reuse cached results only when no unfinished or failed job owns them.
    cached <- tryCatch(assessment_complete(assessment), error = function(error) error)
    if (inherits(cached, "error")) {
      shared_state$assessment_loading <- FALSE
      showNotification(paste("Could not validate assessment cache:", conditionMessage(cached)),
                       type = "error", duration = NULL)
      return(invisible(FALSE))
    }
    if (!job_running && !inherits(previous_result, "error") && cached) {
      shared_state$assessment <- assessment
      session$sendCustomMessage("assessmentResultsLoading", list(assessment = assessment))
      showNotification(
        "Results already calculated for this assessment. Loading...",
        type = "message",
        duration = 10
      )
      return(invisible(TRUE))
    }

    # CASE B: Wait for the job already running for this folder.
    if (job_running) {
      assessment_process_name <<- assessment
      shared_state$assessment_loading <- FALSE
      shared_state$assessment_running <- TRUE
      showNotification(
        "Another user has already initiated this assessment. Waiting for completion...",
        type = "warning",
        duration = 10
      )
      return(invisible(TRUE))
    }

    # CASE C: Start a new shared background job for this folder.
    marker <- completion_marker(assessment)
    reuse_local <- all(file.exists(c(marker, assessment_output_files(assessment)))) &&
      !inherits(previous_result, "error")
    if (file.exists(marker) && !file.remove(marker)) {
      shared_state$assessment_loading <- FALSE
      showNotification("Could not invalidate the previous assessment completion marker.",
                       type = "error", duration = NULL)
      return(invisible(FALSE))
    }
    process <- tryCatch({
      if (!dir.exists(target_dir) &&
          !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)) {
        stop("Could not create the assessment output directory.")
      }
      callr::r_bg(
        function(app_root, assessment, storage, reuse_local) {
          setwd(app_root)
          source(file.path(app_root, "app_storage.R"), local = TRUE)
          assessment_run_job(app_root, assessment, storage, reuse_local)
        },
        args = list(
          app_root = app_root,
          assessment = assessment,
          storage = assessment_storage,
          reuse_local = reuse_local
        ),
        stdout = file.path(target_dir, "assessment-stdout.log"),
        stderr = file.path(target_dir, "assessment-stderr.log")
      )
    }, error = function(error) error)

    if (inherits(process, "error")) {
      shared_state$assessment_loading <- FALSE
      showNotification(
        paste("Assessment preparation could not start:", conditionMessage(process)),
        type = "error",
        duration = NULL
      )
      return(invisible(FALSE))
    }

    global_active_jobs[[target_dir]] <<- process
    assessment_process_name <<- assessment
    shared_state$assessment_loading <- FALSE
    shared_state$assessment_running <- TRUE
    showNotification(
      paste("Assessment preparation started for", assessment,
            "in the background (restore, calculate or publish)."),
      type = "default",
      duration = 10
    )
    
    invisible(TRUE)
  }
  
  observe({
    invalidateLater(5000, session)
    
    if (is.null(assessment_process_name)) {
      return()
    }

    assessment <- assessment_process_name
    target_dir <- file.path(assessment_storage$data_dir, assessment, "output")
    job <- global_active_jobs[[target_dir]]
    if (job$is_alive()) {
      return()
    }
    
    result <- tryCatch(
      job$get_result(),
      error = function(error)
        error
    )
    if (!inherits(result, "error")) {
      result <- tryCatch({
        if (!assessment_complete(assessment)) {
          stop("Assessment completion marker, persistence receipt or required output files are missing.")
        }
        result
      }, error = function(error) error)
    }
    assessment_process_name <<- NULL
    shared_state$assessment_running <- FALSE
    
    if (inherits(result, "error")) {
      showNotification(
        paste(
          "Assessment preparation failed:",
          conditionMessage(result)
        ),
        type = "error",
        duration = NULL
      )
      return()
    }
    
    shared_state$assessment <- assessment
    shared_state$assessment_loading <- TRUE
    session$sendCustomMessage("assessmentResultsLoading", list(assessment = assessment))
    showNotification(
      paste("Assessment", assessment, "completed successfully."),
      type = "message",
      duration = 10
    )
  })
  
  observeEvent(TRUE, {
    shared_state$data_ready <- TRUE
  }, once = TRUE)
  
  #
  # Initialize modules only after data is available
  #
  observeEvent(shared_state$data_ready, {
    moduleAssessmentServer(
      "Assessment",
      shared_state = shared_state,
      glossary = glossary,
      run_assessment = run_assessment
    )
    
    moduleAssessmentIndicatorsServer(
      "AssessmentIndicator",
      shared_state = shared_state,
      glossary = glossary,
      run_assessment = run_assessment
    )
    
    moduleAnnualIndicatorsServer(
      "AnnualIndicator",
      shared_state = shared_state,
      glossary = glossary,
      run_assessment = run_assessment
    )
    
    moduleStationsServer(
      "Stations",
      shared_state = shared_state,
      station_configuration = station_configuration,
      run_assessment = run_assessment
    )
    
  }, once = TRUE)
}

shinyApp(ui = ui, server = server)
