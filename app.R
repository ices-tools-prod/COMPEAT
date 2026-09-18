source("./global.R")
source("./app_helpers.R")
source("./app_stations.R")
source("./app_annual_indicators.R")
source("./app_assessment_indicators.R")
source("./app_assessment.R")

glossary <- read.csv(file.path(app_root, "app_glossary.csv"))
station_configuration <- read_yaml(file.path(app_root, "app_stations_config.yml"))

ui <- tagList(
  shinyjs::useShinyjs(),
  tags$script(HTML("
    $(document).ready(function() {
      $('[data-toggle=\"tooltip\"]').tooltip();
    });
  ")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css",
                      href = "./styles.css")),
  navbarPage(
    position = "static-top",
    collapsible = TRUE,
    windowTitle = "COMPEAT",
    id = "tabset",
    fluid = TRUE,
    theme = bslib::bs_theme(bootswatch = "flatly"),
    title = span("Commom Procedure Eutrophication Assessment Tool (COMPEAT)"),
    tabPanel(
      "Assessment",
      moduleAssessmentUI("Assessment")
    ),
    tabPanel(
      "Indicators",
      tabsetPanel(
        moduleAssessmentIndicatorsUI("AssessmentIndicator"),
        moduleAnnualIndicatorsUI("AnnualIndicator")
      )
    ),
    tabPanel(
      "Stations",
      moduleStationsUI("Stations")
    ),
  )
)

server <- function(input, output, session) {

  shared_state <- reactiveValues(
    assessment = NULL,
    data_ready = FALSE,
    assessment_running = FALSE
  )

  assessment_output_files <- function(assessment) {
    file.path(
      app_root, "data", assessment, "output",
      c("Assessment.csv.gz", "Assessment_Indicator.csv.gz", "Annual_Indicator.csv.gz")
    )
  }

  run_assessment <- function(assessment) {
    req(assessment %in% assessment_periods)

    if (all(file.exists(assessment_output_files(assessment)))) {
      shared_state$assessment <- assessment
      return(invisible(TRUE))
    }

    shared_state$assessment_running <- TRUE
    showNotification(
      paste("Assessment generation started for", assessment, "in the background."),
      type = "message",
      duration = 10
    )

    future({
      setwd(app_root)
      library(data.table)
      library(ncdf4)
      library(R.utils)
      library(readxl)
      library(sf)
      library(tidyverse)
      source(file.path(app_root, "data.R"), local = list2env(list(assessmentPeriod = assessment)))

      if (!all(file.exists(assessment_output_files(assessment)))) {
        stop("Assessment generation completed without creating all result files.")
      }

      TRUE
    }, seed = TRUE) %...>% (function(result) {
      shared_state$assessment_running <- FALSE
      shared_state$assessment <- assessment
      showNotification(
        paste("Assessment", assessment, "completed successfully."),
        type = "message",
        duration = 10
      )
      invisible(result)
    }) %...!% (function(error) {
      shared_state$assessment_running <- FALSE
      showNotification(
        paste("Assessment generation failed:", conditionMessage(error)),
        type = "error",
        duration = NULL
      )
      NULL
    })

    invisible(TRUE)
  }

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
