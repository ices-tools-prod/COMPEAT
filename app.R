library(bslib)
library(data.table)
library(DT)
library(htmltools)
library(httr)
library(leaflet)
library(leaflet.extras)
library(ncdf4)
library(R.utils)
library(readxl)
library(sf)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(tidyverse)
library(yaml)

source("./app_helpers.R")
source("./app_stations.R")
source("./app_annual_indicators.R")
source("./app_assessment_indicators.R")
source("./app_assessment.R")

glossary <- read.csv("./app_glossary.csv")
station_configuration <- read_yaml("./app_stations_config.yml")

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
      "./data", assessment, "output",
      c("Assessment.csv.gz", "Assessment_Indicator.csv.gz", "Annual_Indicator.csv.gz")
    )
  }

  run_assessment <- function(assessment) {
    req(assessment %in% assessment_periods)

    if (all(file.exists(assessment_output_files(assessment)))) {
      return(invisible(TRUE))
    }

    shared_state$assessment_running <- TRUE
    on.exit(shared_state$assessment_running <- FALSE, add = TRUE)

    withProgress(message = paste("Running", assessment), value = 0, {
      tryCatch({
        source("data.R", local = list2env(list(assessmentPeriod = assessment)))
        incProgress(1)
      }, error = function(e) {
        showNotification(
          paste("Assessment generation failed:", e$message),
          type = "error",
          duration = NULL
        )
        stop(e)
      })
    })

    if (!all(file.exists(assessment_output_files(assessment)))) {
      stop("Assessment generation completed without creating all result files.")
    }

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
