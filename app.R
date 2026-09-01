library(bslib)
library(data.table)
library(DT)
library(htmltools)
library(httr)
library(leaflet)
library(leaflet.extras)
library(readxl)
library(sf)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(stringr)
library(tidyverse)
library(yaml)

run_assessment <- function() {
  if (!dir.exists(file.path("data", "COMP 5 (2021-2026)"))){
    source("data.R")
  } 
}

source("./app_helpers.R")
source("./app_stations.R")
source("./app_annual_indicators.R")
source("./app_assessment_indicators.R")
source("./app_assessment.R")

glossary <- read.csv("./app_glossary.csv")
station_configuration <- read_yaml("./app_stations_config.yml")

ui <- tagList(
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
        moduleAssessmentIndicatorsUI("AssessInd"),
        moduleAnnualIndicatorsUI("AnnualInd")
      )
    ),
    tabPanel(
      "Stations",
      moduleStationsUI("Stations")
    ),
  )
)

server <- function(input, output, session) {
  shared_state <- reactiveValues(assessment = NULL)

  # Fetch available assessments
  available_assessments <- list.dirs("./data",
                                     recursive = FALSE,
                                     full.names = FALSE) %>% sort(decreasing = TRUE)

  # Initialize shared_state$assessment with the first available assessment
  observe({
    if (is.null(shared_state$assessment) && length(available_assessments) > 0) {
      shared_state$assessment <- available_assessments[1]
    }
  })
  
  # Optional: If no assessments are available, handle accordingly
  observe({
    if (length(available_assessments) == 0) {
      # showModal(modalDialog(
      #   title = "No Assessments Found",
      #   "Please run and add an assessments.",
      #   easyClose = TRUE,
      #   footer = NULL
      # ))
      run_assessment()
    }
  })

  # Initialize Modules without their own assessment selectors
  moduleAssessmentServer("Assessment", shared_state = shared_state, glossary = glossary)
  moduleAssessmentIndicatorsServer("AssessInd", shared_state = shared_state, glossary = glossary)
  moduleAnnualIndicatorsServer("AnnualInd", shared_state = shared_state, glossary = glossary)
  moduleStationsServer("Stations", shared_state = shared_state, station_configuration = station_configuration)
}

shinyApp(ui = ui, server = server)
