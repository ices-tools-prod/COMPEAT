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
  
  shared_state <- reactiveValues(
    assessment = NULL,
    data_ready = FALSE
  )
  
  get_assessments <- function() {
    list.dirs(
      "./data",
      recursive = FALSE,
      full.names = FALSE
    ) |>
      sort(decreasing = TRUE)
  }
  
  # Run once at startup
  observeEvent(TRUE, {
    
    assessments <- get_assessments()
    
    # Generate data only when needed
    if (length(assessments) == 0) {
      
      showNotification(
        "No assessments found. Generating initial data...",
        type = "message",
        duration = NULL
      )
      
      tryCatch({
        
        source("data.R", local = TRUE)
        
        assessments <- get_assessments()
        
        if (length(assessments) == 0) {
          stop("Data generation completed but no assessments were created.")
        }
        
      }, error = function(e) {
        
        showNotification(
          paste("Assessment generation failed:", e$message),
          type = "error",
          duration = NULL
        )
        
        return(NULL)
      })
    }
    
    shared_state$assessment <- assessments[[1]]
    shared_state$data_ready <- TRUE
    
  }, once = TRUE)
  
  #
  # Initialize modules only after data is available
  #
  observeEvent(shared_state$data_ready, {
    
    req(shared_state$assessment)
    
    moduleAssessmentServer(
      "Assessment",
      shared_state = shared_state,
      glossary = glossary
    )
    
    moduleAssessmentIndicatorsServer(
      "AssessInd",
      shared_state = shared_state,
      glossary = glossary
    )
    
    moduleAnnualIndicatorsServer(
      "AnnualInd",
      shared_state = shared_state,
      glossary = glossary
    )
    
    moduleStationsServer(
      "Stations",
      shared_state = shared_state,
      station_configuration = station_configuration
    )
    
  }, once = TRUE)
}

shinyApp(ui = ui, server = server)
