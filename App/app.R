library(bslib)
library(data.table)
library(DT)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(R.methodsS3)
library(R.oo)
library(R.utils, pos = grep("shiny", search()) + 1)
library(sf)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(stringr)
library(tidyverse)
library(yaml)

source("./helpers.R")
source("./moduleStations.R")
source("./moduleAnnualIndicators.R")
source("./moduleAssessmentIndicators.R")
source("./moduleAssessment.R")
glossary <- readRDS("./Data/glossary.rds")
station_configuration <- read_yaml("./stations_config.yml")

ui <- tagList(
  tags$script(HTML("
    $(document).ready(function() {
      $('[data-toggle=\"tooltip\"]').tooltip();
    });
  ")),
  
  navbarPage(
    position = "static-top",
    collapsible = TRUE,
    windowTitle = "COMPEAT",
    id = "tabset",
    fluid = TRUE,
    theme = bslib::bs_theme(bootswatch = "flatly"),
      
    title = span("Commom Procedure Eutrophication Assessment Tool (COMPEAT)"), 
    
    tabPanel("Assessment",
             moduleAssessmentUI("Assessment")
    ),
    tabPanel("Indicators",
             tabsetPanel(
               moduleAssessmentIndicatorsUI("AssessInd"),
               moduleAnnualIndicatorsUI("AnnualInd"))
    ),
    tabPanel("Stations",
             moduleStationsUI("Stations")
    ),
  )
)
 

server <- function(input, output, session) {

      shared_state <- reactiveValues(assessment = NULL)
      
       # Fetch available assessments
       available_assessments <- list.dirs("./Data", recursive = FALSE, full.names = FALSE) %>% sort(decreasing = TRUE)
       
       # Initialize shared_state$assessment with the first available assessment
       observe({
         if (is.null(shared_state$assessment) && length(available_assessments) > 0) {
           shared_state$assessment <- available_assessments[1]
         }
       })
       
       # Optional: If no assessments are available, handle accordingly
       observe({
         if (length(available_assessments) == 0) {
           showModal(modalDialog(
             title = "No Assessments Found",
             "Please add assessments to the ./Data directory.",
             easyClose = TRUE,
             footer = NULL
           ))
         }
      })
    
    
  # Initialize Modules without their own assessment selectors
  moduleAssessmentServer("Assessment", shared_state = shared_state, glossary)
  moduleAssessmentIndicatorsServer("AssessInd", shared_state = shared_state, glossary)
  moduleAnnualIndicatorsServer("AnnualInd", shared_state = shared_state)
  moduleStationsServer("Stations", shared_state = shared_state)
  
}

shinyApp(ui = ui, server = server)

