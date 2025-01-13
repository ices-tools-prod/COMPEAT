library(bslib)
library(data.table)
library(DT)
library(htmltools)
library(leaflet)
library(R.methodsS3)
library(R.oo)
library(R.utils, pos = grep("shiny", search()) + 1)
library(sf)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(stringr)
library(tidyverse)

source("./helpers.R")
source("./moduleStations.R")
source("./moduleAnnualIndicators.R")
source("./moduleAssessmentIndicators.R")
source("./moduleAssessment.R")
glossary <- readRDS("./Data/glossary.rds")

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

  # Dropdown selector that adjusts to assessments being added / removed from ./Data
  
  shared_state <- reactiveValues(assessment = NULL)
  
  moduleAssessmentServer("Assessment", shared_state = shared_state, glossary)
  moduleAssessmentIndicatorsServer("AssessInd", shared_state = shared_state, glossary)
  moduleAnnualIndicatorsServer("AnnualInd", shared_state = shared_state)
  moduleStationsServer("Stations", shared_state = shared_state)
  

}

shinyApp(ui = ui, server = server)

