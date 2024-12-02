library(shiny)
library(R.methodsS3)
library(R.oo)
library(R.utils, pos = grep("shiny", search()) + 1)
library(shinycssloaders)
library(shinyjs)
library(sf)
library(data.table)
library(tidyverse)
library(leaflet)
library(DT)

source("./helpers.R")
source("./moduleStations.R")
source("./moduleAnnualIndicators.R")
source("./moduleAssessmentIndicators.R")
source("./moduleAssessment.R")

ui <- tagList(
  navbarPage(
    position = "static-top",
    collapsible = TRUE,
    windowTitle = "COMPEAT",
    id = "tabset",
    fluid = TRUE,
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
  
  moduleStationsServer("Stations", shared_state = shared_state)
  moduleAssessmentIndicatorsServer("AssessInd", shared_state = shared_state)
  moduleAnnualIndicatorsServer("AnnualInd", shared_state = shared_state)
  moduleAssessmentServer("Assessment", shared_state = shared_state)
  

}

shinyApp(ui = ui, server = server)

