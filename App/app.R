library(shiny)
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
    header = div(style = "padding: 15px;", uiOutput("assessmentSelect")),
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
  shinyselect_from_directory(dir = "./Data", selector = "select", id = "assessment", uiOutput = "Select Assessment Period:", outputid = "assessmentSelect", module = T, output, session)
  reactiveAssessment <- reactive({input$assessment})
  
  moduleStationsServer("Stations", assessment = reactiveAssessment)
  moduleAssessmentIndicatorsServer("AssessInd", assessment = reactiveAssessment)
  moduleAnnualIndicatorsServer("AnnualInd", assessment = reactiveAssessment)
  moduleAssessmentServer("Assessment", assessment = reactiveAssessment)
  

}

shinyApp(ui = ui, server = server)

