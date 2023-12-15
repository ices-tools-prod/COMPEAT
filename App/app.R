library(shiny)
library(shinycssloaders)
library(shinyjs)
library(sf)
library(data.table)
library(tidyverse)
library(leaflet)
library(DT)

source("./Helpers.R")
source("./moduleStations.R")
source("./moduleAnnualIndicators.R")
source("./moduleAssessmentIndicators.R")
source("./moduleAssessment.R")

ui <- tagList(
  navbarPage(
    position = "static-top",
    collapsible = TRUE,
    # tab title
    windowTitle = "COMPEAT",
    id = "tabset",
    fluid = TRUE,
    # navbar title
    title = span(tags$img(src ="www/ospar_logo.png",
                          style = "padding-right:10px;padding-bottom:10px; padding-top:0px; margin-top: -10px",
                          height = "50px"), "Commom Procedure Eutrophication Assessment Tool (COMPEAT)"),
    tabPanel("Stations",
             moduleStationsUI("Stations")
    ),
    tabPanel("Indicators",
             tabsetPanel(
                  moduleAssessmentIndicatorsUI("AssessInd"),
                  moduleAnnualIndicatorsUI("AnnualInd"))
                
    ),
    tabPanel("Assessment",
            moduleAssessmentUI("Assessment")
    )
  )
)
 

server <- function(input, output, session) {

  
  moduleStationsServer("Stations")
  moduleAssessmentIndicatorsServer("AssessInd")
  moduleAnnualIndicatorsServer("AnnualInd")
  moduleAssessmentServer("Assessment")
  

}

shinyApp(ui = ui, server = server)

