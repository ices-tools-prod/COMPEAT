library(shiny)
library(shinycssloaders)
library(shinyjs)
library(sf)
library(data.table)
library(tidyverse)
library(leaflet)
library(DT)

shinyUI(
  fluidPage(
    titlePanel("COMmon Procedure Eutrophication Assessment Tool (COMPEAT)"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "period", "Period", c("COMP 1 (1990-2000)" = 1, "COMP 2 (2001-2006)" = 2, "COMP 3 (2006-2014)" = 3, "COMP 4 (2015-2020)" = 4), 4),
        selectInput(inputId = "category", "Category", c("All" = 0, "Nutrient levels - Nitrogen" = 11, "Nutrient levels - Phosphorus" = 12, "Direct effects" = 2, "Indirect effects" = 3), "All"),
        selectInput(inputId = "indicator", "Indicator", c("All", "DIN", "DIP"), "All"),
        selectInput(inputId = "Unit", "Unit", c("All"), "All")
        #selectInput(inputId = "Unit", label = "Unit", choices = c("choose" = "", levels(units$Code)))
        ),
      mainPanel(
        leafletOutput(outputId = 'assessmentMap') %>% withSpinner(),
        DTOutput(outputId = "assessmentTable")
        )
      )
    )
  )