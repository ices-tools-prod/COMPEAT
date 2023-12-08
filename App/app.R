library(shiny)
library(shinycssloaders)
library(shinyjs)
library(sf)
library(data.table)
library(tidyverse)
library(leaflet)
library(DT)


library(shinipsum)


source("./Helpers.R")
source("./moduleStations.R")
source("./moduleIndicators.R")

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
    tabPanel("Data",
             moduleStationsUI("Stations")
    ),
    tabPanel("Indicators",
             moduleIndicatorsUI("Indicators")
    ),
    tabPanel("Assessment",
    fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "period", "Period", c("COMP 1 (1990-2000)" = 1, "COMP 2 (2001-2006)" = 2, "COMP 3 (2006-2014)" = 3, "COMP 4 (2015-2020)" = 4), 4),
      selectInput(inputId = "category", "Category", c("All" = 0, "Nutrient levels - Nitrogen" = 11, "Nutrient levels - Phosphorus" = 12, "Direct effects" = 2, "Indirect effects" = 3), "All"),
      selectInput(inputId = "indicator", "Indicator", c("All", "DIN", "DIP"), "All"),
      selectInput(inputId = "Unit", "Unit", c("All"), "All")
    ),
    mainPanel(
      leafletOutput(outputId = 'assessmentMap') %>% withSpinner(),
      DTOutput(outputId = "assessmentTable")
    )
  )
)
)
)
)

server <- function(input, output, session) {

  dataPath <- file.path("../Data")
  assessmentPaths <- list.dirs(dataPath, recursive = FALSE)
  assessmentPath <- file.path(dataPath, "COMP 4 (2015-2020)")
  
  units <- st_read(file.path(assessmentPath, "gridunits.shp"))
  assessment <- fread(file.path(assessmentPath, "Assessment.csv"))
  assessment_indicator <- fread(file.path(assessmentPath, "Assessment_Indicator.csv"))
  annual_indicator <- fread(file.path(assessmentPath, "Annual_Indicator.csv"))
  
  moduleStationsServer("Stations")
  moduleIndicatorsServer("Indicators")
  
  wk <- merge(select(units, UnitID), assessment, all.x = TRUE)
  
  eqrs_color_bin <- colorBin(palette = eqrs_palette, domain = wk$EQRS, bins = eqrs_bins, reverse = TRUE)
  c_color_bin <- colorBin(palette = c_palette, domain = wk$C, bins = c_bins, reverse = TRUE)
  
  
  label <- paste(
    "<b>", wk$Description, " (", wk$Code, ")</b><br />",
    "Status (EQRS): ", round(wk$EQRS, 2), " (", wk$EQRS_Class, ") <br/>", 
    "Confidence (C): ", round(wk$C, 0), " (", wk$C_Class, ")", 
    sep="") %>%
    lapply(htmltools::HTML)
  
  output$assessmentMap = renderLeaflet({
    leaflet(wk) %>%
      setView(lng = -1, lat = 53, zoom = 3) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addPolygons(
        group = "EQRS",
        data = wk,
        fillColor = ~eqrs_color_bin(EQRS),
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="black", 
        weight=0.3,
        label = label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addPolygons(
        group = "C",
        data = wk,
        fillColor = ~c_color_bin(C),
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="black", 
        weight=0.3,
        label = label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomleft",
        colors = eqrs_palette,
        opacity=1,
        labels = eqrs_labels,
        title = "Status (EQRS)"
      ) %>%
      addLayersControl(
        baseGroups = c("EQRS", "C")
      )
  })
  
  output$assessmentTable <- renderDT(
    assessment[, .(Code, Description, EQRS, C)]
  )
  
  observeEvent(input$assessmentMap_groups, {
    my_map <- leafletProxy("assessmentMap") %>% clearControls()
    
    if (input$assessmentMap_groups == 'EQRS'){
      my_map <- my_map %>%
        addLegend(
          position = "bottomleft",
          colors = eqrs_palette,
          opacity = 1,
          labels = eqrs_labels,
          title = "Status (EQRS)")
    } else {
      my_map <- my_map %>%
        addLegend(
          position = "bottomleft",
          colors = c_palette,
          opacity = 1,
          labels = c_labels,
          title = "Confidence (C)")
    }
  })
}

shinyApp(ui = ui, server = server)

