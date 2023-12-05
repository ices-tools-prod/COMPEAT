library(shiny)
library(shinycssloaders)
library(shinyjs)
library(sf)
library(data.table)
library(tidyverse)
library(leaflet)
library(DT)

ui <- fluidPage(
  titlePanel("Commom Procedure Eutrophication Assessment Tool (COMPEAT)"),
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

server <- function(input, output, session) {
  dataPath <- file.path("Data")
  assessmentPaths <- list.dirs(dataPath, recursive = FALSE)
  assessmentPath <- file.path(dataPath, "COMP 4 (2015-2020)")
  
  units <- st_read(file.path(dataPath, "units.shp"))
  assessment <- fread(file.path(assessmentPath, "Assessment.csv"))
  assessment_indicator <- fread(file.path(assessmentPath, "Assessment_Indicator.csv"))
  annual_indicator <- fread(file.path(assessmentPath, "Annual_Indicator.csv"))
  
  wk <- merge(select(units, UnitID), assessment, all.x = TRUE)
  
  # Create a color palette with handmade bins.
  eqrs_palette <- c("#3BB300", "#99FF66", "#FFCABF", "#FF8066", "#FF0000")
  eqrs_bins <- c(1.0,0.8,0.6,0.4,0.2,0.0)
  eqrs_labels <- c(">= 0.8 - 1.0 (High)", ">= 0.6 - 0.8 (Good)", ">= 0.4 - 0.6 (Moderate)", ">= 0.2 - 0.4 (Poor)", ">= 0.0 - 0.2 (Bad)")
  eqrs_color_bin <- colorBin(palette = eqrs_palette, domain = wk$EQRS, bins = eqrs_bins, reverse = TRUE)
  
  # eqrs_color_bin <- reactive({
  #   if (input$category == 0) {
  #     colorBin(palette = eqrs_palette, domain = wk$EQRS, bins = eqrs_bins, reverse = TRUE)
  #   }
  #   if (input$category == 11) {
  #     colorBin(palette = eqrs_palette, domain = wk$EQRS_11, bins = eqrs_bins, reverse = TRUE)
  #   }
  # })
  
  c_palette <- c("#3BB300", "#FFCABF", "#FF0000")
  c_bins <- c(100, 75, 50, 0)
  c_color_bin <- colorBin(palette = c_palette, domain = wk$C, bins = c_bins, reverse = TRUE)
  c_labels <- c(">= 75 % (High)", "50 - 74 % (Moderate)", "< 50 % (Low)")
  
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

