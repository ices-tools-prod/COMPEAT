# Define UI for the module
moduleAssessmentIndicatorsUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Map",
    tagList(
      sidebarLayout(
        sidebarPanel = sidebarPanel(width = 2,
          uiOutput(ns("unitSelector")),
          uiOutput(ns("indicatorSelector")),
          radioButtons(inputId = ns("display"),
                              "Select Assessment outcomes",
                              choices = c("Status (EQRS)" = "EQRS_Cl", 
                                          "Confidence (C)" = "C_Class",
                                          "Temporal Confidence (TC)" = "TC_Clss",
                                          "Spatial Confidence (SC)" = "SC_Clss")),
          shiny::downloadButton(ns("downloadIndicators"), "Download")
        ), 
        mainPanel = mainPanel(
          shiny::fluidRow(leafletOutput(ns("map"))),
          shiny::fluidRow(DT::DTOutput(ns("data")))
        )
      )
  )
  )
}

# Define server logic for the module
moduleAssessmentIndicatorsServer <- function(id, assessment) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    indicator_data <- reactive({
      if(!is.null(assessment())){
        indicators <- fread(paste0("./Data/", assessment(), "/Annual_Indicator.csv"))
      }
    })
    
    
    output$indicatorSelector <- renderUI({ 
      req(indicator_data())
      indicators <- unique(indicator_data()$Name) %>% sort()
      shiny::selectInput(session$ns("indicator"), "Select Indicator:", choices = indicators)
    })
    
   
    output$data <- renderDT({
      datatable(indicator_data(), 
                filter = 'top', 
                extensions = 'FixedColumns', 
                options = list(
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 3)))
    })
    
    
    indicator_shape <- reactive({
      sf::read_sf(paste0("./Data/", assessment(), "/Assessment_Indicator.shp"), stringsAsFactors = T)
    })
    
      
    plot_data_sf <- reactive({
      if(!is.null(input$indicator)){
        dplyr::filter(indicator_shape(), Name == input$indicator)
      }
    })

        
    output$map <- renderLeaflet({
      req(plot_data_sf())
      plot_dat <- plot_data_sf()
      
      # Transform the spatial data to WGS84
      plot_dat <- st_transform(plot_dat, crs = 4326)
      
      if (nrow(plot_dat) > 0) {
        if (input$display == "EQRS_Cl") {
          plot_dat$EQRS_Cl <- factor(st_drop_geometry(plot_dat)[[input$display]], 
                                     levels = eqrs_levels, ordered = TRUE)
          
          pal <- colorFactor(eqrs_palette, plot_dat$EQRS_Cl)
          
        } else {
          plot_dat[[input$display]] <- factor(st_drop_geometry(plot_dat)[[input$display]], 
                                              levels = c_levels, ordered = TRUE)
          
          pal <- colorFactor(c_palette, plot_dat[[input$display]])
        }
        
        leaflet(plot_dat) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
          addPolygons(
            fillColor = ~pal(plot_dat[[input$display]]),
            stroke = TRUE, 
            fillOpacity = 0.9, 
            color = "black", 
            weight = 0.3,
            label = ~paste0("Eutrophication Status ", assessment(), plot_dat[[input$display]]),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          )
      }
    })
    
  }
)}