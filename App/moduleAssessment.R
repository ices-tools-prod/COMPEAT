# Define UI for the module
moduleAssessmentUI <- function(id) {
  ns <- NS(id)
           tagList(
               sidebarLayout(
                 sidebarPanel(width = 2,
                   shiny::radioButtons(inputId = ns("display"),
                                       "Select Assessment outcome",
                                       choices = c("Status" = "EQRS_Class", 
                                                   "Confidence" = "C_Class",
                                                   "Temporal Confidence" = "TC_Clss",
                                                   "Spatial Confidence" = "SC_Clss")),
                   selectInput(inputId = "category", "Assessment Category", c("Overall Assessment" = 0, "Nutrient levels - Nitrogen" = 11, "Nutrient levels - Phosphorus" = 12, "Direct effects" = 2, "Indirect effects" = 3), "All")
                 ),
                 mainPanel(
                   leafletOutput(outputId = ns("assessmentMap")) %>% withSpinner(),
                   DTOutput(outputId = ns("assessmentTable"))
                 )
               )
             )
           
}


# Define server logic for the module
moduleAssessmentServer <- function(id, assessment) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    assessment_data <- reactive({
      if(!is.null(assessment())){
        assessment <- fread(paste0("../Data/", assessment(), "/Assessment.csv"))
      }
    })

    
    units <- reactive({
      if(!is.null(assessment())){
      sf::read_sf(paste0("../Data/", assessment(), "/gridunits.shp"), stringsAsFactors = T)
      }
    })
    
    
    merged_data <- reactive({
      shiny::validate(
        need(!is.null(assessment()), "No assessment selected"),
        need(!is.null(units()), "Units not available")
      )
      req(units(), assessment_data())
      merge(select(units(), UnitID), assessment_data(), all.x = TRUE)
    })
    
    
    output$assessmentMap = renderLeaflet({
      req(merged_data())
      plot_dat <- merged_data()
      validate(
        need(c("EQRS_Class", "C_Class") %in% colnames(plot_dat), message = "Check that columns 'EQRS_Class', 'C_Class' are present within the data")
      )
      
      # Transform the spatial data to WGS84
      plot_dat <- st_transform(plot_dat, crs = 4326)
      
      if (nrow(plot_dat) > 0) {
        if (input$display == "EQRS_Class") {
          plot_dat[[input$display]] <- factor(st_drop_geometry(plot_dat)[[input$display]], 
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
    
    output$assessmentTable <- renderDT({
      
      req(!is.null(assessment_data())) 
      datatable(assessment_data()[, .(Code, Description, EQRS, C)], 
                filter = 'top', 
                extensions = 'FixedColumns', 
                options = list(
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 2)))
    })
    
    # observeEvent(assessment()Map_groups, {
    #   my_map <- leafletProxy("assessmentMap") %>% clearControls()
    #   
    #   if (assessment()Map_groups == 'EQRS'){
    #     my_map <- my_map %>%
    #       addLegend(
    #         position = "bottomleft",
    #         colors = eqrs_palette,
    #         opacity = 1,
    #         labels = eqrs_labels,
    #         title = "Status (EQRS)")
    #   } else {
    #     my_map <- my_map %>%
    #       addLegend(
    #         position = "bottomleft",
    #         colors = c_palette,
    #         opacity = 1,
    #         labels = c_labels,
    #         title = "Confidence (C)")
    #   }
    # })
    
    
  }
  )}