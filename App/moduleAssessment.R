# Define UI for the module
moduleAssessmentUI <- function(id) {
  ns <- NS(id)
           tagList(
               sidebarLayout(
                 sidebarPanel(width = 2,
                   selectInput(inputId = ns("category"), "Select Assessment", c("Overall" = 0, "Nutrient levels - Nitrogen" = 11, "Nutrient levels - Phosphorus" = 12, "Direct effects" = 2, "Indirect effects" = 3), "All"),
                   shiny::radioButtons(inputId = ns("display"),
                                       "Select Assessment outcome",
                                       choices = c("Status (EQRS)" = "EQRS", 
                                                   "Confidence (C)" = "C"))
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
        assessment <- fread(paste0("./Data/", assessment(), "/Assessment.csv"))
      }
    })

    
    units <- reactive({
      if(!is.null(assessment())){
      sf::read_sf(paste0("./Data/", assessment(), "/units.shp"), stringsAsFactors = T)
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
      
      if (nrow(plot_dat) > 0) {
        validate(
          need(c("EQRS_Class", "EQRS_11_Class", "EQRS_12_Class", "EQRS_2_Class", "EQRS_3_Class", "C_Class", "C_11_Class", "C_12_Class", "C_2_Class", "C_3_Class") %in% colnames(plot_dat), message = 'Check that columns "EQRS_Class", "EQRS_11_Class", "EQRS_12_Class", "EQRS_2_Class", "EQRS_3_Class", "C_Class", "C_11_Class", "C_12_Class", "C_2_Class", "C_3_Class" are present within the data')
        )
        
        type <- tolower(input$display)
        category <- input$category
        
        if(category == 0) {
          display_col <- paste(input$display, "Class", sep = "_")
          
        } else {
          
          display_col <- paste(input$display, toupper(category), "Class", sep = "_")
        }
        
        # Transform the spatial data to WGS84
        plot_dat <- st_transform(plot_dat, crs = 4326)
        

          plot_dat[[display_col]] <- factor(st_drop_geometry(plot_dat)[[display_col]], 
                                     levels = get(paste0(type, "_levels")), ordered = TRUE)
          
          pal <- colorFactor(get(paste0(type, "_palette")), plot_dat[[display_col]])
          
          
        
        leaflet(plot_dat) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
          addPolygons(
            fillColor = ~pal(plot_dat[[display_col]]),
            stroke = TRUE, 
            fillOpacity = 0.9, 
            color = "black", 
            weight = 0.3,
            label = ~paste0("Eutrophication Status ", assessment(), plot_dat[[display_col]]),
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
    
  }
  )}