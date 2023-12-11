# Define UI for the module
moduleAssessmentIndicatorsUI <- function(id) {
  ns <- NS(id)
  tabPanel("Map",
    tagList(
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          uiOutput(ns("indicatorSelector")),
          uiOutput(ns("unitSelector")),
          uiOutput(ns("assessmentRadioButtons")),
          shiny::radioButtons(inputId = ns("display"),
                              "Select Assessmentoutcomes",
                              choices = c("Status" = "EQRS_Cl", 
                                          "Confidence" = "C_Class",
                                          "Temporal Confidence" = "TC_Clss",
                                          "Spatial Confidence" = "SC_Clss")),
          shiny::downloadButton(ns("downloadIndicators"), "Download")
        ), 
        mainPanel = mainPanel(
          shiny::fluidRow("Map",
                       plotOutput(ns("map")),
              
          ),
          shiny::fluidRow("Data",
            DT::DTOutput(ns("data"))
          )
        )
      )
  )
  )
}

# Define server logic for the module
moduleAssessmentIndicatorsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Radio buttons can adjust to assessments being added / removed from ./Data
    radio_buttons_from_directory(dir = "../Data", id = "assessment", output, session)
    
    indicator_data <- reactive({
      
      if(!is.null(input$assessment)){
        indicators <- fread(paste0("../Data/", input$assessment, "/Annual_Indicator.csv"))
      }
    })
    
    output$indicatorSelector <- renderUI({ 
      req(indicator_data())
      indicators <- unique(indicator_data()$Name)
      shiny::selectInput(session$ns("indicator"), "Select Indicator:", choices = indicators)
    })
    
   
    output$data <- renderDT({
      indicator_data()
    })
    
    indicator_shape <- sf::read_sf("../Data/COMP 4 (2015-2020)/Assessment_Indicator.shp", stringsAsFactors = T)
    
    plot_data_sf <- reactive({
      
      if(!is.null(input$indicator)){
        dplyr::filter(indicator_shape, Name == input$indicator)
      }
    })
    
    output$map <- renderPlot({
      req(plot_data_sf())
      plot_dat <- plot_data_sf()
      if (nrow(plot_dat) > 0){

        if (input$display == "EQRS_Cl") {
          plot_dat["EQRS_Cl"] <- st_drop_geometry(plot_dat) %>% dplyr::pull(input$display) %>% factor(levels = eqrs_levels, ordered = T)
          ggplot(plot_dat) +
            ggtitle(label = paste0("Eutrophication Status ", input$assessment)) +
            geom_sf(aes(fill = EQRS_Cl)) +
            scale_fill_manual(name = "EQRS", values = eqrs_palette, labels = eqrs_labels)

        } else {
          plot_dat[input$display] <- st_drop_geometry(plot_dat) %>% dplyr::pull(input$display) %>% factor(levels = c_levels, ordered = T)
          ggplot(plot_dat) +
            ggtitle(label = paste0("Eutrophication Status ", input$assessment)) +
            geom_sf(aes_string(fill = input$display)) +
            scale_fill_manual(name = input$display, values = c_palette, labels = c_labels)
        }
      }
    })
    
    
    
    
  }
  )}