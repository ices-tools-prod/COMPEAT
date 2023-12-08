# Define UI for the module
moduleIndicatorsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        shiny::selectInput(inputId = ns("indicator"),
                            "Select Indicator",
                            choices = c("Bottle" = "BOT", "CTD" ="CTD", "Pump" = "PMP")),
        shiny::radioButtons(inputId = ns("status"),
                           "Toggle status / confidence",
                           choices = c("Status" = "stat", "Confidence" = "conf")),
                   uiOutput(ns("assessmentRadioButtons")),
        shiny::downloadButton(ns("downloadIndicators"), "Download")
      ), 
      mainPanel = mainPanel(
        shiny::fluidRow(
          shiny::tabsetPanel(
            tabPanel("Map"),
            tabPanel("Charts")
          )
        ),
        shiny::fluidRow(
          DT::DTOutput(ns("data"))
        )
      ))
  )
}

# Define server logic for the module
moduleIndicatorsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Radio buttons can adjust to assessments being added / removed from ./Data
    radio_buttons_from_directory(dir = "../Data", id = "assessment", output, session)
    
    
    indicators_shape <- sf::read_sf("../Data/COMP 4 (2015-2020)/Assessment_Indicator.shp")
    
    plot_data <- reactive({
      indicators <- fread(paste0("../Data/", input$assessment, "/Assessment_Indicator.csv"))
      dplyr::filter(indicators, Name == input$indicator)
    })
    
    output$map <- renderPlot({
      
      ggplot(data = indicators_shape,mapping = aes(input$indicator))
      
    })
    
    
    output$data <- renderDT({
      shinipsum::random_DT(ncol = 7, nrow = 10, type = "numeric")
    }
     
     
    )
  }
)}