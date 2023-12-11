# Define UI for the module
moduleAnnualIndicatorsUI <- function(id) {
  ns <- NS(id)
  tabPanel("Charts",
    tagList(
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          uiOutput(ns("indicatorSelector")),
          uiOutput(ns("unitSelector")),
          uiOutput(ns("assessmentRadioButtons")),
          shiny::downloadButton(ns("downloadIndicators"), "Download")), 
        mainPanel = mainPanel(shiny::plotOutput(ns("chart")))
      )
    )
  )
}

# Define server logic for the module
moduleAnnualIndicatorsServer <- function(id) {
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
      shiny::selectInput(session$ns("indicator"), "Choose an Indicator:", choices = indicators)
    })
    
    output$unitSelector <- renderUI({ 
      req(indicator_data())
      units <- unique(indicator_data()$Description)
      shiny::selectInput(session$ns("unit"), "Choose an Indicator:", choices = units)
    })
    
    plot_data <- reactive({
      
      if(!is.null(input$indicator)){
        dplyr::filter(indicator_data(), Name == input$indicator)
      }
    })
    

    output$chart <- renderPlot({
      req(plot_data())
      
      if (nrow(plot_data()) > 0){
        unit_data <- filter(plot_data(), Description == input$unit)
      }
      if (nrow(unit_data) > 0){
        x_breaks <- seq(plot_data()[1,]$YearMin, plot_data()[1,]$YearMax)
        indicatorMetric <- unit_data[1,]$Metric
        plot <- ggplot(unit_data, aes(x = factor(Period, levels = YearMin:YearMax), y = ES)) +
          # labs(title = title , subtitle = subtitle) +
          geom_col() +
          geom_text(aes(label = N), vjust = -0.25, hjust = -0.25) +
          geom_hline(aes(yintercept = ET)) +
          scale_x_discrete(NULL, breaks = x_breaks, drop=FALSE) +
          scale_y_continuous(NULL)
        
        if (indicatorMetric == "Mean") {
          plot <- plot +
            geom_errorbar(aes(ymin = ES - CI, ymax = ES + CI), width = .2)
        }
        plot
      }
    })
    
    
    
    
  }
  )}