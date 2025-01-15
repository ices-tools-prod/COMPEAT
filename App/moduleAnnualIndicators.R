# Define UI for the module
moduleAnnualIndicatorsUI <- function(id) {
  ns <- NS(id)
  tabPanel("Charts",
    tagList(
      layout_sidebar(fg = "black", 
                     sidebar = bslib::sidebar(width = "15vw", fg = "black", open = TRUE,
                                              selectInput(
                                                inputId = ns("assessmentSelect"),
                                                label = "Select Assessment Period:",
                                                choices = list.dirs("./Data", recursive = FALSE, full.names = FALSE) %>% sort(decreasing = TRUE),
                                                selected = NULL  # Initially NULL; server will set the default
                                              ),
        uiOutput(ns("unitSelector")),
        uiOutput(ns("indicatorSelector")),
        shiny::downloadButton(ns("downloadAnnualIndicators"), "Download")), 
        card(style = paste0("height: ", 85, "vh;"),
             full_screen = TRUE,
             plotOutput(ns("chart")))
      )
    )
  )
}

# Define server logic for the module
moduleAnnualIndicatorsServer <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update the selected assessment from shared_state on module load
    observe({
      updateSelectInput(session, "assessmentSelect", selected = shared_state$assessment)
    })
    
    # When user changes the assessmentSelect, update shared_state$assessment
    observeEvent(input$assessmentSelect, {
      req(input$assessmentSelect)
      if (input$assessmentSelect != shared_state$assessment) {
        shared_state$assessment <- input$assessmentSelect
      }
    })
    
    # When shared_state$assessment changes (from other modules), update this module's assessmentSelect
    observeEvent(shared_state$assessment, {
      req(shared_state$assessment)
      req(input$assessmentSelect)
      if (input$assessmentSelect != shared_state$assessment) {
        updateSelectInput(session, "assessmentSelect", selected = shared_state$assessment)
      }
    }, ignoreInit = T)
    
    
    file_paths_annual_indicators <- reactive({
      if(!is.null(shared_state$assessment)){
        paste0("./Data/", shared_state$assessment, "/Annual_Indicator.csv")
      }
    })
    
    indicator_data <- reactive({
      if(!is.null(shared_state$assessment)){
        fread(paste0("./Data/", shared_state$assessment, "/Annual_Indicator.csv"))
      }
    })

    output$indicatorSelector <- renderUI({ 
      req(indicator_data())
      indicators <- unique(indicator_data()$Name)
      shiny::selectInput(session$ns("indicator"), "Select Indicator:", choices = indicators)
    })
   
     
    output$unitSelector <- renderUI({ 
      req(indicator_data())
      units <- unique(indicator_data()$Description)
      shiny::selectInput(session$ns("unit"), "Select Unit:", choices = units)
    })
    
    
    plot_data <- reactive({
      if(!is.null(input$indicator)){
        dplyr::filter(indicator_data(), Name == input$indicator)
      }
    })
    

    output$chart <- renderPlot({
      req(plot_data())
      theme_custom <- theme(
        
        text = element_text(size = 18),
          
        panel.grid = element_blank(),#
        panel.grid.major.y = element_line(colour = "#e3e1e1", linetype = 2),
      )
      
      theme_set(theme_bw(base_size = 18) + theme_custom)
      if (nrow(plot_data()) > 0){
        unit_data <- filter(plot_data(), Description == input$unit)
      }
      
      shiny::validate(need(nrow(unit_data) >0, "No data available for this Indicator in the selected Unit and Assessment"))
      
      if (nrow(unit_data) > 0){
        
        x_breaks <- seq(plot_data()[1,]$YearMin, plot_data()[1,]$YearMax)
        indicatorMetric <- unit_data[1,]$Metric
        plot <- ggplot(unit_data, aes(x = factor(Period, levels = YearMin:YearMax), y = ES)) +
          geom_col() +
          geom_text(aes(label = paste("N =",N)), vjust = -1, hjust = 1.2, size = 5) +
          geom_hline(aes(yintercept = ET, colour = "Red"), show.legend = F) +
          annotate("text", x = 5.5, y = unique(unit_data$ET), 
                   label = "Eutrophication Target/Threshold", vjust = 1.2, colour = "Red", size = 6) +
          scale_x_discrete("Year", breaks = x_breaks, drop=FALSE) +
          scale_y_continuous(name = unique(unit_data$Units), limits = c(0, NA)) +
          labs(title = paste(input$indicator, "in Unit:", input$unit),
               subtitle = paste0("(", stringr::str_to_title(indicatorMetric),")"),
               caption = paste("OSPAR COMPEAT Assessment:", shared_state$assessment))
        
        if (indicatorMetric == "Mean") {
          plot <- plot +
            geom_errorbar(aes(ymin = ES - CI, ymax = ES + CI), width = .2, show.legend = F)
        }
        plot
      }
    })
    
    output$downloadAnnualIndicators <- shiny::downloadHandler(
      filename = function () {
        stringr::str_remove(file_paths_annual_indicators(), pattern = "../")
      },
      content = function(file) {
        file.copy(file_paths_annual_indicators(), file)
      }
    )
  }
)}
