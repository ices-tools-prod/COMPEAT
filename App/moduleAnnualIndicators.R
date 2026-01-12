# Define UI for the module
moduleAnnualIndicatorsUI <- function(id) {
  ns <- NS(id)
  tabPanel("Indicator trends",
    tagList(
      layout_sidebar(fg = "black", 
                     sidebar = bslib::sidebar(width = 300, fg = "black", open = TRUE,
                                              selectInput(
                                                inputId = ns("assessmentSelect"),
                                                label = "Select Assessment Period:",
                                                choices = list.dirs("./Data", recursive = FALSE, full.names = FALSE) %>% sort(decreasing = TRUE),
                                                selected = NULL  # Initially NULL; server will set the default
                                              ),
        uiOutput(ns("unitSelector")),
        uiOutput(ns("indicatorSelector")),
        sliderInput(ns("plot_display_size"), label = "Plot size (% Screen Height)", min = 0, max = 100, value = 70), 
        uiOutput(ns("indicator_cols_ui")),
        shiny::downloadButton(ns("downloadAnnualIndicators"), "Download")),
        uiOutput(ns("main_panel"))
      )
    )
  )
}

# Define server logic for the module
moduleAnnualIndicatorsServer <- function(id, shared_state, glossary) {
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
        fread(paste0("./Data/", shared_state$assessment, "/Annual_Indicator.csv")) %>%
          mutate(across(where(is.double), ~ round(.x, digits = 2)))
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
      req(!is.null(input$indicator))
      req(!is.null(input$unit))
        dplyr::filter(indicator_data(), Name == input$indicator, Description == input$unit)
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
    
    starting_columns <- c("UnitID", "Description", "Name", "Parameters", "Metric", "Units", "Period", "N", "ES", "ET", "STC", "TC_Class", "SSC", "SC_Class", "C", "C_Class", "EQRS", "EQRS_Class")
    
    output$indicator_cols_ui <- renderUI({
      req(!is.null(indicator_data()))
      
      selectizeInput(ns("indicator_cols"), multiple = T, "Adjust table columns", choices = sort(colnames(indicator_data())), selected = starting_columns)
    })
    
    output$data <- renderDT({
      req(plot_data())
      req(input$indicator_cols)
      
      display_cols <- intersect(colnames(plot_data()), input$indicator_cols)
      dat <- plot_data()[plot_data()$Name == input$indicator, ..display_cols]
      tool_tips <- prepare_tooltips_with_fallback(column_names = colnames(dat), glossary)
      
      datatable(dat, 
                colnames = tool_tips,
                escape = FALSE,
                filter = 'top', 
                extensions = 'FixedColumns', 
                options = list(
                  dom = "t",
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 1),
                  fnDrawCallback = htmlwidgets::JS(
                    "function() { $('[data-toggle=\"tooltip\"]').tooltip(); }"
                  )))
    })
    
    output$main_panel <- renderUI({
      tagList(
        card(style = paste0("height: ", input$plot_display_size*0.95, "vh;"),
           full_screen = TRUE,
           plotOutput(ns("chart"))),
      card(style = paste0("height: ", 75, "vh;"),
           full_screen = T,
           DTOutput(ns("data")) %>% withSpinner())
      )
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
