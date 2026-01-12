# Define UI for the module
moduleAssessmentIndicatorsUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Indicator score",
    tagList(
      layout_sidebar(fg = "black", 
        sidebar = bslib::sidebar(width = 300, fg = "black", open = TRUE,
           selectInput(
             inputId = ns("assessmentSelect"),
             label = "Select Assessment Period:",
             choices = list.dirs("./Data", recursive = FALSE, full.names = FALSE) %>% sort(decreasing = TRUE),
             selected = NULL  # Initially NULL; server will set the default
           ),
          uiOutput(ns("indicatorSelector")),
          radioButtons(inputId = ns("confidence"),
                       "Select confidence measure:",
                       choices = c("Confidence" = "C_Class",
                                   "Temporal Confidence" = "TC_Clss",
                                   "Spatial Confidence" = "SC_Clss")),
          sliderInput(ns("map_display_size"), label = "Map size (% Screen Height)", min = 0, max = 100, value = 50), 
          checkboxInput(ns("show_legend"), "Show Legend", value = TRUE),
          downloadButton(ns("downloadAssessmentIndicators"), "Download"),
          accordion(open=TRUE,
            accordion_panel(title = "Customise Table ",
                            uiOutput(ns("indicator_cols_ui"))
              ),
            accordion_panel("Glossary", DTOutput(ns("glossary")))
          )
        ), 
        uiOutput(ns("main_panel"))
      )
  )
  )
}

# Define server logic for the module
moduleAssessmentIndicatorsServer <- function(id, shared_state, glossary) {
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
      req(input$assessmentSelect)
      req(shared_state$assessment)
      if (input$assessmentSelect != shared_state$assessment) {
        updateSelectInput(session, "assessmentSelect", selected = shared_state$assessment)
      }
    }, ignoreInit = T)
    
    
    file_paths_assessment_indicators <- reactive({
      if(!is.null(shared_state$assessment)){
        paste0("./Data/", shared_state$assessment, "/Assessment_Indicator.csv")
      }
    })

    indicator_data <- reactive({
      req(!is.null(shared_state$assessment))
        fread(file_paths_assessment_indicators()) %>% 
          mutate(across(where(is.double), ~ round(.x, 2)))
    })
    
    output$indicatorSelector <- renderUI({ 
      req(indicator_data())
      indicators <- unique(indicator_data()$Name) %>% sort()
      selectInput(session$ns("indicator"), "Select Indicator:", choices = indicators)
    })
    
    indicator_shape <- reactive({
      sf::read_sf(paste0("./Data/", shared_state$assessment, "/Assessment_Indicator.shp"), stringsAsFactors = T)
      })
    
      
    plot_data_sf <- reactive({
      if(!is.null(input$indicator)){
        dplyr::filter(indicator_shape(), Name == input$indicator) %>% 
            mutate(across(where(is.double), ~ round(.x, 2)))
      }
    })

        
    output$map1 <- renderLeaflet({
      req(plot_data_sf())
      req(nrow(plot_data_sf()) >0)
      
      plot_dat <- plot_data_sf()
      
      # Transform the spatial data to WGS84
      plot_dat <- st_transform(plot_dat, crs = 4326)
      
      
        plot_dat$EQRS_Cl <- factor(st_drop_geometry(plot_dat)[["EQRS_Cl"]], 
                                     levels = eqrs_levels, ordered = TRUE)
          
        pal <- colorFactor(eqrs_palette, plot_dat$EQRS_Cl)
     
        label_text <- make_indicator_hovertext_content(plot_data = plot_dat, output = "EQRS", confidence = NULL, var = input$indicator)
        
        leaflet_map <- 
          leaflet(plot_dat, 
                  options = leafletOptions(
                    attributionControl=FALSE)) %>%
          enableTileCaching() %>%
          addProviderTiles(providers$Esri.WorldImagery,
                           options=tileOptions(useCache=TRUE,crossOrigin=TRUE)) %>%  
          addPolygons(
            fillColor = ~pal(plot_dat[["EQRS_Cl"]]),
            stroke = TRUE, 
            fillOpacity = 0.9, 
            color = "black", 
            weight = 0.3,
            label = lapply(label_text, htmltools::HTML),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          ) 

        if(input$show_legend) {
          leaflet_map <- leaflet_map %>%
            addLegend(
            position = "bottomright",
            pal = pal,
            values = plot_dat[["EQRS_Cl"]],
            title = "Eutrophication Status",
            opacity = 1)
        }
      leaflet_map
})
    
    
    
    output$map2 <- renderLeaflet({
      req(plot_data_sf())
      req(input$confidence)
      plot_dat <- plot_data_sf()
      
      # Transform the spatial data to WGS84
      plot_dat <- st_transform(plot_dat, crs = 4326)
      
      if (nrow(plot_dat) > 0) {
        
          plot_dat[[input$confidence]] <- factor(st_drop_geometry(plot_dat)[[input$confidence]], 
                                              levels = c_levels, ordered = TRUE)
          
          pal <- colorFactor(c_palette, plot_dat[[input$confidence]])
          label_text <- make_indicator_hovertext_content(plot_data = plot_dat, output = "C", confidence = input$confidence, var = input$indicator)
          
        leaflet_map <- 
          leaflet(plot_dat, 
                  options = leafletOptions(
                    attributionControl=FALSE)) %>%
          enableTileCaching() %>%
          addProviderTiles(providers$Esri.WorldImagery,
                           options=tileOptions(useCache=TRUE,crossOrigin=TRUE)) %>% 
          addPolygons(
            fillColor = ~pal(plot_dat[[input$confidence]]),
            stroke = TRUE, 
            fillOpacity = 0.9, 
            color = "black", 
            weight = 0.3,
            label = lapply(label_text, htmltools::HTML),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          ) 
          
          if(input$show_legend) {
            leaflet_map <-  
              leaflet_map %>% 
                addLegend(
                position = "bottomright",
                pal = pal,
                values = plot_dat[[input$confidence]],
                title = case_when(input$confidence == "C_Class" ~ "Confidence",
                                  input$confidence == "TC_Clss" ~ "Temporal Confidence",
                                  input$confidence == "SC_Clss" ~ "Spatial Confidence"),
                opacity = 1)
          }
        leaflet_map
      }
    })
    
    output$downloadAssessmentIndicators <- shiny::downloadHandler(
      filename = function () {
        stringr::str_remove(file_paths_assessment_indicators(), pattern = "../")
      },
      content = function(file) {
        file.copy(file_paths_assessment_indicators(), file)
      }
    )
    
      starting_columns <- c("Description", "Name", "Parameters", "Metric", "Units", "Period", "N", "ES", "ET", "STC", "TC_Class", "SSC", "SC_Class", "C", "C_Class", "EQRS", "EQRS_Class")
    
      output$indicator_cols_ui <- renderUI({
      req(!is.null(indicator_data()))

      selectizeInput(ns("indicator_cols"), multiple = T, "Adjust table columns", choices = sort(colnames(indicator_data())), selected = starting_columns)
    })
    
    output$glossary <- renderDT({
      names(glossary) <- stringr::str_to_title(names(glossary))
      datatable(glossary[], 
                options = list(dom = 'ftp', 
                               paging = T,  
                               ordering = T,
                               info = FALSE
                ), rownames = F)
    })
    
    output$data <- renderDT({
      req(indicator_data())
      req(input$indicator_cols)
      display_cols <- intersect(colnames(indicator_data()), input$indicator_cols)
      dat <- indicator_data()[indicator_data()$Name == input$indicator, ..display_cols]
      tool_tips <- prepare_tooltips_with_fallback(column_names = colnames(dat), glossary)
      
      datatable(dat, 
                colnames = tool_tips,
                escape = FALSE,
                filter = 'top', 
                extensions = 'FixedColumns', 
                rownames = F,
                options = list(
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 1),
                  fnDrawCallback = htmlwidgets::JS(
                    "function() { $('[data-toggle=\"tooltip\"]').tooltip(); }"
                  )))
    })
    
    
    output$main_panel <- renderUI({
      tagList(
        accordion(open = TRUE,
          accordion_panel(title = "Maps",
            fluidRow(
              column(width = 6,
                     card(style = paste0("height: ", input$map_display_size*0.9, "vh;"),
                          full_screen = T, 
                          card_header("Status",class = "bg-primary"),
                          leafletOutput(ns("map1"), height = "100%"))%>% withSpinner()),
              column(width = 6,
                     card(style = paste0("height: ", input$map_display_size*0.9, "vh;"),
                          full_screen = T, 
                          card_header("Confidence",
                                      ,class = "bg-primary"),
                          leafletOutput(ns("map2"), height = "100%"))%>% withSpinner(),
              )
            ))),
        card(style = paste0("height: ", 85, "vh;"),
             full_screen = T,
             DTOutput(ns("data"))) %>% withSpinner()
      )
    })
  }
)}
