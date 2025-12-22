# Define UI for the module
moduleAssessmentUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(fg = "black", 
      sidebar = bslib::sidebar(width = 300, fg = "black", open = TRUE,
                               selectInput(
                                 inputId = ns("assessmentSelect"),
                                 label = "Select Assessment Period:",
                                 choices = list.dirs("./Data", recursive = FALSE, full.names = FALSE) %>% sort(decreasing = TRUE),
                                 selected = NULL  # Initially NULL; server will set the default
                               ),
        selectInput(
          inputId = ns("category"),
          label = "Select Assessment Category:",
          choices = c(
            "Overall Assessment" = 0,
            "Nutrient levels: Nitrogen" = 11,
            "Nutrient levels: Phosphorus" = 12,
            "Direct effects" = 2,
            "Indirect effects" = 3)),
        checkboxInput(ns("show_legend"), "Show Legend", value = TRUE),
        sliderInput(ns("map_display_size"), label = "Map size (% Screen Height)", min = 0, max = 100, value = 50), 
        shiny::downloadButton(ns("downloadAssessmentFile"), "Download")
      ),
      uiOutput(ns("main_panel"))
    )
  )
}

# Define server logic for the module
moduleAssessmentServer <- function(id, shared_state, glossary) {
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
    })
    
    
    file_paths_assessment <- reactive({
      req(!is.null(shared_state$assessment))
      paste0("./Data/", shared_state$assessment, "/Assessment.csv")
    })

    name_schema <- c("11" = "Nitrogen", 
                     "12" = "Phosphorus", 
                     "2" = "Direct Effects",
                     "3" = "Indirect Effects")
    
    assessment_data <- reactive({
      req(!is.null(shared_state$assessment))
      fread(paste0("./Data/", shared_state$assessment, "/Assessment.csv"))
    }) 
    
    units <- reactive({
      req(!is.null(shared_state$assessment))
      sf::read_sf(paste0("./Data/", shared_state$assessment, "/Units.shp"), stringsAsFactors = TRUE)
    }) 
    
    var <- reactive({
      switch(input$category,
             "0" = "All",
             "11" = "Nitrogen", 
             "12" = "Phosphorus", 
             "2" = "Direct Effects",
             "3" = "Indirect Effects")
    })
    
    merged_data <- reactive({
      shiny::validate(
        need(!is.null(shared_state$assessment), "No assessment selected"),
        need(!is.null(units()), "Units not available")
      )
      req(units(), assessment_data())
      
      dat <- merge(select(units(), UnitID), assessment_data(), all.x = TRUE)
      new_names <- str_replace_all(names(dat), name_schema)
      names(dat) <- new_names
      
      if(var() != "All") {
        cols_for_display <- stringr::str_subset(names(dat), var()) 
        dat <- dat %>% dplyr::select(Code, Description, all_of(cols_for_display)) %>% 
          dplyr::relocate(contains("C_"), .after = last_col())
        
      } else {
        cols_for_display <- c("NE", "EQR", "EQRS", "EQRS_Class", "NC", "C", "C_Class")
        dat <- dat %>% dplyr::select(Code, Description, all_of(cols_for_display)) 
      }
        dat <- dat %>% mutate(across(where(is.double), ~ round(.x, digits = 2)))
    })
    
    output$map1 <- renderLeaflet({
      req(merged_data())
      plot_dat <- merged_data()
      
        plot_dat <- st_transform(plot_dat, crs = 4326)
        type_lower <- "eqrs"
        type <- "EQRS"
        category <- input$category
        
        if(category == "0") {
          display_col <- paste(type, "Class", sep = "_")
        } else {
          display_col <- paste(type, var(), "Class", sep = "_")
        }


        plot_dat[[display_col]] <- factor(st_drop_geometry(plot_dat)[[display_col]], 
                                   levels = get(paste0(type_lower, "_levels")), ordered = TRUE)
        
        pal <- colorFactor(get(paste0(type_lower, "_palette")), plot_dat[[display_col]])
        label_text <- make_assessment_hovertext_content(plot_data = plot_dat, output = type, category = category, var = var)
          
        
        leaflet_map <- 
          leaflet(plot_dat) %>%
          enableTileCaching() %>%
          addProviderTiles(providers$Esri.WorldImagery,
                           options=tileOptions(useCache=TRUE,crossOrigin=TRUE)) %>%  
          addPolygons(
            fillColor = ~pal(get(display_col)),
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
              values = plot_dat[[display_col]],
              title = case_when(type == "EQRS" ~ "Status",
                                type == "C" ~ "Confidence"),
              opacity = 1
            )
        }
        leaflet_map
    }) 
    
    output$map2 = renderLeaflet({
      req(merged_data())
      plot_dat <- merged_data()
      
      if (nrow(plot_dat) > 0) {
        # validate(
        #   need(c("EQRS_Class", "EQRS_11_Class", "EQRS_12_Class", "EQRS_2_Class", "EQRS_3_Class", "C_Class", "C_11_Class", "C_12_Class", "C_2_Class", "C_3_Class") %in% colnames(plot_dat), message = 'Check that columns "EQRS_Class", "EQRS_11_Class", "EQRS_12_Class", "EQRS_2_Class", "EQRS_3_Class", "C_Class", "C_11_Class", "C_12_Class", "C_2_Class", "C_3_Class" are present within the data')
        # )
        plot_dat <- st_transform(plot_dat, crs = 4326)
        type_lower <- "c"
        type <- "C"
        category <- input$category
        
        if(category == "0") {
          display_col <- paste(type, "Class", sep = "_")
        } else {
          display_col <- paste(type, var(), "Class", sep = "_")
        }


        plot_dat[[display_col]] <- factor(st_drop_geometry(plot_dat)[[display_col]], 
                                   levels = get(paste0(type_lower, "_levels")), ordered = TRUE)
        
        pal <- colorFactor(get(paste0(type_lower, "_palette")), plot_dat[[display_col]])
        
        label_text <- make_assessment_hovertext_content(plot_data = plot_dat, output = type, category = category, var = var)
          
        leaflet_map <- 
          leaflet(plot_dat) %>%
          enableTileCaching() %>%
          addProviderTiles(providers$Esri.WorldImagery,
                           options=tileOptions(useCache=TRUE,crossOrigin=TRUE)) %>%  
          addPolygons(
            fillColor = ~pal(get(display_col)),
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
            values = plot_dat[[display_col]],
            title = case_when(type == "EQRS" ~ "Eutrophication Status",
                              type == "C" ~ "Confidence Level"),
            opacity = 1
          )
        }
        leaflet_map
      }
    })
    
    dt_data <- reactive({
      req(!is.null(merged_data()))
      dat <- st_drop_geometry(merged_data())
      
    })
    
    output$assessmentTable <- renderDT({
      req(!is.null(dt_data()))
      
      dat <- dt_data()
      tool_tips <- prepare_tooltips_with_fallback(column_names = colnames(dat), glossary)
      
      datatable(
        data = dat, 
        colnames = tool_tips,
        escape = FALSE,
        filter = 'top',
        extensions = 'FixedColumns',
        options = list(
          scrollX = TRUE,
          fixedColumns = list(leftColumns = 2),
          fnDrawCallback = htmlwidgets::JS(
            "function() { $('[data-toggle=\"tooltip\"]').tooltip(); }"
          )
          )
        )
      })
    
    output$downloadAssessmentFile <- shiny::downloadHandler(
      filename = function () {
        str_remove(file_paths_assessment(), pattern = "../")
      },
      content = function(file) {
        file.copy(file_paths_assessment(), file)
      }
    )
    
    output$main_panel <- renderUI({
      tagList(
        accordion(open = TRUE,
          accordion_panel(title = "Maps", 
            fluidRow(
              column(width = 6,
                          card(style = paste0("height: ", input$map_display_size*0.9, "vh;"),
                            full_screen = T, 
                               card_header("Status",class = "bg-primary"),
                               leafletOutput(
                                 outputId = ns("map1"), height = "100%"))%>% withSpinner(),
          ),
              column(width = 6,
                     card(style = paste0("height: ", input$map_display_size*0.9, "vh;"),
                          full_screen = T,
                          card_header("Confidence", class = "bg-primary"),
                          leafletOutput(outputId = ns("map2"), height = "100%"))%>% withSpinner(),
            ))
          )),card(style = paste0("height: ", 85, "vh;"),
                 full_screen = T,
                 #card_header("Table", class = "bg-primary"),
                 DTOutput(outputId = ns("assessmentTable")) %>% withSpinner()
            )
          
        
      )
    })
    
    
    
    
  }
  )}