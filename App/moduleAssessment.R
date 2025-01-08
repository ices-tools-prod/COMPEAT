# Define UI for the module
moduleAssessmentUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(fg = "black", 
      sidebar = bslib::sidebar(width = "15vw", fg = "black", open = T,
        uiOutput(ns("assessmentSelect")),
        selectInput(
          inputId = ns("category"),
          label = "Select Assessment",
          choices = c(
            "All" = 0,
            "Nutrient levels: Nitrogen" = 11,
            "Nutrient levels: Phosphorus" = 12,
            "Direct effects" = 2,
            "Indirect effects" = 3)),
        checkboxInput(ns("show_legend"), "Show Legend", value = TRUE),
        sliderInput(ns("map_display_size"), label = "Map size (% Screen Height)",min = 0,max = 100,value = 50), 
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
    
    shinyselect_from_directory(dir = "./Data", selector = "dropdown", id = "assessment", uiOutput = "Select Assessment Period:", outputid = "assessmentSelect", module = T, output, session)
    
    observeEvent(input$assessment, {
      shared_state$assessment <- input$assessment
    })
    
    file_paths_assessment <- reactive({
      
      if(!is.null(shared_state$assessment)){
        browser()
        paste0("./Data/", shared_state$assessment, "/Assessment.csv")
      }
    })

    name_schema <- c("11" = "Nitrogen", 
                     "12" = "Phosphorus", 
                     "2" = "Direct Effects",
                     "3" = "Indirect Effects")
    
    assessment_data <- reactive({
      
      if(!is.null(shared_state$assessment)){
        assessment <- fread(paste0("./Data/", shared_state$assessment, "/Assessment.csv"))
        }
    })
    
    units <- reactive({
      if(!is.null(shared_state$assessment)){
      sf::read_sf(paste0("./Data/", shared_state$assessment, "/Units.shp"), stringsAsFactors = T)
      }
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
        dat <- dat %>% dplyr::select(UnitID, Code, Description, all_of(cols_for_display)) %>% 
          dplyr::relocate(contains("C_"), .after = last_col())
        
      } else {
        cols_for_display <- c("NE", "EQR", "EQRS", "EQRS_Class", "NC", "C", "C_Class")
        dat <- dat %>% dplyr::select(UnitID, Code, Description, all_of(cols_for_display)) 
      }
        dat <- dat %>% mutate(across(c(5:6,9), ~ round(.x, digits = 2)))
    })
    
    output$map1 <- renderLeaflet({
      req(merged_data())
      plot_dat <- merged_data()
      
      if (nrow(plot_dat) > 0) {
        # validate(
        #   need(c("EQRS_Class", "EQRS_11_Class", "EQRS_12_Class", "EQRS_2_Class", "EQRS_3_Class", "C_Class", "C_11_Class", "C_12_Class", "C_2_Class", "C_3_Class") %in% colnames(plot_dat), message = 'Check that columns "EQRS_Class", "EQRS_11_Class", "EQRS_12_Class", "EQRS_2_Class", "EQRS_3_Class", "C_Class", "C_11_Class", "C_12_Class", "C_2_Class", "C_3_Class" are present within the data')
        # )
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
        
        label_text <- make_hovertext_content(plot_data = plot_dat, output = type, category = category, var = var)
          
        
        leaflet_map <- 
          leaflet(plot_dat) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
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
        
        label_text <- make_hovertext_content(plot_data = plot_dat, output = type, category = category, var = var)
          
        leaflet_map <- 
          leaflet(plot_dat) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
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
      cols <- colnames(dat)
      col_names <- mutate(glossary, 
                          display_names = paste0('<span data-toggle="tooltip" title="', description, '">', abbreviation, '</span>'))
    
      datatable(
        data = dt_data(), 
        colnames = col_names$display_names[match(cols, col_names$abbreviation)],
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
        str_remove(file_paths_shared_state$assessment, pattern = "../")
      },
      content = function(file) {
        file.copy(file_paths_shared_state$assessment, file)
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
          ), accordion_panel(title = "Table",
              card(style = paste0("height: ", 600, "px;"),
                   full_screen = T,
                   #card_header("Table", class = "bg-primary"),
                   DTOutput(outputId = ns("assessmentTable")) %>% withSpinner()
              )
          )
        )
      )
    })
    
    
    
    
  }
  )}