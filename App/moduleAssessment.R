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
            "Nutrient levels - Nitrogen" = 11,
            "Nutrient levels - Phosphorus" = 12,
            "Direct effects" = 2,
            "Indirect effects" = 3)),
        shiny::downloadButton(ns("downloadAssessmentFile"), "Download")
        ),
      fluidRow(column(width = 6,
        card(height = "35vh", full_screen = T, 
             card_header("Map1",class = "bg-primary"),
          leafletOutput(
            outputId = ns("map1")) %>% withSpinner()),
                      ),
        column(width = 6,
        card(height = "35vh", full_screen = T,
             card_header("Map2", class = "bg-primary"),
             leafletOutput(outputId = ns("map2")) %>% withSpinner()),
      )),
      card(height = "50vh", full_screen = T,
           card_header("Table", class = "bg-primary"),DTOutput(
          outputId = ns("assessmentTable"))
      )
        
      )
    )
  }

# Define server logic for the module
moduleAssessmentServer <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shinyselect_from_directory(dir = "./Data", selector = "dropdown", id = "assessment", uiOutput = "Select Assessment Period:", outputid = "assessmentSelect", module = T, output, session)
    
    observeEvent(input$assessment, {
      shared_state$assessment <- input$assessment
    })
    
    file_paths_assessment <- reactive({
      browser()
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
    
    output$map1 = renderLeaflet({
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
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = plot_dat[[display_col]],
            title = case_when(type == "EQRS" ~ "Eutrophication Status",
                              type == "C" ~ "Confidence Level"),
            opacity = 1
          )
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
          
          #browser()
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
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = plot_dat[[display_col]],
            title = case_when(type == "EQRS" ~ "Eutrophication Status",
                              type == "C" ~ "Confidence Level"),
            opacity = 1
          )
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
    
    
    output$assessmentTable <- renderDT({
      req(!is.null(merged_data()))
      
      datatable(
        data = st_drop_geometry(merged_data()), #[, .(Code, Description, EQRS, EQRS_Class, EQRS_N = NE, C, C_Class, C_N = NC)],
        filter = 'top',
        extensions = 'FixedColumns',
        options = list(
          scrollX = TRUE,
          fixedColumns = list(leftColumns = 2)
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
    
    
    
  }
  )}