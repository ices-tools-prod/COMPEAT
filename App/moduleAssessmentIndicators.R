# Define UI for the module
moduleAssessmentIndicatorsUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Map",
    tagList(
      layout_sidebar(fg = "black", 
        sidebar = bslib::sidebar(width = "15vw", fg = "black", open = T,
          uiOutput(ns("indicatorSelector")),
          radioButtons(inputId = ns("confidence"),
                       "Select confidence measure:",
                       choices = c("Confidence" = "C_Class",
                                   "Temporal Confidence" = "TC_Clss",
                                   "Spatial Confidence" = "SC_Clss")),
          sliderInput(ns("map_display_size"), label = "Map size (% Screen Height)",min = 0,max = 100,value = 50), 
          checkboxInput(ns("show_legend"), "Show Legend", value = TRUE),
          downloadButton(ns("downloadAssessmentIndicators"), "Download")
        ), 
        uiOutput(ns("main_panel"))
        
      )
  )
  )
}

# Define server logic for the module
moduleAssessmentIndicatorsServer <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shinyselect_from_directory(dir = "./Data", selector = "dropdown", id = "assessment", uiOutput = "Select Assessment Period:", outputid = "assessmentSelect", module = T, output, session)
    
    observeEvent(input$assessment, {
      shared_state$assessment <- input$assessment
    })
    
    file_paths_assessment_indicators <- reactive({
      if(!is.null(shared_state$assessment)){
        paste0("./Data/", shared_state$assessment, "/Assessment_Indicator.csv")
      }
    })

    indicator_data <- reactive({
      if(!is.null(shared_state$assessment)){
        indicators <- fread(paste0("./Data/", shared_state$assessment, "/Annual_Indicator.csv"))
      }
    })
    
    
    output$indicatorSelector <- renderUI({ 
      req(indicator_data())
      indicators <- unique(indicator_data()$Name) %>% sort()
      selectInput(session$ns("indicator"), "Select Indicator:", choices = indicators)
    })
    
   
    output$data <- renderDT({
      req(indicator_data())
      
      dat <-  indicator_data() %>% 
        filter(Name == input$indicator)
      datatable(dat, 
                filter = 'top', 
                extensions = 'FixedColumns', 
                options = list(
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 3)))
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
          leaflet(plot_dat) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
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
          leaflet(plot_dat) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%
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