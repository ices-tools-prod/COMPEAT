# Define UI for the module
moduleStationsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        shiny::radioButtons(inputId = ns("stationType"),
                            "Select data type",
                            choices = c("Bottle" = "BOT", "CTD" ="CTD", "Pump" = "PMP")),
        shiny::downloadButton(ns("downloadStations"), "Download")
    ), 
    mainPanel = mainPanel(
      shiny::fluidRow(
        leaflet::leafletOutput(ns("map"))
      )
  ))
)
}

# Define server logic for the module
moduleStationsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    type_names <- c("BOT", "CTD", "PMP")
    file_paths <- paste0("../Data/COMP 4 (2015-2020)/", rep("StationSamples",3), type_names, ".csv") 
    station_data <- purrr::map(.x = file_paths, ~ read.csv(.x))
    names(station_data) <- type_names
    
    
    stat_dat <- reactive({
      dat <- station_data[[input$stationType]]
    })
    
    
    output$map <- renderLeaflet({
        station_samples_sf <- sf::st_as_sf(stat_dat(), coords = c("Longitude..degrees_east.", "Latitude..degrees_north."))

        view_centre <- get_centroid(station_samples_sf)
        map <- leaflet() %>% 
          addProviderTiles(providers$Esri.WorldImagery) %>%
          setView(lng = view_centre$long, lat = view_centre$lat, zoom = 3) %>% 
          addCircleMarkers(lng = ~ Longitude..degrees_east., lat = ~ Latitude..degrees_north., data = stat_dat(),
                           clusterOptions = markerClusterOptions())
        
      map
    }) #%>% bindCache(input$stationType)
    
    download_names <- reactive({

      lookup <- data.frame(input = type_names, id = 1:3)
      val <- lookup[lookup$input == input$stationType, "id"]
      download_file <- file_paths[val]
  
    })
    
    output$downloadStations <- shiny::downloadHandler(
      
      filename = stringr::str_remove(download_names(), pattern = "../"),
      content = function(file) {
        file.copy(download_names(), file)
    })
    
  })
}