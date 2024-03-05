# Define UI for the module
moduleStationsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel = sidebarPanel(width = 2,
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
moduleStationsServer <- function(id, assessment) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    type_names <- c("BOT", "CTD", "PMP")
    lookup <- data.frame(input = type_names, id = 1:3)
    
    file_paths <- reactive({
      if(!is.null(assessment())){
        paste0("./Data/",assessment(), rep("/Stations",3), type_names, ".csv.gz")
        }
      })
    
    
    req_file <- shiny::reactive({
        val <- lookup[lookup$input == input$stationType, "id"]
        file_name <- file_paths()[val]
    })
    
    
    station_data <- reactive({
      if(!is.null(assessment())){
      dat <- fread(req_file())
      }
    })
    
    
    output$map <- renderLeaflet({
      if(!is.null(assessment())){
        station_samples_sf <- sf::st_as_sf(station_data(), coords = c("Longitude..degrees_east.", "Latitude..degrees_north."))

        view_centre <- get_centroid(station_samples_sf)
        map <- leaflet() %>% 
          addProviderTiles(providers$Esri.WorldImagery) %>%
          setView(lng = view_centre$long, lat = view_centre$lat, zoom = 3) %>% 
          addCircleMarkers(lng = ~ Longitude..degrees_east., lat = ~ Latitude..degrees_north., data = station_data(),
                           clusterOptions = markerClusterOptions())
        
      map
      }
    }) 
    
    
    output$downloadStations <- shiny::downloadHandler(
      
      
      filename = function () {
        stringr::str_remove(req_file(), pattern = "../")
      },
      content = function(file) {
        file.copy(req_file(), file)
      }
    )
    
  })
}