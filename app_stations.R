# Define UI for the module
moduleStationsUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(fg = "black", 
                   sidebar = bslib::sidebar(width = "300", fg = "black", open = TRUE,
                                            selectInput(
                                              inputId = ns("assessmentSelect"),
                                              label = "Select Assessment Period:",
                                              choices = c("Select assessment" = "", assessment_periods),
                                              selected = ""
                                            ),
                                            uiOutput(ns("typeSelect")),
                                            shiny::downloadButton(ns("downloadStations"), "Download Station Samples (Wait 5s)")
                                            ), 
      card(style = paste0("height: ", 85, "vh;"),
        full_screen = T,
        card_body(leaflet::leafletOutput(ns("map")),
                  padding = 0)
      )
  )
)
}

# Define server logic for the module
moduleStationsServer <- function(id, shared_state, station_configuration, run_assessment) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      shinyjs::toggleState(
        ns("assessmentSelect"),
        condition = !isTRUE(shared_state$assessment_running)
      )
    })
    
    observeEvent(input$assessmentSelect, {
      if (isTRUE(shared_state$assessment_running)) {
        return()
      }
      assessment <- input$assessmentSelect
      if (!is.null(assessment) && assessment != "") {
        run_assessment(assessment)
        shared_state$assessment <- assessment
      }
    }, ignoreInit = TRUE)
    
    # When shared_state$assessment changes (from other modules), update this module's assessmentSelect
    observeEvent(shared_state$assessment, {
      selected <- if (is.null(shared_state$assessment)) "" else shared_state$assessment
      if (!identical(input$assessmentSelect, selected)) {
        updateSelectInput(session, "assessmentSelect", selected = selected)
      }
    })
    
    type_names <- c("Bottle" = "BOT", "CTD" ="CTD", "Pump" = "PMP", "SUR" = "SUR")
    
    types <- reactive({
      required_station_types <- type_names[type_names %in% names(station_configuration[[input$assessmentSelect]])]
      lookup <- data.frame(input = required_station_types, id = 1:length(required_station_types))
      list(required_station_types = required_station_types, lookup = lookup)
      })
    
    output$typeSelect <- renderUI({
      req(types)
      shiny::radioButtons(inputId = ns("stationType"),
                          "Select data type",
                          choices = types()$required_station_types)
    })
    
    file_paths <- reactive({
      if(!is.null(shared_state$assessment)){
        paste0("./data/",shared_state$assessment, rep("/output/Stations", 3), type_names, ".csv.gz")
        }
      })
    
    req_file <- reactive({
      req(types)
      req(input$stationType)
        val <- types()$lookup[types()$lookup$input == input$stationType, "id"]
        file_name <- file_paths()[val]
    })
    
    station_data <- reactive({
      if(!is.null(shared_state$assessment)){
      dat <- fread(req_file(), select = c("Longitude..degrees_east.", "Latitude..degrees_north."))
      }
    }) %>% bindCache(shared_state$assessment, input$stationType)
    
    output$map <- renderLeaflet({
      if(!is.null(shared_state$assessment)){
        station_samples_sf <- sf::st_as_sf(station_data(), coords = c("Longitude..degrees_east.", "Latitude..degrees_north."))

        view_centre <- get_centroid(station_samples_sf)
        map <- leaflet(options = leafletOptions(
                         attributionControl=FALSE)) %>% 
          enableTileCaching() %>%
          addProviderTiles(providers$Esri.WorldImagery,
                           options= list(tileOptions = tileOptions(useCache=TRUE,crossOrigin=TRUE),
                                         minZoom=2)) %>% 
          setView(lng = view_centre$long, lat = view_centre$lat, zoom = 3) %>% 
          addCircleMarkers(lng = ~ Longitude..degrees_east., lat = ~ Latitude..degrees_north., data = station_data(),
                           clusterOptions = markerClusterOptions())
        
      map
      }
    }) %>% bindCache(shared_state$assessment, input$stationType)
    
    output$downloadStations <- shiny::downloadHandler(
      filename = function () {
        url <- station_configuration[[input$assessmentSelect]][[input$stationType]]
        filename <- basename(url)
      },
      content = function(file, config) {
        withProgress(message = paste("Fetching", input$stationType, "station sample data for ", input$assessmentSelect), value = 0, {
          incProgress(0.2, detail = "This can take up to 1 minute...")
        td <- tempfile("temp_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
        dest <- file.path(td, "stationdata")
        url <- station_configuration[[input$assessmentSelect]][[input$stationType]]
        h <- curl::new_handle()
        curl::handle_setopt(h, connecttimeout = 30, timeout = 60)
        dat <- curl::curl_download(url, destfile = dest, quiet = TRUE, handle = h)
        file.copy(dat, file)
        })
      }
    )
  })
}