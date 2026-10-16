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
      uiOutput(ns("main_panel"))
  )
)
}

# Define server logic for the module
moduleStationsServer <- function(id, shared_state, station_configuration, run_assessment) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      shinyjs::toggleState(
        "downloadStations",
        condition = !is.null(shared_state$assessment)
      )
      shinyjs::toggleState(
        "assessmentSelect",
        condition = !isTRUE(shared_state$assessment_running) &&
          !isTRUE(shared_state$assessment_loading)
      )
    })
    
    observeEvent(input$assessmentSelect, {
      if (isTRUE(shared_state$assessment_running) ||
          isTRUE(shared_state$assessment_loading)) {
        updateSelectInput(session, "assessmentSelect",
                          selected = shared_state$requested_assessment)
        return()
      }
      assessment <- input$assessmentSelect
      if (!identical(assessment, shared_state$requested_assessment)) {
        run_assessment(assessment)
      }
    }, ignoreInit = TRUE)
    
    observeEvent(shared_state$requested_assessment, {
      selected <- if (is.null(shared_state$requested_assessment)) "" else shared_state$requested_assessment
      if (!identical(input$assessmentSelect, selected)) {
        updateSelectInput(session, "assessmentSelect", selected = selected)
      }
    }, ignoreNULL = FALSE)
    
    type_names <- c("Bottle" = "BOT", "CTD" ="CTD", "Pump" = "PMP", "SUR" = "SUR")
    
    types <- reactive({
      req(shared_state$assessment)
      required_station_types <- type_names[type_names %in% names(station_configuration[[shared_state$assessment]])]
      lookup <- data.frame(input = required_station_types, id = match(required_station_types, type_names))
      list(required_station_types = required_station_types, lookup = lookup)
      })

    output$main_panel <- renderUI({
      if (is.null(shared_state$assessment)) return(NULL)
      bslib::card(
        style = "height: 85vh;",
        full_screen = TRUE,
        bslib::card_body(leaflet::leafletOutput(ns("map")), padding = 0)
      )
    })
    
    output$typeSelect <- renderUI({
      req(types())
      shiny::radioButtons(inputId = ns("stationType"),
                          "Select data type",
                          choices = types()$required_station_types)
    })
    
    file_paths <- reactive({
      if(!is.null(shared_state$assessment)){
        file.path(app_root, "data", shared_state$assessment, "output", paste0("Stations", type_names, ".csv.gz"))
      }
    })
    
    req_file <- reactive({
      req(types())
      req(input$stationType)
        val <- types()$lookup[types()$lookup$input == input$stationType, "id"]
        req(length(val) == 1L)
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
        req(shared_state$assessment, input$stationType)
        url <- station_configuration[[shared_state$assessment]][[input$stationType]]
        filename <- basename(url)
      },
      content = function(file, config) {
        req(shared_state$assessment, input$stationType)
        withProgress(message = paste("Fetching", input$stationType, "station sample data for ", shared_state$assessment), value = 0, {
          incProgress(0.2, detail = "This can take up to 1 minute...")
        td <- tempfile("temp_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
        dest <- file.path(td, "stationdata")
        url <- station_configuration[[shared_state$assessment]][[input$stationType]]
        h <- curl::new_handle()
        curl::handle_setopt(h, connecttimeout = 30, timeout = 60)
        dat <- curl::curl_download(url, destfile = dest, quiet = TRUE, handle = h)
        file.copy(dat, file)
        })
      }
    )
  })
}