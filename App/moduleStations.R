# Define UI for the module
moduleStationsUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(fg = "black", 
                   sidebar = bslib::sidebar(width = "15vw", fg = "black", open = T,
                                            selectInput(
                                              inputId = ns("assessmentSelect"),
                                              label = "Select Assessment Period:",
                                              choices = list.dirs("./Data", recursive = FALSE, full.names = FALSE) %>% sort(decreasing = TRUE),
                                              selected = NULL  # Initially NULL; server will set the default
                                            ),
                                            uiOutput(ns("typeSelect")),
                                            # shiny::radioButtons(inputId = ns("stationType"),
                                            #                     "Select data type",
                                            #                     choices = c("Bottle" = "BOT", "CTD" ="CTD", "Pump" = "PMP")),
                                            shiny::downloadButton(ns("downloadStations"), "Download Station Samples (Wait 5s)") %>% withSpinner()
                                            ), 
      card(style = paste0("height: ", 85, "vh;"),
        full_screen = T,
        leaflet::leafletOutput(ns("map"))
      )
  )
)
}

# Define server logic for the module
moduleStationsServer <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    type_names <- c("Bottle" = "BOT", "CTD" ="CTD", "Pump" = "PMP", "SUR" = "SUR")
    
    types <- reactive({
      required_station_types <- type_names[type_names %in% names(configuration[[input$assessmentSelect]])]
      lookup <- data.frame(input = required_station_types, id = 1:length(required_station_types))
      list(required_station_types = required_station_types,
           lookup = lookup)
    }
      
    )
    output$typeSelect <- renderUI({
      req(types)
      shiny::radioButtons(inputId = ns("stationType"),
                          "Select data type",
                          choices = types()$required_station_types)
    })
    
    file_paths <- reactive({
      if(!is.null(shared_state$assessment)){
        paste0("./Data/",shared_state$assessment, rep("/Stations",3), type_names, ".csv.gz")
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
        map <- leaflet() %>% 
          addProviderTiles(providers$Esri.WorldImagery, options = list(minZoom=3.5)) %>%
          setView(lng = view_centre$long, lat = view_centre$lat, zoom = 3) %>% 
          addCircleMarkers(lng = ~ Longitude..degrees_east., lat = ~ Latitude..degrees_north., data = station_data(),
                           clusterOptions = markerClusterOptions())
        
      map
      }
    }) %>% bindCache(shared_state$assessment, input$stationType)
    
    output$downloadStations <- shiny::downloadHandler(
      filename = function () {
        
        paste0("_Da",input$assessmentSelect,"_StationSamples", input$stationType, ".csv.gz") 
      },
      content = function(file, config) {
        withProgress(message = paste("Fetching", input$stationType, "station sample data for ", input$assessmentSelect), value = 0, {
          incProgress(0.2, detail = "This can take up to 1 minute...")
        td <- tempfile("temp_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
        # dest <- file.path(td, "stationdata")
        assessment <- stringr::str_split_fixed(input$assessmentSelect, " ", n = 3)
        url_patch <- paste0(assessment[2], "/StationSamples",  gsub("\\(|\\)", "", assessment[3]))
        url <- paste0("https://icesoceanography.blob.core.windows.net/compeat/comp",
                      url_patch, input$stationType, "_",
                      configuration[[input$assessmentSelect]][[input$stationType]],
                      ".csv.gz")
        # h <- curl::new_handle()
        # curl::handle_setopt(h, connecttimeout = 30, timeout = 60)
        # dat <- curl::curl_download(url, destfile = dest, quiet = TRUE, handle = h)
        # file.copy(dat, file)
        #zip::zipr(zipfile = file, files = csv_path, root = td)

        dat <- fread(input = url)
        csv_path <- file.path(td, stringr::str_replace(paste0("COMP", url_patch, input$stationType, "_",
                                         configuration[[input$assessmentSelect]][[input$stationType]], ".csv"), pattern = "/", replacement = "_"))
        utils::write.csv(dat, csv_path, row.names = FALSE)
        R.utils::gzip(csv_path, file, ext ="gz")
        })
      }
    )
  })
}