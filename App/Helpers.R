# Create a color palette with handmade bins.

eqrs_bins <- c(1.0,0.8,0.6,0.4,0.2,0.0)
eqrs_labels <- c("High" = ">= 0.8 - 1.0 (High)", "Good" = ">= 0.6 - 0.8 (Good)", "Moderate" = ">= 0.4 - 0.6 (Moderate)", "Poor" = ">= 0.2 - 0.4 (Poor)", "Bad" = ">= 0.0 - 0.2 (Bad)")
eqrs_palette <- c("High" = "#3BB300", "Good" = "#99FF66", "Moderate" = "#FFCABF", "Poor" = "#FF8066", "Bad" = "#FF0000")
eqrs_levels <- c("High", "Good", "Moderate", "Poor", "Bad")

# eqrs_color_bin <- reactive({
#   if (input$category == 0) {
#     colorBin(palette = eqrs_palette, domain = wk$EQRS, bins = eqrs_bins, reverse = TRUE)
#   }
#   if (input$category == 11) {
#     colorBin(palette = eqrs_palette, domain = wk$EQRS_11, bins = eqrs_bins, reverse = TRUE)
#   }
# })

c_palette <- c("High" = "#3BB300", "Moderate" = "#FFCABF", "Low" = "#FF0000")
c_levels <- c("High", "Moderate", "Low")
c_bins <- c(100, 75, 50, 0)
c_labels <- c("High" = ">= 75 % (High)", "Moderate" = "50 - 74 % (Moderate)", "Low" = "< 50 % (Low)")


get_centroid <- function(sf_object) {

  bounding <- sf::st_bbox(sf_object)
  centroid <- sf::st_as_sfc(bounding) %>% sf::st_centroid()
  long <- centroid[[1]][1]
  lat <- centroid[[1]][2]
  data.frame(long = long, lat = lat)
  
  
}

#' Creates radio buttons for r shiny ui based on directory contents
#'
#' @param dir 
#' @param id 
#' @param output 
#' @param session 
#' @param selector 
#' @param module 
#'
#' @return
#' @export
#'
#' @examples
shinyselect_from_directory <- function(dir, id, outputid, uiOutput, selector = c("radio", "select"), module = F, output, session) {
  
  if(module ==T){
    Inputid <- session$ns(id) 
  } else {
    Inputid <- session$id 
  }
      
  directory_names <- list.dirs(dir, full.names = F, recursive = F) %>% sort(decreasing = TRUE)
  output[[outputid]] <- renderUI({
    if(selector == "radio") {
      shiny::radioButtons(Inputid, uiOutput, choices = directory_names)
    } else if(selector == "select")
      shiny::selectInput(Inputid, uiOutput, choices = directory_names)
  })
}