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
    } else if(selector == "dropdown")
      shiny::selectInput(Inputid, uiOutput, choices = directory_names)
  })
}

make_hovertext_content <- function(plot_data, output, category, var){

  stopifnot(output %in% c("EQRS", "C"))
  stopifnot(category %in% c(0, 11, 12, 2, 3))
  
  if(output == "EQRS"){
    if(category == 0){
      
      label_text <- with(plot_data, paste0(
        "<b>", get("Description"), " (", get("Code"),")", ":</b><br>",
        "EQRS Class: ", get("EQRS_Class"), "</b><br>",
        "EQRS", ": ", get("EQRS"), "</b><br>",
        "EQR", ": ", get("EQR"), "</b><br>",
        "N E", ": ", get("NE")
      ))
      
    } else {
      
      label_text <- with(plot_data, paste0(
        "<b>", get("Description"), " (", get("Code"),")", ":</b><br>",
        "EQRS_", var(), "_Class: ", get(paste0("EQRS_", var(), "_Class")), "</b><br>",
        "EQRS_", var(), ": ", get(paste0("EQRS_", var())), "</b><br>",
        "EQR_", var(), ": ", get(paste0("EQR_", var())), "</b><br>",
        "N_", var(), ": ", get(paste0("N_", var()))
      ))
    }
  } else {
    if(category == 0){
      label_text <- with(plot_data, paste0(
        "<b>", get("Description"), " (", get("Code"),")", ":</b><br>",
        "C_Class: ", get("C_Class"), "</b><br>",
        "C", ": ", get("C"), "</b><br>",
        "N_C", ": ", get("NC"), "</b><br>"
      ))
      
    } else {
      label_text <- with(plot_data, paste0(
        "<b>", get("Description"), " (", get("Code"),")", ":</b><br>",
        "C_", var(), "_Class: ", get(paste0("C_", var(), "_Class")), "</b><br>",
        "C_", var(), ": ", get(paste0("C_", var())), "</b><br>",
        "N_", var(), ": ", get(paste0("N_", var()))
      ))
    }
  }
}
