# Create a color palette with handmade bins.
eqrs_palette <- c("#3BB300", "#99FF66", "#FFCABF", "#FF8066", "#FF0000")
eqrs_bins <- c(1.0,0.8,0.6,0.4,0.2,0.0)
eqrs_labels <- c(">= 0.8 - 1.0 (High)", ">= 0.6 - 0.8 (Good)", ">= 0.4 - 0.6 (Moderate)", ">= 0.2 - 0.4 (Poor)", ">= 0.0 - 0.2 (Bad)")

# eqrs_color_bin <- reactive({
#   if (input$category == 0) {
#     colorBin(palette = eqrs_palette, domain = wk$EQRS, bins = eqrs_bins, reverse = TRUE)
#   }
#   if (input$category == 11) {
#     colorBin(palette = eqrs_palette, domain = wk$EQRS_11, bins = eqrs_bins, reverse = TRUE)
#   }
# })

c_palette <- c("#3BB300", "#FFCABF", "#FF0000")
c_bins <- c(100, 75, 50, 0)
c_labels <- c(">= 75 % (High)", "50 - 74 % (Moderate)", "< 50 % (Low)")


get_centroid <- function(sf_object) {

  bounding <- sf::st_bbox(sf_object)
  centroid <- sf::st_as_sfc(bounding) %>% sf::st_centroid()
  long <- centroid[[1]][1]
  lat <- centroid[[1]][2]
  data.frame(long = long, lat = lat)
  
  
}
