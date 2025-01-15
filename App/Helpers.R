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


make_assessment_hovertext_content <- function(plot_data, output, category, var) {
  
  stopifnot(output %in% c("EQRS", "C"))
  stopifnot(category %in% c(0, 11, 12, 2, 3))
  
  if (output == "EQRS") {
    if (category == 0) {
      
      label_text <- with(plot_data, paste0(
        # First line outside the table
        "<div><strong>", get("Description"), " (", get("Code"),")<br>Overall Assessment</strong><br><br></div>",
        
        # Start of table with external border and collapsed style
        "<table style='border: 1px solid black; border-collapse: collapse; width: auto;'>",
        
        # Each row has internal borders for each cell
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>EQRS Class</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("EQRS_Class"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>EQRS</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("EQRS"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>EQR</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("EQR"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>N E</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("NE"), "</td></tr>",
        
        "</table>"
      ))
      
    } else {
      var <- var()
      label_text <- with(plot_data, paste0(
        "<div><strong>", get("Description"), " (", get("Code"),")<br>",var, "</strong><br><br></div>",
        "<table style='border: 1px solid black; border-collapse: collapse; width: auto;'>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>EQRS_", var, "_Class</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get(paste0("EQRS_", var, "_Class")), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>EQRS_", var, "</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get(paste0("EQRS_", var)), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>EQR_", var, "</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get(paste0("EQR_", var)), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>N_", var, "</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get(paste0("N_", var)), "</td></tr>",
        
        "</table>"
      ))
    }
    
  } else {
    if (category == 0) {
      
      label_text <- with(plot_data, paste0(
        "<div><strong>", get("Description"), " (", get("Code"),")<br>Overall Assessment</strong><br><br></div>",
        "<table style='border: 1px solid black; border-collapse: collapse; width: auto;'>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>C Class</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("C_Class"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>C</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("C"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>N C</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("NC"), "</td></tr>",
        
        "</table>"
      ))
      
    } else {
      var <- var()
      label_text <- with(plot_data, paste0(
        "<div><strong>", get("Description"), " (", get("Code"),")<br>",var, "</strong><br><br></div>",
        "<table style='border: 1px solid black; border-collapse: collapse; width: auto;'>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>C_", var, "_Class</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get(paste0("C_", var, "_Class")), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>C_", var, "</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get(paste0("C_", var)), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>N_", var, "</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get(paste0("N_", var)), "</td></tr>",
        
        "</table>"
      ))
    }
  }
  
  label_text
}

make_indicator_hovertext_content <- function(plot_data, output, confidence, var) {
  
  stopifnot(output %in% c("EQRS", "C"))
  
  if (output == "EQRS") {
      label_text <- with(plot_data, paste0(
        "<div><strong>", get("UntDscr"), " (", get("UnitCod"),")<br>",var, "</strong><br><br></div>",
        "<table style='border: 1px solid black; border-collapse: collapse; width: auto;'>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>EQRS_Class </b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("EQRS_Cl"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>EQRS </b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("EQRS"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>EQR </b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("EQR"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>N </b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("N"), "</td></tr>",
        
        "</table>"
      ))

  } else {
   
    
    if(confidence == "C_Class") {
      
      
      label_text <- with(plot_data, paste0(
        "<div><strong>", get("UntDscr"), " (", get("UnitCod"),")<br>",var, "</strong><br><br></div>",
        "<table style='border: 1px solid black; border-collapse: collapse; width: auto;'>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>C_Class</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get(confidence), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>C</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("C"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>N</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("N"), "</td></tr>",
        
        "</table>"
      ))
    } else if (confidence == "TC_Clss"){
      
      label_text <- with(plot_data, paste0(
        "<div><strong>", get("UntDscr"), " (", get("UnitCod"),")<br>",var, "</strong><br><br></div>",
        "<table style='border: 1px solid black; border-collapse: collapse; width: auto;'>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>TC_Class</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get(confidence), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>TC</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("TC"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>N</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("N"), "</td></tr>",
        
        "</table>"
      ))
      
    } else if (confidence == "SC_Clss"){
      
      label_text <- with(plot_data, paste0(
        "<div><strong>", get("UntDscr"), " (", get("UnitCod"),")<br>",var, "</strong><br><br></div>",
        "<table style='border: 1px solid black; border-collapse: collapse; width: auto;'>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>SC_Class</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get(confidence), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>SC</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("SC"), "</td></tr>",
        
        "<tr><td style='border: 1px solid black; padding-inline: 5px'><b>N</b></td>
             <td style='border: 1px solid black; padding-inline: 5px'>", get("N"), "</td></tr>",
        
        "</table>"
      ))
      
    
    }
  }
  label_text
}

