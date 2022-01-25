#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
output$ts <- renderPlotly({
  wk <- wk3[wk3$IndicatorID == 4 & wk3$UnitCode %in% input$area,]
  
  # unitID <- st_drop_geometry(units)[Code == u]$UnitID
  # unitCode <- u
  # unitName <- st_drop_geometry(units)[Code == u]$Description
  
  title <- paste0("Eutrophication State [ES] and Threshold [ET] ", 1990, "-", 2020)
  #subtitle <- paste0(indicatorName, " (", indicatorCode, ")", " in ", unitName, " (", unitCode, ")", "\n")
  subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
  subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
  subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, "m, ")
  subtitle <- paste0(subtitle, "Metric: ", indicatorMetric, ", ")
  subtitle <- paste0(subtitle, "Unit: ", indicatorUnit)

  p <- ggplot(wk, aes(x = as.Date(as.character(Period), "%Y"), y = ES, group = UnitCode, color = UnitCode)) +
    labs(title = title , subtitle = subtitle) +
    geom_point() +
    geom_line() +
    #geom_text(aes(label = N), vjust = -0.25, hjust = -0.25) +
    #adding lines for boundaries between high/good/moderate etc
    geom_hline(aes(yintercept = ET)) +
    geom_hline(aes(yintercept = 7.75), linetype = 2) +
    geom_hline(aes(yintercept = 4.25), linetype = 2) +
    geom_hline(aes(yintercept = 2.5), linetype = 2) +
    #labelling high/good/moderate etc
    annotate("text", x = as.Date("2005", "%Y"), y = 5, label = "Moderate", parse = 2, size = 3.5, color = "grey") + 
    annotate("text", x = as.Date("2005", "%Y"), y = 6.75, label = "Good", parse = 2, size = 3.5, color = "grey") +
    annotate("text", x = as.Date("2005", "%Y"), y = 3.5, label = "Poor", parse = 2, size = 3.5, color = "grey") +
    annotate("text", x = as.Date("2005", "%Y"), y = 1.5, label = "Bad", parse = 2, size = 3.5, color = "grey") +
    annotate("text", x = as.Date("2005", "%Y"), y = 8.75, label = "High", parse = 2, size = 3.5, color = "grey") +
    scale_x_date(date_labels = "%Y") +
    xlab("Year") +
    ylab("Oxygen (mg/l)") +
    scale_y_continuous(limits = c(0,9), breaks = c(0,2,4,6,8))
  ggplotly(p) 
})

})
