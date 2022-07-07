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
output$sat <- renderPlot({
  indicatorID == 4
  # indicatorID <- indicators[i, IndicatorID]
  # indicatorCode <- indicators[i, Code]
  # indicatorName <- indicators[i, Name]
  # indicatorUnit <- indicators[i, Units]
  # indicatorYearMin <- indicators[i, YearMin]
  # indicatorYearMax <- indicators[i, YearMax]
  # indicatorMonthMin <- indicators[i, MonthMin]
  # indicatorMonthMax <- indicators[i, MonthMax]
  # indicatorDepthMin <- indicators[i, DepthMin]
  # indicatorDepthMax <- indicators[i, DepthMax]
  # indicatorYearMin <- indicators[i, YearMin]
  # indicatorMetric <- indicators[i, Metric]
  # unitID <- as.data.table(units)[j, UnitID]
  # unitCode <- as.data.table(units)[j, Code]
  # unitName <- as.data.table(units)[j, Description]
  # 
  title <- paste0("Eutrophication State  ", indicatorYearMin, "-", indicatorYearMax)
  subtitle <- paste0("Oxygen Saturation", " (", "O2SAT", ")", " in ", " (", input$area[1], ")", "\n")
  subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
  subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
  subtitle <- paste0(subtitle, "Metric: ", "Deepest samples within 10m of seabed", ", ")
  subtitle <- paste0(subtitle, "Unit: ", "% saturation")
  

  wk <- wk1[wk1$IndicatorID == indicatorID & wk1$Code == input$area[1],]
  
  
  
  ggplot(wk, aes(x = factor(Period, levels = indicatorYearMin:indicatorYearMax), y = ES)) +
    labs(title = title , subtitle = subtitle) +
    geom_boxplot() +
    geom_jitter() +
    #geom_text(aes(label = N), vjust = -0.25, hjust = -0.25) +
    #geom_errorbar(aes(ymin = ES - CI, ymax = ES + CI), width = .2) +
    scale_x_discrete(NULL, factor(indicatorYearMin:indicatorYearMax), drop=FALSE) +
    scale_y_continuous(NULL)
      
})
output$eqrs_map <- renderLeaflet({
  EQRS_Class_colors <- c("#3BB300", "#99FF66", "#FFCABF", "#FF8066", "#FF0000")
  EQRS_Class_limits <- c("High", "Good", "Moderate", "Poor", "Bad")
  EQRS_Class_labels <- c(">= 0.8 - 1.0 (High)", ">= 0.6 - 0.8 (Good)", ">= 0.4 - 0.6 (Moderate)", ">= 0.2 - 0.4 (Poor)", ">= 0.0 - 0.2 (Bad)")
  #pal <- colorNumeric(palette = oceColorsChlorophyll(255), domain = c(input$range[1],input$range[2]), na.color = "#00000000")
  EQRSpal <- colorFactor(EQRS_Class_colors, levels = c("High", "Good", "Moderate", "Poor", "Bad"), na.color = "#00000000")
  wk5 <- as.data.table(wk5)
  wk <- wk5[IndicatorID == 4] %>% setkey(UnitID)
  setkey(units, UnitID)
  wk <- merge(units, wk, all.x = TRUE) 
  leaflet(wk) %>%
    addTiles() %>%
    #addPolygons(data = COMP4_areas_update, fillColor = ~pal(chl_mean), color = "black", fillOpacity = 1, weight = 1, group = "Eutrophication State", popup = paste0("Mean chl = ", as.character(round(COMP4_areas_update$chl_mean,2)), ";  No. data points = ", as.character(COMP4_areas_update$chl_count))) %>%
    addPolygons(data = wk, fillColor = ~EQRSpal(EQRS_Class), color = "black", fillOpacity = 1, weight = 1, group = "EQRS", popup = wk$EQRS_Class) %>%
    #addLegend("bottomright", pal = pal, values = c(input$range[1],input$range[2]), title = paste0("Chl (mg/l)"), group = "Eutrophication State") %>%
    #addLegend("bottomright", pal = EQRSpal, values = ~EQRS_Class, title = "EQRS Class")
    addLegend("bottomright", colors = EQRS_Class_colors, labels = EQRS_Class_labels, title = "EQRS Class", group = "EQRS") %>% # this is a nasty manual fix to get the legend values drawn in the correct order
    # addLayersControl(
    #   overlayGroups = c("Eutrophication State", "EQRS"), position = c("bottomleft"),
    #   options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
    # ) %>%
    # hideGroup("EQRS") %>% 
    setView(3,55, zoom = 5)
})
# output$eqrs_map <- renderPlot({
#   # Status map (EQRS)
#   EQRS_Class_colors <- c("#3BB300", "#99FF66", "#FFCABF", "#FF8066", "#FF0000")
#   EQRS_Class_limits <- c("High", "Good", "Moderate", "Poor", "Bad")
#   EQRS_Class_labels <- c(">= 0.8 - 1.0 (High)", ">= 0.6 - 0.8 (Good)", ">= 0.4 - 0.6 (Moderate)", ">= 0.2 - 0.4 (Poor)", ">= 0.0 - 0.2 (Bad)")
#   title <- paste0("Eutrophication Status ", indicatorYearMin, "-", indicatorYearMax)
#   subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
#   subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
#   subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
#   subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
#   #fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_EQRS", ".png"))
#   wk5 <- as.data.table(wk5)
#   wk <- wk5[IndicatorID == 4] %>% setkey(UnitID)
#   setkey(units, UnitID)
#   wk <- merge(units, wk, all.x = TRUE) 
#   ggplot(wk) +
#     labs(title = title , subtitle = subtitle) +
#     geom_sf(aes(fill = EQRS_Class)) +
#     scale_fill_manual(name = "EQRS", values = EQRS_Class_colors, limits = EQRS_Class_limits, labels = EQRS_Class_labels)
#   #ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)
# })

output$mk <- renderTable({
  mk[mk$UnitID== input$area,]
})

})
