i = 4


  indicatorID <- indicators[i, IndicatorID]
  criteriaID <- indicators[i, CategoryID]
  name <- indicators[i, Name]
  year.min <- indicators[i, YearMin]
  year.max <- indicators[i, YearMax]
  month.min <- indicators[i, MonthMin]
  month.max <- indicators[i, MonthMax]
  depth.min <- indicators[i, DepthMin]
  depth.max <- indicators[i, DepthMax]
  metric <- indicators[i, Metric]
  response <- indicators[i, Response]
  
  # Copy data
  wk <- as.data.table(stationSamples)  
  
  # Create Period
  wk[, Period := ifelse(month.min > month.max & Month >= month.min, Year + 1, Year)]
  
  # Create Indicator
  
    wk[, ES := O2SAT] # 
    wk[, ESQ := QV.ODV.Dissolved.Oxygen..ml.l.] # probably should include temp and sal quality too

  
  # Filter stations rows and columns --> UnitID, GridID, GridArea, Period, Month, StationID, Depth, Temperature, Salinity, ES
  if (month.min > month.max) {
    wk0 <- wk[
      (Period >= year.min & Period <= year.max) &
        (Month >= month.min | Month <= month.max) &
        (Depth..m. >= depth.min & Depth..m. <= depth.max) &
        !is.na(ES) &
        ESQ <= 1 &
        !is.na(UnitID),
      .(IndicatorID = indicatorID, UnitID, GridSize, GridID, GridArea, Period, Month, StationID, Longitude = Longitude..degrees_east., Latitude = Latitude..degrees_north., DepthToBottom = ifelse(is.na(Bot..Depth..m.), Bathymetric..m., Bot..Depth..m.), Depth = Depth..m., Temperature = Temperature..degC., Salinity = Practical.Salinity..dmnless., ES)]
  } else {
    wk0 <- wk[
      (Period >= year.min & Period <= year.max) &
        (Month >= month.min & Month <= month.max) &
        (Depth..m. >= depth.min & Depth..m. <= depth.max) &
        !is.na(ES) &
        ESQ <= 1 &
        !is.na(UnitID),
      .(IndicatorID = indicatorID, UnitID, GridSize, GridID, GridArea, Period, Month, StationID, Longitude = Longitude..degrees_east., Latitude = Latitude..degrees_north., DepthToBottom = ifelse(is.na(Bot..Depth..m.), Bathymetric..m., Bot..Depth..m.), Depth = Depth..m., Temperature = Temperature..degC., Salinity = Practical.Salinity..dmnless., ES)]
  }
  

  
  if (metric == '5th percentile of deepest sample within 10 meters from bottom') {
    # Select deepest sample at each station within 10 meters from bottom
    wk1 <- wk0[wk0[, .I[Depth == max(Depth) & DepthToBottom - Depth <= 10], by = StationID]$V1]
    wk1 <- wk1[, .(ES = ES, SD = sd(ES), N = .N), keyby = .(IndicatorID, UnitID, GridID, GridArea, Period, Month, StationID)]
    
    # Calculate annual 5th percentile --> UnitID, Period, ES, SD, N, NM
    #wk2 <- wk1[, .(ES = quantile(ES, 0.05, na.rm = TRUE), SD = sd(ES), N = .N, NM = uniqueN(Month)), keyby = .(IndicatorID, UnitID, GridID, Period)]
  }




#ggplot(wk1, aes(x=as.factor(Period), y=ES)) + 
  geom_boxplot()

#ggplot(wk1, aes(x=as.factor(UnitID), y=ES)) + 
  geom_boxplot()


# Create Annual Indicator bar charts

  indicatorID <- indicators[i, IndicatorID]
  indicatorCode <- indicators[i, Code]
  indicatorName <- indicators[i, Name]
  indicatorUnit <- indicators[i, Units]
  indicatorYearMin <- indicators[i, YearMin]
  indicatorYearMax <- indicators[i, YearMax]
  indicatorMonthMin <- indicators[i, MonthMin]
  indicatorMonthMax <- indicators[i, MonthMax]
  indicatorDepthMin <- indicators[i, DepthMin]
  indicatorDepthMax <- indicators[i, DepthMax]
  indicatorYearMin <- indicators[i, YearMin]
  indicatorMetric <- indicators[i, Metric]
  for (j in 1:nrow(units)) {
    unitID <- as.data.table(units)[j, UnitID]
    unitCode <- as.data.table(units)[j, Code]
    unitName <- as.data.table(units)[j, Description]
    
    title <- paste0("Eutrophication State ", indicatorYearMin, "-", indicatorYearMax)
    subtitle <- paste0("Oxygen Saturation", " (", "O2SAT", ")", " in ", unitName, " (", unitCode, ")", "\n")
    subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
    subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
    subtitle <- paste0(subtitle, "Metric: ", "Deepest samples within 10m of seabed", ", ")
    subtitle <- paste0(subtitle, "Unit: ", "% saturation")
    fileName <- gsub(":", "", paste0("Annual_Indicator_Bar_", "O2SAT", "_", unitCode, ".png"))
    
    wk <- wk1[IndicatorID == indicatorID & UnitID == unitID]
    

    if (nrow(wk) > 0 & indicatorMetric %in% c("Minimum", "5th percentile", "5th percentile of deepest sample within 10 meters from bottom", "10th percentile", "90th percentile")) {
      ggplot(wk, aes(x = factor(Period, levels = indicatorYearMin:indicatorYearMax), y = ES)) +
        labs(title = title , subtitle = subtitle) +
        geom_boxplot() +
        geom_jitter() +
        #geom_text(aes(label = N), vjust = -0.25, hjust = -0.25) +
        #geom_errorbar(aes(ymin = ES - CI, ymax = ES + CI), width = .2) +
        scale_x_discrete(NULL, factor(indicatorYearMin:indicatorYearMax), drop=FALSE) +
        scale_y_continuous(NULL)
      
      ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)
    }
  }
  
wk1 <- merge(st_drop_geometry(units[1:4]), wk1, by = "UnitID")
fwrite(wk1, file = file.path(outputPath, "O2SAT.csv"))
