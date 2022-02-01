library(ggplot2)
library(plotly)
library(sf)
library(data.table)
library(readxl)

# Read indicator configuration files -------------------------------------------
assessmentPeriod <- "2015-2020" # COMP4
# Define paths

inputPath <- file.path("../Input/", assessmentPeriod)
configurationFile <- file.path(inputPath, "Configuration2015-2020.xlsx")
indicators <- as.data.table(read_excel(configurationFile, sheet = "Indicators")) %>% setkey(IndicatorID) 

indicatorID <- indicators[4, IndicatorID]
indicatorCode <- indicators[4, Code]
indicatorName <- indicators[4, Name]
indicatorUnit <- indicators[4, Units]
indicatorDepthMin <- indicators[4, DepthMin]
indicatorDepthMax <- indicators[4, DepthMax]
indicatorMetric <- indicators[4, Metric]
indicatorMonthMin <- indicators[4, MonthMin]
indicatorMonthMax <- indicators[4, MonthMax]
indicatorYearMin <- indicators[4, YearMin]
indicatorYearMax <- indicators[4, YearMax]

unitsFile <- file.path(inputPath, "AssessmentUnits.csv")
# Read units from WKT
units <- st_read(unitsFile) %>%
  st_set_crs(4326)
# Remove unnecessary dimensions and convert data.frame to data.table
units <- as.data.table(st_zm(units)) 

# Order, Rename and Remove columns 
units <- units[order(ID), .(Code = ID, Description = LongName, GEOM = geometry)] %>%
  st_sf()

# Assign IDs
units$UnitID = 1:nrow(units)

#Read in annual indicator results for all periods

wk3_COMP1 <- read.csv("../Output_oxy_q05/1990-2000/Annual_Indicator.csv")
wk3_COMP2 <- read.csv("../Output_oxy_q05/2001-2006/Annual_Indicator.csv")
wk3_COMP3 <- read.csv("../Output_oxy_q05/2006-2014/Annual_Indicator.csv")
wk3_COMP3 <- wk3_COMP3[wk3_COMP3$Period != "2006",] #removing 2006 prior to combining to avoid duplicates
wk3_COMP4 <- read.csv("../Output_oxy_q05/2015-2020/Annual_Indicator.csv")
wk3 <- rbind(wk3_COMP1, wk3_COMP2, wk3_COMP3, wk3_COMP4)
wk3 <- merge(wk3, st_drop_geometry(units) %>% dplyr::select(UnitID, UnitCode = Code, UnitName = Description), by = c("UnitID"), all.x = TRUE)

#Read in indicator results for COMP4
wk5 <- read.csv("../Output_oxy_q05/2015-2020/Assessment_Indicator.csv")



