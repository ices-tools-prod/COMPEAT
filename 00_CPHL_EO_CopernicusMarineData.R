# Download and process daily satellite data from Copernicus Marine Service
#
# https://data.marine.copernicus.eu/product/OCEANCOLOUR_ATL_BGC_L3_MY_009_113/services
#
# cmems_obs-oc_atl_bgc-plankton_my_l3-multi-1km_P1D
# 
# Assessment unit bounding box xmin: -16.07389 ymin: 34.87719 xmax: 12.67383 ymax: 63.88748
# 
# Download daily cmems_obs-oc_atl_bgc-optics_myint_l3-multi-1km_P1D from Copernicus Marine Service
# 

filePath <- "C:/Users/Hjalte/Downloads/CopernicusMarine"

# Download using Copernicus Marine Toolbox - Executable ------------------------

#copernicusmarine login

path_copernicusmarine <- "C:/Users/Hjalte/Downloads/CopernicusMarine/copernicusmarine.exe"

date_start <- as.Date("2025-03-01")
date_end <- as.Date("2025-09-30")

dates <- as.character(seq(date_start, date_end))

for (date in dates) {
  command <- paste(path_copernicusmarine, "subset",
                   "--dataset-id cmems_obs-oc_atl_bgc-plankton_my_l3-multi-1km_P1D",
                   "--variable CHL",
                   "--start-datetime ", date,
                   "--end-datetime ", date,
                   "--minimum-longitude -16.07389",
                   "--maximum-longitude 12.67383",
                   "--minimum-latitude 34.87719",
                   "--maximum-latitude 63.88748",
                   "--coordinates-selection-method strict-inside",
                   "--netcdf-compression-level 4",
                   "--disable-progress-bar",
                   "--log-level ERROR",
                   "-o C:/Users/Hjalte/Downloads/CopernicusMarine/nc/",
                   sep = " ")
  system(command)
}

# Extract data -----------------------------------------------------------------
if (length(list.files(file.path(filePath, "csv"))) == 0) {
  filenames <- list.files(path = file.path(filePath, "nc"), pattern = "*.nc", full.names = TRUE)
  
  dt1 <- lapply(filenames, function(filename) {
    # Extract date string
    datestring <- regmatches(filename, regexpr("[0-9]{4}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])", filename))

    # Extract year, month and day
    year <- substring(datestring, 1, 4) %>% as.numeric()
    month <- substring(datestring, 6, 7) %>% as.numeric()
    day <- substring(datestring, 9, 10) %>% as.numeric()

    #if(year >= 2015 && year <= 2020 && month >= 3 && month <= 9) {
      # Open netcdf file
      nc <- nc_open(filename)
      
      # Read variables
      lon <- ncvar_get(nc, "longitude")
      lat <- ncvar_get(nc, "latitude")
      chl <- ncvar_get(nc, "CHL")
      
      # Read fill value
      fillvalue <- ncatt_get(nc, "CHL", "_FillValue")
      
      # Close netcdf file
      nc_close(nc)
      
      # Replace netCDF fill values with NA's
      chl[chl == fillvalue$value] <- NA
      
      # Make data table filtering out NA
      dt <- na.omit(data.table(cbind(year,month,day,expand.grid(lon,lat),as.vector(chl))))
      
      # Change column names
      colnames(dt) <- c("Year","Month","Day","Longitude","Latitude","Chlorophyll")
      
      # Write data table
      fwrite(dt,file.path(filePath, "csv", paste0(datestring, ".csv")))
    #}
  })
}

# Process data -----------------------------------------------------------------
library(data.table)
library(sf)
library(tidyverse)
library(ncdf4)

# Read Units
# units <- st_read(file.path("C:/GitHub/COMPEAT/Input/2015-2020", "AssessmentUnits.csv"))
# 
# Read Grid Units
gridunits <- st_read(file.path("C:/GitHub/COMPEAT/Output_chl_weighted/2015-2020", "gridunits.shp"))

# Define years
years <- c("2021", "2022", "2023", "2024", "2025") #, "2026")
#years <- c("2015", "2016", "2017", "2018", "2019", "2020")
#years <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014")
#years <- c("2001", "2002", "2003", "2004", "2005", "2006")
#years <- c("1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997","1998", "1999", "2000")
#years <- c("1998", "1999", "2000")

dt6 <- lapply(years, function(year) {
  # Get file names for the given year
  filenames <- list.files(path = file.path(filePath, "csv"), pattern = year, full.names = TRUE)
  # Read csv files into list of data tables of each file 
  dt1 <- lapply(filenames, function(filename) { fread(filename) })
  # Combine list of data tables into one
  dt2 <- rbindlist(c(dt1))
  # Extract Longitude/Latitude pairs    
  dt3 <- dt2[, .N, keyby = .(Longitude, Latitude)] %>% setkey(Longitude, Latitude)
  # Make positions spatial keeping original latitude/longitude
  dt3 <- st_as_sf(dt3, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)
  # Project into ETRS 1989 LAEA (3035) projection
  dt3 <- st_transform(dt3, crs = 3035)
  # Classify positions (lat/lon) into grid units
  dt3 <- st_join(dt3, gridunits, join = st_intersects, left = FALSE)
  # Remove spatial column in order to merge station samples
  dt3 <- st_set_geometry(dt3, NULL)
  # Merge grid units back in
  dt4 <- dt2[as.data.table(dt3)[, .(Longitude, Latitude, UnitID, GridID, GridArea)], on = .(Longitude, Latitude)]
  # Clean up
  rm(dt2, dt3)
  # UnitID, Period, ES, SD, N, NM
  dt5 <- dt4[, .(ES = mean(Chlorophyll), SD = sd(Chlorophyll), N = .N, NM = uniqueN(Month)), keyby = .(UnitID, Period = Year)]
  # UnitID, Period, GridID, GridArea - Find grid area sampled per unit per indicator
  a <- dt4[, .N, by = .(UnitID, GridID, GridArea)] # UnitGrids
  b <- a[, .(GridArea = sum(as.numeric(GridArea))), keyby = .(UnitID)] # GridArea
  #c <- as.data.table(units)[, .(UnitArea = as.numeric(UnitArea)), keyby = .(UnitID)] # UnitArea
  #d <- c[b, on = .(UnitID = UnitID)] # UnitAreas ~ GridAreas
  dt5 <- merge(dt5, b, by = c("UnitID"), all.x = TRUE)
})

# Combine annual results into one
dt7 <- rbindlist(dt6)

# Write data table
fwrite(dt7, file.path(filePath, "Indicator_CPHL_EO_02_2021-2026.csv"))
#fwrite(dt7, file.path(filePath, "Indicator_CPHL_EO_02_2015-2020.csv"))
#fwrite(dt7, file.path(filePath, "Indicator_CPHL_EO_02_2006-2014.csv"))
#fwrite(dt7, file.path(filePath, "Indicator_CPHL_EO_02_2001-2006.csv"))
#fwrite(dt7, file.path(filePath, "Indicator_CPHL_EO_02_1990-2000.csv"))
