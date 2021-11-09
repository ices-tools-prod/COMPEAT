library(data.table)
library(sf)
library(tidyverse)
library(ncdf4)
library(R.utils)

pathARGANS <- "D:/COMPEAT/CPHL_EO_ARGANS/OSPARIV"
pathRBINS <- "D:/COMPEAT/CPHL_EO_RBINS/v1.02"

# Convert ARGANS netCDF into CSV if it hasn't already been done
if (length(list.files(file.path(pathARGANS, "csv"))) == 0) {
  filenames <- list.files(path = file.path(pathARGANS, "modis"), pattern = "*.nc.gz", full.names = TRUE)
  
  dt1 <- lapply(filenames, function(filename) {
    # Extract date string
    datestring <- regmatches(filename, regexpr("[0-9]{8}", filename))  
    
    # Extract year, month and day
    year <- substring(datestring,1,4) %>% as.numeric()
    month <- substring(datestring,5,6) %>% as.numeric()
    day <- substring(datestring,7,8) %>% as.numeric()
    
    if(year >= 2009 && year <= 2020 && month >= 3 && month <= 10) {
      # Open netcdf file
      nc <- nc_open(gunzip(filename, skip = TRUE, remove = FALSE))
      
      # Read variables
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
      chl <- ncvar_get(nc, "chlorophyll-a")
      
      # Read fill value
      fillvalue <- ncatt_get(nc, "chlorophyll-a", "_FillValue")
      
      # Close netcdf file
      nc_close(nc)
      
      # Replace netCDF fill values with NA's
      chl[chl == fillvalue$value] <- NA
      
      # Replace netCDF -99 values with NA's
      chl[chl == -99] <- NA
      
      # Make data table filtering out NA
      dt <- na.omit(data.table(cbind(year, month, day, expand.grid(lon,lat), as.vector(chl))))
      
      # Change column names
      colnames(dt) <- c("Year", "Month", "Day", "Longitude", "Latitude", "Chlorophyll")
      
      # Write data table
      fwrite(dt, file.path(pathARGANS, "csv", paste0(datestring, ".csv")))
    }
  })
}

# Convert RBINS netCDF into CSV if it hasn't already been done
if (length(list.files(file.path(pathRBINS, "csv"))) == 0) {
  filenames <- list.files(path = file.path(pathRBINS, "CHL_daily_2009_2020"), pattern = "*.nc", full.names = TRUE)
  
  dt1 <- lapply(filenames, function(filename) {
    # Extract date string
    datestring <- regmatches(filename, regexpr("[0-9]{8}", filename))
    
    # Extract year, month and day
    year <- substring(datestring,1,4) %>% as.numeric()
    month <- substring(datestring,5,6) %>% as.numeric()
    day <- substring(datestring,7,8) %>% as.numeric()
    
    if(year >= 2009 && year <= 2020 && month >= 3 && month <= 10) {
      # Open netcdf file
      nc <- nc_open(filename)
      
      # Read variables
      lon <- ncvar_get(nc, "lon")
      lat <- ncvar_get(nc, "lat")
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
      fwrite(dt,file.path(pathRBINS, "csv", paste0(datestring, ".csv")))
    }
  })
}

# Read Grid Units
gridunits <- st_read(file.path("Output", "gridunits.shp"))

# Define years
years <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

dt6 <- lapply(years, function(year) {
  # Get file names for the given year
  filenamesARGANS <- list.files(path = file.path(pathARGANS, "csv"), pattern = paste0(year, "[0-9]{4}"), full.names = TRUE)
  filenamesRBINS <- list.files(path = file.path(pathRBINS, "csv"), pattern = paste0(year, "[0-9]{4}"), full.names = TRUE)
  # Read csv files into list of data tables of each file 
  dt1ARGANS <- lapply(filenamesARGANS, function(filename) { fread(filename) })
  dt1RBINS <- lapply(filenamesRBINS, function(filename) { fread(filename) })
  # Combine list of data tables into one
  dt2 <- rbindlist(c(dt1ARGANS, dt1RBINS))
  
  rm(filenamesARGANS, filenamesRBINS , dt1ARGANS, dt1RBINS)
  
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
fwrite(dt7, file.path("D:/COMPEAT", "Indicator_CPHL_EO_02.csv"))
