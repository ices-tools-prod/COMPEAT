library(sf)
library(data.table)
library(ncdf4)
library(tidyverse)
library(R.utils)

# Read OSPAR assessment units
OSPAR_Assessment_Units <- st_read("MSSQL:server=SQL09;database=OceanCOMPEAT_20152020_COMP4;trusted_connection=yes;", layer = "AssessmentUnit")
st_crs(OSPAR_Assessment_Units) <- 4326

# Drop columns except ID i.e. Code
OSPAR_Assessment_Units <- OSPAR_Assessment_Units[1]

# Correct column names
colnames(OSPAR_Assessment_Units)[1] <- "AssessmentUnitCode"

# Change projection
OSPAR_Assessment_Units <- st_transform(OSPAR_Assessment_Units, crs = 3035)

# Get netcdf files
path <- "D:\\COMPEAT\\CPHL_EO_ARGANS\\OSPARIV\\modis\\"
filenames <- list.files(path = path, pattern = "*.nc.gz", full.names = TRUE)

# Loop netcdf files
dt1 <- lapply(filenames, function(filename) {
  # Extract date string
  datestring <- regmatches(filename, regexpr("[0-9]{8}", filename))  
  
  # Extract year, month and day
  year <- substring(datestring,1,4) %>% as.numeric()
  month <- substring(datestring,5,6) %>% as.numeric()
  day <- substring(datestring,7,8) %>% as.numeric()
  
  if(year >= 2015 && year <= 2020 && month >= 3 && month <= 10) {
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
    dt <- na.omit(data.table(cbind(year,month,day,expand.grid(lon,lat),as.vector(chl))))
    
    # Change column names
    colnames(dt) <- c("Year","Month","Day","Longitude","Latitude","Chlorophyll")
    
    # Make positions spatial keeping original latitude/longitude
    dt <- sf::st_as_sf(dt, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)
    
    # Project into ETRS 1989 LAEA (3035) projection
    dt <- sf::st_transform(dt, crs = 3035)
    
    # Classify positions (lat/lon) into 20km grid cells
    #dt <- st_join(dt, OSPAR_20km_Grid,join = st_intersects,left = FALSE)
    
    # Classify positions (lat/lon) into assessment units
    dt <- st_join(dt, OSPAR_Assessment_Units,join = st_intersects,left = FALSE)
    
    # Remove spatial column in order to merge station samples
    dt <- st_set_geometry(dt, NULL)
    
    # Write data table
    fwrite(dt,paste0("D:\\COMPEAT\\CPHL_EO_ARGANS\\OSPARIV\\csv\\",datestring,".csv"))
  }
})

# Read csv files into data table
path <- "D:\\COMPEAT\\CPHL_EO_ARGANS\\OSPARIV\\csv\\"

years <- c("2015", "2016", "2017", "2018", "2019", "2020")

dt4 <- lapply(years, function(year) {
  # Get file names for the given year
  filenames <- list.files(path = path, pattern=paste0(year, "[0-9]{4}"), full.names = TRUE)
  # Read csv files into list of data tables of each file 
  dt1 <- lapply(filenames, function(filename) { fread(filename) })
  # Combine list of data tables into one
  dt2 <- rbindlist(dt1)
  # Calculate indicator
  dt3 <- dt2[, list(Mean = mean(Chlorophyll), STD = sd(Chlorophyll), .N), list(AssessmentUnitCode,Year)]
})
# Combine annual results into one
dt5 <- rbindlist(dt4)

# Write data table
fwrite(dt5,paste0("D:\\COMPEAT\\Indicator_CPHL_EO_ARGANS_02.csv"))
