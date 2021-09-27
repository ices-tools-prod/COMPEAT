# Install and load R packages ---------------------------------------------
# 
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "ncdf4", "data.table")
ipak(packages)

# Read OSPAR 20km grids
#OSPAR_20km_Grid <- sf::st_read("Input/OSPAR_LAEA_20K_Grid_Level1.2.shp")

# Correct column names
#colnames(OSPAR_20km_Grid) <- c("GridID","geometry")

# Read OSPAR assessment units
OSPAR_Assessment_Units <- sf::st_read("MSSQL:server=SQL09;database=OceanCOMPEAT_20062014_COMP4;trusted_connection=yes;", layer = "AssessmentUnit", query = "SELECT ID, GEOM FROM AssessmentUnit")
st_crs(OSPAR_Assessment_Units) <- 4326
#OSPAR_Assessment_Units <- sf::st_read("Input/COMP4_Assessment_Areas_v7b/COMP4_Assessment_Areas_v7b.shp")

# Drop columns except ID
#OSPAR_Assessment_Units <- OSPAR_Assessment_Units[1]

# Correct column names
colnames(OSPAR_Assessment_Units) <- c("AssessmentUnitID","geometry")

OSPAR_Assessment_Units <- sf::st_transform(OSPAR_Assessment_Units, crs = 3035)

# Get netcdf files
path <- "D:/JMP EUNOSAT/JMP_EUNOSAT_CHL_v10/"
filenames <- list.files(path = path, pattern="*.nc", full.names = TRUE)

# Loop netcdf files
dt1 <- lapply(filenames,function(filename){
  # Extract date string
  datestring <- regmatches(filename, regexpr("[0-9]{8}", filename))  

  # Extract year, month and day
  year <- substring(datestring,1,4) %>% as.numeric()
  month <- substring(datestring,5,6) %>% as.numeric()
  day <- substring(datestring,7,8) %>% as.numeric()

  if(year >= 2006 && year <= 2014 && month >= 3 && month <= 10) {
    # Open netcdf file
    nc <- nc_open(filename)

    # Read variables
    lon <- ncvar_get(nc, "Longitude")
    lat <- ncvar_get(nc, "Latitude")
    chl <- ncvar_get(nc, "CHL_EUNOSAT_v10")
    
    # Read fill value
    fillvalue <- ncatt_get(nc, "CHL_EUNOSAT_v10", "_FillValue")
    
    # Close netcdf file
    nc_close(nc)
    
    # Replace netCDF fill values with NA's
    chl[chl == fillvalue$value] <- NA
    
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
    fwrite(dt,paste("D:/JMP EUNOSAT/TMP/",datestring,".csv"))
  }
})

# Remove NULL obejects from list
#dt1[sapply(dt1,is.null)] <- NULL

# Combine data tables 
#dt2 <- rbindlist(dt1)

# Read csv files into data table
path <- "D:/JMP EUNOSAT/TMP/"
filenames <- list.files(path = path, pattern="*.csv", full.names = TRUE)
#filenames <- list.files(path = path, pattern="200[6-9][0-9]{4}", full.names = TRUE) #979
#filenames <- list.files(path = path, pattern="201[0-9]{5}", full.names = TRUE) #1225
dt1 <- lapply(filenames,function(filename){fread(filename)})
dt2 <- rbindlist(dt1)
#fwrite(dt2,paste("D:/JMP EUNOSAT/","TMP.csv"))

# Filter ...
dt3 <- dt2[,list(Mean = mean(Chlorophyll),STD = sd(Chlorophyll), .N),list(AssessmentUnitID,Year)]

fwrite(dt3,paste("D:/JMP EUNOSAT/","Indicator_CPHL_EO_02.csv"))


plot(OSPAR_Assessment_Units,col="NA")
plot(dt["Chlorophyll"], col = "blue", add = TRUE)
plot(dt2["Chlorophyll"])