# Install and load R packages --------------------------------------------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "data.table", "tidyverse", "readxl", "ggplot2", "ggmap", "mapview", "httr")
ipak(packages)

# Define assessment period i.e. uncomment the period you want to run the assessment for!
#assessmentPeriod <- "1990-2000" # COMP1
#assessmentPeriod <- "2001-2006" # COMP2
#assessmentPeriod <- "2006-2014" # COMP3
assessmentPeriod <- "2015-2020" # COMP4

# Set flag to determined if dissolved inorganic nutrients are being salinity nomalised 
dissolved_inorganic_nutrients_are_salinity_normalised <- FALSE

# Set flag to determined if the combined chlorophyll a in-situ/satellite indicator is a simple mean or a weighted mean based on confidence measures
combined_Chlorophylla_IsWeighted <- TRUE

# Define paths
inputPath <- file.path("Input", assessmentPeriod)
if (combined_Chlorophylla_IsWeighted == "TRUE") {
  outputPath <- file.path("Output_chl_weighted", assessmentPeriod)
} else {
  outputPath <- file.path("Output", assessmentPeriod)
}


# Create paths
dir.create(inputPath, showWarnings = FALSE, recursive = TRUE)
dir.create(outputPath, showWarnings = FALSE, recursive = TRUE)

# Download and unpack files needed for the assessment --------------------------
download.file.unzip.maybe <- function(url, refetch = FALSE, path = ".") {
  dest <- file.path(path, sub("\\?.+", "", basename(url)))
  if (refetch || !file.exists(dest)) {
    download.file(url, dest, mode = "wb")
    if (tools::file_ext(dest) == "zip") {
      unzip(dest, exdir = path)
    }
  }
}

urls <- c()
unitsFile <- file.path(inputPath, "")
configurationFile <- file.path(inputPath, "")
stationSamplesBOTFile <- file.path(inputPath, "")
stationSamplesCTDFile <- file.path(inputPath, "")
stationSamplesPMPFile <- file.path(inputPath, "")
indicator_CPHL_EO_02 <- file.path(inputPath, "")

if (assessmentPeriod == "1990-2000") {
  urls <- c("https://www.dropbox.com/s/hm086ahtu1en4fl/AssessmentUnits.zip?dl=1",
            "https://www.dropbox.com/s/eouhsa10p8ri5qs/Configuration1990-2000.xlsx?dl=1",
            "https://www.dropbox.com/s/hf9y7kgf4ijsu1u/StationSamples1990-2000BOT.txt.gz?dl=1",
            "https://www.dropbox.com/s/5mjwp9jw6tlms1o/StationSamples1990-2000CTD.txt.gz?dl=1",
            "https://www.dropbox.com/s/ubjlo32nk2kpfdn/StationSamples1990-2000PMP.txt.gz?dl=1",
            "https://www.dropbox.com/s/d07i8xzxh1qq8xd/Indicator_CPHL_EO_02_1990-2000.csv?dl=1")
  unitsFile <- file.path(inputPath, "AssessmentUnits.csv")
  configurationFile <- file.path(inputPath, "Configuration1990-2000.xlsx")
  stationSamplesBOTFile <- file.path(inputPath, "StationSamples1990-2000BOT.txt.gz")
  stationSamplesCTDFile <- file.path(inputPath, "StationSamples1990-2000CTD.txt.gz")
  stationSamplesPMPFile <- file.path(inputPath, "StationSamples1990-2000PMP.txt.gz")
  indicator_CPHL_EO_02 <- file.path(inputPath, "Indicator_CPHL_EO_02_1990-2000.csv")
} else if (assessmentPeriod == "2001-2006") {
  urls <- c("https://www.dropbox.com/s/hm086ahtu1en4fl/AssessmentUnits.zip?dl=1",
            "https://www.dropbox.com/s/st7p60a8rr4yu8s/Configuration2001-2006.xlsx?dl=1",
            "https://www.dropbox.com/s/se7bdaaeu8alftl/StationSamples2001-2006BOT.txt.gz?dl=1",
            "https://www.dropbox.com/s/tla1vislh2rk50d/StationSamples2001-2006CTD.txt.gz?dl=1",
            "https://www.dropbox.com/s/vtl40rkoelzafav/StationSamples2001-2006PMP.txt.gz?dl=1",
            "https://www.dropbox.com/s/yrbqfmfnfk32rum/Indicator_CPHL_EO_02_2001-2006.csv?dl=1")
  unitsFile <- file.path(inputPath, "AssessmentUnits.csv")
  configurationFile <- file.path(inputPath, "Configuration2001-2006.xlsx")
  stationSamplesBOTFile <- file.path(inputPath, "StationSamples2001-2006BOT.txt.gz")
  stationSamplesCTDFile <- file.path(inputPath, "StationSamples2001-2006CTD.txt.gz")
  stationSamplesPMPFile <- file.path(inputPath, "StationSamples2001-2006PMP.txt.gz")
  indicator_CPHL_EO_02 <- file.path(inputPath, "Indicator_CPHL_EO_02_2001-2006.csv")
} else if (assessmentPeriod == "2006-2014") {
  urls <- c("https://www.dropbox.com/s/hm086ahtu1en4fl/AssessmentUnits.zip?dl=1",
            "https://www.dropbox.com/s/q5awsqulaj1z4jw/Configuration2006-2014.xlsx?dl=1",
            "https://www.dropbox.com/s/iioqnq7j8c2h88k/StationSamples2006-2014BOT.txt.gz?dl=1",
            "https://www.dropbox.com/s/8s5xo7s3tpkx2qj/StationSamples2006-2014CTD.txt.gz?dl=1",
            "https://www.dropbox.com/s/urs21lncfy5b28p/StationSamples2006-2014PMP.txt.gz?dl=1",
            "https://www.dropbox.com/s/kpbugtd4rwdxr9s/Indicator_CPHL_EO_02_2006-2014.csv?dl=1")
  unitsFile <- file.path(inputPath, "AssessmentUnits.csv")
  configurationFile <- file.path(inputPath, "Configuration2006-2014.xlsx")
  stationSamplesBOTFile <- file.path(inputPath, "StationSamples2006-2014BOT.txt.gz")
  stationSamplesCTDFile <- file.path(inputPath, "StationSamples2006-2014CTD.txt.gz")
  stationSamplesPMPFile <- file.path(inputPath, "StationSamples2006-2014PMP.txt.gz")
  indicator_CPHL_EO_02 <- file.path(inputPath, "Indicator_CPHL_EO_02_2006-2014.csv")
} else if (assessmentPeriod == "2015-2020") {
  urls <- c("https://www.dropbox.com/s/hm086ahtu1en4fl/AssessmentUnits.zip?dl=1",
            "https://www.dropbox.com/s/pt7g4b7q9gh18yf/Configuration2015-2020.xlsx?dl=1",
            "https://www.dropbox.com/s/580h30a839pxpn5/StationSamples2015-2020BOT.txt.gz?dl=1",
            "https://www.dropbox.com/s/eanvbzf957iq3f7/StationSamples2015-2020CTD.txt.gz?dl=1",
            "https://www.dropbox.com/s/2c6jho2pk64i7tl/StationSamples2015-2020PMP.txt.gz?dl=1",
            "https://www.dropbox.com/s/d5gpsbcqsbtz09l/Indicator_CPHL_EO_02_2015-2020.csv?dl=1")
  unitsFile <- file.path(inputPath, "AssessmentUnits.csv")
  configurationFile <- file.path(inputPath, "Configuration2015-2020.xlsx")
  stationSamplesBOTFile <- file.path(inputPath, "StationSamples2015-2020BOT.txt.gz")
  stationSamplesCTDFile <- file.path(inputPath, "StationSamples2015-2020CTD.txt.gz")
  stationSamplesPMPFile <- file.path(inputPath, "StationSamples2015-2020PMP.txt.gz")
  indicator_CPHL_EO_02 <- file.path(inputPath, "Indicator_CPHL_EO_02_2015-2020.csv")
}

files <- sapply(urls, download.file.unzip.maybe, path = inputPath)

# Assessment Units + Grid Units-------------------------------------------------

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

# Write to database
# st_write(
#   units,
#   dsn = "MSSQL:server=SQL09;database=OceanCOMPEAT_20152020_COMP4;trusted_connection=yes;",
#   layer = "AssessmentUnit",
#   layer_options = c("LAUNDER=NO", "GEOM_NAME=GEOM", "FID=ID")
#   )

# Identify invalid geometries
st_is_valid(units, reason=TRUE)

# Transform projection into ETRS_1989_LAEA
units <- st_transform(units, crs = 3035)

# Calculate area
units$UnitArea <- st_area(units)

# Identify invalid geometries
st_is_valid(units)

# Make geometries valid by doing the buffer of nothing trick
units <- sf::st_buffer(units, 0.0)

# Identify overlapping assessment units
#st_overlaps(units)

setkey(units, UnitID)

# Make grid units
make.gridunits <- function(units, gridSize) {
  units <- st_transform(units, crs = 3035)
  
  bbox <- st_bbox(units)
  
  xmin <- floor(bbox$xmin / gridSize) * gridSize
  ymin <- floor(bbox$ymin / gridSize) * gridSize
  xmax <- ceiling(bbox$xmax / gridSize) * gridSize
  ymax <- ceiling(bbox$ymax / gridSize) * gridSize
  
  xn <- (xmax - xmin) / gridSize
  yn <- (ymax - ymin) / gridSize
  
  grid <- st_make_grid(units, cellsize = gridSize, c(xmin, ymin), n = c(xn, yn), crs = 3035) %>%
    st_sf()
  
  grid$GridID = 1:nrow(grid)
  
  gridunits <- st_intersection(grid, units)
  
  gridunits$Area <- st_area(gridunits)
  
  return(gridunits)
}

gridunits10 <- make.gridunits(units, 10000)
gridunits30 <- make.gridunits(units, 30000)
gridunits60 <- make.gridunits(units, 60000)

unitGridSize <- as.data.table(read_excel(configurationFile, sheet = "UnitGridSize")) %>% setkey(UnitID)

a <- merge(unitGridSize[GridSize == 10000], gridunits10 %>% select(UnitID, GridID, GridArea = Area))
b <- merge(unitGridSize[GridSize == 30000], gridunits30 %>% select(UnitID, GridID, GridArea = Area))
c <- merge(unitGridSize[GridSize == 60000], gridunits60 %>% select(UnitID, GridID, GridArea = Area))
gridunits <- st_as_sf(rbindlist(list(a,b,c)))
rm(a,b,c)

gridunits <- st_cast(gridunits)

st_write(gridunits, file.path(outputPath, "gridunits.shp"), delete_layer = TRUE)

# Plot
ggplot() + geom_sf(data = units) + coord_sf()
ggsave(file.path(outputPath, "Assessment_Units.png"), width = 12, height = 9, dpi = 300)
ggplot() + geom_sf(data = gridunits10) + coord_sf()
ggsave(file.path(outputPath, "Assessment_GridUnits10.png"), width = 12, height = 9, dpi = 300)
ggplot() + geom_sf(data = gridunits30) + coord_sf()
ggsave(file.path(outputPath, "Assessment_GridUnits30.png"), width = 12, height = 9, dpi = 300)
ggplot() + geom_sf(data = gridunits60) + coord_sf()
ggsave(file.path(outputPath, "Assessment_GridUnits60.png"), width = 12, height = 9, dpi = 300)
ggplot() + geom_sf(data = st_cast(gridunits)) + coord_sf()
ggsave(file.path(outputPath, "Assessment_GridUnits.png"), width = 12, height = 9, dpi = 300)

# Read station samples ---------------------------------------------------------

# Ocean hydro chemistry - Bottle and low resolution CTD data
stationSamplesBOT <- fread(input = stationSamplesBOTFile, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesBOT[, Type := "B"]

# Ocean hydro chemistry - High resolution CTD data
stationSamplesCTD <- fread(input = stationSamplesCTDFile, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesCTD[, Type := "C"]

# Ocean hydro chemistry - Pump data
stationSamplesPMP <- fread(input = stationSamplesPMPFile, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)
stationSamplesPMP[, Type := "P"]

# Combine station samples
stationSamples <- rbindlist(list(stationSamplesBOT, stationSamplesCTD, stationSamplesPMP), use.names = TRUE, fill = TRUE)

# Remove original data tables
rm(stationSamplesBOT, stationSamplesCTD, stationSamplesPMP)

# Unique stations by natural key
uniqueN(stationSamples, by = c("Cruise", "Station", "Type", "Year", "Month", "Day", "Hour", "Minute", "Longitude..degrees_east.", "Latitude..degrees_north."))

# Assign station ID by natural key
stationSamples[, StationID := .GRP, by = .(Cruise, Station, Type, Year, Month, Day, Hour, Minute, Longitude..degrees_east., Latitude..degrees_north.)]

# Classify station samples into grid units -------------------------------------

# Extract unique stations i.e. longitude/latitude pairs
stations <- unique(stationSamples[, .( Longitude..degrees_east., Latitude..degrees_north.)])

# Make stations spatial keeping original latitude/longitude
stations <- st_as_sf(stations, coords = c("Longitude..degrees_east.", "Latitude..degrees_north."), remove = FALSE, crs = 4326)

# Transform projection into ETRS_1989_LAEA
stations <- st_transform(stations, crs = 3035)

# Classify stations into grid units
stations <- st_join(stations, st_cast(gridunits), join = st_intersects)

# Delete stations not classified
stations <- na.omit(stations)

# Remove spatial column and nake into data table
stations <- st_set_geometry(stations, NULL) %>% as.data.table()

# Merge stations back into station samples - getting rid of station samples not classified into assessment units
stationSamples <- stations[stationSamples, on = .(Longitude..degrees_east., Latitude..degrees_north.), nomatch = 0]



# Get bathymetric depth for the oxygen indicator -----------------------------

# Function to get bathymetric depth from EMODnet bathymetry REST web service
get.bathymetric <- function(x, y, host = "https://rest.emodnet-bathymetry.eu") {
  query = paste0("/depth_sample?geom=POINT(", x, " ", y,")")
  path = httr::modify_url(paste0(host, query))
  r = GET(path)
  # to catch empty responses in a proper way
  if(is.numeric(content(r)$avg)){
    return(paste(content(r)$min, content(r)$max, content(r)$avg, content(r)$stdev, sep = "_"))
  } else {
    return(NA_real_)
  }
}

# Extract station samples with oxygen and missing bottom depth
stationSamplesWithOxygen <- stationSamples[!is.na(Dissolved.Oxygen..ml.l.) & is.na(Bot..Depth..m.)]

# Extract stations with oxygen
stationsWithOxygen <- unique(stationSamplesWithOxygen[, .(Longitude..degrees_east., Latitude..degrees_north.)])

# Get bathymetrics for stations with oxygen
bathymetrics <- map2(stationsWithOxygen$Longitude..degrees_east., stationsWithOxygen$Latitude..degrees_north., get.bathymetric) %>% unlist

stationsWithOxygen$Bathymetric <- bathymetrics

stationsWithOxygen <- stationsWithOxygen %>%
  separate(Bathymetric, c("BathymetricMin", "BathymetricMax", "BathymetricAvg", "BathymetricStDev"), sep = "_") %>%
  mutate(
    BathymetricMin = -as.numeric(BathymetricMin),
    BathymetricMax = -as.numeric(BathymetricMax),
    BathymetricAvg = -as.numeric(BathymetricAvg),
    BathymetricStDev = -as.numeric(BathymetricStDev),
  )

# Merge bathymetric back into station samples
stationSamples <- as.data.table(stationsWithOxygen)[, .(Longitude..degrees_east., Latitude..degrees_north., Bathymetric..m. = BathymetricAvg)][stationSamples, on = .(Longitude..degrees_east., Latitude..degrees_north.)]

#defining function for calculating 02 conc at set saturation (this is in uMol/L)
Csat <- function(temp, salinity){
  # Coefficents
  A0 = 2.00907
  A1 = 3.22014
  A2 = 4.05010
  A3 = 4.94457
  A4 = -0.256847
  A5 = 3.88767
  B0 = -0.00624523
  B1 = -0.00737614
  B2 = -0.0103410
  B3 = -0.00817083
  C0 = -4.88682E-07
  
  Ts = log((298.15-temp)/(273.15+temp))
  
  O2.sat = A0+(A1*Ts)+(A2*Ts^2)+
    (A3*Ts^3)+(A4*Ts^4)+(A5*Ts^5)+
    salinity*(B0+(B1*Ts)+(B2*Ts^2)+(B3*Ts^3))+
    (C0*salinity^2)
  
  return((exp(O2.sat))* 44.6608)     # output in uMol/L
}
#Calculating Oxygen concentration and saturation
stationSamples$O2CONC100..umol.l. <- Csat(stationSamples$Temperature..degC., stationSamples$Practical.Salinity..dmnless.) # this is the theoretical concentration at 100% saturation
stationSamples$O2CONC100..mg.l. <- stationSamples$O2CONC100..umol.l.*(31.998/1000) # just in different units
stationSamples$Dissolved.Oxygen..mg.l. <- stationSamples$Dissolved.Oxygen..ml.l. / 0.7 # Convert ml/l to mg/l by factor of 0.7
stationSamples$O2SAT <- (stationSamples$Dissolved.Oxygen..mg.l./stationSamples$O2CONC100..mg.l.)*100 #the saturation is calculated by calculating the actual concentration as a percentage of the theoretical concentration at 100% saturation
#calculating oxygen debt, will be negative when supersaturated

# Read indicator configuration files -------------------------------------------
indicators <- as.data.table(read_excel(configurationFile, sheet = "Indicators")) %>% setkey(IndicatorID)
indicatorUnits <- as.data.table(read_excel(configurationFile, sheet = "IndicatorUnits")) %>% setkey(IndicatorID, UnitID)
indicatorUnitResults <- as.data.table(read_excel(configurationFile, sheet = "IndicatorUnitResults")) %>% setkey(IndicatorID, UnitID)

wk2list = list()

## For O2SAT plots go to O2SAT.R at this point.

# Loop indicators --------------------------------------------------------------
for(i in 1:nrow(indicators)){
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
  if (name == 'Dissolved Inorganic Nitrogen') {
    wk$ES <- apply(wk[, list(Nitrate.Nitrogen..NO3.N...umol.l., Nitrite.Nitrogen..NO2.N...umol.l., Ammonium.Nitrogen..NH4.N...umol.l.)], 1, function(x){
      if (all(is.na(x)) | is.na(x[1])) {
        NA
      }
      else {
        sum(x, na.rm = TRUE)
      }
    })
    wk$ESQ <- apply(wk[, .(QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l., QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l., QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l.)], 1, function(x){
     max(x, na.rm = TRUE)
    })
  } else if (name == 'Dissolved Inorganic Phosphorus') {
    wk[, ES := Phosphate.Phosphorus..PO4.P...umol.l.]
    wk[, ESQ := QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l.]
  } else if (name == 'Chlorophyll a (in-situ)') {
    wk[, ES := Chlorophyll.a..ug.l.]
    wk[, ESQ := QV.ODV.Chlorophyll.a..ug.l.]
  } else if (name == 'Oxygen Deficiency') {
    wk[, ES := Dissolved.Oxygen..ml.l. / 0.7] # Convert ml/l to mg/l by factor of 0.7
    wk[, ESQ := QV.ODV.Dissolved.Oxygen..ml.l.]
  } else if (name == 'Total Nitrogen') {
    wk[, ES := Total.Nitrogen..N...umol.l.]
    wk[, ESQ := QV.ODV.Total.Nitrogen..N...umol.l.]
  } else if (name == 'Total Phosphorus') {
    wk[, ES := Total.Phosphorus..P...umol.l.]
    wk[, ESQ := QV.ODV.Total.Phosphorus..P...umol.l.]
  } else if (name == 'Secchi Depth') {
    wk[, ES := Secchi.Depth..m..METAVAR.FLOAT]
    wk[, ESQ := QV.ODV.Secchi.Depth..m.]
  } else if (name == 'Dissolved Inorganic Nitrogen/Dissolved Inorganic Phosphorus') {
    wk$ES <- apply(wk[, list(Nitrate.Nitrogen..NO3.N...umol.l., Nitrite.Nitrogen..NO2.N...umol.l., Ammonium.Nitrogen..NH4.N...umol.l.)], 1, function(x){
      if (all(is.na(x)) | is.na(x[1])) {
        NA
      }
      else {
        sum(x, na.rm = TRUE)
      }
    })
    wk[, ES := ES/Phosphate.Phosphorus..PO4.P...umol.l.]
    wk$ESQ <- apply(wk[, .(QV.ODV.Nitrate.Nitrogen..NO3.N...umol.l., QV.ODV.Nitrite.Nitrogen..NO2.N...umol.l., QV.ODV.Ammonium.Nitrogen..NH4.N...umol.l., QV.ODV.Phosphate.Phosphorus..PO4.P...umol.l.)], 1, function(x){
      max(x, na.rm = TRUE)
    })
  } else if (name == 'Total Nitrogen/Total Phosphorus') {
    wk[, ES := Total.Nitrogen..N...umol.l./Total.Phosphorus..P...umol.l.]
    wk$ESQ <- apply(wk[, .(QV.ODV.Total.Nitrogen..N...umol.l., QV.ODV.Total.Phosphorus..P...umol.l.)], 1, function(x){
      max(x, na.rm = TRUE)
    })
  } else {
    next
  }

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

  # Dissolved inorganic nutrients salinity normalisation
  if (dissolved_inorganic_nutrients_are_salinity_normalised == TRUE) {
    if (name == 'Dissolved Inorganic Nitrogen' || name == 'Dissolved Inorganic Phosphorus') {
      # Get linear regression coefficients on ES~Salinity and Mean Salinity
      wk00 <- wk0[!is.na(Practical.Salinity..dmnless.),
                  .(N = .N,
                    S = mean(Practical.Salinity..dmnless., na.rm = TRUE),
                    B = coef(lm(ES~Practical.Salinity..dmnless.))[1],
                    A = coef(lm(ES~Practical.Salinity..dmnless.))[2],
                    P = ifelse(.N >= 2, summary(lm(ES~Practical.Salinity..dmnless.))$coef[2, 4], NA_real_),
                    R2 = summary(lm(ES~Practical.Salinity..dmnless.))$adj.r.squared),
                  keyby = .(IndicatorID, UnitID)]
    }
    
    # Merge data tables
    wk0 <- wk00[wk0]
    
    # Normalise indicator concentration if the indicator has a significant relation to salinity e.g. above the 95% confidence level (p<0.05)
    # ES_normalised = ES_observed + A * (S_reference - S_observed)
    # https://www.ospar.org/site/assets/files/37302/national_common_procedure_report_2016_sweden.pdf
    wk0[, ES := ifelse(P < 0.05 & !is.na(P) & !is.na(Practical.Salinity..dmnless.), ES + A * (S - Practical.Salinity..dmnless.), ES)]
  }

  if (metric == 'Mean'){
    # Calculate station depth mean
    wk0 <- wk0[, .(ES = mean(ES), SD = sd(ES), N = .N), keyby = .(IndicatorID, UnitID, GridID, GridArea, Period, Month, StationID, Depth)]
    
    # Calculate station mean --> UnitID, GridID, GridArea, Period, Month, ES, SD, N
    wk1 <- wk0[, .(ES = mean(ES), SD = sd(ES), N = .N), keyby = .(IndicatorID, UnitID, GridID, GridArea, Period, Month, StationID)]
    
    # Calculate annual mean --> UnitID, Period, ES, SD, N, NM
    wk2 <- wk1[, .(ES = mean(ES), SD = sd(ES), N = .N, NM = uniqueN(Month)), keyby = .(IndicatorID, UnitID, Period)]
  } else if (metric == 'Minimum') {
    # Calculate station minimum --> UnitID, GridID, GridArea, Period, Month, ES, SD, N
    wk1 <- wk0[, .(ES = min(ES), SD = sd(ES), N = .N), keyby = .(IndicatorID, UnitID, GridID, GridArea, Period, Month, StationID)]
    
    # Calculate annual minimum --> UnitID, Period, ES, SD, N, NM
    wk2 <- wk1[, .(ES = min(ES), SD = sd(ES), N = .N, NM = uniqueN(Month)), keyby = .(IndicatorID, UnitID, Period)]
  } else if (metric == '5th percentile of deepest sample within 10 meters from bottom') {
    # Select deepest sample at each station within 10 meters from bottom
    wk1 <- wk0[wk0[, .I[Depth == max(Depth) & DepthToBottom - Depth <= 10], by = StationID]$V1]
    wk1 <- wk1[, .(ES = ES, SD = sd(ES), N = .N), keyby = .(IndicatorID, UnitID, GridID, GridArea, Period, Month, StationID)]

    # Calculate annual 5th percentile --> UnitID, Period, ES, SD, N, NM
    wk2 <- wk1[, .(ES = quantile(ES, 0.05, na.rm = TRUE), SD = sd(ES), N = .N, NM = uniqueN(Month)), keyby = .(IndicatorID, UnitID, Period)]
  }
  
  # Calculate grid area --> UnitID, Period, ES, SD, N, NM, GridArea
  a <- wk1[, .N, keyby = .(IndicatorID, UnitID, Period, GridID, GridArea)] # UnitGrids
  b <- a[, .(GridArea = sum(as.numeric(GridArea))), keyby = .(IndicatorID, UnitID, Period)] #GridAreas
  wk2 <- merge(wk2, b, by = c("IndicatorID", "UnitID", "Period"), all.x = TRUE)
  rm(a,b)

  wk2list[[i]] <- wk2
}

# Combine station and annual indicator results
wk2 <- rbindlist(wk2list)

# Combine with indicator results reported
wk2 <- rbindlist(list(wk2, indicatorUnitResults), fill = TRUE)

# Add Chlorophyll a EO indicator if it exists
if (file.exists(indicator_CPHL_EO_02)) {
  wk2_CPHL_EO <- fread(indicator_CPHL_EO_02)
  wk2_CPHL_EO[, IndicatorID := 302]
  wk2_CPHL_EO <- wk2_CPHL_EO[, .(IndicatorID, UnitID, Period, ES, SD, N, NM, GridArea)]
  wk2 <- rbindlist(list(wk2, wk2_CPHL_EO), fill = TRUE)
} else {
  warning("CPHL EO data not available and CPHL EO indicator included")
}

# Combine with indicator and indicator unit configuration tables
wk3 <- indicators[indicatorUnits[wk2]]

# Calculate General Temporal Confidence (GTC) - Confidence in number of annual observations
wk3[, GTC := ifelse(N > GTC_HM, 100, ifelse(N < GTC_ML, 0, 50))]

# Calculate Number of Months Potential
wk3[, NMP := ifelse(MonthMin > MonthMax, 12 - MonthMin + 1 + MonthMax, MonthMax - MonthMin + 1)]

# Calculate Specific Temporal Confidence (STC) - Confidence in number of annual missing months
wk3[, STC := ifelse(NMP - NM <= STC_HM, 100, ifelse(NMP - NM >= STC_ML, 0, 50))]

# Calculate General Spatial Confidence (GSC) - Confidence in number of annual observations per number of grids
wk3 <- wk3[as.data.table(gridunits)[, .(NG = as.numeric(sum(GridArea) / mean(GridSize^2))), .(UnitID)], on = .(UnitID = UnitID), nomatch=0]
wk3[, GSC := ifelse(N / NG > GSC_HM, 100, ifelse(N / NG < GSC_ML, 0, 50))]

# Calculate Specific Spatial Confidence (SSC) - Confidence in area of sampled grid units as a percentage to the total unit area
wk3 <- merge(wk3, as.data.table(units)[, .(UnitArea = as.numeric(UnitArea)), keyby = .(UnitID)], by = c("UnitID"), all.x = TRUE)
wk3[, SSC := ifelse(GridArea / UnitArea * 100 > SSC_HM, 100, ifelse(GridArea / UnitArea * 100 < SSC_ML, 0, 50))]

if (combined_Chlorophylla_IsWeighted) {
  # Calculate combined chlorophyll a indicator as a weighted average
  wk3[, C := (GTC + STC + GSC + SSC) / 4]
  wk3[IndicatorID == 301, W := ifelse(C >= 75, 50/50, ifelse(C >= 50, 30/70, 10/90))]
  wk3[IndicatorID == 302, W := 1]
  wk3_CPHL <- wk3[IndicatorID %in% c(301, 302), .(IndicatorID = 3, ES = weighted.mean(ES, W), SD = NA, N = sum(N), NM = max(NM), GridArea = max(GridArea), GTC = weighted.mean(GTC, W), NMP = max(NMP), STC = weighted.mean(STC, W), GSC = weighted.mean(GSC, W), SSC = weighted.mean(SSC, W)), by = .(UnitID, Period)] %>% setkey(IndicatorID, UnitID)
  wk3_CPHL <- indicators[indicatorUnits[wk3_CPHL]]
  wk3 <- rbindlist(list(wk3, wk3_CPHL), fill = TRUE)
} else {
  # Calculate combined chlorophyll a indicator as a simple average
  wk3_CPHL <- wk3[IndicatorID %in% c(301, 302), .(IndicatorID = 3, ES = mean(ES), SD = NA, N = sum(N), NM = max(NM), GridArea = max(GridArea), GTC = mean(GTC), NMP = max(NMP), STC = mean(STC), GSC = mean(GSC), SSC = mean(SSC)), by = .(UnitID, Period)] %>% setkey(IndicatorID, UnitID)
  wk3_CPHL <- indicators[indicatorUnits[wk3_CPHL]]
  wk3 <- rbindlist(list(wk3, wk3_CPHL), fill = TRUE)
}

# Standard Error
wk3[, SE := SD / sqrt(N)]

# 95 % Confidence Interval
wk3[, CI := qnorm(0.975) * SE]

# Calculate (BEST)
wk3[, BEST := ifelse(Response == 1, ET / (1 + ACDEV / 100), ET / (1 - ACDEV / 100))]

# Calculate Ecological Quality Ratio (ERQ)
wk3[, EQR := ifelse(Response == 1, ifelse(BEST > ES, 1, BEST / ES), ifelse(ES > BEST, 1, ES / BEST))]

# Calculate Ecological Quality Ratio Boundaries (ERQ_HG/GM/MP/PB)
wk3[, EQR_GM := ifelse(Response == 1, 1 / (1 + ACDEV / 100), 1 - ACDEV / 100)]
wk3[, EQR_HG := 0.5 * 0.95 + 0.5 * EQR_GM]
wk3[, EQR_PB := 2 * EQR_GM - 0.95]
wk3[, EQR_MP := 0.5 * EQR_GM + 0.5 * EQR_PB]

# Calculate Ecological Quality Ratio Scaled (EQRS)
wk3[, EQRS := ifelse(EQR <= EQR_PB, (EQR - 0) * (0.2 - 0) / (EQR_PB - 0) + 0,
                     ifelse(EQR <= EQR_MP, (EQR - EQR_PB) * (0.4 - 0.2) / (EQR_MP - EQR_PB) + 0.2,
                            ifelse(EQR <= EQR_GM, (EQR - EQR_MP) * (0.6 - 0.4) / (EQR_GM - EQR_MP) + 0.4,
                                   ifelse(EQR <= EQR_HG, (EQR - EQR_GM) * (0.8 - 0.6) / (EQR_HG - EQR_GM) + 0.6,
                                          (EQR - EQR_HG) * (1 - 0.8) / (1 - EQR_HG) + 0.8))))]

wk3[, EQRS_Class := ifelse(EQRS >= 0.8, "High",
                           ifelse(EQRS >= 0.6, "Good",
                                  ifelse(EQRS >= 0.4, "Moderate",
                                         ifelse(EQRS >= 0.2, "Poor","Bad"))))]

# Calculate assessment ES --> UnitID, Period, ES, SD, N, N_OBS, EQR, EQRS, GTC, STC, GSC, SSC
wk4 <- wk3[, .(Period = ifelse(min(Period) > 9999, min(Period), min(Period) * 10000 + max(Period)), ES = mean(ES), SD = sd(ES), EQR = mean(EQR), EQRS = mean(EQRS), N = .N, N_OBS = sum(N), GTC = mean(GTC), STC = mean(STC), GSC = mean(GSC), SSC = mean(SSC)), .(IndicatorID, UnitID)]

wk4[, EQRS_Class := ifelse(EQRS >= 0.8, "High",
                           ifelse(EQRS >= 0.6, "Good",
                                  ifelse(EQRS >= 0.4, "Moderate",
                                         ifelse(EQRS >= 0.2, "Poor","Bad"))))]

# Add Year Count where STC = 100 --> NSTC100
wk4 <- wk3[STC == 100, .(NSTC100 = .N), .(IndicatorID, UnitID)][wk4, on = .(IndicatorID, UnitID)]

# Adjust Specific Spatial Confidence if number of years where STC = 100 is at least half of the number of years with measurements
wk4[, STC := ifelse(!is.na(NSTC100) & NSTC100 >= N/2, 100, STC)]

# Combine with indicator and indicator unit configuration tables
wk5 <- indicators[indicatorUnits[wk4]]

# Confidence Assessment --------------------------------------------------------

# Calculate Temporal Confidence averaging General and Specific Temporal Confidence 
wk5 <- wk5[, TC := (GTC + STC) / 2]

wk5[, TC_Class := ifelse(TC >= 75, "High", ifelse(TC >= 50, "Moderate", "Low"))]

# Calculate Spatial Confidence averaging General and Specific Spatial Confidence 
wk5 <- wk5[, SC := (GSC + SSC) / 2]

wk5[, SC_Class := ifelse(SC >= 75, "High", ifelse(SC >= 50, "Moderate", "Low"))]

# Standard Error - using number of years in the assessment period and the associated standard deviation
#wk5[, SE := SD / sqrt(N)]

# Accuracy Confidence for Non-Problem Area
#wk5[, AC_NPA := ifelse(Response == 1, pnorm(ET, ES, SD), pnorm(ES, ET, SD))]

# Standard Error - using number of observations behind the annual mean - to be used in Accuracy Confidence Calculation!!!
wk5[, AC_SE := SD / sqrt(N_OBS)]

# Accuracy Confidence for Non-Problem Area
wk5[, AC_NPA := ifelse(Response == 1, pnorm(ET, ES, AC_SE), pnorm(ES, ET, AC_SE))]

# Accuracy Confidence for Problem Area
wk5[, AC_PA := 1 - AC_NPA]

# Accuracy Confidence Area Class - Not sure what this should be used for?
#wk5[, ACAC := ifelse(AC_NPA > 0.5, "NPA", ifelse(AC_NPA < 0.5, "PA", "PPA"))]

# Accuracy Confidence
wk5[, AC := ifelse(AC_NPA > AC_PA, AC_NPA, AC_PA)]

# Accuracy Confidence Class
wk5[, ACC := ifelse(AC > 0.9, 100, ifelse(AC < 0.7, 0, 50))]

wk5[, ACC_Class := ifelse(ACC >= 75, "High", ifelse(ACC >= 50, "Moderate", "Low"))]

# Calculate Overall Confidence
wk5 <- wk5[, C := (TC + SC + ACC) / 3]

wk5[, C_Class := ifelse(C >= 75, "High", ifelse(C >= 50, "Moderate", "Low"))]

# Category ---------------------------------------------------------------------

# Category result as a weighted average of the indicators in each category per unit - CategoryID, UnitID, N, EQR, EQRS, C
wk6 <- wk5[IndicatorID < 100 & !is.na(EQRS), .(.N, EQR = weighted.mean(EQR, IW, na.rm = TRUE), EQRS = weighted.mean(EQRS, IW, na.rm = TRUE), C = weighted.mean(C, IW, na.rm = TRUE)), .(CategoryID, UnitID)]

wk7 <- dcast(wk6, UnitID ~ CategoryID, value.var = c("N","EQR","EQRS","C"))

# Assessment -------------------------------------------------------------------

# Assessment result - UnitID, N, EQR, EQRS, C
wk81 <- wk6[CategoryID %in% c(2,3), .(NE = .N, EQR = min(EQR), EQRS = min(EQRS)), (UnitID)] %>% setkey(UnitID)
wk82 <- wk6[, .(NC = .N, C = mean(C)), (UnitID)] %>% setkey(UnitID)
wk8 <- wk81[wk82]

wk9 <- wk7[wk8, on = .(UnitID = UnitID), nomatch=0]

# Assign Status and Confidence Classes
wk9[, EQRS_Class := ifelse(EQRS >= 0.8, "High",
                           ifelse(EQRS >= 0.6, "Good",
                                  ifelse(EQRS >= 0.4, "Moderate",
                                         ifelse(EQRS >= 0.2, "Poor","Bad"))))]
wk9[, EQRS_11_Class := ifelse(EQRS_11 >= 0.8, "High",
                             ifelse(EQRS_11 >= 0.6, "Good",
                                    ifelse(EQRS_11 >= 0.4, "Moderate",
                                           ifelse(EQRS_11 >= 0.2, "Poor","Bad"))))]
wk9[, EQRS_12_Class := ifelse(EQRS_12 >= 0.8, "High",
                              ifelse(EQRS_12 >= 0.6, "Good",
                                     ifelse(EQRS_12 >= 0.4, "Moderate",
                                            ifelse(EQRS_12 >= 0.2, "Poor","Bad"))))]
wk9[, EQRS_2_Class := ifelse(EQRS_2 >= 0.8, "High",
                             ifelse(EQRS_2 >= 0.6, "Good",
                                    ifelse(EQRS_2 >= 0.4, "Moderate",
                                           ifelse(EQRS_2 >= 0.2, "Poor","Bad"))))]
wk9[, EQRS_3_Class := ifelse(EQRS_3 >= 0.8, "High",
                             ifelse(EQRS_3 >= 0.6, "Good",
                                    ifelse(EQRS_3 >= 0.4, "Moderate",
                                           ifelse(EQRS_3 >= 0.2, "Poor","Bad"))))]

wk9[, C_Class := ifelse(C >= 75, "High",
                        ifelse(C >= 50, "Moderate", "Low"))]
wk9[, C_11_Class := ifelse(C_11 >= 75, "High",
                          ifelse(C_11 >= 50, "Moderate", "Low"))]
wk9[, C_12_Class := ifelse(C_12 >= 75, "High",
                          ifelse(C_12 >= 50, "Moderate", "Low"))]
wk9[, C_2_Class := ifelse(C_2 >= 75, "High",
                          ifelse(C_2 >= 50, "Moderate", "Low"))]
wk9[, C_3_Class := ifelse(C_3 >= 75, "High",
                          ifelse(C_3 >= 50, "Moderate", "Low"))]

# Write results
wk3 <- merge(st_drop_geometry(units[1:4]), wk3, by = "UnitID")
fwrite(wk3, file = file.path(outputPath, "Annual_Indicator.csv"))
wk5 <- merge(st_drop_geometry(units[1:4]), wk5, by = "UnitID")
fwrite(wk5, file = file.path(outputPath, "Assessment_Indicator.csv"))
wk9 <- merge(st_drop_geometry(units[1:4]), wk9, by = "UnitID")
fwrite(wk9, file = file.path(outputPath, "Assessment.csv"))

# Create plots
EQRS_Class_colors <- c("#3BB300", "#99FF66", "#FFCABF", "#FF8066", "#FF0000")
EQRS_Class_limits <- c("High", "Good", "Moderate", "Poor", "Bad")
EQRS_Class_labels <- c(">= 0.8 - 1.0 (High)", ">= 0.6 - 0.8 (Good)", ">= 0.4 - 0.6 (Moderate)", ">= 0.2 - 0.4 (Poor)", ">= 0.0 - 0.2 (Bad)")

C_Class_colors <- c("#3BB300", "#FFCABF", "#FF0000")
C_Class_limits <- c("High", "Moderate", "Low")
C_Class_labels <- c(">= 75 % (High)", "50 - 74 % (Moderate)", "< 50 % (Low)")

# Assessment map Status + Confidence
wk <- merge(units, wk9, all.x = TRUE)

# Status maps
ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Status ", assessmentPeriod)) +
  geom_sf(aes(fill = EQRS_Class)) +
  scale_fill_manual(name = "EQRS", values = EQRS_Class_colors, limits = EQRS_Class_limits, labels = EQRS_Class_labels)
ggsave(file.path(outputPath, "Assessment_Map_EQRS.png"), width = 12, height = 9, dpi = 300)

ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Status ", assessmentPeriod)) +
  geom_sf(aes(fill = EQRS_11_Class)) +
  scale_fill_manual(name = "EQRS_11", values = EQRS_Class_colors, limits = EQRS_Class_limits, labels = EQRS_Class_labels)
ggsave(file.path(outputPath, "Assessment_Map_EQRS_11.png"), width = 12, height = 9, dpi = 300)

ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Status ", assessmentPeriod)) +
  geom_sf(aes(fill = EQRS_12_Class)) +
  scale_fill_manual(name = "EQRS_12", values = EQRS_Class_colors, limits = EQRS_Class_limits, labels = EQRS_Class_labels)
ggsave(file.path(outputPath, "Assessment_Map_EQRS_12.png"), width = 12, height = 9, dpi = 300)

ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Status ", assessmentPeriod)) +
  geom_sf(aes(fill = EQRS_2_Class)) +
  scale_fill_manual(name = "EQRS_2", values = EQRS_Class_colors, limits = EQRS_Class_limits, labels = EQRS_Class_labels)
ggsave(file.path(outputPath, "Assessment_Map_EQRS_2.png"), width = 12, height = 9, dpi = 300)

ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Status ", assessmentPeriod)) +
  geom_sf(aes(fill = EQRS_3_Class)) +
  scale_fill_manual(name = "EQRS_3", values = EQRS_Class_colors, limits = EQRS_Class_limits, labels = EQRS_Class_labels)
ggsave(file.path(outputPath, "Assessment_Map_EQRS_3.png"), width = 12, height = 9, dpi = 300)

# Confidence maps
ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Confidence ", assessmentPeriod)) +
  geom_sf(aes(fill = C_Class)) +
  scale_fill_manual(name = "C", values = C_Class_colors, limits = C_Class_limits, labels = C_Class_labels)
ggsave(file.path(outputPath, "Assessment_Map_C.png"), width = 12, height = 9, dpi = 300)

ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Confidence ", assessmentPeriod)) +
  geom_sf(aes(fill = C_11_Class)) +
  scale_fill_manual(name = "C_11", values = C_Class_colors, limits = C_Class_limits, labels = C_Class_labels)
ggsave(file.path(outputPath, "Assessment_Map_C_11.png"), width = 12, height = 9, dpi = 300)

ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Confidence ", assessmentPeriod)) +
  geom_sf(aes(fill = C_12_Class)) +
  scale_fill_manual(name = "C_12", values = C_Class_colors, limits = C_Class_limits, labels = C_Class_labels)
ggsave(file.path(outputPath, "Assessment_Map_C_12.png"), width = 12, height = 9, dpi = 300)

ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Confidence ", assessmentPeriod)) +
  geom_sf(aes(fill = C_2_Class)) +
  scale_fill_manual(name = "C_2", values = C_Class_colors, limits = C_Class_limits, labels = C_Class_labels)
ggsave(file.path(outputPath, "Assessment_Map_C_2.png"), width = 12, height = 9, dpi = 300)

ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Confidence ", assessmentPeriod)) +
  geom_sf(aes(fill = C_3_Class)) +
  scale_fill_manual(name = "C_3", values = C_Class_colors, limits = C_Class_limits, labels = C_Class_labels)
ggsave(file.path(outputPath, "Assessment_Map_C_3.png"), width = 12, height = 9, dpi = 300)

# Create Assessment Indicator maps
for (i in 1:nrow(indicators)) {
  indicatorID <- indicators[i, IndicatorID]
  indicatorCode <- indicators[i, Code]
  indicatorName <- indicators[i, Name]
  indicatorYearMin <- indicators[i, YearMin]
  indicatorYearMax <- indicators[i, YearMax]
  indicatorMonthMin <- indicators[i, MonthMin]
  indicatorMonthMax <- indicators[i, MonthMax]
  indicatorDepthMin <- indicators[i, DepthMin]
  indicatorDepthMax <- indicators[i, DepthMax]
  indicatorYearMin <- indicators[i, YearMin]
  indicatorMetric <- indicators[i, Metric]
  
  wk <- wk5[IndicatorID == indicatorID] %>% setkey(UnitID)
  
  wk <- merge(units, wk, all.x = TRUE)  

  # Status map (EQRS)
  title <- paste0("Eutrophication Status ", indicatorYearMin, "-", indicatorYearMax)
  subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
  subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
  subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
  subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
  fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_EQRS", ".png"))
  
  ggplot(wk) +
    labs(title = title , subtitle = subtitle) +
    geom_sf(aes(fill = EQRS_Class)) +
    scale_fill_manual(name = "EQRS", values = EQRS_Class_colors, limits = EQRS_Class_limits, labels = EQRS_Class_labels)
  ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)
  
  # Temporal Confidence map (TC)
  title <- paste0("Eutrophication Temporal Confidence ", indicatorYearMin, "-", indicatorYearMax)
  subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
  subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
  subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
  subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
  fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_TC", ".png"))
  
  ggplot(wk) +
    labs(title = title , subtitle = subtitle) +
    geom_sf(aes(fill = TC_Class)) +
    scale_fill_manual(name = "TC", values = C_Class_colors, limits = C_Class_limits, labels = C_Class_labels)
  ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)
  
  # Spatial Confidence map (SC)
  title <- paste0("Eutrophication Spatial Confidence ", indicatorYearMin, "-", indicatorYearMax)
  subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
  subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
  subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
  subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
  fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_SC", ".png"))
  
  ggplot(wk) +
    labs(title = title , subtitle = subtitle) +
    geom_sf(aes(fill = SC_Class)) +
    scale_fill_manual(name = "SC", values = C_Class_colors, limits = C_Class_limits, labels = C_Class_labels)
  ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)
  
  # Accuracy Confidence Class map (ACC)
  title <- paste0("Eutrophication Accuracy Class Confidence ", indicatorYearMin, "-", indicatorYearMax)
  subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
  subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
  subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
  subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
  fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_ACC", ".png"))
  
  ggplot(wk) +
    labs(title = title , subtitle = subtitle) +
    geom_sf(aes(fill = ACC_Class)) +
    scale_fill_manual(name = "ACC", values = C_Class_colors, limits = C_Class_limits, labels = C_Class_labels)
  ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)
  
  # Confidence map (C)
  title <- paste0("Eutrophication Confidence ", indicatorYearMin, "-", indicatorYearMax)
  subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
  subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
  subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
  subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
  fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_C", ".png"))
  
  ggplot(wk) +
    labs(title = title , subtitle = subtitle) +
    geom_sf(aes(fill = C_Class)) +
    scale_fill_manual(name = "C", values = C_Class_colors, limits = C_Class_limits, labels = C_Class_labels)
  ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)
}

# Create Annual Indicator bar charts
for (i in 1:nrow(indicators)) {
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

    title <- paste0("Eutrophication State [ES, CI, N] and Threshold [ET] ", indicatorYearMin, "-", indicatorYearMax)
    subtitle <- paste0(indicatorName, " (", indicatorCode, ")", " in ", unitName, " (", unitCode, ")", "\n")
    subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
    subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
    subtitle <- paste0(subtitle, "Metric: ", indicatorMetric, ", ")
    subtitle <- paste0(subtitle, "Unit: ", indicatorUnit)
    fileName <- gsub(":", "", paste0("Annual_Indicator_Bar_", indicatorCode, "_", unitCode, ".png"))

    wk <- wk3[IndicatorID == indicatorID & UnitID == unitID]

    if (nrow(wk) > 0 & indicatorMetric %in% c("Mean")) {
      ggplot(wk, aes(x = factor(Period, levels = indicatorYearMin:indicatorYearMax), y = ES)) +
        labs(title = title , subtitle = subtitle) +
        geom_col() +
        geom_text(aes(label = N), vjust = -0.25, hjust = -0.25) +
        geom_errorbar(aes(ymin = ES - CI, ymax = ES + CI), width = .2) +
        geom_hline(aes(yintercept = ET)) +
        scale_x_discrete(NULL, factor(indicatorYearMin:indicatorYearMax), drop=FALSE) +
        scale_y_continuous(NULL)

      ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)
    }
    if (nrow(wk) > 0 & indicatorMetric %in% c("Minimum", "5th percentile", "5th percentile of deepest sample within 10 meters from bottom", "10th percentile", "90th percentile")) {
      ggplot(wk, aes(x = factor(Period, levels = indicatorYearMin:indicatorYearMax), y = ES)) +
        labs(title = title , subtitle = subtitle) +
        geom_col() +
        geom_text(aes(label = N), vjust = -0.25, hjust = -0.25) +
        #geom_errorbar(aes(ymin = ES - CI, ymax = ES + CI), width = .2) +
        geom_hline(aes(yintercept = ET)) +
        scale_x_discrete(NULL, factor(indicatorYearMin:indicatorYearMax), drop=FALSE) +
        scale_y_continuous(NULL)
      
      ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)
    }
  }
}
