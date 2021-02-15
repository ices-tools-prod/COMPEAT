# Install and load R packages --------------------------------------------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "data.table", "tidyverse", "ggplot2", "ggmap", "mapview")
ipak(packages)

# Define paths
inputPath <- "Input"
outputPath <- "Output"

# Define assessment period - Uncomment the period you want to run the assessment for!
assessmentPeriod <- "2006-2014"
#assessmentPeriod <- "2015-2020"

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

if (assessmentPeriod == "2006-2014"){
  # Assessment Period 2006-2014
  urls <- c("https://www.dropbox.com/s/xzktj5nejp6tyn8/AssessmentUnits.zip?dl=1",
            "https://www.dropbox.com/s/2wf5keany1jv5je/Indicators.csv?dl=1",
            "https://www.dropbox.com/s/n6p0x5onmi9ugga/IndicatorUnits.csv?dl=1",
            "https://www.dropbox.com/s/l1ymgionvhcjk2w/UnitGridSize.csv?dl=1",
            "https://www.dropbox.com/s/vwdoi9slemltdzh/StationSamples.txt.gz?dl=1")  
} else {
  # Assessment Period 2015-2020
  urls <- c("https://www.dropbox.com/s/zpu0t1zc3uk1jlw/AssessmentUnits.zip?dl=1",
            "https://www.dropbox.com/s/0idmdxxcbinz4qf/Indicators.csv?dl=1",
            "https://www.dropbox.com/s/jqb03sfdqa18cph/IndicatorUnits.csv?dl=1",
            "https://www.dropbox.com/s/cubpuuus8ab7aki/UnitGridSize.csv?dl=1",
            "https://www.dropbox.com/s/2er9ngl5rnon426/StationSamples.txt.gz?dl=1")  
}

files <- sapply(urls, download.file.unzip.maybe, path = inputPath)

unitsFile <- file.path(inputPath, "AssessmentUnits.csv")
indicatorsFile <- file.path(inputPath, "Indicators.csv")
indicatorUnitsFile <- file.path(inputPath, "IndicatorUnits.csv")
unitGridSizeFile <- file.path(inputPath, "UnitGridSize.csv")
stationSamplesFile <- file.path(inputPath, "StationSamples.txt.gz")

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

# Identify invalid geometries
st_is_valid(units)

# Write to database
# st_write(
#   units,
#   dsn = "MSSQL:server=SQL09;database=OceanCOMPEAT_20062014_COMP4;trusted_connection=yes;",
#   layer = "AssessmentUnit",
#   layer_options = c("LAUNDER=NO", "GEOM_NAME=GEOM", "FID=ID")
# )

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

unitGridSize <-  fread(input = unitGridSizeFile) %>% setkey(UnitID)

a <- merge(unitGridSize[GridSize == 10000], gridunits10 %>% select(UnitID, GridID, GridArea = Area))
b <- merge(unitGridSize[GridSize == 30000], gridunits30 %>% select(UnitID, GridID, GridArea = Area))
c <- merge(unitGridSize[GridSize == 60000], gridunits60 %>% select(UnitID, GridID, GridArea = Area))
gridunits <- st_as_sf(rbindlist(list(a,b,c)))
rm(a,b,c)

# Plot
#ggplot() + geom_sf(data = units) + coord_sf()
#ggplot() + geom_sf(data = gridunits10) + coord_sf()
#ggplot() + geom_sf(data = gridunits30) + coord_sf()
#ggplot() + geom_sf(data = gridunits60) + coord_sf()
#ggplot() + geom_sf(data = gridunits) + coord_sf()

# Read stationSamples ----------------------------------------------------------
stationSamples <- fread(input = stationSamplesFile, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)

# Stations
#stationSamples[, StationID := .GRP, by = .(Cruise, Station, Year, Month, Day, Hour, Minute, Latitude..degrees_north., Longitude..degrees_east.)]
#stationSamples[, .N, .(StationID, Cruise, Station, Year, Month, Day, Hour, Minute, Latitude..degrees_north., Longitude..degrees_east.)]
#stationSamples[, .N, .(StationID.METAVAR.INDEXED_TEXT)]

# Samples
#stationSamples[, SampleID := .GRP, by = .(Cruise, Station, Year, Month, Day, Hour, Minute, Latitude..degrees_north., Longitude..degrees_east., Depth..m.db..PRIMARYVAR.DOUBLE)]
#stationSamples[, .N, .(StationID, Cruise, Station, Year, Month, Day, Hour, Minute, Latitude..degrees_north., Longitude..degrees_east., Depth..m.db..PRIMARYVAR.DOUBLE)]
#stationSamples[, .N, .(SampleID.METAVAR.INDEXED_TEXT)]

# Make stations spatial keeping original latitude/longitude
stationSamples <- st_as_sf(stationSamples, coords = c("Longitude..degrees_east.", "Latitude..degrees_north."), remove = FALSE, crs = 4326)

# Transform projection into ETRS_1989_LAEA
stationSamples <- st_transform(stationSamples, crs = 3035)

# Classify stations into assessment units
#stationSamples$UnitID <- st_intersects(stationSamples, units) %>% as.numeric()

# Classify stations into 10,30 and 60k gridunits
#stationSamples <- st_join(stationSamples, gridunits10 %>% select(GridID.10k = GridID, Area.10k = Area), join = st_intersects)
#stationSamples <- st_join(stationSamples, gridunits30 %>% select(GridID.30k = GridID, Area.30k = Area), join = st_intersects)
#stationSamples <- st_join(stationSamples, gridunits60 %>% select(GridID.60k = GridID, Area.60k = Area), join = st_intersects)

stationSamples <- st_join(stationSamples, st_cast(gridunits), join = st_intersects)

# Remove spatial column
stationSamples <- st_set_geometry(stationSamples, NULL)

# Read indicator configuration files -------------------------------------------
indicators <- fread(input = indicatorsFile) %>% setkey(IndicatorID) 
indicatorUnits <- fread(input = indicatorUnitsFile) %>% setkey(IndicatorID, UnitID)

wk1list = list()
wk2list = list()

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
    wk$ES <- apply(wk[, list(Nitrate..umol.l., Nitrite..umol.l., Ammonium..umol.l.)], 1, function(x){
      if (all(is.na(x)) | is.na(x[1])) {
        NA
      }
      else {
        sum(x, na.rm = TRUE)
      }
    })
  }
  else if (name == 'Dissolved Inorganic Phosphorus') {
    wk[,ES := Phosphate..umol.l.]
  }
  else if (name == 'Chlorophyll a') {
    wk[, ES := Chlorophyll.a..ug.l.]
  }
  else if (name == 'Oxygen Deficiency') {
    wk[, ES := Oxygen..ml.l. / 0.7] # Convert ml/l to mg/l by factor of 0.7
  }
  else if (name == 'Total Nitrogen') {
    wk[,ES := Total.Nitrogen..umol.l.]
  }
  else if (name == 'Total Phosphorus') {
    wk[,ES := Total.Phosphorus..umol.l.]
  }
  else if (name == 'Secchi Depth') {
    wk[,ES := Secchi.Depth..m..METAVAR.DOUBLE]
  }
  else if (name == 'Dissolved Inorganic Nitrogen/Dissolved Inorganic Phosphorus') {
    wk$ES <- apply(wk[, list(Nitrate..umol.l., Nitrite..umol.l., Ammonium..umol.l.)], 1, function(x){
      if (all(is.na(x)) | is.na(x[1])) {
        NA
      }
      else {
        sum(x, na.rm = TRUE)
      }
    })
    wk[,ES := ES/Phosphate..umol.l.]
  }
  else if (name == 'Total Nitrogen/Total Phosphorus') {
    wk[,ES := Total.Nitrogen..umol.l./Total.Phosphorus..umol.l.]
  }
  else {
    next
  }

  # Add unit grid size
  wk <- wk[unitGridSize, on="UnitID", nomatch=0]

  # Filter stations rows and columns --> UnitID, GridID, GridArea, Period, Month, StationID, Depth, Temperature, Salinity, ES
  if (month.min > month.max) {
    wk0 <- wk[
      (Period >= year.min & Period <= year.max) &
        (Month >= month.min | Month <= month.max) &
        (Depth..m.db..PRIMARYVAR.DOUBLE >= depth.min & Depth..m.db..PRIMARYVAR.DOUBLE <= depth.max) &
        !is.na(ES) & 
        !is.na(UnitID),
      .(IndicatorID = indicatorID, UnitID, GridSize, GridID, GridArea, Period, Month, StationID = StationID.METAVAR.INDEXED_TEXT, Depth = Depth..m.db..PRIMARYVAR.DOUBLE, Temperature = Temperature..degC., Salinity = Salinity..., ES)]
  } else {
    wk0 <- wk[
      (Period >= year.min & Period <= year.max) &
        (Month >= month.min & Month <= month.max) &
        (Depth..m.db..PRIMARYVAR.DOUBLE >= depth.min & Depth..m.db..PRIMARYVAR.DOUBLE <= depth.max) &
        !is.na(ES) & 
        !is.na(UnitID),
      .(IndicatorID = indicatorID, UnitID, GridSize, GridID, GridArea, Period, Month, StationID = StationID.METAVAR.INDEXED_TEXT, Depth = Depth..m.db..PRIMARYVAR.DOUBLE, Temperature = Temperature..degC., Salinity = Salinity..., ES)]
  }

  # Salinity Normalisation for Nutrients
  if (name == 'Dissolved Inorganic Nitrogen' || name == 'Dissolved Inorganic Phosphorus') {
    # Get linear regression coefficients on ES~Salinity and Mean Salinity
    wk00 <- wk0[!is.na(Salinity),
                .(N = .N,
                  MeanSalinity = mean(Salinity, na.rm = TRUE),
                  B = coef(lm(ES~Salinity))[1],
                  A = coef(lm(ES~Salinity))[2],
                  P = ifelse(.N >= 2, summary(lm(ES~Salinity))$coef[2, 4], NA_real_),
                  R2 = summary(lm(ES~Salinity))$adj.r.squared),
                keyby = .(IndicatorID, UnitID)]
  }
  
  # Merge data tables
  wk0 <- wk00[wk0]

  # Normalise indicator concentration if the indicator has a significant relation to salinity e.g. above the 95% confidence level (p<0.05)
  # ES_normalised = ES_observed + A * (S_reference - S_observed)
  # https://www.ospar.org/site/assets/files/37302/national_common_procedure_report_2016_sweden.pdf
  wk0[, ESS := ifelse(P < 0.05 & !is.na(P) & !is.na(Salinity), ES + A * (MeanSalinity - Salinity), ES)]
  
  # NB! Salinity Normalisation above currently only implemented as test and not taken forward yet!  
  
  if (metric == 'Mean'){
    # Calculate station mean --> UnitID, GridID, GridArea, Period, Month, ES, SD, N
    wk1 <- wk0[, .(ES = mean(ES), SD = sd(ES), N = .N), keyby = .(IndicatorID, UnitID, GridID, GridArea, Period, Month, StationID)]
    
    # Calculate annual mean --> UnitID, Period, ES, SD, N, NM
    wk2 <- wk1[, .(ES = mean(ES), SD = sd(ES), N = .N, NM = uniqueN(Month)), keyby = .(IndicatorID, UnitID, Period)]
  } else if (metric == 'Minimum') {
    # Calculate station minimum --> UnitID, GridID, GridArea, Period, Month, ES, SD, N
    wk1 <- wk0[, .(ES = min(ES), SD = sd(ES), N = .N), keyby = .(IndicatorID, UnitID, GridID, GridArea, Period, Month, StationID)]
    
    # Calculate annual minimum --> UnitID, Period, ES, SD, N, NM
    wk2 <- wk1[, .(ES = min(ES), SD = sd(ES), N = .N, NM = uniqueN(Month)), keyby = .(IndicatorID, UnitID, Period)]
  }
  
  wk1list[[i]] <- wk1
  wk2list[[i]] <- wk2
}

# Combine station and annual indicator results
wk1 <- rbindlist(wk1list)
wk2 <- rbindlist(wk2list)

# Combine with indicator and indicator unit configuration tables
wk3 <- indicators[indicatorUnits[wk2]]

# Standard Error
wk3[, SE := SD / sqrt(N)]

# 95 % Confidence Interval
wk3[, CI := qnorm(0.975) * SE]

# Calculate Eutrophication Ratio (ER)
wk3[, ER := ifelse(Response == 1, ES / ET, ET / ES)]

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

# Calculate General Temporal Confidence (GTC) - Confidence in number of annual observations
wk3[, GTC := ifelse(N > GTC_HM, 100, ifelse(N < GTC_ML, 0, 50))]

# Calculate Number of Months Potential
wk3[, NMP := ifelse(MonthMin > MonthMax, 12 - MonthMin + 1 + MonthMax, MonthMax - MonthMin + 1)]

# Calculate Specific Temporal Confidence (STC) - Confidence in number of annual missing months
wk3[, STC := ifelse(NMP - NM <= STC_HM, 100, ifelse(NMP - NM >= STC_ML, 0, 50))]

# Calculate General Spatial Confidence (GSC) - Confidence in number of annual observations per number of grids 
#wk3 <- wk3[as.data.table(gridunits)[, .(NG = .N), .(UnitID)], on = .(UnitID = UnitID), nomatch=0]
wk3 <- wk3[as.data.table(gridunits)[, .(NG = as.numeric(sum(GridArea) / mean(GridSize^2))), .(UnitID)], on = .(UnitID = UnitID), nomatch=0]
wk3[, GSC := ifelse(N / NG > GSC_HM, 100, ifelse(N / NG < GSC_ML, 0, 50))]

# Calculate Specific Spatial Confidence (SSC) - Confidence in area of sampled grid units as a percentage to the total unit area
a <- wk1[, .N, keyby = .(IndicatorID, UnitID, Period, GridID, GridArea)] # UnitGrids
b <- a[, .(GridArea = sum(as.numeric(GridArea))), keyby = .(IndicatorID, UnitID, Period)] #GridAreas
c <- as.data.table(units)[, .(UnitArea = as.numeric(UnitArea)), keyby = .(UnitID)] # UnitAreas
d <- c[b, on = .(UnitID = UnitID)] # UnitAreas ~ GridAreas
wk3 <- wk3[d[,.(IndicatorID, UnitID, Period, UnitArea, GridArea)], on = .(IndicatorID = IndicatorID, UnitID = UnitID, Period = Period)]
wk3[, SSC := ifelse(GridArea / UnitArea * 100 > SSC_HM, 100, ifelse(GridArea / UnitArea * 100 < SSC_ML, 0, 50))]
rm(a,b,c,d)

# Calculate assessment ES --> UnitID, Period, ES, SD, N, GTC, STC, GSC, SSC
wk4 <- wk3[, .(Period = min(Period) * 10000 + max(Period), ES = mean(ES), SD = sd(ES), N = .N, N_OBS = sum(N), GTC = mean(GTC), STC = mean(STC), GSC = mean(GSC), SSC = mean(SSC)), .(IndicatorID, UnitID)]

# Add Year Count where STC = 100
wk4 <- wk3[STC == 100, .(NSTC100 = .N), .(IndicatorID, UnitID)][wk4, on = .(IndicatorID, UnitID)]
wk4[, STC := ifelse(!is.na(NSTC100) & NSTC100 >= N/2, 100, STC)]

wk4 <- wk4[, TC := (GTC + STC) / 2]

wk4 <- wk4[, SC := (GSC + SSC) / 2]

wk4 <- wk4[, C := (TC + SC) / 2]

# Combine with indicator and indicator unit configuration tables
wk5 <- indicators[indicatorUnits[wk4]]

# Standard Error
wk5[, SE := SD / sqrt(N)]

# 95 % Confidence Interval
wk5[, CI := qnorm(0.975) * SE]

#-------------------------------------------------------------------------------
# Accuracy Confidence Assessment
# ------------------------------------------------------------------------------

# Accuracy Confidence Level for Non-Problem Area
wk5[, ACL_NPA := ifelse(Response == 1, pnorm(ET, ES, SD), pnorm(ES, ET, SD))]

# Accuracy Confidence Level for Problem Area
wk5[, ACL_PA := 1 - ACL_NPA]

# Accuracy Confidence Level Area Class
wk5[, ACLAC := ifelse(ACL_NPA > 0.5, 1, ifelse(ACL_NPA < 0.5, 3, 2))]

# Accuracy Confidence Level
wk5[, ACL := ifelse(ACL_NPA > ACL_PA, ACL_NPA, ACL_PA)]

# Accuracy Confidence Level Class
wk5[, ACLC := ifelse(ACL > 0.9, 100, ifelse(ACL < 0.7, 0, 50))]

# ------------------------------------------------------------------------------

# Standard Error using number of observations behind the annual mean !!!
wk5[, SE_OBS := SD / sqrt(N_OBS)]

# Accuracy Confidence Level for Non-Problem Area
wk5[, ACL_NPA_OBS := ifelse(Response == 1, pnorm(ET, ES, SE_OBS), pnorm(ES, ET, SE_OBS))]

# Accuracy Confidence Level for Problem Area
wk5[, ACL_PA_OBS := 1 - ACL_NPA_OBS]

# Accuracy Confidence Level Area Class
wk5[, ACLAC_OBS := ifelse(ACL_NPA_OBS > 0.5, 1, ifelse(ACL_NPA_OBS < 0.5, 3, 2))]

# Accuracy Confidence Level
wk5[, ACL_OBS := ifelse(ACL_NPA_OBS > ACL_PA_OBS, ACL_NPA_OBS, ACL_PA_OBS)]

# Accuracy Confidence Level Class
wk5[, ACLC_OBS := ifelse(ACL_OBS > 0.9, 100, ifelse(ACL_OBS < 0.7, 0, 50))]

# ------------------------------------------------------------------------------

# Calculate Eutrophication Ratio (ER)
wk5[, ER := ifelse(Response == 1, ES / ET, ET / ES)]

# Calculate (BEST)
wk5[, BEST := ifelse(Response == 1, ET / (1 + ACDEV / 100), ET / (1 - ACDEV / 100))]

# Calculate Ecological Quality Ratio (ERQ)
wk5[, EQR := ifelse(Response == 1, ifelse(BEST > ES, 1, BEST / ES), ifelse(ES > BEST, 1, ES / BEST))]

# Calculate Ecological Quality Ratio Boundaries (ERQ_HG/GM/MP/PB)
wk5[, EQR_GM := ifelse(Response == 1, 1 / (1 + ACDEV / 100), 1 - ACDEV / 100)]
wk5[, EQR_HG := 0.5 * 0.95 + 0.5 * EQR_GM]
wk5[, EQR_PB := 2 * EQR_GM - 0.95]
wk5[, EQR_MP := 0.5 * EQR_GM + 0.5 * EQR_PB]

# Calculate Ecological Quality Ratio Scaled (EQRS)
wk5[, EQRS := ifelse(EQR <= EQR_PB, (EQR - 0) * (0.2 - 0) / (EQR_PB - 0) + 0,
                     ifelse(EQR <= EQR_MP, (EQR - EQR_PB) * (0.4 - 0.2) / (EQR_MP - EQR_PB) + 0.2,
                            ifelse(EQR <= EQR_GM, (EQR - EQR_MP) * (0.6 - 0.4) / (EQR_GM - EQR_MP) + 0.4,
                                   ifelse(EQR <= EQR_HG, (EQR - EQR_GM) * (0.8 - 0.6) / (EQR_HG - EQR_GM) + 0.6,
                                          (EQR - EQR_HG) * (1 - 0.8) / (1 - EQR_HG) + 0.8))))]

wk5[, EQRS_Class := ifelse(EQRS >= 0.8, "High",
                           ifelse(EQRS >= 0.6, "Good",
                                  ifelse(EQRS >= 0.4, "Moderate",
                                         ifelse(EQRS >= 0.2, "Poor","Bad"))))]

# Category ---------------------------------------------------------------------

# Category result as a weighted average of the indicators in each category per unit - CategoryID, UnitID, N, ER, EQR, EQRS, C
wk6 <- wk5[!is.na(ER), .(.N, ER = weighted.mean(ER, IW, na.rm = TRUE), EQR = weighted.mean(EQR, IW, na.rm = TRUE), EQRS = weighted.mean(EQRS, IW, na.rm = TRUE), C = weighted.mean(C, IW, na.rm = TRUE)), .(CategoryID, UnitID)]

wk7 <- dcast(wk6, UnitID ~ CategoryID, value.var = c("N","ER","EQR","EQRS","C"))

# Assessment -------------------------------------------------------------------

# Assessment result - UnitID, N, ER, EQR, EQRS, C
wk81 <- wk6[CategoryID %in% c(2,3), .(NE = .N, ER = max(ER), EQR = min(EQR), EQRS = min(EQRS)), (UnitID)] %>% setkey(UnitID)
wk82 <- wk6[, .(NC = .N, C = mean(C)), (UnitID)] %>% setkey(UnitID)
wk8 <- wk81[wk82]

wk9 <- wk7[wk8, on = .(UnitID = UnitID), nomatch=0]

wk9[, EQRS_Class := ifelse(EQRS >= 0.8, "High",
                           ifelse(EQRS >= 0.6, "Good",
                                  ifelse(EQRS >= 0.4, "Moderate",
                                         ifelse(EQRS >= 0.2, "Poor","Bad"))))]

# Write results
fwrite(wk3, file = file.path(outputPath, "Annual_Indicator.csv"))
fwrite(wk5, file = file.path(outputPath, "Assessment_Indicator.csv"))
fwrite(wk9, file = file.path(outputPath, "Assessment.csv"))

# Create plots
EQRS_Class_colors <- c("#3BB300", "#99FF66", "#FFCABF", "#FF8066", "#FF0000")
EQRS_Class_breaks <- c("High", "Good", "Moderate", "Poor", "Bad")
EQRS_Class_labels <- c(">= 0.8 - 1.0 (High)", ">= 0.6 - 0.8 (Good)", ">= 0.4 - 0.6 (Moderate)", ">= 0.2 - 0.4 (Poor)", ">= 0.0 - 0.2 (Bad)")

#basemap <- get_map(location=c(lon = -1, lat = 53), zoom = 5)

# Assessment map
wk <- merge(units, wk9, all.x = TRUE)

ggplot(wk) +
  ggtitle(label = paste0("Eutrophication Status ", assessmentPeriod)) +
  geom_sf(aes(fill = EQRS_Class)) +
  scale_fill_manual(name = "EQRS", values = EQRS_Class_colors, breaks = EQRS_Class_breaks, labels = EQRS_Class_labels)

ggsave(file.path(outputPath, "Assessment_Map.png"), width = 12, height = 9, dpi = 300)

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

  title <- paste0("Eutrophication Status ", indicatorYearMin, "-", indicatorYearMax)
  subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
  subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
  subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
  subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
  fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, ".png"))
    
  wk <- wk5[IndicatorID == indicatorID] %>% setkey(UnitID)
  
  wk <- merge(units, wk, all.x = TRUE)
  
  ggplot(wk) +
    labs(title = title , subtitle = subtitle) +
    geom_sf(aes(fill = EQRS_Class)) +
    scale_fill_manual(name = "EQRS", values = EQRS_Class_colors, breaks = EQRS_Class_breaks, labels = EQRS_Class_labels)
  
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

    if (nrow(wk) > 0) {
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
  }
}
