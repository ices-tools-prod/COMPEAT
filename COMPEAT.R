# Install and load R packages --------------------------------------------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "data.table", "ggplot2", "tidyverse", "mapview")
ipak(packages)

# Define paths
inputPath <- "Input"
outputPath <- "Output"

# Create paths
dir.create(inputPath, showWarnings = FALSE, recursive = TRUE)
dir.create(outputPath, showWarnings = FALSE, recursive = TRUE)

# Data -------------------------------------------------------------------------

# Download and unpack files needed for the assessment
download.file.unzip.maybe <- function(url, refetch = FALSE, path = ".") {
  dest <- file.path(path, sub("\\?.+", "", basename(url)))
  if (refetch || !file.exists(dest)) {
    download.file(url, dest, mode = "wb")
    if (tools::file_ext(dest) == "zip") {
      unzip(dest, exdir = path)
    }
  }
}

urls <- c("https://www.dropbox.com/s/xzktj5nejp6tyn8/AssessmentUnits.zip?dl=1",
          "https://www.dropbox.com/s/gf0mxjy7gy9qprp/Indicators.txt?dl=1",
          "https://www.dropbox.com/s/zp4u49oz0orihjg/IndicatorUnits.txt?dl=1",
          "https://www.dropbox.com/s/00qach95phe7wtm/UnitGridSize.txt?dl=1",
          "https://www.dropbox.com/s/tvt1j8afh1afdp5/StationSamples.zip?dl=1")

files <- sapply(urls, download.file.unzip.maybe, path = inputPath)

unitsFile <- file.path(inputPath, "AssessmentUnits.csv")
indicatorsFile <- file.path(inputPath, "Indicators.txt")
indicatorUnitsFile <- file.path(inputPath, "IndicatorUnits.txt")
unitGridSizeFile <- file.path(inputPath, "UnitGridSize.txt")
stationSamplesFile <- file.path(inputPath, "StationSamples.txt")

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

unitGridSize <-  fread(input = unitGridSizeFile, sep = "\t") %>% setkey(UnitID)

a <- merge(unitGridSize[GridSize == 10], gridunits10 %>% select(UnitID, GridID, GridArea = Area))
b <- merge(unitGridSize[GridSize == 30], gridunits30 %>% select(UnitID, GridID, GridArea = Area))
c <- merge(unitGridSize[GridSize == 60], gridunits60 %>% select(UnitID, GridID, GridArea = Area))
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
indicators <- fread(input = indicatorsFile, sep = "\t") %>% setkey(IndicatorID) 
indicatorUnits <- fread(input = indicatorUnitsFile, sep = "\t") %>% setkey(IndicatorID, UnitID)

wk1list = list()
wk2list = list()

# Loop indicator units ---------------------------------------------------------
# for (i in 1:nrow(indicatorUnits)) {
#   indicatorID <- indicatorUnits[i, IndicatorID]
# }

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

  # Add unit grid size
  wk <- wk[unitGridSize, on="UnitID", nomatch=0]

  # Filter stations rows and columns --> UnitID, GridID, GridArea, Period, Month, StationID, Depth, ES
  if (month.min > month.max) {
    wk0 <- wk[
      (Period >= year.min & Period <= year.max) &
        (Month >= month.min | Month <= month.max) &
        (Depth..m.db..PRIMARYVAR.DOUBLE >= depth.min & Depth..m.db..PRIMARYVAR.DOUBLE <= depth.max) &
        !is.na(ES) & 
        !is.na(UnitID),
      .(IndicatorID = indicatorID, UnitID, GridSize, GridID, GridArea, Period, Month, StationID = StationID.METAVAR.INDEXED_TEXT, Depth = Depth..m.db..PRIMARYVAR.DOUBLE, ES)]
  } else {
    wk0 <- wk[
      (Period >= year.min & Period <= year.max) &
        (Month >= month.min & Month <= month.max) &
        (Depth..m.db..PRIMARYVAR.DOUBLE >= depth.min & Depth..m.db..PRIMARYVAR.DOUBLE <= depth.max) &
        !is.na(ES) & 
        !is.na(UnitID),
      .(IndicatorID = indicatorID, UnitID, GridSize, GridID, GridArea, Period, Month, StationID = StationID.METAVAR.INDEXED_TEXT, Depth = Depth..m.db..PRIMARYVAR.DOUBLE, ES)]
  }
  
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

# Calculate Eutrophication Ratio (ER)
wk3[, ER := ifelse(Response == 1, ES / ET, ET / ES)]

# Calculate (BEST)
wk3[, BEST := ifelse(Response == 1, ET / (1 + ACDEV / 100), ET / (1 - ACDEV / 100))]

# Calculate Ecological Quality Ratio (ERQ)
wk3[, EQR := ifelse(Response == 1, BEST / ES, ES / BEST)]

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

# Calculate General Temporal Confidence (GTC) - Confidence in number of annual observations
wk3[, GTC := ifelse(N > GTC_HM, 100, ifelse(N < GTC_ML, 0, 50))]

# Calculate Number of Months Potential
wk3[, NMP := ifelse(MonthMin > MonthMax, 12 - MonthMin + 1 + MonthMax, MonthMax - MonthMin + 1)]

# Calculate Specific Temporal Confidence (STC) - Confidence in number of annual missing months
wk3[, STC := ifelse(NMP - NM <= STC_HM, 100, ifelse(NMP - NM >= STC_ML, 0, 50))]

# Calculate General Spatial Confidence (GSC) - Confidence in number of annual observations per number of grids 
wk3 <- wk3[as.data.table(gridunits)[, .(NG = .N), .(UnitID)], on = .(UnitID = UnitID), nomatch=0]
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
wk4 <- wk3[, .(Period = min(Period) * 10000 + max(Period), ES = mean(ES), SD = sd(ES), N = .N, GTC = mean(GTC), STC = mean(STC), GSC = mean(GSC), SSC = mean(SSC)), .(IndicatorID, UnitID)]

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

# Calculate Eutrophication Ratio (ER)
wk5[, ER := ifelse(Response == 1, ES / ET, ET / ES)]

# Calculate (BEST)
wk5[, BEST := ifelse(Response == 1, ET / (1 + ACDEV / 100), ET / (1 - ACDEV / 100))]

# Calculate Ecological Quality Ratio (ERQ)
wk5[, EQR := ifelse(Response == 1, BEST / ES, ES / BEST)]

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

# Category ---------------------------------------------------------------------

# Category result as a weighted average of the indicators in each category per unit - CategoryID, UnitID, N, ER, EQR, EQRS, C
wk6 <- wk5[, .(.N, ER = sum(ER * IW / 100), EQR = sum(EQR * IW / 100), EQRS = sum(EQRS * IW / 100), C = sum(C * IW / 100)), .(CategoryID, UnitID)]

wk7 <- dcast(wk6, UnitID ~ CategoryID, value.var = c("N","ER","EQR","EQRS","C"))

# Assessment -------------------------------------------------------------------

# Assessment result - UnitID, N, ER, EQR, EQRS, C
wk8 <- wk6[, .(.N, ER = max(ER), EQR = min(EQR), EQRS = min(EQRS), C = mean(C)), (UnitID)]

wk9 <- wk7[wk8, on = .(UnitID = UnitID), nomatch=0]

# Write results
fwrite(wk3, file = file.path(outputPath, "Indicator_Annual.csv"))
fwrite(wk5, file = file.path(outputPath, "Indicator_Assessment.csv"))
fwrite(wk9, file = file.path(outputPath, "Assessment.csv"))

