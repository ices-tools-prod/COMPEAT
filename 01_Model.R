# Install and load R packages ---------------------------------------------
# 
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "data.table", "ggplot2", "TTAinterfaceTrendAnalysis")
ipak(packages)

# Station Samples --------------------------------------------------------------
# Extract and Classify stations into OSPAR assessment units from ICES Oceanographic database into OSPAR COMP Assessment database

# Read classified station samples
stationSamples <- fread("Input/StationSamples.txt", sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)

# Dissolved inorganic Nitrogen - DIN (Winter) ----------------------------------
#   Parameters: [NO3-N] + [NO2-N] + [NH4-N]
#   Depth: <= 10
#   Period: December - February
#   Aggregation Method: Arithmetric mean of mean by station per year

# Copy data
wk <- stationSamples

# Count unique stations
wk[,.(count = uniqueN(StationID))]

# Create grouping variable
wk$Period <- with(wk, ifelse(Month == 12, Year + 1, Year))

# Create indicator
coalesce <- function(x) {
  if (all(is.na(x)) | is.na(x[1])){
    NA
  } else {
    sum(x, na.rm = TRUE)
  } 
}
wk$DIN..umol.l. <- apply(wk[, list(Nitrate..umol.l., Nitrite..umol.l., Ammonium..umol.l.)], 1, coalesce)

# Filter stations rows and columns --> AssessmentUnitID, Period, Depth, Temperature, Salinity, DIN
wk0 <- wk[Depth..m.db. <= 10 & (Month >= 12 | Month <= 2) & (Period >= 2006 & Period <= 2014) & !is.na(DIN..umol.l.), list(AssessmentUnitID = AssessmentUnitID.METAVAR.INDEXED_TEXT, Period, StationID, Depth = Depth..m.db., Temperature = Temperature..degC., Salinity = Salinity..., ES = DIN..umol.l.)]

# Get linear regression coefficients on Indicator~Salinity + mean Salinity
wk00 <- wk0[,
    list(
      N = .N,
      MeanSalinity = mean(Salinity, na.rm = TRUE),
      B = coef(lm(ES~Salinity))[1],
      A = coef(lm(ES~Salinity))[2],
      P = summary(lm(ES~Salinity))$coef[2, 4],
      R2 = summary(lm(ES~Salinity))$adj.r.squared),
    by = AssessmentUnitID]

# Merge data tables
setkey(wk0, "AssessmentUnitID")
setkey(wk00, "AssessmentUnitID")
wk000 <- wk0[wk00]

# Normalise indicator concentration if the indicator has a significant relation to salinity e.g. above the 95% confidence level (p<0.05)
# ES_normalised = ES_observed + A * (S_reference - S_observed)
# https://www.ospar.org/site/assets/files/37302/national_common_procedure_report_2016_sweden.pdf
wk000$ESS <- with(wk000, ifelse(P < 0.05, ES + A * (MeanSalinity - Salinity), ES)) 

# Calculate indicator station mean --> AssessmentUnitID, Period, StationID, ES
wk1 <- wk000[, list(ES = mean(ES)), list(AssessmentUnitID, Period, StationID)]

# Calculate indicator annual mean --> AssessmentUnitID, Period, ES, SD, N
wk2 <- wk1[, list(ES = mean(ES), SD = sd(ES), N = .N), list(AssessmentUnitID, Period)]

# Calculate indicator assessment unit mean --> AssessmentUnitID, ES, SD, N
wk4 <- wk2[, list(ES = mean(ES), SD = sd(ES), N = sum(N)), list(AssessmentUnitID)]

# Salinity Mixing diagram per assessment unit
ggplot(wk0, aes(Salinity, ES)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE, formula = y ~ x) +
  facet_wrap(~AssessmentUnitID)

# Trend diagram per assessment unit
ggplot(wk000, aes(Period, ESS)) +
  geom_point() +
  facet_wrap(~AssessmentUnitID)










# Dissolved inorganic Phophorus - DIP (Winter) ---------------------------------

# Chlorophyll a (Summer) -------------------------------------------------------

# Dissolved Oxygen () ---------------------------------------------------------- 