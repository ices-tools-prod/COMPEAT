# Install and load R packages ---------------------------------------------
# 
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "data.table", "ggplot2")
ipak(packages)

# Read assessment units from WKT
units <- st_read(dsn = "Assessment_Areas/COMP4_Assessment_Areas.csv") %>%
  st_set_crs(4326)

# Remove unnessasary dimensions and convert data.frame to data.table
units <- as.data.table(st_zm(units)) 

# Order, Rename and Remove columns 
units <- units[order(ID),list(Code = ID, Description = LongName, GEOM = geometry)] %>%
  st_sf()

# Assign IDs
units$UnitID = 1:nrow(units)

# Identify invalid geometries
st_is_valid(units, reason = TRUE)

# Write to database
#st_write(
#  units,
#  dsn = "MSSQL:server=SQL09;database=OceanCOMPEAT_20062014_COMP4;trusted_connection=yes;",
#  layer = "AssessmentUnit",
#  layer_options = c("LAUNDER=NO", "GEOM_NAME=GEOM", "FID=ID")
#  )

# Read from database
#units <- st_read(dsn = "MSSQL:server=SQL09;database=OceanCOMPEAT_20062014_COMP4;trusted_connection=yes;", layer = "AssessmentUnit", promote_to_multi = FALSE) %>%
#  st_set_crs(4326)

# Plot
#plot(units$GEOM, axes = TRUE)
#ggplot() + geom_sf(data = units) + coord_sf() 

# Transform projection into ETRS_1989_LAEA
units <- st_transform(units, crs = 3035)

# Identify invalid geometries
st_is_valid(units)

# Make geometries valid by doing the buffer of nothing trick
units <- sf::st_buffer(units, 0.0)

# Identify overlapping assessment units
st_overlaps(units)

bbox <- st_bbox(units)

gridSize <- 60000

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

#st_write(grid, "D:/COMPEAT/grid.shp")
#st_write(gridunits, "D:/COMPEAT/gridunits.shp")
#st_write(units, "D:/COMPEAT/units.shp")

#plot(grid$geometry, axes = TRUE)
#plot(units$GEOM, add = TRUE)
#plot(gridunits, add = TRUE, col = "red")


grid <- function(units, gridSize) {
  units <- st_transform(units, crs = 3035) # Transform projection into ETRS_1989_LAEA
  
  bbox <- st_bbox(units)

  xmin <- floor(bbox$xmin / gridSize) * gridSize
  ymin <- floor(bbox$ymin / gridSize) * gridSize
  xmax <- ceiling(bbox$xmax / gridSize) * gridSize
  ymax <- ceiling(bbox$ymax / gridSize) * gridSize
  
  xn <- (xmax - xmin) / gridSize
  yn <- (ymax - ymin) / gridSize
  
  grid <- st_make_grid(units, cellsize = gridSize, c(xmin, ymin), n = c(xn, yn), crs = 3035) %>% st_sf()
  
  grid$GridID = 1:nrow(grid)
}

grid <- grid(units, 60000)

