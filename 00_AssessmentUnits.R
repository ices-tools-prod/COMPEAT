# OSPAR Assessment Units -------------------------------------------------------

# Read from database
#OSPAR_Assessment_Units <- sf::st_read("MSSQL:server=SQL09;database=OceanCOMPEAT_20062014_COMP4;trusted_connection=yes;", "AssessmentUnit")

# Set CRS
#sf::st_crs(OSPAR_Assessment_Units) = 4326

# Read from shapefile
#OSPAR_Assessment_Units <- sf::st_read("Input/COMP4_assessment_areas_v7b/COMP4_assessment_areas_v7b.shp")

# Write as WKT
#sf::st_write(OSPAR_Assessment_Units, "Input/COMP4_assessment_areas_v7b/COMP4_assessment_areas_v6d.csv", layer_options = "GEOMETRY=AS_WKT")

# Read from WKT
OSPAR_Assessment_Units <- sf::st_read("Assessment_Areas/COMP4_Assessment_Areas.csv")

# Set CRS
sf::st_crs(OSPAR_Assessment_Units) = 4326

# Plot
plot(OSPAR_Assessment_Units)

ggplot() + geom_sf(data = OSPAR_Assessment_Units) + coord_sf() 

# Identify invalid assessment units
sf::st_is_valid(OSPAR_Assessment_Units, reason = TRUE)

# Identify overlapping assessment units
sf::st_overlaps(OSPAR_Assessment_Units, sparse = FALSE)
