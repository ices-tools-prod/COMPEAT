# OSPAR Assessment Units -------------------------------------------------------

# Read SE Assessment Units
OSPAR_Assessment_Units_SE <- sf::st_read("Input/COMP_SE_area.shp")

# Reproject SE Assessment Units into WGS1984 
OSPAR_Assessment_Units_SE <- sf::st_transform(OSPAR_Assessment_Units_SE, crs = 4326)

# Write SE Assessment Units into Assessment Database
sf::st_write(OSPAR_Assessment_Units_SE, "MSSQL:server=SQL08;database=OceanCOMP_20062014;trusted_connection=yes;", "AssessmentUnitSE")

# Read GE Assessment Units
OSPAR_Assessment_Units_GE <- sf::st_read("Input/comp3_GE_gesamt.shp")

# Write GE Assessment Units into Assessment Database
sf::st_write(OSPAR_Assessment_Units_GE, "MSSQL:server=SQL08;database=OceanCOMP_20062014;trusted_connection=yes;", "AssessmentUnitGE")

# Combine Assessment Units within the OSPAR Assessment Database
# SELECT [OGR_Geometry], 'GE' AS [Country], [Area] AS [Description], [Assessment] AS [Classification] FROM [AssessmentUnitGE]
# SELECT [OGR_Geometry], 'SE' AS [Country], [Typ] AS [Description], [Assessment] AS [Classification] FROM [AssessmentUnitSE]

# Read combined OSPAR Assessment Units
OSPAR_Assessment_Units <- sf::st_read("MSSQL:server=SQL08;database=OceanCOMP_20062014;trusted_connection=yes;", "AssessmentUnit")

# Identify invalid assessment units
sf::st_is_valid(OSPAR_Assessment_Units)

# Identify overlapping assessment units
sf::st_overlaps(OSPAR_Assessment_Units, sparse = FALSE)