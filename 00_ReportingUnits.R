# OSPAR Reporting Units --------------------------------------------------------
# 
# Download OSPAR_Reporting_Units.gdb at https://www.ospar.org/work-areas/cross-cutting-issues/intermediate-assessment-2017-resources into Input folder
sf::st_layers("Input/OSPAR_Reporting_Units.gdb")

OSPAR_Reporting_Units_Level0 <- sf::st_read("Input/OSPAR_Reporting_Units.gdb", layer = "OSPAR_RU_Level0")
plot(st_geometry(OSPAR_Reporting_Units_Level0))

OSPAR_Reporting_Units_Level1 <- sf::st_read("Input/OSPAR_Reporting_Units.gdb", layer = "OSPAR_RU_Level1")
plot(st_geometry(OSPAR_Reporting_Units_Level1))

OSPAR_Reporting_Units_Level2 <- sf::st_read("Input/OSPAR_Reporting_Units.gdb", layer = "OSPAR_RU_Level2_v2_160119")
plot(st_geometry(OSPAR_Reporting_Units_Level2))

OSPAR_Reporting_Units_Level3 <- sf::st_read("Input/OSPAR_Reporting_Units.gdb", layer = "OSPAR_RU_Level3_v2_160119")
plot(st_geometry(OSPAR_Reporting_Units_Level3))

OSPAR_Reporting_Units_Level4 <- sf::st_read("Input/OSPAR_Reporting_Units.gdb", layer = "OSPAR_RU_Level4_v2_160119")
plot(st_geometry(OSPAR_Reporting_Units_Level4))

# Write layer as shapefiles
sf::st_write(OSPAR_Reporting_Units_Level0, "Input/OSPAR_RU_Level0.shp")
sf::st_write(OSPAR_Reporting_Units_Level1, "Input/OSPAR_RU_Level1.shp")
sf::st_write(OSPAR_Reporting_Units_Level2, "Input/OSPAR_RU_Level2.shp")
sf::st_write(OSPAR_Reporting_Units_Level3, "Input/OSPAR_RU_Level3.shp")
sf::st_write(OSPAR_Reporting_Units_Level4, "Input/OSPAR_RU_Level4.shp")

# Write layer to database
sf::st_write(OSPAR_Reporting_Units_Level0, "MSSQL:server=SQL08;database=OceanGIS;trusted_connection=yes;", "OSPAR_Reporting_Units_Level0")
sf::st_write(OSPAR_Reporting_Units_Level1, "MSSQL:server=SQL08;database=OceanGIS;trusted_connection=yes;", "OSPAR_Reporting_Units_Level1")
sf::st_write(OSPAR_Reporting_Units_Level2, "MSSQL:server=SQL08;database=OceanGIS;trusted_connection=yes;", "OSPAR_Reporting_Units_Level2")
sf::st_write(OSPAR_Reporting_Units_Level3, "MSSQL:server=SQL08;database=OceanGIS;trusted_connection=yes;", "OSPAR_Reporting_Units_Level3")
sf::st_write(OSPAR_Reporting_Units_Level4, "MSSQL:server=SQL08;database=OceanGIS;trusted_connection=yes;", "OSPAR_Reporting_Units_Level4")