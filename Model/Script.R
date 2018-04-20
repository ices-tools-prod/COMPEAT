# Install and load R packages ---------------------------------------------
# 
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "dplyr")
ipak(packages)

# OSPAR Reporting Units ---------------------------------------------------
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