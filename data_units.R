## Download and Preprocess assessment units

# Skip this step, if units shape file exists
if (!file.exists("data/units.shp")) {
  # Read assessment units from WKT
  units <- st_read(paste0("/vsizip/", "/vsicurl/", "https://icesoceanography.blob.core.windows.net/compeat/AssessmentUnits.zip")) %>%
    st_set_crs(4326)
  
  # Remove unnecessary dimensions and convert data.frame to data.table
  units <- as.data.table(st_zm(units)) 
  
  # Order, Rename and Remove columns 
  units <- units[order(ID), .(Code = ID, Name = LongName, geometry)] %>%
    st_sf()
  
  # Assign Ids
  units$Id = 1:nrow(units)
  
  # Rearrange columns
  units <- units[, c("Id", "Code", "Name", "geometry")]
  
  # Output units shape and csv file
  st_write(units, "data/units.shp")
  st_write(units, "data/units.csv")
}
