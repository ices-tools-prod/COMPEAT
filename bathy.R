library(raster)
#These tiles have been downloaded from https://portal.emodnet-bathymetry.eu/ by going to downloads>DTM tiles>esri ascii.They are then merged into one here.
#KC - I have not included this in the git repo due to size (several GB). I would suggest resaving the stationsamples file after the bathy column has been added so it is not necessary to load the bathymetry every time.

bathy <- raster::merge(raster("C:/Users/KC05/Downloads/emodnet_bathy/C3_2020.asc"),
                       raster("C:/Users/KC05/Downloads/emodnet_bathy/C4_2020.asc"),
                       raster("C:/Users/KC05/Downloads/emodnet_bathy/C5_2020.asc"),
                       raster("C:/Users/KC05/Downloads/emodnet_bathy/D3_2020.asc"),
                       raster("C:/Users/KC05/Downloads/emodnet_bathy/D4_2020.asc"),
                       raster("C:/Users/KC05/Downloads/emodnet_bathy/D5_2020.asc"),
                       raster("C:/Users/KC05/Downloads/emodnet_bathy/E3_2020.asc"),
                       raster("C:/Users/KC05/Downloads/emodnet_bathy/E4_2020.asc"), 
                       raster("C:/Users/KC05/Downloads/emodnet_bathy/E5_2020.asc"),
                       raster("C:/Users/KC05/Downloads/emodnet_bathy/F3_2020.asc"))

crs(bathy) <- CRS('+init=EPSG:4326')
raster::writeRaster(bathy, "emodnet_bathy_2020.tiff",overwrite=TRUE)
save(bathy, file = "emodnet_bathy_2020.rdata")

