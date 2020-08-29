##Load the data file (change the path if needed)
data.OSPAR <- read.table("F:/Bureau/COMPEAT_20062014_Data.txt", sep="\t", dec=".", header=T, na.string="", as.is = TRUE , fileEncoding="UTF-8-BOM")

##Change the date format (concatenate year, month and day column in yyyy-mm-dd format)
data.OSPAR$Dates <- paste0(data.OSPAR$Year,"-",sprintf("%02d",data.OSPAR$Month),"-",sprintf("%02d",data.OSPAR$Day))

##Change the name of the column which contain the categories to analyse (2 possibilities : analyse by cruises [1st choice] or by stations[2d choice])
names(data.OSPAR)[match("Cruise", names(data.OSPAR))] <- "Category"
 #names(data.OSPAR)[match("Station", names(data.OSPAR))] <- "Category"

##Change salinity column name for TTA compatibility
names(data.OSPAR)[grep("Salinity", names(data.OSPAR))] <- "Salinity"

##Change depth column name for TTA compatibility (the column "Depth [m|db]:PRIMARYVAR:DOUBLE" has been selected by default, if the appropiate depth
## column is "Bot. Depth [m]", you can replace "Depth..m.db" by "Bot.Depth" in the following code)
names(data.OSPAR)[grep("Depth..m.db", names(data.OSPAR))] <- "Depth"

##Replace NULL by empty cases
data.OSPAR <- lapply(data.OSPAR, function(x) {ifelse((x=="NULL")=="FALSE", x, x<-NA)})

##Save the file (change the path if needed)
write.table(data.OSPAR, "F:/Bureau/COMPEAT_20062014_Data_TTA.txt", quote=F, sep="\t", row.names = F, na="")


