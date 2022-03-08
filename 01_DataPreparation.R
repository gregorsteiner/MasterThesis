
######## Preliminaries ########

source("00_AuxFunctions.R")

# 
# ######## Monthly Temp. Data by County ########
# 
# # read data
# dat <- read.table("https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmpccy-v1.0.0-20220204",
#                   colClasses = c("character", rep("numeric", 12)))
# colnames(dat)[2:13] <- month.abb
# 
# # create new variables
# dat <- within(dat, {
#   stateFIPS <- substring(dat[, 1], 1, 2)
#   countyFIPS <- substring(dat[, 1], 3, 5)
#   Year <- substring(dat[, 1], 8, 11)
# })
# 
# # remove 1st columns
# dat <- dat[, -1]
# 
# # reshape
# dat.long <- tidyr::pivot_longer(dat, cols = 1:12, 
#                                 names_to = "Month", values_to = "AvgTemp")
# 
# # export
# saveRDS(dat.long, "Monthly_Temp_Data.RDS")



######## Daily Temp. Data by County ########



# get stations metadata
stations <- read.table("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt",
                       header = FALSE, fill = TRUE)



# Get data from all the stations
data <- do.call(rbind, Map(function(file.name, counter){
  
  # read file
  dat <- VFS::read.dly(paste0("StationsData/ghcnd_hcn/", file.name))
  
  # get ID from filename
  ID <- strsplit(file.name, "[.]")[[1]][1]
  
  # from 1970 on and only keep relevant variables
  dat <- dat[dat$YEAR >= 1990, c("YEAR", "MONTH", "DAY", "PRCP.VALUE", "TMAX.VALUE", "TMIN.VALUE")]
  
  # add coordinates and county and state
  dat <- within(dat,{
    LATITUDE <- as.numeric(stations[stations[, 1] == ID, 2])
    LONGITUDE <- as.numeric(stations[stations[, 1] == ID, 3])
    StateCounty <- latlong2county(data.frame(x = LONGITUDE, y = LATITUDE))
  })

  # print progress
  print(counter)
  
  # return
  return(dat)

}, list.files("StationsData/ghcnd_hcn/"), 1:length(list.files("StationsData/ghcnd_hcn/"))))


# aggregate from stations to counties
library(data.table)
data <- data.table(data)

data.agg <- data[, list(PRCP = mean(PRCP.VALUE, na.rm = TRUE),
                        TMAX = mean(TMAX.VALUE, na.rm = TRUE),
                        TMIN = mean(TMIN.VALUE, na.rm = TRUE)),
                 by = list(StateCounty, YEAR, MONTH, DAY)]


# Add Date and make the StateCounty look nicer
data.agg <- within(data.agg, {
  StateCounty <- sapply(strsplit(StateCounty, ","), function(x) paste0(toupper(x[2]), " COUNTY", ", ", toupper(x[1])))
  DATE <- as.Date(paste(YEAR, MONTH, DAY, sep = "-"))
})


# save data
saveRDS(data.agg, "WeatherDataCounty.RDS")






