
######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)



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




######## FARS data from NHTSA ########


# years of interest
years <- 1975:2020

# Download data for each year (available for 1975-2020)
fars.list <- lapply(years, function(year){
  # make url
  url <- paste0("https://www.nhtsa.gov/file-downloads/download?p=nhtsa/downloads/FARS/",
                year, "/National/FARS", year, "NationalCSV.zip")
  
  
  # file naming convention seems to change after 2014
  file <- ifelse(year > 2014, "accident.CSV", "ACCIDENT.CSV")
  
  # some follow different conventions
  if(year == 2018) file <- "accident.csv"
  if(year == 2015) file <- "accident.csv"
  if(year %in% 2012:2014) file <- "ACCIDENT.csv"
  
  # download, unzip and save data
  temp <- tempfile()
  download.file(url, temp)
  unzip(temp, file)
  dat <- read.csv(file)
  unlink(temp)
  
  # return
  return(dat)

})


# check if all years are included
any(duplicated(sapply(fars.list, function(x) x[1, "YEAR"])))

# get columns which are available for all years
common.names <- Reduce(intersect, sapply(fars.list, names))

# keep these names, rbind and save as data.table
fars.dat <- data.table(do.call(rbind, lapply(fars.list, "[", common.names)))


# fix year variable
fars.dat[, YEAR := ifelse(nchar(YEAR) == 2, YEAR + 1900, YEAR)]

# create date variable
fars.dat[, DATE := as.Date(paste(YEAR, MONTH, DAY, sep = "-"))]

# aggregate by date and location
fars.dat.agg <- fars.dat[, .(FatalAccidents = length(ST_CASE)), by = .(DATE, STATE, COUNTY)]

# fix state and county names
fars.dat.agg <- within(fars.dat.agg, {
  # state <- dplyr::case_when(
  #   nchar(state) == 1 ~ paste0("0", as.character(state)),
  #   nchar(state) == 2 ~ as.character(state)
  #   )
  COUNTY <- dplyr::case_when(
    nchar(COUNTY) == 1 ~ paste0("00", as.character(COUNTY)),
    nchar(COUNTY) == 2 ~ paste0("0", as.character(COUNTY)),
    nchar(COUNTY) == 3 ~ as.character(COUNTY)
  )
  CountyFIPS <- as.numeric(paste0(as.character(STATE), COUNTY))
})

# add county names
fars.dat.agg <- merge(fars.dat.agg, maps::county.fips,
                       by.x = "CountyFIPS", by.y = "fips")

# and reformat
fars.dat.agg <- within(fars.dat.agg, {
  StateCounty <- sapply(strsplit(polyname, ","), function(x) paste0(toupper(x[2]), " COUNTY", ", ", toupper(x[1])))
  polyname <- NULL
})



# save data
saveRDS(fars.dat.agg, "AccidentDataAggregated.RDS")

