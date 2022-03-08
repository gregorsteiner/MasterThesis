
######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)

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





######## Car Accident Data ########

# read data
dat.crash <- data.table(haven::read_dta("20140100_indiv_records.dta"))

# aggregate by county and state
dat.crash.agg <- dat.crash[, .(FatalAccidents = length(st_case)), by = .(date, state, county)]

# fix state and county names
dat.crash.agg <- within(dat.crash.agg, {
  # state <- dplyr::case_when(
  #   nchar(state) == 1 ~ paste0("0", as.character(state)),
  #   nchar(state) == 2 ~ as.character(state)
  #   )
  county <- dplyr::case_when(
    nchar(county) == 1 ~ paste0("00", as.character(county)),
    nchar(county) == 2 ~ paste0("0", as.character(county)),
    nchar(county) == 3 ~ as.character(county)
  )
  CountyFIPS <- as.numeric(paste0(as.character(state), county))
})

# add county names
dat.crash.agg <- merge(dat.crash.agg, maps::county.fips,
                       by.x = "CountyFIPS", by.y = "fips")

# and reformat
dat.crash.agg <- within(dat.crash.agg, {
  StateCounty <- sapply(strsplit(polyname, ","), function(x) paste0(toupper(x[2]), " COUNTY", ", ", toupper(x[1])))
  polyname <- NULL
})




######## Merge together ########

dat.weather <- data.table(readRDS("WeatherDataCounty.RDS"))


# merge
dat.comb <- merge(dat.weather, dat.crash.agg,
                  by.x = c("StateCounty", "DATE"),
                  by.y = c("StateCounty", "date"),
                  all.x = TRUE)

# get latest data for which I have crash data
dat.comb$DATE <- as.Date(dat.comb$DATE)
crash.data.end <- max(unlist(dat.comb[!is.na(dat.comb$FatalAccidents), "DATE"]))

# add 0s where fatal accidents are NA
bool <- is.na(dat.comb$FatalAccidents) & dat.comb$DATE <= crash.data.end
dat.comb[bool, "FatalAccidents"] <- 0

# delete irrelevant columns
dat.comb <- within(dat.comb, {
  YEAR <- NULL
  MONTH <- NULL
  DAY <- NULL
  state <- NULL
  county <- NULL
})

# delete observations where Fatal car accidents is NA
dat.comb <- dat.comb[!is.na(dat.comb$FatalAccidents), ]

# save as RDS
saveRDS(dat.comb, "DataCombined.RDS")


