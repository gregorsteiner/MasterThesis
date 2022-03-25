

######## Preliminaries ########


source("00_AuxFunctions.R")
library(data.table)


######## Merge together ########

# read weather data
dat.weather <- readRDS("WeatherDataCounty.RDS")

# merge with fips data to have county codes
fips <- maps::county.fips
fips$StateCounty <- sapply(strsplit(fips$polyname, ","), function(x) paste0(toupper(x[2]), " COUNTY", ", ", toupper(x[1])))

dat.weather <- merge(dat.weather, fips,
                     by = c("StateCounty"))

# change columns
dat.weather <- dat.weather[, .(StateCounty, CountyFIPS = fips, DATE, PRCP, TMAX, TMIN)]


# read accident data
dat.fars <- readRDS("AccidentDataAggregated.RDS")

# merge
dat.comb <- merge(dat.weather, dat.fars,
                  by = c("CountyFIPS", "StateCounty", "DATE"), all = TRUE)



# get latest data for which I have crash data
dat.comb$DATE <- as.Date(dat.comb$DATE)
crash.data.end <- dat.comb[!is.na(FatalAccidents), max(DATE, na.rm = TRUE)]


# add 0s where fatal accidents are NA
bool <- is.na(dat.comb$FatalAccidents) & dat.comb$DATE <= crash.data.end
dat.comb[bool, "FatalAccidents"] <- 0

# delete observations where Fatal car accidents is NA
dat.comb <- dat.comb[!is.na(FatalAccidents)]

# delete implausible temperature values
dat.comb <- dat.comb[is.na(TMAX) | TMAX < 100]

# add heatwave indicator
setDT(dat.comb)
dat.comb[, ":="(EHE.MAX = EHE(TMAX, DATE), EHE.MIN = EHE(TMIN, DATE)), by = .(StateCounty)]


# save as RDS
saveRDS(dat.comb, "DataCombined.RDS")

