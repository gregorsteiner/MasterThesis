


######## Merge together ########

dat.weather <- data.table(readRDS("WeatherDataCounty.RDS"))
dat.fars <- data.table(readRDS("AccidentDataAggregated.RDS"))

# merge
dat.comb <- merge(dat.weather, dat.fars,
                  by.x = c("StateCounty", "DATE"),
                  by.y = c("StateCounty", "DATE"),
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


# add heatwave indicator
dat.comb[, ":="(EHE.MAX = EHE(TMAX, DATE), EHE.MIN = EHE(TMIN, DATE)), by = .(StateCounty)]


# save as RDS
saveRDS(dat.comb, "DataCombined.RDS")

