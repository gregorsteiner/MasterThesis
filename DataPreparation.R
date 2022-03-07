
######## Monthly Temp. Data by County ########

# read data
dat <- read.table("https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmpccy-v1.0.0-20220204",
                  colClasses = c("character", rep("numeric", 12)))
colnames(dat)[2:13] <- month.abb

# create new variables
dat <- within(dat, {
  stateFIPS <- substring(dat[, 1], 1, 2)
  countyFIPS <- substring(dat[, 1], 3, 5)
  Year <- substring(dat[, 1], 8, 11)
})

# remove 1st columns
dat <- dat[, -1]

# reshape
dat.long <- tidyr::pivot_longer(dat, cols = 1:12, 
                                names_to = "Month", values_to = "AvgTemp")

# export
saveRDS(dat.long, "Monthly_Temp_Data.RDS")