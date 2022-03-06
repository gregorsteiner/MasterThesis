


url <- "https://www.ncei.noaa.gov/pub/data/uscrn/products/heat01/CRNHE0101-TN_Crossville_7_NW.csv"
dat <- read.csv(url)

# create date variable
dat$DATE <- as.Date(substring(dat$DATE_TIME, 1, 8), "%Y%m%d")

# use minimum apparent temperature by day
dat.agg <- aggregate(dat$APPARENT_TEMPERATURE_C, list(dat$DATE), mean, na.rm = TRUE)
colnames(dat.agg) <- c("Date", "AppTemp")

# drop NAs or INF
dat.agg <- dat.agg[is.finite(dat.agg$AppTemp), ]

# compute threshold (85th percentile of july and august apparent temperatures)
bool.jul.aug <- format(dat.agg$Date, "%m") %in% c("07", "08") 
threshhold <- quantile(dat.agg[bool.jul.aug, "AppTemp"], 0.85)

# Mark as heatwave all consecutive days that exceed the threshhold
bool <- dat.agg$AppTemp > threshhold
dat.agg$Heatwave <- 0

for (i in 1:(nrow(dat.agg)-1)) {
  if(all(bool[i:(i+1)])){
    dat.agg[i:(i+1), "Heatwave"] <- 1
  }
}

# add name and coordinates
dat.agg <- within(dat.agg, {
  Name <- "Crossville"
  Longitude <- dat[1, "LONGITUDE"]
  Latitude <- dat[1, "LATITUDE"]
})





# make graph
plot(dat.agg$Date, dat.agg$AppTemp, type = "l",
     ylab = "Apparent Temperature", xlab = "")









