
######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)
library(usmap)

# read data
data <- readRDS("DataCombined.RDS")

# set parameters for saving plots
wid <- 800
hei <- 400


######## Table of summary statistics ########

stargazer::stargazer(data[, .(FatalAccidents, MaximumTemperature = TMAX,
                              MinimumTemperature = TMIN, 
                              Precipitation = PRCP)],
                     type = "latex", summary.stat = c("n", "mean", "sd", "min", "median", "max"))



######## Fatal Accidents by Year ########


# plot number of fatal car accidents

setDT(data)
FatAccs <- data[, .(FatAccs = sum(FatalAccidents)), by = .(YEAR = format(DATE, "%Y"))]
FatAccs <- FatAccs[!is.na(YEAR)]

png("FatalAccidentsYearly.png", width = wid, height = hei)

par(mar = c(2.5, 4, 1, 1))
invisible(within(FatAccs, {
  plot(YEAR, FatAccs, type = "n",
       xlab = "", ylab = "Fatal car accidents")
  grid()
  lines(YEAR, FatAccs, col = 4, lwd = 2)
}))

dev.off()


######## Map of Weather data ########

library(ggplot2)

# choose one point in time
data.map <- data[DATE == "2015-07-01"]

# the usmap package needs the column name to be fips
data.map$fips <- data.map$CountyFIPS

plot_usmap(data = data.map, values = "TMAX") +
  scale_fill_viridis_c(name = "Max. Temperature")
  

plot_usmap(data = data.map, values = "FatalAccidents") +
  scale_fill_viridis_c(name = "Max. Temperature")




######## Poisson ########

lambda <- mean(data$FatalAccidents)

# create empirical probability mass
empi <- rep(0, length(0:max(data$FatalAccidents)))
names(empi) <- 0:max(data$FatalAccidents)
tab <- table(data$FatalAccidents) / nrow(data)
empi[names(tab)] <- tab

# and theoretical from poisson distribution
theo <- dpois(0:max(data$FatalAccidents), lambda = lambda)

png("Poisson.png", width = wid, height = hei)

par(mar = c(4, 4, 1, 1))
plot(names(empi), empi, type = "h", lwd = 5, col = 4,
     xlab = "Number of fatal car accidents", ylab = "Probability mass")
lines(as.numeric(names(empi)) + 0.2, theo, type = "h", lwd = 5, col = 3)
legend("topright", legend = c("Empirical", "Theoretical Poisson"),
       lty = c(1, 1), lwd = c(3, 3), col = c(4, 3), cex = 1.2)


dev.off()



######## Map conflict data ########

library(tmap)


dat.raster <- geodata::gadm("GTM", level = 1, path = "GeoData")


dat.clim <- geodata::worldclim_country("GTM", "tavg", "GeoData")







