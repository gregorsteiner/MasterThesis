
######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)
library(usmap)

# read data
data <- readRDS("DataCombined.RDS")

# set parameters for saving plots
wid <- 600
hei <- 400



######## Fatal Accidents by Year ########


# plot number of fatal car accidents

setDT(data)
FatAccs <- data[, .(FatAccs = sum(FatalAccidents)), by = .(YEAR = format(DATE, "%Y"))]

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

data$fips <- data$CountyFIPS



plot_usmap(data = data[!is.na(data$fips), ], values = "TMAX")





######## Poisson ########

lambda <- mean(data$FatalAccidents)
empi <- table(data$FatalAccidents) / nrow(data)
theo <- dpois(0:max(data$FatalAccidents), lambda = lambda)

png("Poisson.png", width = wid, height = hei)

par(mar = c(2.5, 4, 1, 1))
plot(as.numeric(names(empi)), empi, type = "h", lwd = 5, col = 4,
     xlab = "", ylab = "")
lines(as.numeric(names(empi)) + 0.2, theo, type = "h", lwd = 5, col = 3)
legend("topright", legend = c("Empirical", "Theoretical Poisson"),
       lty = c(1, 1), lwd = c(3, 3), col = c(4, 3), cex = 1.2)


dev.off()



