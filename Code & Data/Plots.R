
######## Preliminaries ########

source("AuxFunctions.R")
library(data.table)
library(usmap)
library(ggplot2)

hei <- 400
wid <- 600


dat <- setDT(readRDS("Data.RDS"))

######## Maps ########


# plot median income 
plot_usmap(data = seda.comb[year == 2018 & grade == 3,
                           .(inc50all = exp(lninc50all), fips = sedacounty)],
           values = "inc50all") +
  scale_fill_viridis_c(name = "Median Income (2018)")


# plot test scores
scores.agg <- dat[, .("TestScores" = mean(cs_mn_all, na.rm = TRUE)),
                  by = .(fips = sedacounty)]

png("TestScoresMap.png", width = wid, height = hei)

plot_usmap(data = scores.agg, values = "TestScores") +
  scale_fill_viridis_c(name = "Test Scores") +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))


dev.off()


# plot cumulative disasters
png("DisasterMap.png", width = wid, height = hei)

plot_usmap(data = fema.cum, values = "Disasters") +
  scale_fill_viridis_c(name = "Disasters") +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))

dev.off()

