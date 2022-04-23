
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


# plot assistance received

# plot cumulative disasters
png("AssistanceMap.png", width = wid, height = hei)

plot_usmap(data = fema.assist.agg[, .("Assistance received" = federalAssistance),
                                  by = .(fips)], values = "Assistance received") +
  scale_fill_viridis_c(name = "Assistance received", trans = "log",
                       breaks = c(20000, 400000, 9000000, 200000000)) +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))

dev.off()



