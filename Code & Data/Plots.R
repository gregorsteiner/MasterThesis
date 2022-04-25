
######## Preliminaries ########

source("AuxFunctions.R")
library(data.table)
library(usmap)
library(ggplot2)

hei <- 400
wid <- 600


dat <- setDT(readRDS("Data.RDS"))



######## Summary Statistics ########

dat.summary <- dat[, .(Disasters, "Disaster Dummy" = as.factor(DisasterDummy),
                       "Cumulative Disasters" = CumuDisasters, "Grade" = as.factor(grade),
                       "Subject" = factor(subject, labels = c("Mathematics", "RLA")),
                       "Mean test score" = cs_mn_all, "White-Black gap" = cs_mn_wbg,
                       "Male-Female gap" = cs_mn_mfg, "Disadvantaged gap" = cs_mn_neg)]

vtable::sumtable(dat.summary,
                 out = "latex", file = "../TeX Files/SummaryStats.tex", anchor = "SumStats")

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

plot_usmap(data = fema.assist.agg[, .("Assistance received" = sum(federalAssistance)),
                                  by = .(fips)], values = "Assistance received") +
  scale_fill_viridis_c(name = "Assistance received", trans = "log",
                       breaks = c(20000, 400000, 9000000, 200000000)) +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))

dev.off()



