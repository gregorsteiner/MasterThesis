
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
                       "Male-Female gap" = cs_mn_mfg, "Disadvantaged gap" = cs_mn_neg,
                       "Log Income" = lninc50all, "Unemployment" = unempall)]

vtable::sumtable(dat.summary,
                 out = "latex", file = "../TeX Files/SummaryStats.tex", anchor = "SumStats")

######## Maps ########


# plot cumulative disasters
fema.cum <- aggregate(list("Disasters" = dat$CumuDisasters),
                      list("fips" = dat$fips), function(x) x[length(x)])


png("DisasterMap.png", width = wid, height = hei)

plot_usmap(data = fema.cum, values = "Disasters") +
  scale_fill_viridis_c(name = "",
                       breaks = c(0, 5, 10, max(fema.cum$Disasters, na.rm = TRUE))) +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))

dev.off()


# plot assistance received
png("AssistanceMap.png", width = wid, height = hei)

plot_usmap(data = fema.assist.agg[, .("Assistance received" = sum(federalAssistance)),
                                  by = .(fips)], values = "Assistance received") +
  scale_fill_viridis_c(name = "", trans = "log",
                       breaks = c(20000, 400000, 9000000, 200000000)) +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))

dev.off()



