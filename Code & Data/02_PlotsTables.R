
######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)
library(usmap)
library(ggplot2)

hei <- 400
wid <- 600


dat <- setDT(readRDS("Data.RDS"))
assist.cov <- setDT(readRDS("AssistanceCovData.RDS"))



######## Summary Statistics ########

dat.summary <- dat[, .(Disasters, "Disaster Dummy" = as.factor(DisasterDummy),
                       "Cumulative Disasters" = CumuDisasters, "Grade" = as.factor(grade),
                       "Subject" = factor(subject, labels = c("Mathematics", "RLA")),
                       "Mean test score" = cs_mn_all, "White-Black gap" = cs_mn_wbg,
                       "White-Hispanic gap" = cs_mn_whg, "Male-Female gap" = cs_mn_mfg,
                       "Disadvantaged gap" = cs_mn_neg,
                       "Total Damage" = totalDamage, "Federal Assistance" = federalAssistance,
                       "Log Income" = lninc50all)]

vtable::sumtable(dat.summary,
                 out = "latex", file = "../TeX Files/SummaryStats.tex", anchor = "SumStats")


######## Application characteristics ########

# voter share by applicant status
plot(density(assist.cov[AssistanceApplicant == 1 & !is.na(MedInc2016),
                        MedInc2016]),
     main = "", xlab = "Median Income (2016)", type = "n")
grid()

lines(density(assist.cov[AssistanceApplicant == 1 & !is.na(MedInc2016),
                         MedInc2016]),
      col = 3, lwd = 2)
lines(density(assist.cov[AssistanceApplicant == 0 & !is.na(MedInc2016),
                         MedInc2016]),
      col = 4, lwd = 2)
legend("topright", legend = c("Applied", "Did not apply"),
       lwd = 2, col = c(3, 4))


par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
invis.Map(function(x, xlab){
  # filter by applicant status and remove nas
  bool1 <- assist.cov$AssistanceApplicant == 1 & !is.na(x)
  bool2 <- assist.cov$AssistanceApplicant == 0 & !is.na(x)
  # plot
  plot(density(x[bool2]),
       main = "", xlab = xlab, type = "n")
  grid()
  
  lines(density(x[bool1]),
        col = 3, lwd = 2)
  lines(density(x[bool2]),
        col = 4, lwd = 2)
  # add legend
  legend("topright", legend = c("Applied", "Did not apply"),
         lwd = 2, col = c(3, 4))
}, assist.cov[, .(MedInc2016, ShareDem2016)],
c("Median Income (2016)", "Democratic Voter Share (2016 Election)"))



######## Maps ########


# plot cumulative disasters
fema.cum <- aggregate(list("Disasters" = as.numeric(dat$CumuDisasters)),
                      list("fips" = dat$fips), function(x) {
                        
                        res <- x[!is.na(x)][length(x[!is.na(x)])]
                        if(length(res) == 0) return(0)
                        
                        return(res)
                        
                        })

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

dat.plot <- melt(dat[, .("Damage" = sum(totalDamage, na.rm = TRUE) + 1,
                         "Federal Assistance" = sum(federalAssistance, na.rm = TRUE) + 1),
                     by = .(fips)],
                 id.vars = c("fips"), measure.vars = c("Damage", "Assistance"))

png("AssistanceMap.png", width = wid + 100, height = hei + 200)

plot_usmap(data = dat.plot,
           values = "value") +
  scale_fill_viridis_c(name = "", trans = "log",
                       breaks = c(1e1, 1e3, 1e5, 1e7, 1e9)) +
  facet_grid(cols = vars(variable)) + 
  theme(legend.position = c(0.4, -0.3),
        legend.key.size = grid::unit(0.6, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.direction = "vertical",
        plot.margin = grid::unit(c(0,0,-60,0), "mm"),
        strip.text.x = element_text(size = 12))
dev.off()



# plot political outcomes
plot_usmap(data = dat[, .("Share Democrats" = mean(ShareDem, na.rm = TRUE)),
                      by = .(fips)], values = "Share Democrats") +
  scale_fill_viridis_c(name = "") +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))




