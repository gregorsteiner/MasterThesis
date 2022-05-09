
######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)
library(usmap)
library(ggplot2)

hei <- 10
wid <- 16
unit <- "cm"

dat <- setDT(readRDS("Data.RDS"))
assist <- setDT(readRDS("AssistanceData.RDS"))
assist.cov <- setDT(readRDS("AssistanceCovData.RDS"))


######## Summary Statistics ########

# dataset summary
dat.summary <- dat[, .(Disasters, Treatment = factor(DisasterTreat),
                       "Subject" = factor(subject, labels = c("Mathematics", "RLA")),
                       "Mean test score" = cs_mn_all,
                       "Mean test score (black students)" = cs_mn_blk,
                       "Mean test score (hispanic students)" = cs_mn_hsp,
                       "Mean test score (female students)" = cs_mn_fem,
                       "Mean test score (econ. disadv. students)" = cs_mn_ecd)]

vtable::sumtable(dat.summary,
                 out = "latex", file = "../TeX Files/SummaryStats.tex", anchor = "SumStats")


# boxplots for dependent variables
png("DepVarsBoxplot.png",
    width = wid, height = hei, units = unit, res = 1200)

par(mar = c(3, 3, 1, 1))
boxplot(dat[, .("Overall" = cs_mn_all,
                "Black" = cs_mn_blk,
                "Hispanic" = cs_mn_hsp,
                "Female" = cs_mn_fem,
                "Econ. disadv." = cs_mn_ecd)],
        col = 4)

dev.off()


######## Application characteristics ########

# voter share and median income by applicant status


# png("AssistanceCovDensity.png",
#     width = wid, height = 16, units = "cm", res = 1200)
# 
# col <- c(3, 4)
# 
# par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))
# invis.Map(function(x, xlab){
#   # filter by applicant status and remove nas
#   bool1 <- assist.cov$AssistanceApplicant == 1 & !is.na(x)
#   bool2 <- assist.cov$AssistanceApplicant == 0 & !is.na(x)
#   # plot
#   plot(density(x[bool2]),
#        main = "", xlab = xlab, type = "n", cex = 0.8)
#   grid()
#   
#   lines(density(x[bool1]),
#         col = col[1], lwd = 2)
#   lines(density(x[bool2]),
#         col = col[2], lwd = 2)
#   # add legend
#   legend("topright", legend = c("Applied", "Did not apply"),
#          lwd = 2, col = col)
# }, assist.cov[, .(MedInc2016, ShareDem2016, PovertyRate, SingleMother)],
# c("Median Income (2016)", "Democratic Votes (2016 Election)",
#   "Poverty Rate (2016)", "Share of Single Mothers (2016)"))
# 
# dev.off()



# Boxplots

# set applicant status as factor
assist.cov[, AssistanceApplicant := factor(AssistanceApplicant,
                                           levels = c(0, 1),
                                           labels = c("Did not apply", "Applied"))]


col <- c(3, 4)
png("AssistanceCovBoxplot.png",
    width = wid, height = wid, units = unit, res = 400)

par(mfrow = c(2, 2), mar = c(3, 4, 1, 1))
invis.Map(function(x, ylab){
  # create boxplot
  boxplot(x ~ AssistanceApplicant,
          data = assist.cov, col = col,
          xlab = "", ylab = ylab)
  
}, assist.cov[, .(MedInc2016, ShareDem2016, PovertyRate, SingleMother)],
c("Median Income (2016)", "Democratic Votes (2016 Election)",
  "Poverty Rate (2016)", "Share of Single Mothers (2016)"))

dev.off()



# # Treatment plot
# dat.treat <- dat[order(TimeToTreat),
#                  .(MeanScoreMath = mean(cs_mn_all[subject == "mth"], na.rm = TRUE),
#                    MeanScoreRLA = mean(cs_mn_all[subject == "rla"], na.rm = TRUE)),
#                  by = .("Years to Treatment" = TimeToTreat)]
# 
# dat.treat[, ":="(TreatmentMath = MeanScoreMath[is.na(`Years to Treatment`)] - MeanScoreMath,
#                  TreatmentRLA = MeanScoreRLA[is.na(`Years to Treatment`)] - MeanScoreRLA)]
# 
# plot(dat.treat$`Years to Treatment`, dat.treat$TreatmentMath, type = "n",
#      ylab = "Difference in mean test scores", xlab = "Years to Treatment",
#      ylim = range(dat.treat$TreatmentMath, dat.treat$TreatmentRLA) + c(-0.01, 0.01))
# grid()
# abline(v = 0, lwd = 2, col = 2)
# points(dat.treat$`Years to Treatment`, dat.treat$TreatmentMath,
#        pch = 18, col = 3)
# lines(dat.treat$`Years to Treatment`, dat.treat$TreatmentMath,
#       lwd = 2, col = 3)
# points(dat.treat$`Years to Treatment`, dat.treat$TreatmentRLA,
#        pch = 18, col = 4)
# lines(dat.treat$`Years to Treatment`, dat.treat$TreatmentRLA,
#       lwd = 2, col = 4)




######## Maps ########


# plot cumulative disasters
dat[, CumuDisasters := cumsum(Disasters), by = .(fips, grade, subject)]
fema.cum <- aggregate(list("Disasters" = as.numeric(dat$CumuDisasters)),
                      list("fips" = dat$fips), function(x) {
                        
                        res <- x[!is.na(x)][length(x[!is.na(x)])]
                        if(length(res) == 0) return(0)
                        
                        return(res)
                        
                        })

png("DisasterMap.png",
    width = wid, height = hei, units = unit, res = 1200)

plot_usmap(data = fema.cum, values = "Disasters") +
  scale_fill_viridis_c(name = "", option = "H",
                       breaks = c(0, 5, 10, max(fema.cum$Disasters, na.rm = TRUE))) +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))

dev.off()



# plot cumulative Storms
dat[, CumuStorms := cumsum(Storms), by = .(fips, grade, subject)]
storms.cum <- aggregate(list("Storms" = as.numeric(dat$CumuStorms)),
                        list("fips" = dat$fips), function(x) {
                          res <- x[!is.na(x)][length(x[!is.na(x)])]
                          if(length(res) == 0) return(0)
                          
                          return(res)
                        })

png("StormMap.png",
    width = wid, height = hei, units = unit, res = 1200)

plot_usmap(data = storms.cum, values = "Storms") +
  scale_fill_viridis_c(name = "", option = "H",
                       breaks = c(0, 5, 10, 15, max(storms.cum$Storms))) +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))

dev.off()


# plot assistance received

dat.plot <- melt(assist[, .("Damage" = sum(totalDamage, na.rm = TRUE) + 1,
                            "Assistance" = sum(federalAssistance, na.rm = TRUE) + 1),
                        by = .(fips)],
                 id.vars = c("fips"), measure.vars = c("Damage", "Assistance"))

png("AssistanceMap.png",
    width = wid, height = hei, units = unit, res = 1200)

plot_usmap(data = dat.plot,
           values = "value") +
  scale_fill_viridis_c(name = "", trans = "log", option = "H",
                       breaks = c(1e1, 1e3, 1e5, 1e7, 1e9)) +
  facet_grid(cols = vars(variable)) + 
  theme(legend.position = c(0.4, -0.35),
        legend.key.size = grid::unit(0.4, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.direction = "vertical",
        plot.margin = grid::unit(c(0,0,0,0), "mm"),
        strip.text.x = element_text(size = 12))
dev.off()




