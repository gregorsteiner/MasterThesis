

######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)
library(fixest)


# read data
dat <- readRDS("Data.RDS")
#assist <- readRDS("AssistanceData.RDS")
assist.cov <- readRDS("AssistanceCovData.RDS")

# color scheme
cols <- c("firebrick", "cornflowerblue")

######## Models ########


# Sun & Abraham
model.math <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                 sunab(TreatStart, year, ref.p = c(-1, -3000), bin.rel = c(-5:-3000)) | year + fips + grade,
               data = dat[subject == "mth"], cluster = "TreatStart")

model.rla <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                     sunab(TreatStart, year, ref.p = c(-1, -3000), bin.rel = c(-5:-9)) | year + fips + grade,
                   data = dat[subject == "rla"], cluster = "TreatStart")

# automatically export as tex file
etable(model.math, file = "../TeX Files/MainResultsMath.tex", replace = TRUE,
       label = "MainResultsMath", title = "Results (Mathematics)",
       dict = c(cs_mn_all = "Overall",
                cs_mn_blk = "Black",
                cs_mn_hsp = "Hispanic",
                cs_mn_fem = "Female",
                cs_mn_ecd = "Econ. Disadv.",
                year = "Year", grade = "Grade", fips = "County",
                TreatStart = "Cohort"))

etable(model.rla, file = "../TeX Files/MainResultsRLA.tex", replace = TRUE,
       label = "MainResultsRLA", title = "Results (RLA)",
       dict = c(cs_mn_all = "Overall",
                cs_mn_blk = "Black",
                cs_mn_hsp = "Hispanic",
                cs_mn_fem = "Female",
                cs_mn_ecd = "Econ. Disadv.",
                year = "Year", grade = "Grade", fips = "County",
                TreatStart = "Cohort"))



# plot results
dep.vars <- c("Overall", "Black", "Hispanic", "Female", "Econ. Disadv.")


# layout

pdf("ResultsPlot.pdf", width = 15 / 2.5, height = 20 / 2.5)

layout(matrix(c(1, 1:6, 6), ncol = 2, byrow = TRUE), heights = c(4, 4, 4, 1))

par(mar = c(2, 4, 2, 1))
iplot(list(model.math[[1]], model.rla[[1]]), main = "Overall", xlab = "Years to treatment",
      col = cols, ci.col = cols, ci.lwd = 1, ci.width = 0.2,
      pt.pch = 19, ylim = c(-0.07, 0.07))

invis.Map(function(math, rla, name){
  
  iplot(list(math, rla), main = name, xlab = "Years to treatment",
        col = cols, ci.col = cols, pt.pch = 19,
        ci.lwd = 1, ci.width = 0.2, ylim = c(-0.12, 0.12))
  
}, model.math[2:5], model.rla[2:5], dep.vars[2:5])

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Math", "RLA"),
       col = cols, lwd = 2, cex = 1, inset = 0, horiz = TRUE)


dev.off()




# STorms

model.math.storm <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                            sunab(TreatStartStorm, year, ref.p = c(-1, -3000), bin.rel = c(-5:-3000)) | year + fips + grade,
                          data = dat[subject == "mth"])

model.rla.storm <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                           sunab(TreatStartStorm, year, ref.p = c(-1, -3000), bin.rel = c(-5:-9)) | year + fips + grade,
                         data = dat[subject == "rla"])



pdf("ResultsPlotStorm.pdf", width = 15 / 2.5, height = 20 / 2.5)

layout(matrix(c(1, 1:6, 6), ncol = 2, byrow = TRUE), heights = c(4, 4, 4, 1))

par(mar = c(2, 4, 2, 1))
iplot(list(model.math.storm[[1]], model.rla.storm[[1]]), main = "Overall", xlab = "Years to treatment",
      col = cols, ci.col = cols, ci.lwd = 1, ci.width = 0.2,
      pt.pch = 19, ylim = c(-0.07, 0.07))

invis.Map(function(math, rla, name){
  
  iplot(list(math, rla), main = name, xlab = "Years to treatment",
        col = cols, ci.col = cols, pt.pch = 19,
        ci.lwd = 1, ci.width = 0.2, ylim = c(-0.12, 0.12))
  
}, model.math.storm[2:5], model.rla.storm[2:5], dep.vars[2:5])

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Math", "RLA"),
       col = cols, lwd = 2, cex = 1, inset = 0, horiz = TRUE)


dev.off()



  
  
  
