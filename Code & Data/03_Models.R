

######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)
library(fixest)


# read data
dat <- readRDS("Data.RDS")
#assist <- readRDS("AssistanceData.RDS")
assist.cov <- readRDS("AssistanceCovData.RDS")


######## Models ########

# fema
model.math <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                 sunab(TreatStart, year, ref.p = c(-1, -3000), bin.rel = c(-5:-3000)) | year + fips + grade,
               data = dat[subject == "mth"], cluster = "TreatStart")

model.rla <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                     sunab(TreatStart, year, ref.p = c(-1, -3000), bin.rel = c(-5:-9)) | year + fips + grade,
                   data = dat[subject == "rla"], cluster = "TreatStart")



# STorms

model.math.storm <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                            sunab(TreatStartStorm, year, ref.p = c(-1, -3000), bin.rel = c(-5:-3000)) | year + fips + grade,
                          data = dat[subject == "mth"])

model.rla.storm <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                           sunab(TreatStartStorm, year, ref.p = c(-1, -3000), bin.rel = c(-5:-9)) | year + fips + grade,
                         data = dat[subject == "rla"])



# heat models

model.temp.math <-  feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~
                            tmax | year + fips + grade,
                          data = dat[subject == "mth"], vcov = "iid")

model.temp.rla <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~
                          tmax | year + fips + grade,
                        data = dat[subject == "rla"], vcov = "iid")

model.days.math <-  feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~
                            DaysAbove30 | year + fips + grade,
                          data = dat[subject == "mth"], vcov = "iid")

model.days.rla <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~
                          DaysAbove30 | year + fips + grade,
                        data = dat[subject == "rla"], vcov = "iid")


coefmatrix <- do.call(rbind, Map(function(model, digs){
  # get coefficients and standard errors
  coefs <- sapply(model, "[[", "coefficients")
  ses <- sapply(model, "[[", "se")
  # round
  coefs.round <- round(coefs, digs)
  ses.round <- round(ses, digs)
  # paste them together
  res <- matrix(c(coefs.round, ses.round),
                ncol = 5, byrow = TRUE)
  # and potentially add star placeholders
  res[1, ] <- ifelse(abs(coefs / ses) > 1.96, paste0(res[1, ], "star"), res[1, ])
  # add dollar sign placeholders for math mode and 
  res[1, ] <- paste0("dollar", res[1, ], "dollar")
  res[2, ] <- paste0("dollar(", res[2, ], ")dollar")
  
  return(res)
  
}, list(model.temp.math, model.temp.rla, model.days.math, model.days.rla), c(4, 4, 6, 6)))

rownames(coefmatrix) <- c("Max. Temp. (Math)", "", "Max. Temp. (RLA)", "",
                          "Days ab. 30 (Math)", "", "Days ab. 30 (RLA)", "")
colnames(coefmatrix) <- c("Overall", "Black", "Hispanic", "Female", "Econ. Disadv.")
    
# create tex table
textable <- knitr::kable(coefmatrix, format = "latex", booktabs = TRUE)
# gsub special characters
textable <- gsub("dollar", "$", gsub("star", "^{***}", textable))


writeLines(textable, "../TeX Files/HeatResults.tex")




# export tables
dict <- c(cs_mn_all = "Overall", cs_mn_blk = "Black", cs_mn_hsp = "Hispanic",
          cs_mn_fem = "Female", cs_mn_ecd = "Econ. Disadv.",
          year = "Year", grade = "Grade", fips = "County",
          tmax = "Max. Temp.", DaysAbove30 = "Days ab. 30")

etable(model.temp.rla, file = "../TeX Files/TempResultsRLA.tex", replace = TRUE,
       label = "TempResultsRLA", title = "Temperature Results (RLA)",
       dict = dict)

etable(model.temp.math, file = "../TeX Files/TempResultsMath.tex", replace = TRUE,
       label = "TempResultsMath", title = "Temperature Results (Mathematics)",
       dict = dict)

etable(model.days.rla, file = "../TeX Files/DaysResultsRLA.tex", replace = TRUE,
       label = "DaysResultsRLA", title = "Hot Days Results (RLA)",
       dict = dict)

etable(model.days.math, file = "../TeX Files/DaysResultsMath.tex", replace = TRUE,
       label = "DaysResultsMath", title = "Hot Days Results (Mathematics)",
       dict = dict)



######## Export Graphs ########


# dependent variables
dep.vars <- c("Overall", "Black", "Hispanic", "Female", "Econ. Disadv.")

# color scheme
cols <- c("firebrick", "cornflowerblue")

# fema
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



# storms
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





# change plot layout for slides


# overall fema
pdf("ResultsPlotPresentation.pdf", width = 15 / 2.5, height = 12 / 2.5)
layout(matrix(c(1, 2), ncol = 1, byrow = TRUE), heights = c(4, 1))

par(mar = c(2, 4, 2, 1))
iplot(list(model.math[[1]], model.rla[[1]]), main = "", xlab = "Years to treatment",
      col = cols, ci.col = cols, ci.lwd = 1, ci.width = 0.2,
      pt.pch = 19, ylim = c(-0.07, 0.07))

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Math", "RLA"),
       col = cols, lwd = 2, cex = 1, inset = 0, horiz = TRUE)

dev.off()

# subgroups fema
pdf("ResultsPlotPresentationSubgroups.pdf", width = 15 / 2.5, height = 12 / 2.5)

layout(matrix(c(1:5, 5), ncol = 2, byrow = TRUE), heights = c(4, 4, 1))

par(mar = c(2, 4, 2, 1))
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



# overall storms
pdf("ResultsPlotStormsPresentation.pdf", width = 15 / 2.5, height = 12 / 2.5)

layout(matrix(c(1, 2), ncol = 1, byrow = TRUE), heights = c(4, 1))

par(mar = c(2, 4, 2, 1))
iplot(list(model.math.storm[[1]], model.rla.storm[[1]]), main = "", xlab = "Years to treatment",
      col = cols, ci.col = cols, ci.lwd = 1, ci.width = 0.2,
      pt.pch = 19, ylim = c(-0.07, 0.07))

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Math", "RLA"),
       col = cols, lwd = 2, cex = 1, inset = 0, horiz = TRUE)

dev.off()

# subgroups storms
pdf("ResultsPlotStormsPresentationSubgroups.pdf", width = 15 / 2.5, height = 12 / 2.5)

layout(matrix(c(1:5, 5), ncol = 2, byrow = TRUE), heights = c(4, 4, 1))

par(mar = c(2, 4, 2, 1))
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

######## Power Analysis ########

cols <- viridisLite::viridis(length(0:8))
pdf("PowerAnalysis.pdf", width = 15 / 2.5, height = 12 / 2.5)

layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(5, 1))
invis.Map(function(model, subject){
  
  N <- model$nobs
  k <- model$nparams
  
  par(mar = c(4, 4, 2, 1))
  plot(c(-0.1, 0.1), c(0, 1), type = "n",
       xlab = "True Value", ylab = "Power", main = subject)
  
  invis.Map(function(i, col){
    id <- paste0("year::", i)
    sigma.hat <- model$coeftable[id, "Std. Error"]
    
    power <- function(beta, N, k, sigma.hat, alpha = 0.05){
      # compute power
      pow <- 1 - pt(qt(1 - alpha/2, N-k) - beta / sigma.hat, N-k) + pt(qt(alpha/2, N-k) - beta / sigma.hat, N-k)
      return(pow)
    }
    
    betas <- seq(-0.1, 0.1, 0.0001)
    lines(betas, power(betas, N = N, k = k, sigma.hat = sigma.hat),
          type = "l", col = col, lwd = 2)
    
  }, 0:8, cols)
}, list(model.math[[1]], model.rla[[1]]), c("Mathematics", "RLA"))

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = paste0("Year ", 0:8),
       col = cols, lwd = 2, cex = 1, inset = 0, ncol = 5)

dev.off()
