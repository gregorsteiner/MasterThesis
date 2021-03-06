

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
model.math <- feols(c(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                 sunab(TreatStart, year, ref.p = c(-1, -3000), bin.rel = c(-3:-3000, 6:9)) | year + fips + grade,
               data = dat[subject == "mth"], cluster = "fips")

model.rla <- feols(c(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                     sunab(TreatStart, year, ref.p = c(-1, -3000), bin.rel = c(-3:-3000, 6:9)) | year + fips + grade,
                   data = dat[subject == "rla"], cluster = "fips")



# STorms
model.math.storm <- feols(c(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                            sunab(TreatStartStorm, year, ref.p = c(-1, -3000), bin.rel = c(-3:-3000, 6:9)) | year + fips + grade,
                          data = dat[subject == "mth"], cluster = "fips")

model.rla.storm <- feols(c(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                           sunab(TreatStartStorm, year, ref.p = c(-1, -3000), bin.rel = c(-3:-3000, 6:9)) | year + fips + grade,
                         data = dat[subject == "rla"], cluster = "fips")



# fema with only storms as robustness check

model.math.fema.storm <- feols(c(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                                 sunab(TreatStartStormFEMA, year, ref.p = c(-1, -3000), bin.rel = c(-3:-3000, 6:9)) | year + fips + grade,
                               data = dat[subject == "mth"], cluster = "fips")

model.rla.fema.storm <- feols(c(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                                sunab(TreatStartStormFEMA, year, ref.p = c(-1, -3000), bin.rel = c(-3:-3000, 6:9)) | year + fips + grade,
                              data = dat[subject == "rla"], cluster = "fips")



######## Get observations per bin ########

dat.reltime <- unique(dat[, .(fips, RelTime, RelTimeStorm, RelTimeStormFEMA)])

tab <- t(mapply(function(x){
  table(x)
}, dat.reltime[, -1]))

rownames(tab) <- c("FEMA Disasters", "NWS Storms", "FEMA Storms")

writeLines(kableExtra::kable_styling(knitr::kable(tab, format = "latex", booktabs = TRUE,
                                                  caption = "Number of counties per relative period",
                                                  label = "BinSizes"),
                                     latex_options = "scale_down"),
           "../TeX Files/BinSizes.tex")



######## Export Graphs ########


# dependent variables
dep.vars <- c("Overall", "White", "Black", "Hispanic", "Female", "Econ. Disadv.")

# color scheme
cols <- c("firebrick", "cornflowerblue")

# fema
pdf("ResultsPlot.pdf", width = 15 / 2.5, height = 20 / 2.5)

layout(matrix(c(1:6, 7, 7), ncol = 2, byrow = TRUE), heights = c(4, 4, 4, 1))

par(mar = c(2, 4, 2, 1))

invis.Map(function(math, rla, name){
  
  iplot(list(math, rla), main = name, xlab = "Years to treatment",
        col = cols, ci.col = cols, pt.pch = 19,
        ci.lwd = 1, ci.width = 0.2)
  
}, model.math, model.rla, dep.vars)

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Math", "RLA"),
       col = cols, lwd = 2, cex = 1, inset = 0, horiz = TRUE)


dev.off()



# storms
pdf("ResultsPlotStorm.pdf", width = 15 / 2.5, height = 20 / 2.5)

layout(matrix(c(1:6, 7, 7), ncol = 2, byrow = TRUE), heights = c(4, 4, 4, 1))

par(mar = c(2, 4, 2, 1))
invis.Map(function(math, rla, name){
  
  iplot(list(math, rla), main = name, xlab = "Years to treatment",
        col = cols, ci.col = cols, pt.pch = 19,
        ci.lwd = 1, ci.width = 0.2)
  
}, model.math.storm, model.rla.storm, dep.vars)

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Math", "RLA"),
       col = cols, lwd = 2, cex = 1, inset = 0, horiz = TRUE)


dev.off()



# fema storms
pdf("ResultsPlotFEMAStorm.pdf", width = 15 / 2.5, height = 20 / 2.5)

layout(matrix(c(1:6, 7, 7), ncol = 2, byrow = TRUE), heights = c(4, 4, 4, 1))

par(mar = c(2, 4, 2, 1))
invis.Map(function(math, rla, name){
  
  iplot(list(math, rla), main = name, xlab = "Years to treatment",
        col = cols, ci.col = cols, pt.pch = 19,
        ci.lwd = 1, ci.width = 0.2)
  
}, model.math.fema.storm, model.rla.fema.storm, dep.vars)

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Math", "RLA"),
       col = cols, lwd = 2, cex = 1, inset = 0, horiz = TRUE)


dev.off()






# change plot layout for slides

# overall fema
pdf("ResultsPlotPresentation.pdf", width = 22 / 2.5, height = 16 / 2.5)

layout(matrix(c(1:6, 7, 7), ncol = 2, byrow = TRUE), heights = c(4, 4, 4, 1))

par(mar = c(2, 4, 2, 1))

invis.Map(function(math, rla, name){
  
  iplot(list(math, rla), main = name, xlab = "Years to treatment",
        col = cols, ci.col = cols, pt.pch = 19,
        ci.lwd = 1, ci.width = 0.2)
  
}, model.math, model.rla, dep.vars)

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Math", "RLA"),
       col = cols, lwd = 2, cex = 1, inset = 0, horiz = TRUE)

dev.off()

# storms
pdf("ResultsPlotStormPresentation.pdf", width = 22 / 2.5, height = 16 / 2.5)

layout(matrix(c(1:6, 7, 7), ncol = 2, byrow = TRUE), heights = c(4, 4, 4, 1))

par(mar = c(2, 4, 2, 1))
invis.Map(function(math, rla, name){
  
  iplot(list(math, rla), main = name, xlab = "Years to treatment",
        col = cols, ci.col = cols, pt.pch = 19,
        ci.lwd = 1, ci.width = 0.2)
  
}, model.math.storm, model.rla.storm, dep.vars)

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Math", "RLA"),
       col = cols, lwd = 2, cex = 1, inset = 0, horiz = TRUE)


dev.off()

# fema storms
pdf("ResultsPlotFEMAStormPresentation.pdf", width = 22 / 2.5, height = 16 / 2.5)

layout(matrix(c(1:6, 7, 7), ncol = 2, byrow = TRUE), heights = c(4, 4, 4, 1))

par(mar = c(2, 4, 2, 1))
invis.Map(function(math, rla, name){
  
  iplot(list(math, rla), main = name, xlab = "Years to treatment",
        col = cols, ci.col = cols, pt.pch = 19,
        ci.lwd = 1, ci.width = 0.2)
  
}, model.math.fema.storm, model.rla.fema.storm, dep.vars)

par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Math", "RLA"),
       col = cols, lwd = 2, cex = 1, inset = 0, horiz = TRUE)


dev.off()



######## Heat Models ########

model.temp.math <-  feols(c(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~
                            tmax | year + fips + grade,
                          data = dat[subject == "mth"], vcov = "iid")

model.temp.rla <- feols(c(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~
                          tmax | year + fips + grade,
                        data = dat[subject == "rla"], vcov = "iid")

model.days.math <-  feols(c(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~
                            DaysAbove30 | year + fips + grade,
                          data = dat[subject == "mth"], vcov = "iid")

model.days.rla <- feols(c(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~
                          DaysAbove30 | year + fips + grade,
                        data = dat[subject == "rla"], vcov = "iid")



# change cientifc notation options such that the coefficient table looks as desired
options(scipen = 99)

coefmatrix <- do.call(rbind, Map(function(model, digs){
  # get coefficients and standard errors
  coefs <- sapply(model, "[[", "coefficients")
  ses <- sapply(model, "[[", "se")
  # round
  coefs.round <- round(coefs, digs)
  ses.round <- round(ses, digs)
  # paste them together
  res <- matrix(c(coefs.round, ses.round),
                ncol = 6, byrow = TRUE)
  # and potentially add star placeholders
  res[1, ] <- ifelse(abs(coefs / ses) > 1.96, paste0(res[1, ], "star"), res[1, ])
  # add dollar sign placeholders for math mode and 
  res[1, ] <- paste0("dollar", res[1, ], "dollar")
  res[2, ] <- paste0("dollar(", res[2, ], ")dollar")
  
  return(res)
  
}, list(model.temp.math, model.temp.rla, model.days.math, model.days.rla), c(5, 5, 6, 6)))

rownames(coefmatrix) <- c("Max. Temp. (Math)", "", "Max. Temp. (RLA)", "",
                          "Days ab. 30 (Math)", "", "Days ab. 30 (RLA)", "")
colnames(coefmatrix) <- c("Overall", "White", "Black", "Hispanic", "Female", "Econ. Disadv.")


# add mean as last row
coefmatrix <- rbind(coefmatrix,
                    "Mean" = round(sapply(dat[, .(cs_mn_all, cs_mn_wht, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd)], mean, na.rm = TRUE), 3))

# create tex table
textable <- knitr::kable(coefmatrix, format = "latex", booktabs = TRUE,
                         linesep = c(rep(c('', '\\addlinespace'), 3), "", "\\midrule"),
                         caption = "Estimated coefficients for heat models",
                         label = "HeatResults")

# add footnote
textable <- kableExtra::add_footnote(textable, notation = "none",
                                     label = "Note: Standard errors in parentheses. Stars indicate significance at a 5% level.")

# scaled down version for presentation
textable <- kableExtra::kable_styling(textable, latex_options = "scale_down")

# gsub special characters
textable <- gsub("dollar", "$", gsub("star", "^{***}", textable))


writeLines(textable, "../TeX Files/HeatResults.tex")
writeLines(textable, "../Presentation/HeatResults.tex")


