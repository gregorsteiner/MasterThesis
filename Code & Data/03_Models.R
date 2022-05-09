

######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)
library(fixest)


hei <- 400
wid <- 600

# read data
dat <- readRDS("Data.RDS")
#assist <- readRDS("AssistanceData.RDS")
assist.cov <- readRDS("AssistanceCovData.RDS")


######## Models ########


# Sun & Abraham
model.math <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                 sunab(TreatStart, year, bin.rel = c(-5:-9)) | year + fips + grade,
               data = dat[subject == "mth"], vcov = "iid")

model.rla <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                     sunab(TreatStart, year, bin.rel = c(-5:-9)) | year + fips + grade,
                   data = dat[subject == "rla"], vcov = "iid")

# automatically export as tex file
etable(model.math, file = "../TeX Files/MainResultsMath.tex", replace = TRUE,
       label = "MainResultsMath", title = "Results (Mathematics)",
       dict = c("Overall" = cs_mn_all,
                "Black" = cs_mn_blk,
                "Hispanic" = cs_mn_hsp,
                "Female" = cs_mn_fem,
                "Econ. Disadv." = cs_mn_ecd,
                year = "Year", grade = "Grade", fips = "County",
                lninc50all = "Log Income"))

etable(model.rla, file = "../TeX Files/MainResultsRLA.tex", replace = TRUE,
       label = "MainResultsRLA", title = "Results (RLA)",
       dict = c("Overall" = cs_mn_all,
                "Black" = cs_mn_blk,
                "Hispanic" = cs_mn_hsp,
                "Female" = cs_mn_fem,
                "Econ. Disadv." = cs_mn_ecd,
                year = "Year", grade = "Grade", fips = "County",
                lninc50all = "Log Income"))



# plot results
dep.vars <- c("Overall", "Black", "Hispanic", "Female", "Econ. Disadv.")
cols <- c(3, 4)


png("ResultsPlot.png", width = 15, height = 20, units = "cm", res = 1200)

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
invis.Map(function(math, rla, name){
  
  iplot(list(math, rla), main = name, xlab = "Years to treatment",
        col = cols, ci.col = cols, pt.pch = 19)
  legend("topleft", legend = c("Math", "RLA"),
         col = cols, pch = 19)
  
}, model.math, model.rla, dep.vars)

dev.off()



# CS using the did package
att <- did::att_gt(yname = "cs_mn_all", tname = "year", idname = "fips", gname = "TreatStart",
                   data = dat[grade == 3 & subject == "mth"])
att_agg <- did::aggte(att, type = "dynamic")
did::ggdid(att_agg) +
  theme_light()


# Compare SA & CS using the staggered package
cs <- staggered::staggered_cs(dat[grade == 3 & subject == "mth"],
                              i = "fips", t = "year", g = "TreatStart",
                              y = "cs_mn_all",
                              estimand = "eventstudy", eventTime = -8:7)

sa <- staggered::staggered_sa(dat[grade == 3 & subject == "mth"],
                              i = "fips", t = "year", g = "TreatStart",
                              y = "cs_mn_all",
                              estimand = "eventstudy", eventTime = -8:7)



plot(sa$eventTime, sa$estimate, type = "n",
     xlim = range(sa$eventTime),
     ylim = range(c(sa$estimate - 2 * sa$se, cs$estimate + 2*cs$se)),
     ylab = "Estimate & Conf. Interval",
     xlab = "Years to treatment")
grid()
abline(h = 0, col = 2, lty = "dashed")
plotCI(sa, add = TRUE, col = 3)
plotCI(cs, add = TRUE, shift = TRUE, col = 4)
legend("bottomleft", legend = c("SA", "CS"),
       col = c(3, 4), pch = 19)






# sun & abraham with storms data
model.math.storm <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                            sunab(TreatStartStorm, year, bin.rel = c(-5:-9)) | year + fips + grade,
                          data = dat[subject == "mth"], vcov = "iid")

model.rla.storm <- feols(c(cs_mn_all, cs_mn_blk, cs_mn_hsp, cs_mn_fem, cs_mn_ecd) ~ 
                           sunab(TreatStartStorm, year, bin.rel = c(-5:-9)) | year + fips + grade,
                         data = dat[subject == "rla"], vcov = "iid")

# plot results
dep.vars <- c("Overall", "Black", "Hispanic", "Female", "Econ. Disadv.")
cols <- c(3, 4)

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))
invis.Map(function(math, rla, name){
  
  iplot(list(math, rla), main = name, xlab = "Years to treatment",
        col = cols, ci.col = cols, pt.pch = 19)
  legend("topleft", legend = c("Math", "RLA"),
         col = cols, pch = 19)
  
}, model.math.storm, model.rla.storm, dep.vars)





# logistic regression for assistance covariates
assist.cov$MedInclog <- log(assist.cov$MedInc2016)
model.logit <- feglm(AssistanceApplicant ~ ShareDem2016 + MedInclog
                     + PovertyRate + SingleMother, data = assist.cov,
                     family = binomial("logit"))
etable(model.logit, file = "../TeX Files/ResultsLogit.tex", replace = TRUE,
       label = "ResultsLogit", title = "Determinants of Assistance Application",
       dict = c(AssistanceApplicant = "Applicant",
                ShareDem2016 = "Share of democratic voters",
                MedInclog = "Median Income (logs)",
                PovertyRate = "Poverty Rate", 
                SingleMother = "Share of single mothers"))



  
  
  
