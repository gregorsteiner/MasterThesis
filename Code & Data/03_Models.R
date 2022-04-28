

######## Preliminaries ########

source("AuxFunctions.R")
library(data.table)
library(fixest)


# read data
dat <- readRDS("Data.RDS")

######## Models ########


# main model
model.math <- feols(c(cs_mn_all, cs_mn_wbg, cs_mn_whg, cs_mn_mfg, cs_mn_neg) ~ 
                 sunab(TreatStart, year)| year + fips + grade,
               data = dat[subject == "mth"], vcov = "iid")

model.rla <- feols(c(cs_mn_all, cs_mn_wbg, cs_mn_whg, cs_mn_mfg, cs_mn_neg) ~ 
                     sunab(TreatStart, year)| year + fips + grade,
                   data = dat[subject == "rla"], vcov = "iid")

# automatically export as tex file
etable(model.math, file = "../TeX Files/MainResultsMath.tex", replace = TRUE,
       label = "MainResultsMath", title = "Results (Mathematics)",
       dict = c(cs_mn_all = "Mean test score", cs_mn_wbg = "White-Black",
                cs_mn_mfg = "Male-Female", cs_mn_neg = "Adv.-Disadv.",
                cs_mn_whg = "White-Hispanic",
                year = "Year", grade = "Grade", fips = "County",
                lninc50all = "Log Income"))

etable(model.rla, file = "../TeX Files/MainResultsRLA.tex", replace = TRUE,
       label = "MainResultsRLA", title = "Results (RLA)",
       dict = c(cs_mn_all = "Mean test score", cs_mn_wbg = "White-Black",
                cs_mn_mfg = "Male-Female", cs_mn_neg = "Adv.-Disadv.",
                cs_mn_whg = "White-Hispanic",
                year = "Year", grade = "Grade", fips = "County",
                lninc50all = "Log Income"))


# plots
dep.vars <- c("Mean test score", "White-Black", "White-Hispanic",
              "Male-Female", "Adv.-Disadv.")
pch <- 16:(15+length(dep.vars))


par(mfrow = c(1, 2))
invisible(Map(function(sub, model){
  iplot(model, main = sub, xlab = "Year",
        pt.col = 1:length(dep.vars), pt.pch = pch)
  legend("topright", legend = dep.vars,
         col = 1:length(dep.vars), pch = pch)
  
}, c("Mathematics", "Reading & Language Arts"), list(model.math, model.rla)))





