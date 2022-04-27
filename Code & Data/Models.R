

######## Preliminaries ########

source("AuxFunctions.R")
library(data.table)
library(fixest)


# read data
dat <- readRDS("Data.RDS")


######## Models ########


# check TWFE weigths
lapply(c("cs_mn_all", "cs_mn_wbg", "cs_mn_mfg", "cs_mn_whg", "cs_mn_neg"), function(x){
  TwoWayFEWeights::twowayfeweights(dat, Y = x, G = "fips", T = "year",
                                   D = "DisasterDummy", cmd_type = "feTR",
                                   controls = c("lninc50all", "unempall"))
})



# county fixed effects
model <- feols(c(cs_mn_all, cs_mn_wbg, cs_mn_whg, cs_mn_mfg, cs_mn_neg) ~ 
                 DisasterDummy + lninc50all| year + fips + grade + subject,
               data = dat, vcov = "iid")

# automatically export as tex file
etable(model, file = "../TeX Files/MainResults.tex", replace = TRUE,
       label = "MainResults", title = "Results",
       dict=c(cs_mn_all = "Mean test score", cs_mn_wbg = "White-Black",
              cs_mn_mfg = "Male-Female", cs_mn_neg = "Adv.-Disadv.",
              cs_mn_whg = "White-Hispanic",
              DisasterDummy = "Disaster", year = "Year", grade = "Grade",
              subject = "Subject", fips = "County",
              lninc50all = "Log Income", unempall = "Unemployment"))


