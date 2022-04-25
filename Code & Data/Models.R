

######## Preliminaries ########

source("AuxFunctions.R")
library(data.table)
library(fixest)


# read data
dat <- readRDS("Data.RDS")


######## Models ########


# check TWFE weigths
TwoWayFEWeights::twowayfeweights(dat, Y = "cs_mn_all",
                                 G = "fips", T = "year", D = "DisasterDummy",
                                 cmd_type = "feTR")


# county fixed effects
model <- feols(c(cs_mn_all, cs_mn_wbg, cs_mn_mfg, cs_mn_neg) ~ 
                 DisasterDummy | year + grade + subject,
               data = dat, vcov = "iid")

# automatically export as tex file
etable(model, coefstat = "confint",
       file = "../TeX Files/MainResults.tex", replace = TRUE,
       label = "MainResults", title = "Results",
       dict=c(cs_mn_all = "Mean test score", cs_mn_wbg = "White-Black gap",
              cs_mn_mfg = "Male-Female gap", cs_mn_neg = "Disadvantaged gap",
              DisasterDummy = "Disaster"))


# check residuals
resid <- residuals(model)

plot(resid[, 1])

qqnorm(resid)
qqline(resid, lwd = 2, col = 2)

