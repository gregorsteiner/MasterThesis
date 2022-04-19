

######## Preliminaries ########

source("AuxFunctions.R")
library(data.table)
library(fixest)


# read data
dat <- readRDS("Data.RDS")


######## Models ########

# county fixed effects
model <- feols(cs_mn_all ~ CumuDisasters | year + grade + subject,
               data = dat, vcov = "iid")
etable(model)

# check residuals
resid <- residuals(model)
qqnorm(resid)
qqline(resid, lwd = 2, col = 2)

