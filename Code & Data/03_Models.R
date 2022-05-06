

######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)
library(fixest)


hei <- 600
wid <- 600

# read data
dat <- readRDS("Data.RDS")



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


# plots
dep.vars <- c("Overall", "Black", "Hispanic", "Female", "Econ. Disadv.")
pch <- 16:(15+length(dep.vars))
cols <- viridisLite::viridis(length(dep.vars))

png("ResultsPlot.png", width = wid + 100, height = hei + 200)

par(mfrow = c(2, 1),
    mar = c(4, 4, 2, 1))
invisible(Map(function(sub, model){
  iplot(model, main = sub, xlab = "Year",
        pt.col = cols, pt.pch = pch, ci.col = cols)
  legend("topleft", legend = dep.vars,
         col = cols, pch = pch)
  
}, c("Mathematics", "Reading & Language Arts"), list(model.math, model.rla)))

dev.off()



# residuals
res <- residuals(model.math)

par(mfrow = c(2, 3))
invisible(apply(res, 2, function(x){
  y <- x[!is.na(x)]
  
  qqnorm(y)
  qqline(y)
}))



