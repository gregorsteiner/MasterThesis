

######## Preliminaries ########

# load packages
library(fixest)

# load data
dat <- readRDS("DataCombined.RDS")

# only use a subset of data to decrease comptuting time for now
dat <- dat[sample(1:nrow(dat), size = 1e5), ]


######## Poisson model ########

dat$YEAR <- format(dat$DATE, "%Y")

model <- fepois(FatalAccidents ~ TMIN + PRCP | YEAR, data = dat)
summary(model)


