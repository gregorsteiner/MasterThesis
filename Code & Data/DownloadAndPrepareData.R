
######## Preliminaries ########

source("AuxFunctions.R")
library(data.table)




######## SEDA Testing data ########

# test scores data
seda.gcs <- fread("C:/Users/gregs/OneDrive/Uni/Economics Master/Literatur Masterarbeit/seda_county_long_cs_4.1.csv")
#dat.seda <- read.csv("https://stacks.stanford.edu/file/druid:db586ns4974/seda_county_long_gcs_4.1.csv")

# covariates data
seda.cov <- fread("C:/Users/gregs/OneDrive/Uni/Economics Master/Literatur Masterarbeit/seda_cov_county_long_4.1.csv")

# merge
seda.comb <- merge(seda.gcs, seda.cov)




######## FEMA disaster data ########


# load data from FEMA 
fema.disasters <- setDT(rfema::open_fema("DisasterDeclarationsSummaries",
                                         ask_before_call = FALSE))

fema.assistance <- setDT(rfema::open_fema("PublicAssistanceApplicantsProgramDeliveries",
                                          ask_before_call = FALSE))


# add disaster type to assistance data
fema.comb <- merge(fema.assistance,
                   unique(fema.disasters[, c("disasterNumber", "incidentType", "incidentBeginDate", "incidentEndDate")]),
                   by = "disasterNumber", all.x = TRUE, all.y = FALSE)

# compute cumulative disasters by county (and remove observations with 000 county)
fema.cum <- fema.disasters[fipsCountyCode != "000",
                           .(Disasters = length(unique(disasterNumber))),
                           by = .(fips = paste0(fipsStateCode, fipsCountyCode))]


# aggregate fema disasters by year and county
fema.dis.agg <- fema.disasters[fipsCountyCode != "000",
                               .(Disasters = length(unique(disasterNumber))),
                               by = .(fips = as.numeric(paste0(fipsStateCode, fipsCountyCode)),
                                      year = as.numeric(fyDeclared))]





######## Merge FEMA and SEDA data ########

# merge seda and fema no. of disasters
dat <- merge(seda.comb, fema.dis.agg,
             by = c("fips", "year"),
             all.x = TRUE, all.y = FALSE)



# fill NA disaster values with 0
dat[, Disasters := ifelse(is.na(Disasters), 0, Disasters)]


# compute cumulative disasters by county
dat[, CumuDisasters := cumsum(Disasters), by = .(fips, grade, subject)]



# export as RDS
saveRDS(dat, "Data.RDS")


