
######## Preliminaries ########

source("AuxFunctions.R")
library(data.table)




######## SEDA Testing data ########

# test scores data
dat.seda.gcs <- read.csv("C:/Users/gregs/OneDrive/Uni/Economics Master/Literatur Masterarbeit/seda_county_long_gcs_4.1.csv")
#dat.seda <- read.csv("https://stacks.stanford.edu/file/druid:db586ns4974/seda_county_long_gcs_4.1.csv")

# covariates data
dat.seda.cov <- read.csv("C:/Users/gregs/OneDrive/Uni/Economics Master/Literatur Masterarbeit/seda_cov_county_long_4.1.csv")

# merge
dat.seda <- merge(dat.seda.gcs, dat.seda.cov)


######## FEMA disaster data ########


# load data from FEMA 
fema.disasters <- rfema::open_fema("DisasterDeclarationsSummaries",
                                   ask_before_call = FALSE)

fema.assistance <- rfema::open_fema("PublicAssistanceApplicantsProgramDeliveries",
                                    ask_before_call = FALSE)


# add disaster type to assistance data
fema.comb <- merge(fema.assistance,
                   unique(fema.disasters[, c("disasterNumber", "incidentType", "incidentBeginDate", "incidentEndDate")]),
                   by = "disasterNumber", all.x = TRUE, all.y = FALSE)





