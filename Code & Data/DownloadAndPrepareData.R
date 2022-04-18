
######## Preliminaries ########

source("AuxFunctions.R")
library(data.table)




######## SEDA Testing data ########
# (from "https://stacks.stanford.edu/file/druid:db586ns4974/seda_county_long_cs_4.1.csv")


seda.comb <- setDT(readRDS("SedaData.RDS"))


######## FEMA disaster data ########


# load data from FEMA 
fema.disasters <- setDT(rfema::open_fema("DisasterDeclarationsSummaries",
                                         ask_before_call = FALSE))

fema.assistance <- setDT(rfema::open_fema("PublicAssistanceApplicantsProgramDeliveries",
                                          ask_before_call = FALSE))


# remove COVID 19
fema.assistance <- fema.assistance[declarationTitle != "COVID-19"]


# add county fips to the assistance data
fema.assistance[, fips := as.numeric(mapply(names_to_fips, stateName, countyApplicantJurisdiction))]

# aggreagte assistance data by county and year
fema.assist.agg <- fema.assistance[, .(totalDamage = sum(as.numeric(totalAppDamageCost)),
                                       federalAssistance = sum(as.numeric(federalShareObligated))),
                                   by = .(fips, year = as.numeric(format(declarationDate, "%Y")))]









# compute cumulative disasters by county
fema.cum <- fema.disasters[, .(Disasters = length(unique(disasterNumber))),
                           by = .(fips = paste0(fipsStateCode, fipsCountyCode))]

# and add statewide cases to all counties within the state
ind.statewide <- substring(fema.cum$fips, 3, 5) == "000"

# isolate the statewide data
statewide <- fema.cum[ind.statewide]
fema.cum <- fema.cum[!ind.statewide]

# by state add statewide cases to all counties
fema.cum <- rbindlist(apply(statewide[order(as.numeric(statewide$fips)), ], 1, function(x){
  # check for all counties in that state
  bool <- substring(fema.cum$fips, 1, 2) == substring(x[1], 1, 2)
  
  # and add the number of disasters for those
  res <- fema.cum[bool]
  res[, "Disasters"] <- res[, "Disasters"] + as.numeric(x[2])
  
  # sort and return
  return(res[order(res$fips), ])
}))



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



# add assistance data
dat <- merge(dat, fema.assist.agg,
             all.x = TRUE, all.y = FALSE)


# export as RDS
saveRDS(dat, "Data.RDS")


