
######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)




######## SEDA Testing data ########
# (from "https://stacks.stanford.edu/file/druid:db586ns4974/seda_county_long_cs_4.1.csv")


seda.comb <- setDT(readRDS("SedaData.RDS"))



######## FEMA disaster data ########


# load data from FEMA 
fema.disasters <- setDT(rfema::open_fema("DisasterDeclarationsSummaries",
                                         ask_before_call = FALSE))



# assistance data
fema.assistance <- setDT(rfema::open_fema("PublicAssistanceApplicantsProgramDeliveries",
                                          ask_before_call = FALSE))


# remove COVID 19
fema.assistance <- fema.assistance[declarationTitle != "COVID-19"]

# drop Puerto Rico
fema.assistance <- fema.assistance[stateName != "Puerto Rico"]

# add county fips to the assistance data
fema.assistance[, fips := as.numeric(mapply(names_to_fips, stateName, countyApplicantJurisdiction))]

# aggreagte assistance data by county and year
fema.assist.agg <- fema.assistance[, .(totalDamage = sum(as.numeric(totalAppDamageCost)),
                                       federalAssistance = sum(as.numeric(federalShareObligated))),
                                   by = .(fips, year = as.numeric(format(declarationDate, "%Y")))]


# merge with empty hull
empty <- data.table(expand.grid("fips" = union(as.numeric(usmap::countypop[, "fips", drop = TRUE]),
                                               unique(fema.assist.agg$fips)),
                                "year" = min(fema.assist.agg$year):max(fema.assist.agg$year)))

fema.assist.agg <- merge(empty, fema.assist.agg,
                         all.x = TRUE)

# remove NA fips
fema.assist.agg <- fema.assist.agg[!is.na(fips)]

# fill NAs with zero
fema.assist.agg[, totalDamage := ifelse(is.na(totalDamage), 0, totalDamage)]
fema.assist.agg[, federalAssistance := ifelse(is.na(federalAssistance), 0, federalAssistance)]                  

# control for statewide cases






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




# add school year variable based on declaration date (as explained in the main text)
fema.disasters[, syDeclared := dplyr::case_when(
  # if in september to december add 1 to the year (Schoolyear x/x+1 is x+1)
  as.numeric(format(declarationDate, "%m")) %in% 9:12 ~ as.numeric(fyDeclared) + 1,
  as.numeric(format(declarationDate, "%m")) %in% 1:3 ~ as.numeric(fyDeclared),
  as.numeric(format(declarationDate, "%m")) %in% 4:8 ~ NA_real_
  )]



# aggregate fema disasters by year and county and add to the empty set
fema.dis.agg <- fema.disasters[, .(Disasters = length(unique(disasterNumber))),
                               by = .(fips = as.numeric(paste0(fipsStateCode, fipsCountyCode)),
                                      year = syDeclared)]

# add statewide to each county in the state
ind.statewide <- substring(fema.dis.agg$fips, 3, 5) == "000"

# isolate the statewide data
statewide <- fema.dis.agg[ind.statewide]
fema.dis.agg <- fema.dis.agg[!ind.statewide]

for (i in 1:nrow(statewide)) {
  # check matching years and fips
  bool <- fema.dis.agg[, fips] == statewide[i, fips] & fema.dis.agg[, year] == statewide[i, year]
  
  # for those add the number of disasters
  if(!is.null(nrow(fema.dis.agg[bool, Disasters]))){
    fema.dis.agg[bool, Disasters] <- fema.dis.agg[bool, Disasters] + statewide[i, Disasters]
  }
  
}



# create empty data expanded to the seda format
empty <- data.table(expand.grid("fips" = as.numeric(usmap::countypop[, "fips", drop = TRUE]),
                                "year" = min(seda.comb$year):max(seda.comb$year)))
fema.dis.agg <- merge(empty, fema.dis.agg,
                      all.x = TRUE, all.y = FALSE)


# fill NA disaster values with 0
fema.dis.agg[, Disasters := ifelse(is.na(Disasters), 0, Disasters)]





######## Election Data ########


# read presidential election data (from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ)
elec <- fread("countypres_2000-2020.csv")

# create assistance covariate data
assist.cov <- elec[year == 2016,
                   .(ShareDem2016 = candidatevotes[party == "DEMOCRAT"] / totalvotes[party == "DEMOCRAT"],
                     ShareRep2016 = candidatevotes[party == "REPUBLICAN"] / totalvotes[party == "REPUBLICAN"],
                     AssistanceApplicant = as.numeric(fips %in% fema.assistance$fips)),
                   by = .(fips = county_fips)]

# add median income
assist.cov <- merge(assist.cov, unique(seda.comb[year == 2016,
                                                 .(MedInc2016 = exp(lninc50all), 
                                                   PovertyRate = povertyall,
                                                   SingleMother = single_momall,
                                                   fips = sedacounty)]),
                    by = "fips", all.x = TRUE, all.y = FALSE)

# save as file
saveRDS(assist.cov, "AssistanceCovData.RDS")


# add to assistance data
fema.assist.agg <- merge(fema.assist.agg, assist.cov,
                         by = "fips", all.x = TRUE, all.y = FALSE)

# save as RDS
saveRDS(fema.assist.agg, "AssistanceData.RDS")


######## Merge FEMA and SEDA data ########

# merge seda and fema no. of disasters
dat <- merge(fema.dis.agg, seda.comb[, .(fips = sedacounty, year, grade, subject,
                                         cs_mn_all, cs_mn_blk, cs_mn_hsp,
                                         cs_mn_fem, cs_mn_ecd,
                                         lninc50all)],
             by = c("fips", "year"),
             all.x = TRUE, all.y = TRUE)




# add absorbing treatment (as described in Sun & Abraham)
dat[, DisasterTreat := as.numeric(cumsum(Disasters) > 0), by = fips]

# # add time to first treatment
# dat[, TimeToTreat := year - min(year[DisasterTreat == 1], na.rm = TRUE), by = fips]

# add year of first treatment
dat[, TreatStart := ifelse(any(DisasterTreat == 1),
                           min(year[DisasterTreat == 1], na.rm = TRUE),
                           1000)
    , by = fips]



# export as RDS
saveRDS(dat, "Data.RDS")


