
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

# drop terrorism
fema.disasters <- fema.disasters[incidentType != "Terrorist"]


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
elec <- fread("Raw Data/countypres_2000-2020.csv")

# create assistance covariate data
assist.cov <- elec[,
                   .(ShareDem2016 = candidatevotes[party == "DEMOCRAT" & year == 2016] / totalvotes[party == "DEMOCRAT" & year == 2016],
                     ShareRep2016 = candidatevotes[party == "REPUBLICAN" & year == 2016] / totalvotes[party == "REPUBLICAN" & year == 2016],
                     ShareDem2008 = candidatevotes[party == "DEMOCRAT" & year == 2008] / totalvotes[party == "DEMOCRAT" & year == 2008],
                     ShareRep2008 = candidatevotes[party == "REPUBLICAN" & year == 2008] / totalvotes[party == "REPUBLICAN" & year == 2008],
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



######## Alternative Disaster Data ########

# read hurricanes 
dat.hur <- fread("Raw Data/storm_data_search_results.csv")

# exclude hurricanes with 0 damage
dat.hur <- dat.hur[DAMAGE_PROPERTY_NUM > 0]

# create year and fips columns
dat.hur$BEGIN_DATE <- as.Date(dat.hur$BEGIN_DATE, format = "%m/%d/%Y")
dat.hur[, `:=`(fips = as.numeric(dplyr::case_when(nchar(CZ_FIPS) == 3 ~ paste0(usmap::fips(STATE_ABBR), CZ_FIPS),
                                                  nchar(CZ_FIPS) == 2 ~ paste0(usmap::fips(STATE_ABBR), 0, CZ_FIPS),
                                                  nchar(CZ_FIPS) == 1 ~ paste0(usmap::fips(STATE_ABBR), "00", CZ_FIPS))),
               year = dplyr::case_when(
                 # if in september to december add 1 to the year (Schoolyear x/x+1 is x+1)
                 as.numeric(format(BEGIN_DATE, "%m")) %in% 9:12 ~ as.numeric(format(BEGIN_DATE, "%Y")) + 1,
                 as.numeric(format(BEGIN_DATE, "%m")) %in% 1:3 ~ as.numeric(format(BEGIN_DATE, "%Y")),
                 as.numeric(format(BEGIN_DATE, "%m")) %in% 4:8 ~ NA_real_
               ))]

# aggregate by fips and year
dat.hur <- dat.hur[, .(Hurricanes = length(EVENT_ID)),
                   by = .(fips, year)]




# read tornado data
dat.tor <- fread("Raw Data/1950-2020_all_tornadoes.csv")

# exclude tornadoes with weak (0 or 1 on EF scale) or missing (-9) magnitude
dat.tor <- dat.tor[mag %in% 2:5]

# pivot county codes from wide to long
dat.tor <- melt(dat.tor, id.vars = c("om", "date", "stf"),
                measure.vars = c("f1", "f2", "f3", "f4"))

# aggregate by year and county
dat.tor <- dat.tor[value != 0, .(Tornadoes = length(om)),
                   by = .(fips = as.numeric(dplyr::case_when(
                     nchar(value) == 3 ~ paste0(stf, value),
                     nchar(value) == 2 ~ paste0(stf, 0, value),
                     nchar(value) == 1 ~ paste0(stf, "00", value)
                   )),
                          year = dplyr::case_when(
                            # if in september to december add 1 to the year (Schoolyear x/x+1 is x+1)
                            as.numeric(format(date, "%m")) %in% 9:12 ~ as.numeric(format(date, "%Y")) + 1,
                            as.numeric(format(date, "%m")) %in% 1:3 ~ as.numeric(format(date, "%Y")),
                            as.numeric(format(date, "%m")) %in% 4:8 ~ NA_real_
                          ))]


# only keep relevant years
dat.tor <- dat.tor[year >= 2009 & year <= 2018]

# merge hurricane and tornado data
dat.storms <- merge(dat.tor, dat.hur,
                    all.x = TRUE, all.y = TRUE)

# add tornadoes and Hurricanes
dat.storms[, Storms := rowSums(dat.storms[, .(Tornadoes, Hurricanes)], na.rm = TRUE)]



######## Merge ########

# merge seda and fema no. of disasters
dat <- merge(fema.dis.agg, seda.comb[, .(fips = sedacounty, year, grade, subject,
                                         cs_mn_all, cs_mn_blk, cs_mn_hsp,
                                         cs_mn_fem, cs_mn_ecd, lninc50all,
                                         perhsp, perblk, perwht)],
             by = c("fips", "year"),
             all.x = TRUE, all.y = TRUE)


# add storms data
dat <- merge(dat, dat.storms[, .(fips, year, Storms)],
             by = c("fips", "year"),
             all.x = TRUE, all.y = TRUE)

# drop NA years and fips
dat <- dat[!is.na(fips) & !is.na(year)]

# set NA storms to 0
dat[, Storms := ifelse(is.na(Storms), 0, Storms)]

# add absorbing treatment (as described in Sun & Abraham)
dat[, `:=`(DisasterTreat = as.numeric(cumsum(Disasters) > 0),
           StormTreat = as.numeric(cumsum(Storms) > 0)),
    by = fips]


# add year of first treatment
dat[, `:=`(TreatStart = ifelse(any(DisasterTreat == 1),
                               min(year[DisasterTreat == 1], na.rm = TRUE),
                               3000),
           TreatStartStorm = ifelse(any(StormTreat == 1),
                                    min(year[StormTreat == 1], na.rm = TRUE),
                                    3000))
    , by = fips]



# export as RDS
saveRDS(dat, "Data.RDS")


