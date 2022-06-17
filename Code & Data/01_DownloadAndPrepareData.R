
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

# drop Covid
fema.disasters <- fema.disasters[!(declarationTitle %in% c("COVID-19", "COVID-19 PANDEMIC"))]

# assistance data
fema.assistance <- setDT(rfema::open_fema("PublicAssistanceApplicantsProgramDeliveries",
                                          ask_before_call = FALSE))


# remove COVID 19
fema.assistance <- fema.assistance[!(declarationTitle %in% c("COVID-19", "COVID-19 PANDEMIC"))]

# drop Puerto Rico
fema.assistance <- fema.assistance[stateName != "Puerto Rico"]

# add county fips to the assistance data
fema.disasters$designatedArea <- gsub("[()]", "", fema.disasters$designatedArea)
fema.assistance <- merge(fema.assistance, fema.disasters[, .(disasterNumber, designatedArea, fipsStateCode, fipsCountyCode)],
                         all.x = TRUE, all.y = FALSE,
                         by.x = c("disasterNumber", "countyApplicantJurisdiction"),
                         by.y = c("disasterNumber", "designatedArea"))
fema.assistance[, fips := as.numeric(paste0(fipsStateCode, fipsCountyCode))]


# export
saveRDS(fema.assistance, "AssistanceDataRawWithFIPS.RDS")


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
  as.numeric(format(declarationDate, "%m")) %in% 9:12 ~ as.numeric(format(declarationDate, "%Y")) + 1,
  as.numeric(format(declarationDate, "%m")) %in% 1:3 ~ as.numeric(format(declarationDate, "%Y")),
  as.numeric(format(declarationDate, "%m")) %in% 4:8 ~ NA_real_
  )]


# save to save time
saveRDS(fema.disasters, "DisasterDataNoTerrorismNoCovid.RDS")



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



# merge disaster and assistance data
fema.dis.agg <- merge(fema.dis.agg, fema.assist.agg, all.x = TRUE, all.y = FALSE)


# fill NAs with zero
fema.dis.agg[, totalDamage := ifelse(is.na(totalDamage), 0, totalDamage)]
fema.dis.agg[, federalAssistance := ifelse(is.na(federalAssistance), 0, federalAssistance)]                  



######## Election Data ########


# read presidential election data (from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ)
elec <- fread("Raw Data/countypres_2000-2020.csv")

# create assistance covariate data
assist.cov <- elec[,
                   .(ShareDem2016 = candidatevotes[party == "DEMOCRAT" & year == 2016] / totalvotes[party == "DEMOCRAT" & year == 2016],
                     ShareRep2016 = candidatevotes[party == "REPUBLICAN" & year == 2016] / totalvotes[party == "REPUBLICAN" & year == 2016],
                     ShareDem2008 = candidatevotes[party == "DEMOCRAT" & year == 2008] / totalvotes[party == "DEMOCRAT" & year == 2008],
                     ShareRep2008 = candidatevotes[party == "REPUBLICAN" & year == 2008] / totalvotes[party == "REPUBLICAN" & year == 2008]),
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



######## Alternative Disaster Data (Storms) ########

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




######## Alternative Disaster Data (Heat) ########


# stations data
stationsraw <- setDT(rnoaa::ghcnd_stations())
stationsraw <- stationsraw[first_year <= 20078 & last_year >= 2018]

# find closest station based on centroid coordinates
stations <- rbindlist(rnoaa::meteo_nearby_stations(lat_lon_df = data.frame(id = housingData::geoCounty[, "fips"],
                                                                           latitude = housingData::geoCounty[, "lat"],
                                                                           longitude = housingData::geoCounty[, "lon"]),
                                                   station_data = stationsraw,
                                                   limit = 1))

# add fips id
stations$fips <- housingData::geoCounty[, "fips"]

# get data for these stations (first download as list and then rbind)
tmplist <- lapply(list(1:800, 801:1600, 1601:2400, 2401:nrow(stations)),
                  function(ind){
                    res <- rnoaa::meteo_pull_monitors(unique(stations[ind, "id"]), var = "TMAX", 
                                                      date_min = "2008-01-01", date_max = "2018-12-31")
                    return(res)
                  })

dat.weather <- setDT(do.call(rbind, tmplist))
rm(tmplist) # delete the list again to save memory

# add (school)year column
dat.weather[, year := dplyr::case_when(
  # if in september to december add 1 to the year (Schoolyear x/x+1 is x+1)
  as.numeric(format(date, "%m")) %in% 9:12 ~ as.numeric(format(date, "%Y")) + 1,
  as.numeric(format(date, "%m")) %in% 1:3 ~ as.numeric(format(date, "%Y")),
  as.numeric(format(date, "%m")) %in% 4:8 ~ NA_real_
)]

# divide temperature by ten to get degrees
dat.weather[, tmax := tmax / 10]


# aggregate by schoolyear and id
dat.weather.agg <- dat.weather[!is.na(year), .(tmax = mean(tmax, na.rm = TRUE),
                                               DaysAbove30 = sum(tmax > 30, na.rm = TRUE)),
                               by = .(id, year)]

# only keep relevant school years
dat.weather.agg <- dat.weather.agg[year %in% 2009:2018]


# merge with fips
dat.weather.agg <- merge(dat.weather.agg, stations[, c("id", "fips")])

# drop id column
dat.weather.agg$id <- NULL

# fips as numeric
dat.weather.agg[, fips := as.numeric(as.character(fips))]




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

# add heat data
dat <- merge(dat, dat.weather.agg,
             by = c("fips", "year"),
             all.x = TRUE, all.y = TRUE)


# # add indicators whether they had a disaster before 2009 or 2016
# dat[, `:=`(DisasterBefore2009 = as.numeric(fips %in% fema.disasters[syDeclared < 2009, as.numeric(paste0(fipsStateCode, fipsCountyCode))]),
#            DisasterBeforeOct2016 = as.numeric(fips %in% fema.disasters[as.Date(declarationDate) < as.Date("2016-10-01"), as.numeric(paste0(fipsStateCode, fipsCountyCode))]))]


# add year of first treatment
dat[, `:=`(TreatStart = ifelse(any(DisasterTreat == 1),
                               min(year[DisasterTreat == 1], na.rm = TRUE),
                               3000),
           TreatStartStorm = ifelse(any(StormTreat == 1),
                                    min(year[StormTreat == 1], na.rm = TRUE),
                                    3000))
    , by = fips]


# add relative time variable
dat[, `:=`(RelTime = ifelse(TreatStart == 3000, NA, year - TreatStart),
           RelTimeStorm = ifelse(TreatStartStorm == 3000, NA, year - TreatStartStorm))]


# export as RDS
saveRDS(dat, "Data.RDS")


