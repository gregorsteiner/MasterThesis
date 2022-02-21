########## Get BLS data using their API ##########
keyBLS <- "853622c5f7544c619a8005ae7676f815"


# fips codes for states to form the id
fips <- c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15:42, 44:51, 53:56)

# Map over states
BLSdf <- do.call(rbind, Map(function(StateNo, StateName){
  # create ID
  if(StateNo < 10) StateNo <- paste0("0", StateNo)
  ID <- paste0("SMU", StateNo, "00000050000000", c(1, 2))
  
  # get Data
  payload <- list(
    "seriesid" = ID,
    "startyear" = 2015,
    "endyear" = 2021,
    "catalog" = FALSE,
    "calculations" = TRUE,
    "annualaverage" = FALSE,
    "registrationKey" = keyBLS)
  dat <- blsAPI::blsAPI(payload, 2, return_data_frame = TRUE)
  
  # add state variable
  dat$State <- StateName
  
  # return
  return(dat)
  
  
}, fips, state.name))


# change Id names to variable names
VarID <- substring(BLSdf$seriesID, nchar(BLSdf$seriesID))

BLSdf[, "seriesID"] <- ifelse(VarID == "1", "NoEmpl", "AvgWklHours")


# from long to wide
BLSdf <- tidyr::pivot_wider(BLSdf,
                            names_from = seriesID,
                            values_from = value)


# Number of employees is in thousands
BLSdf$NoEmpl <- as.numeric(BLSdf$NoEmpl) * 1000

# Compute hours worked
BLSdf$HoursWorked <- as.numeric(BLSdf$AvgWklHours) * BLSdf$NoEmpl


# aggregate to quarterly
qtrs <- dplyr::case_when(
  BLSdf$periodName %in% month.name[1:3] ~ "Q1",
  BLSdf$periodName %in% month.name[4:6] ~ "Q2",
  BLSdf$periodName %in% month.name[7:9] ~ "Q3",
  BLSdf$periodName %in% month.name[10:12] ~ "Q4"
)
BLSdf$YearQtr <- paste(BLSdf$year, qtrs, sep = "-")

BLS.qtr <- aggregate(BLSdf$HoursWorked,
                     list(BLSdf$State, BLSdf$YearQtr),
                     mean) |> setNames(c("State", "YearQtr", "HoursWorked"))


# Multiply by 13 to get hours per quarter
BLS.qtr$HoursWorked <- BLS.qtr$HoursWorked * 13

########## Get Output Data from FRED ##########
keyFRED <- "d22561f4c073eb8bfa30e385b37317e1"

fredr::fredr_set_key(keyFRED)


# map over all states
FREDdf <- do.call(rbind, Map(function(StateName, StateAbb){
  # create ID
  ID <- paste0(StateAbb, "RQGSP")
  
  # get data
  dat <- fredr::fredr(ID,
               observation_start = as.Date("2012-01-01"),
               observation_end = as.Date("2021-12-31"),
               frequency = "q")
  
  # add state name
  dat$State <- StateName
  
  # times 1e6 since output is in million dollars (chained 2012)
  dat$value <- dat$value * 1e6
  
  # only keep relevant columns
  dat <- dat[, c("date", "State", "value")]
  colnames(dat)[3] <- "Output"
  
  # return
  return(dat)
  
}, state.name, state.abb))

FREDdf$YearQtr <- paste(format(as.Date(FREDdf$date), "%Y"),
                        quarters(as.Date(FREDdf$date)),
                        sep = "-")


########## Join & Compute Productivity ##########


# join by state and period
Dat <- merge(FREDdf, BLS.qtr,
             by = c("State", "YearQtr"))

# productivity = Output / hours worked
Dat$Prod <- Dat$Output / Dat$HoursWorked


# Export as Csv
write.csv(Dat, "ProductivityData.csv", row.names = FALSE)













