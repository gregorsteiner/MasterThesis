
######## Auxiliary functions ########


### Coordinates to County function

# Thanks to https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county

library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  name <- countyNames[indices]
  
  # return
  return(name)
  
}



### names to fips function based on the usmaps package
names_to_fips <- function(stateName, countyName){
  # if its is statewide use "000" as county
  if(countyName %in% c("Statewide", "Multiple Counties")) {
    res <- paste0(usmap::fips(stateName), "000")
    return(res)
  }
  
  # a few counties cause problems
  if(countyName == "LaSalle Parish") countyName <- "La Salle Parish"
  if(countyName == "Hoonah?Angoon Census Area") countyName <- "Hoonah-Angoon Census Area"
  if(countyName == "Anchorage, Municipality of") countyName <- "Anchorage Municipality"
  if(countyName == "Honolulu, City and County of") countyName <- "Honolulu County"
  
  # else just use the result
  res <- try(usmap::fips(stateName, countyName))
  return(res)
}







