
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






### Heatwave Indicator function
EHE <- function(x, date, quantile = 0.85){
  
  # x as numeric and date as date
  x <- as.numeric(x)
  date <- as.Date(date)
  
  # compute threshold (85th percentile of july and august temperatures)
  bool.jul.aug <- format(date, "%m") %in% c("07", "08") 
  threshhold <- quantile(x[bool.jul.aug], quantile, na.rm = TRUE)
  
  # Mark as heatwave all consecutive days that exceed the threshhold
  bool <- x > threshhold
  bool <- ifelse(is.na(bool), FALSE, bool)
  
  # define EHE
  EHE <- as.numeric(bool)
  return(EHE)
  
}


