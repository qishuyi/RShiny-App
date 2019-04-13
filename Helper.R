library(sp)
library(rworldmap)

# A function that takes in a dataframe where column 1 contains longitude in degrees and column 2 latitude in degrees
# Borrowed from StackOverflow: 
# https://stackoverflow.com/questions/14334970/convert-latitude-and-longitude-coordinates-to-country-name-in-r
coords2continent = function(points)
{  
  continentsSP <- getMap(resolution='low')
  
  # convert our list of points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(continentsSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, continentsSP)
  
  # return the ADMIN names of each country
  indices$REGION   # returns the continent (7 continent model)
}

