# this script contains functions to calculate the center of gravity between geographic points
# and the median distance from the center to these points

library(tidyverse)
library(geosphere)

calc_median <- function(center, places){
  
  distances <- numeric()
  for (i in 1:nrow(places)){
  distances <- c(distances, distVincentySphere(center, c(places$lon[i],places$lat[i])))  
  }
  return(round(median(distances)/1000,2))
}

calc_center <- function(places){
  # Convert Lat / Lon from degree to radians
  places$latr <- places$lat * pi / 180
  places$lonr <- places$lon * pi / 180
  
  # Convert latr / lonr to Cartesian coordinates for fall locations.
  places$x = cos(places$latr) * cos(places$lonr)
  places$y = cos(places$latr) * sin(places$lonr)
  places$z = sin(places$latr)
  
  # Compute averages for x, y, z coordinates
  x = mean(places$x)
  y = mean(places$y)
  z = mean(places$z)
  
  # Convert average cartesian coordinate back to lat / lon
  Lon = atan2(y, x)
  Hyp = sqrt(x * x + y * y)
  Lat = atan2(z, Hyp)
  
  # Convert Lat / Lon to degrees.
  Lat = Lat * 180 / pi
  Lon = Lon * 180 / pi 
  
  # calculate the median distance from the centroid to the points
  center <- c(Lon, Lat) # SIC! order is reversed!
  median_dist <- calc_median(center, places)
  
  return(c(Lat, Lon, median_dist))
}




