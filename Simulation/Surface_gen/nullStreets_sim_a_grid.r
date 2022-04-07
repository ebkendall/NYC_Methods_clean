library(mvtnorm)
library(rgeos)
library(tidyverse)

# -----------------------------------------------------------------------------
# This is used for the HotSpot, Random, and Uniform Surfaces since we can have
# the highest density of points
# -----------------------------------------------------------------------------

load("../Data/nycSub.RData")

#create data frame of grid points
uniform_DF <- makegrid(nycSub, n = 2000000)

#convert grid to spatial coordinates
uniform_DF_SP <- SpatialPoints(uniform_DF,proj4string = CRS(proj4string(nycSub)))

#filter grid to only be within NYC boundaries
gridWithin <- SpatialPixels(uniform_DF_SP[nycSub,])
gridWithin = as(gridWithin, "SpatialPoints")
gridWithin$index = c(1:length(gridWithin))

save(gridWithin, file = "../Data/gridWithin.rda")

gridPointValues_length = length(gridWithin)
save(gridPointValues_length, file = "../Data/gridPointValues_length.rda")

gridWithin_prec <- vector(mode = "list", length = 77)
for (i in 1:77) {
  print(i)
  temp = point.in.polygon(gridWithin@coords[,1], gridWithin@coords[,2],
                          nycSub@polygons[[i]]@Polygons[[1]]@coords[,1],
                          nycSub@polygons[[i]]@Polygons[[1]]@coords[,2])
  gridWithin_prec[[i]] <- gridWithin[which(temp > 0), ]
}

save(gridWithin_prec, file = "../Data/gridWithin_prec.rda")

