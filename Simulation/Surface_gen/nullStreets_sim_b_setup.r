library(mvtnorm); library(rgeos); library(tidyverse)

load("../Data/gridPointValues_length.rda") # gridPointValues_length
load("../Data/gridWithin.rda")             # gridWithin
load("../Data/gridWithin_prec.rda")        # gridWithin_prec
load("../Data/nycSub.RData")               # nycSub

k = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 1-1000
set.seed(k)

# initialize grid point values to 1
# the index here corresponds to the index from the gridWithin_prec index
gridPointValues_uniform = gridPointValues_hotspot = rep(1, gridPointValues_length)

# Flat   -----------------------------------------------------------------------
save(gridPointValues_uniform, file = paste0("../Data/Surfaces/gridPointValues_uniform_", k, ".rda"))

# HOT SPOT BIAS ----------------------------------------------------------------

#choosing 200 hotspot centers
hotSpotCenters = sample(c(1:gridPointValues_length), 200)

#create buffer around hotspots
hotSpotPolys = vector(mode = "list", length = length(hotSpotCenters))
for (i in 1:length(hotSpotCenters)) {
  ind = hotSpotCenters[i]
  hotSpotPolys[[i]] = gBuffer(gridWithin[ind,], byid = T, width = 4000)
}

#adjust weight of each coordinate point
for (i in 1:length(hotSpotCenters)) {
  hotSpotAffect = point.in.polygon(gridWithin@coords[,1], gridWithin@coords[,2],
                                   hotSpotPolys[[i]]@polygons[[1]]@Polygons[[1]]@coords[,1],
                                   hotSpotPolys[[i]]@polygons[[1]]@Polygons[[1]]@coords[,2])
  gridPointValues_hotspot[which(hotSpotAffect > 0)] = gridPointValues_hotspot[which(hotSpotAffect > 0)] + 1
}

save(gridPointValues_hotspot, file = paste0("../Data/Surfaces/gridPointValues_hotspot_", k, ".rda"))

# RANDOM -----------------------------------------------------------------------
# randomly giving each grid point a value
gridPointValues_cov_r = round(runif(gridPointValues_length) * 10)

save(gridPointValues_cov_r, file = paste0("../Data/Surfaces/gridPointValues_cov_r_", k, ".rda"))

# -----------------------------------------------------------------------------
# Next we need to create a less dense grid that has correlated information and 
# then extrapolate what the more dense point values will be
# -----------------------------------------------------------------------------

# --------------------------- Correlated ---------------------------
rmnvAnder <-  function(mean,sigma){
  drop(mean + chol(sigma)%*%rnorm(length(mean)))
}

#create data frame of grid points        1000000
uniform_DF_cov_c <- makegrid(nycSub, n = 20000)

#convert grid to spatial coordinates
uniform_DF_SP_cov_c <- SpatialPoints(uniform_DF_cov_c,proj4string = CRS(proj4string(nycSub)))

#filter grid to only be within NYC boundaries
gridWithin_cov_c <- SpatialPixels(uniform_DF_SP_cov_c[nycSub,])
gridWithin_cov_c = as(gridWithin_cov_c, "SpatialPoints")

coordDF <- data.frame(Longitude = gridWithin_cov_c@coords[,1], Latitude = gridWithin_cov_c@coords[,2])
corrStructure <- dist(coordDF)
corrStructure <- as.matrix(corrStructure)
rho = 2500
corrStructure <- -1 * corrStructure / rho
corrStructure <- exp(corrStructure)

print("Calculating correlation structure...")
gridPointValues_cov_c = rmnvAnder(rep(0, nrow(coordDF)), corrStructure) # takes 1 minute to produce
print("Done!")

#forcing everything to be positive
gridPointValues_cov_c = gridPointValues_cov_c + abs(min(gridPointValues_cov_c))
gridPointValues_cov_c = as.numeric(round(gridPointValues_cov_c))

# extrapolating the fine grid to match
gridPointValues_cov_c_big = rep(1, gridPointValues_length)

for (ii in 1:gridPointValues_length) {
  print(ii)
  temp_buff = gBuffer(gridWithin[ii,], byid = T, width = 2200 + 500*(k-1))
  corr_points = point.in.polygon(gridWithin_cov_c@coords[,1], gridWithin_cov_c@coords[,2],
                                 temp_buff@polygons[[1]]@Polygons[[1]]@coords[,1],
                                 temp_buff@polygons[[1]]@Polygons[[1]]@coords[,2])
  gridPointValues_cov_c_big[ii] = mean(gridPointValues_cov_c[which(corr_points > 0)])
  if(is.na(gridPointValues_cov_c_big[ii])) {print(paste0("NA: ", ii))}
}

gridPointValues_cov_c_big = round(gridPointValues_cov_c_big)
print(sum(is.na(gridPointValues_cov_c_big)))

save(gridPointValues_cov_c_big, file = paste0("../Data/Surfaces/gridPointValues_cov_c_", k, ".rda"))
