## --------------------------------------------------------------------------
## The goal of this file is to test the new matching method on all buffer
## widths and see if we get more reasonable histograms and p-values
## --------------------------------------------------------------------------

## **** This file is directly based on trial_joey_method.r ****

## Formatting the data
library("sp")
library("sf")
library("rgeos")
library("raster")

surface_type = c("hotspot", "uniform", "cov_r", "cov_c")
save_type = c("HotSpot/", "Uniform/", "Random/", "Correlated/")
folder_type <- c("HotSpot_combine", "Uniform_combine", "Random_combine", "Correlated_combine")

trialNum = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 1-100
set.seed(trialNum)

tau = 10      # CHANGE FOR HOTSPOT STUFF

load("../Data/nycSub.RData")
load("../Data/ind_prec_df.rda")
load("../Data/gridWithin_prec.rda")
load("../Data/indexList_MAIN.RData")
load("../Data/totalStreetBuffInfo_ORIG.RData")

for (s_name in 1:4) {

  Dir = paste0('../Output_noWater/sim_orig/', save_type[s_name])
  print(Dir)

  sim_master <- vector(mode = "list", length = 13)

  for (k in 2:13) {

    load(paste0("../Data/Surfaces/gridPointValues_", surface_type[s_name],
                "_", trialNum, ".rda"))

    gridPointValues = NULL
    if(s_name == 1) {gridPointValues = gridPointValues_hotspot * tau}
    if(s_name == 2) {gridPointValues = gridPointValues_uniform}
    if(s_name == 3) {gridPointValues = gridPointValues_cov_r}
    if(s_name == 4) {gridPointValues = gridPointValues_cov_c_big}

    sim_orig <- data.frame("count1" = rep(NA,164), "count2" = rep(NA,164),
                           "tStats" = rep(NA,164), "tStats_area" = rep(NA,164),
                           "pVals_naive" = rep(NA,164), "area1" = rep(NA,164),
                           "area2" = rep(NA,164), "street1" = rep(NA, 164),
			   "street2" = rep(NA, 164))
    

    print(paste0("Finding points in original borders for k = ", k))
    for (i in indexList_MAIN) {

      # New stuff
      prec_ind_1 = which(nycSub$Precinct == ind_prec_df$prec1[i])
      prec_ind_2 = which(nycSub$Precinct == ind_prec_df$prec2[i])
      
      nyc_small = nycSub[c(prec_ind_1, prec_ind_2), ]
      
      t_grid1 = data.frame(gridWithin_prec[[prec_ind_1]]@coords,
                           "prec" = rep(1, nrow(gridWithin_prec[[prec_ind_1]]@coords)),
                           "o_ind" = 1:nrow(gridWithin_prec[[prec_ind_1]]@coords))
      t_grid2 = data.frame(gridWithin_prec[[prec_ind_2]]@coords,
                           "prec" = rep(2, nrow(gridWithin_prec[[prec_ind_2]]@coords)),
                           "o_ind" = 1:nrow(gridWithin_prec[[prec_ind_2]]@coords))
      
      gridCoords = rbind(t_grid1, t_grid2)
      colnames(gridCoords) = c("x", "y", "prec", "o_ind")
      
      gridVals_ind_master = c(gridWithin_prec[[prec_ind_1]]$index,
                              gridWithin_prec[[prec_ind_2]]$index)
      
      print(paste0("index ", i, " of 164"))
      
      tempOverlap = gIntersection(totalStreetBuffInfo_ORIG[[k]][[i]]$buffer, nyc_small, byid = T)
      poly_order = tempOverlap@plotOrder[1:2]
      
      int_1 = gIntersection(tempOverlap[poly_order[1], ], nycSub[prec_ind_1, ])
      int_2 = gIntersection(tempOverlap[poly_order[1], ], nycSub[prec_ind_2, ])
      
      if(is.null(int_1)) {
        int_1 = 0
      } else {
        int_1 = int_1@polygons[[1]]@area
      }
      
      if(is.null(int_2)) {
        int_2 = 0
      } else {
        int_2 = int_2@polygons[[1]]@area
      }
      
      if(int_1 < int_2) {
        temp = poly_order[1]
        poly_order[1] = poly_order[2]
        poly_order[2] = temp
      }
      
      poly1 = tempOverlap[poly_order[1], ]
      poly2 = tempOverlap[poly_order[2], ]

      poly_ind1 = poly1@polygons[[1]]@plotOrder[1]
      poly_ind2 = poly2@polygons[[1]]@plotOrder[1]

      area1 = poly1@polygons[[1]]@Polygons[[poly_ind1]]@area
      area2 = poly2@polygons[[1]]@Polygons[[poly_ind2]]@area

      p1 = point.in.polygon(gridCoords[,1], gridCoords[,2],
                            poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,1], 
                            poly1@polygons[[1]]@Polygons[[poly_ind1]]@coords[,2])
      p2 = point.in.polygon(gridCoords[,1], gridCoords[,2],
                            poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,1], 
                            poly2@polygons[[1]]@Polygons[[poly_ind2]]@coords[,2])

      ind1 <- which(p1 > 0)
      ind2 <- which(p2 > 0)
      
      s1 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength1
      s2 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength2
      
      gridVals_ind_1 = gridVals_ind_master[ind1]
      gridVals_ind_2 = gridVals_ind_master[ind2]

 
      if(sum(is.na(gridVals_ind_1)) != 0 | sum(is.na(gridVals_ind_2)) != 0) {
        print("NA was found **************************")
      }
      if(sum(gridVals_ind_1 %in% gridVals_ind_2) != 0 | sum(gridVals_ind_2 %in% gridVals_ind_1) != 0) {
        print("Unsucessful division ******************")
      }

      gridValues1 = gridPointValues[gridVals_ind_1]
      gridValues2 = gridPointValues[gridVals_ind_2]

      arr1 <- sum(gridValues1)
      arr2 <- sum(gridValues2)

      count1 = count2 = 0

      #count on one side of boundary
      if(arr1 > 0) {count1 = rpois(1, arr1)}
      else {count1 = rpois(1, 1)} #assume there is at least 1

      #count on the other side of the boundary
      if(arr2 > 0) {count2 = rpois(1, arr2)}
      else {count2 = rpois(1, 1)} #assume there exists at least 1

      t1 = count1
      t2 = count2

      print("Calculating test stats")

      vals = c(t1,s1,t2,s2)
      if(sum(vals == 0) > 0) {
        if(vals[2] == 0 | vals[4] == 0) {
          vals = vals+1
        } else {
          vals[1] = vals[1] + 1
          vals[3] = vals[3] + 1
        }
      }

      tStat = tStat_a = pval = 0

      # Want division to be large / small (streets)
      if ((vals[1]/vals[2]) > (vals[3]/vals[4])) {
        tStat = (vals[1]/vals[2]) / (vals[3]/vals[4])
      } else {
        tStat = (vals[3]/vals[4]) / (vals[1]/vals[2])
      }

      # Want division to be large / small (area)
      if ((vals[1]/area1) > (vals[3]/area2)) {
        tStat_a = (vals[1]/area1) / (vals[3]/area2)
      } else {
        tStat_a = (vals[3]/area2) / (vals[1]/area1)
      }

      n = count1 + count2
      p = 0.5
      pval = 0

      if (count1 <= n/2) {
        pval = pbinom(count1, n, p) + 1 - pbinom(count2, n, p)
      } else {
        pval = pbinom(count2, n, p) + 1 - pbinom(count1, n, p)
      }

      sim_orig[i,] = c(t1, t2, tStat, tStat_a, pval, area1, area2, s1, s2)

    }

    sim_master[[k]] = sim_orig
  }

  save(sim_master, file = paste0(Dir, 'sim_master_', trialNum, '.dat'))
}


## --------------------------- Plot --------------------------------------------
# p_val_df <- vector(mode = "list", length = 3) # Order: HotSpot, Uniform, Random
# p_val_df[[1]] = p_val_df[[2]] = p_val_df[[3]] = vector(mode = "list", length = 13)
# 
# print("Now onto plotting everything")
# 
# pdf(paste0('../Output_noWater/Plots/pValDistr', trialNum, '.pdf'))
# par(mfrow=c(2,3))
# 
# for (k in 2:13) {
# 
#   print(k)
# 
#   for (s_name in 1:3) {
# 
#     load(paste0("../Output_noWater/", folder_type[s_name], "/combinedMatchingSetup", trialNum, ".dat"))
# 
#     Dir = paste0('../Output_noWater/sim_orig/', save_type[s_name])
#     print(Dir)
# 
#     load(paste0(Dir, "sim_master_", trialNum,".dat"))
# 
#     ## Now remove data points where these ratios are much different
#     # wRatioOk = which(comboInfo[[k]]$ratioArea / comboInfo[[k]]$ratioStreet < 1.4 &
#     #                    comboInfo[[k]]$ratioArea / comboInfo[[k]]$ratioStreet > 1/1.4)
#     # combinedMatchingSetupFix2 = comboInfo[[k]][wRatioOk,]
# 
#     combinedMatchingSetupFix2 = comboInfo[[k]]
#     
#     v1 = sd(combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2, na.rm=TRUE)^2
#     v2 = sd(combinedMatchingSetupFix2$ratioArea, na.rm=TRUE)^2
# 
#     pval = rep(NA, nrow(sim_master[[k]]))
# 
#     for (ii in 1 : nrow(sim_master[[k]])) {
#       if (ii %in% indexList_MAIN) {
#         ## find matches
#         area_temp = sim_master[[k]]$area1[ii] + sim_master[[k]]$area2[ii]
#         ratio_temp = max(sim_master[[k]]$area1[ii] / sim_master[[k]]$area2[ii],
#                          sim_master[[k]]$area2[ii] / sim_master[[k]]$area1[ii])
#         stat_temp = sim_master[[k]]$tStats_area[ii]
# 
#         dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2))^2/v1) +
#                            ((ratio_temp - combinedMatchingSetupFix2$ratioArea)^2 / v2))
# 
#         w50 = order(dist_temp)[1:150]
# 
#         null_dist = combinedMatchingSetupFix2$tStat_area[w50]
# 
#         test = density(null_dist, bw = "ucv")
#         xx = test$x
#         yy = test$y
#         dx <- xx[2L] - xx[1L]
#         C <- sum(yy) * dx
#         
#         p.unscaled <- sum(yy[xx >= stat_temp]) * dx
#         p.scaled <- p.unscaled / C
#         
#         
#         pval[ii] = p.scaled
#       }
#     }
# 
#     hist(pval, main = paste0(folder_type[s_name], ": pVal for B_", k*100),
#          xlab = paste0("Perc. < 0.05 is ",  round(mean(pval < 0.05, na.rm=TRUE), 4)),
#          xlim=c(0,1))
# 
#     p_val_df[[s_name]][[k]] = pval
#   }
# }
# dev.off()
# 
# save(p_val_df, file = paste0("../Trial2/sim_orig/p_vals/p_val_df_", trialNum, ".dat"))
