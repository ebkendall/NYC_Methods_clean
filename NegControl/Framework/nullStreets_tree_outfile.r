library("sp")
library("sf")
library("rgeos")
library("raster")

set.seed(11)

load("../Data/nycSub.RData")
load("../Data/ind_prec_df.rda")
load("../Data/indexList_MAIN.RData")
load("../Data/totalStreetBuffInfo_ORIG.RData")
load("../Data/tree_df_coord.RData")
load("../Data/treesByPrec.RData")    # gridWithin_prec treesByPrec

for (k in 2:13) {

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
      
      t_grid1 = data.frame(treesByPrec[[prec_ind_1]],
                           "prec" = rep(1, nrow(treesByPrec[[prec_ind_1]])),
                           "o_ind" = 1:nrow(treesByPrec[[prec_ind_1]]))
      t_grid2 = data.frame(treesByPrec[[prec_ind_2]],
                           "prec" = rep(2, nrow(treesByPrec[[prec_ind_2]])),
                           "o_ind" = 1:nrow(treesByPrec[[prec_ind_2]]))
      
      gridCoords = rbind(t_grid1, t_grid2)
      colnames(gridCoords) = c("x", "y", "prec", "o_ind")
      
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

      t1 = count1 = length(which(p1 > 0)) # now I need to access their index
      t2 = count2 = length(which(p2 > 0))

      s1 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength1
      s2 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength2
      
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

      tStat = tStat_a = pval = NA

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
      pval = NA

      if (count1 <= n/2) {
        pval = pbinom(count1, n, p) + 1 - pbinom(count2, n, p)
      } else {
        pval = pbinom(count2, n, p) + 1 - pbinom(count1, n, p)
      }

      sim_orig[i,] = c(t1, t2, tStat, tStat_a, pval, area1, area2, s1, s2)

    }

    save(sim_orig, file = paste0('../Output_tree/combination/sim_orig_', k, '.dat'))
}

    




## --------------------------- Plot --------------------------------------------
# p_val_df <- vector(mode = "list", length = 13)

# print("Now onto plotting everything")

# pdf('../Output_tree/Plots/pValDistr_tree.pdf')
# par(mfrow=c(2,2))

# for (k in 2:13) {

#     print(k)

#     load(paste0("../Output_tree/combination/combinedMatchingSetup", k, ".dat"))
#     load(paste0("../Output_tree/combination/sim_orig_", k, ".dat"))

#     ## Now remove data points where these ratios are much different
#     wRatioOk = which(combinedMatchingSetupFix$ratioArea / combinedMatchingSetupFix$ratioStreet < 1.4 &
#                        combinedMatchingSetupFix$ratioArea / combinedMatchingSetupFix$ratioStreet > 1/1.4)
#     combinedMatchingSetupFix2 = combinedMatchingSetupFix[wRatioOk,]

#     v1 = sd(combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2, na.rm=TRUE)^2
#     v2 = sd(combinedMatchingSetupFix2$ratioArea, na.rm=TRUE)^2

#     pval = rep(NA, nrow(sim_orig))

#     for (ii in 1 : nrow(sim_orig)) {
#       if (ii %in% indexList_MAIN) {
#         ## find matches
#         area_temp = sim_orig$area1[ii] + sim_orig$area2[ii]
#         ratio_temp = max(sim_orig$area1[ii] / sim_orig$area2[ii],
#                          sim_orig$area2[ii] / sim_orig$area1[ii])
#         stat_temp = sim_orig$tStats_area[ii]

#         dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2))^2/v1) +
#                            ((ratio_temp - combinedMatchingSetupFix2$ratioArea)^2 / v2))

#         w50 = order(dist_temp)[1:150]

#         null_dist = combinedMatchingSetupFix2$tStat_area[w50]
#         pval[ii] = mean(null_dist > stat_temp)
#       }
#     }

#     hist(pval, main = paste0("Trees: pVal for B_", k*100),
#          xlab = paste0("Perc. < 0.05 is ",  round(mean(pval < 0.05, na.rm=TRUE), 4)),
#          xlim=c(0,1))

#     p_val_df[[k]] = pval
# }
# dev.off()

# save(p_val_df, file = paste0("../Output_tree/combination/p_val_df.dat"))
