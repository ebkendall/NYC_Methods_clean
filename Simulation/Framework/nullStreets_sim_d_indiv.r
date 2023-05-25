library("sp")
library("sf")
library("rgeos")
library("raster")

test_stats <- function(gridPointValues, combinedMatchingSetupFix, w50) {

    t_stat_df = data.frame("tStat_area" = c(-1),
                           "naive_pval" = c(-1),
                           "tStat_strt" = c(-1))

    rowInd = 1
    
    for (jj in w50) {

        s1 = combinedMatchingSetupFix$DATA$streets1[jj]
        s2 = combinedMatchingSetupFix$DATA$streets2[jj]

        area1 = combinedMatchingSetupFix$DATA$area1[jj]
        area2 = combinedMatchingSetupFix$DATA$area2[jj]

        gridVals_ind_1 = combinedMatchingSetupFix$GRID_IND_1[[jj]]
        gridVals_ind_2 = combinedMatchingSetupFix$GRID_IND_2[[jj]]

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

        vals = c(t1,s1,t2,s2)
        if(sum(vals == 0) > 0) {
            if(vals[2] == 0 | vals[4] == 0) {
                vals = vals+1
            } else {
                vals[1] = vals[1] + 1
                vals[3] = vals[3] + 1
            }
        } 

        tStat = tStat_a = NA

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

        t_stat_df[rowInd, ] = c(tStat_a, pval, tStat)
        rowInd = rowInd + 1
    }

    return(t_stat_df)
}

test_stats_orig <- function(gridPointValues, sim_orig, ii) {
    
    t_stat_df = data.frame("tStat_area" = c(-1),
                           "naive_pval" = c(-1),
                           "tStat_strt" = c(-1))

    rowInd = 1

    s1 = sim_orig$DATA$streets1[ii]
    s2 = sim_orig$DATA$streets2[ii]

    area1 = sim_orig$DATA$area1[ii]
    area2 = sim_orig$DATA$area2[ii]

    gridValues1 = gridPointValues[sim_orig$GRID_IND_1[[ii]]]
    gridValues2 = gridPointValues[sim_orig$GRID_IND_2[[ii]]]

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

    if (count1 <= n/2) {
        pval = pbinom(count1, n, p) + 1 - pbinom(count2, n, p)
    } else {
        pval = pbinom(count2, n, p) + 1 - pbinom(count1, n, p)
    }

    t_stat_df[rowInd, ] = c(tStat_a, pval, tStat)

    return(t_stat_df)
}

match_count <- seq(20, 1200, by = 20)

trialNum = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 1-1000
set.seed(trialNum)

load("../Data/indexList_MAIN.RData")

file_names <- c(paste0("../Data/Surfaces/gridPointValues_hotspot_", trialNum,".rda"),
                paste0("../Data/Surfaces/gridPointValues_uniform_", trialNum,".rda"),
                paste0("../Data/Surfaces/gridPointValues_cov_r_", trialNum,".rda"),
                paste0("../Data/Surfaces/gridPointValues_cov_c_", trialNum,".rda"))

tau = 0.5  

perc_pval_match <- vector(mode = "list", length = 4) # Order: HotSpot, Uniform, Random, Correlated
perc_pval_match[[1]] = perc_pval_match[[2]] = perc_pval_match[[3]] = perc_pval_match[[4]] = vector(mode = "list", length = 13)

p_val_df <- vector(mode = "list", length = 4) # Order: HotSpot, Uniform, Random, Correlated
p_val_df[[1]] = p_val_df[[2]] = p_val_df[[3]] = p_val_df[[4]] = vector(mode = "list", length = 13)


for (k in 2:13) {

  print(k)
  
  load(paste0('../Output_noWater/nullGridInfo/combinedMatchingSetup', k, ".dat"))
  load(paste0('../Output_noWater/origGridInfo/sim_orig_', k, '.dat'))

  for (s_name in 1:4) {

    load(file_names[s_name])
    gridPointValues = NULL

    if (s_name == 1) {gridPointValues = gridPointValues_hotspot * tau}
    else if (s_name == 2) {gridPointValues = gridPointValues_uniform}
    else if (s_name == 3) {gridPointValues = gridPointValues_cov_r}
    else if (s_name == 4) {gridPointValues = gridPointValues_cov_c_big}
    else {print("Incorrect input to start")}

    wMax_a = max(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))
    wMin_a = min(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))
    
    wMax_s = max(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))
    wMin_s = min(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))
    
    wMatchOk = which((combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) > wMin_a &
                       (combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) < wMax_a &
                       (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) > wMin_s &
                       (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) < wMax_s)
    
    combinedMatchingSetupFix2 = combinedMatchingSetupFix
    combinedMatchingSetupFix2$DATA = combinedMatchingSetupFix2$DATA[wMatchOk,]
    combinedMatchingSetupFix2$GRID_IND_1 = combinedMatchingSetupFix2$GRID_IND_1[wMatchOk]
    combinedMatchingSetupFix2$GRID_IND_2 = combinedMatchingSetupFix2$GRID_IND_2[wMatchOk]
    
    v1 = sd(combinedMatchingSetupFix2$DATA$area1 + combinedMatchingSetupFix2$DATA$area2, na.rm=TRUE)^2
    v2 = sd(combinedMatchingSetupFix2$DATA$ratioArea, na.rm=TRUE)^2

    row_num = 1
    perc_pval_match[[s_name]][[k]] = data.frame("num_match" = match_count,
                                                "perc_pval_less_05" = rep(NA, length(match_count)))
    p_val_df[[s_name]][[k]] = matrix(nrow = length(match_count), ncol = nrow(sim_orig$DATA))

    for(j in match_count) {
      print(j)
      pval = rep(NA, nrow(sim_orig$DATA))

      for (ii in 1 : nrow(sim_orig$DATA)) {
        if (ii %in% indexList_MAIN) {
          ## find matches
          area_temp = sim_orig$DATA$area1[ii] + sim_orig$DATA$area2[ii]
          ratio_temp = max(sim_orig$DATA$area1[ii] / sim_orig$DATA$area2[ii],
                           sim_orig$DATA$area2[ii] / sim_orig$DATA$area1[ii])

          dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$DATA$area1 + combinedMatchingSetupFix2$DATA$area2))^2/v1) +
                             ((ratio_temp - combinedMatchingSetupFix2$DATA$ratioArea)^2 / v2))

        #   w50 = order(dist_temp)[1:j]
        # Choose one mother street --------------------
            match_counter = jj = 1
            streetInd = vector(mode = "list", length = 77)
            for (w in 1:77) {streetInd[[w]] = c(-1) }
            w50 = rep(NA, j)
            close_ind = order(dist_temp)
            while(match_counter <= j) {
                temp = combinedMatchingSetupFix2$DATA[close_ind[jj], ]
                if(!(temp$indigo %in% streetInd[[temp$precinct]])) {
                    w50[match_counter] = close_ind[jj]
                    match_counter = match_counter + 1
                    streetInd[[temp$precinct]] = append(streetInd[[temp$precinct]], temp$indigo)
                }
                jj = jj + 1
            }
        # --------------------------------------------

          tStats_temp = test_stats(gridPointValues, combinedMatchingSetupFix2, w50)
          null_dist = tStats_temp$tStat_area

          orig_temp = test_stats_orig(gridPointValues, sim_orig, ii)
          stat_temp = orig_temp$tStat_area

          test = density(null_dist, bw = "ucv")
          xx = test$x
          yy = test$y
          dx <- xx[2L] - xx[1L]
          C <- sum(yy) * dx
          
          p.unscaled <- sum(yy[xx >= stat_temp]) * dx
          p.scaled <- p.unscaled / C
          
          pval[ii] = p.scaled
        }
      }

      perc_pval = mean(pval < 0.05, na.rm=TRUE)
      perc_pval_match[[s_name]][[k]]$perc_pval_less_05[row_num] = perc_pval
      p_val_df[[s_name]][[k]][row_num, ] = pval
      row_num = row_num + 1
    }
  }
}

save(p_val_df, file = paste0("../Output_noWater/sim_results/p_vals_match_rel/p_val_df_", trialNum, ".dat"))
save(perc_pval_match, file = paste0("../Output_noWater/sim_results/p_vals_match_rel/perc_pval_match_", trialNum, ".dat"))

# ---------------------------------------------------------------
# ------- Plotting everything
# ---------------------------------------------------------------

# load("../Output/sim_orig/p_vals_match_rel/perc_pval_match_1.dat")
# final_plot = perc_pval_match
# plot_mat = perc_pval_match

# for (i in 2:100) {
#     load(paste0("../Output/sim_orig/p_vals_match_rel/perc_pval_match_", i, ".dat"))
#     for(j in 1:3) {
#         for(k in 2:13) {
#             final_plot[[j]][[k]] = rbind(final_plot[[j]][[k]], perc_pval_match[[j]][[k]])
#             plot_mat[[j]][[k]] = cbind(plot_mat[[j]][[k]], perc_pval_match[[j]][[k]]$perc_pval_less_05)
#         }
#     }
# }

# folder_type = c("HotSpot", "Uniform", "Random")

# pdf('../Output/Plots/pVal_num_match.pdf')
# par(mfrow=c(3,1))
# for (i in 2:13) {
#   for(k in 1:3) {
#     print(paste0(k, " ", i))
#     pval = final_plot[[k]][[i]]
#     temp = cbind(plot_mat[[k]][[i]][,1], rowMeans(plot_mat[[k]][[i]][,-1]))
#     # print(t(temp))
#     plot(pval$num_match, pval$perc_pval_less_05, main = paste0(folder_type[k], ": pVal for B", i*100),
#          xaxt="none", xlab = "Perc. < 0.05 is ")
#     axis(1, seq(10,500,20), las=2)
#     abline(h=0.05, col = "red")
#     lines(temp[,1], temp[,2], col = "purple", lwd = 2)
#   }
# }
# dev.off()

# pdf('../Output/Plots/pVal_num_match2.pdf')
# par(mfrow=c(3,1))
# for (i in 2:13) {
#   for(k in 1:3) {
#     pval = plot_mat[[k]][[i]]
#     plot(pval[,1], pval[,2], main = paste0(folder_type[k], ": pVal for B", i*100),
#          xaxt="none", xlab = "Perc. < 0.05 is ")
#     axis(1, seq(10,500,20), las=2)
#     for(w in 1:100) {
#         abline(lm(pval[,w+1] ~ pval[,1]), col = w)
#         # lines(pval[,1], pval[,w+1], col = w)
#     }
#   }
# }
# dev.off()