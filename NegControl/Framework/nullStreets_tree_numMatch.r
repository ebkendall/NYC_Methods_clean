library("sp")
library("sf")
library("rgeos")
library("raster")

match_count <- seq(20, 1000, by = 20)
load("../Data/indexList_MAIN.RData")

set.seed(10)

## --------------------------- Plot --------------------------------------------
# perc_pval_match <- vector(mode = "list", length = 13)

# p_val_df <- vector(mode = "list", length = 13)

# for (k in 2:13) {
k = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

    print(k)

    load(paste0("../Output_tree/nullGridInfo/combinedMatchingSetup", k, ".dat"))

    load(paste0("../Output_tree/origGridInfo/sim_orig_", k,".dat"))

    ## Now remove data points where these ratios are much different
    wRatioOk = which(combinedMatchingSetupFix$DATA$ratioArea / combinedMatchingSetupFix$DATA$ratioStreet < 1.4 &
                       combinedMatchingSetupFix$DATA$ratioArea / combinedMatchingSetupFix$DATA$ratioStreet > 1/1.4)
    combinedMatchingSetupFix2 = combinedMatchingSetupFix$DATA[wRatioOk,]

    v1 = sd(combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2, na.rm=TRUE)^2
    v2 = sd(combinedMatchingSetupFix2$ratioArea, na.rm=TRUE)^2

    row_num = 1
    perc_pval_match = data.frame("num_match" = match_count,
                                 "perc_pval_less_05" = rep(NA, length(match_count)))
    p_val_df = matrix(nrow = length(match_count), ncol = nrow(sim_orig$DATA))
    
    t_stat_area = abs(combinedMatchingSetupFix2$count1 / combinedMatchingSetupFix2$area1 - combinedMatchingSetupFix2$count2 / combinedMatchingSetupFix2$area2)
    
    t_stat_area_orig = abs(sim_orig$DATA$count1 / sim_orig$DATA$area1 - sim_orig$DATA$count2 / sim_orig$DATA$area2)
    
    # t_stat_streets = abs(combinedMatchingSetupFix2$count1 / combinedMatchingSetupFix2$streets1 - combinedMatchingSetupFix2$count2 / combinedMatchingSetupFix2$streets2)
    
    # t_stat_streets_orig = abs(sim_orig$DATA$count1 / sim_orig$DATA$streets1 - sim_orig$DATA$count2 / sim_orig$DATA$streets2)

    sim_orig = sim_orig$DATA
    
    for(j in match_count) {
      print(j)
      pval = rep(NA, nrow(sim_orig))

      for (ii in 1 : nrow(sim_orig)) {
        if (ii %in% indexList_MAIN) {
          ## find matches
          ## Area -------------------------------------------------------------
          area_temp = sim_orig$area1[ii] + sim_orig$area2[ii]
          ratio_temp = max(sim_orig$area1[ii] / sim_orig$area2[ii],
                           sim_orig$area2[ii] / sim_orig$area1[ii])
          stat_temp = t_stat_area_orig[ii]

          dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2))^2/v1) +
                             ((ratio_temp - combinedMatchingSetupFix2$ratioArea)^2 / v2))

          # w50 = order(dist_temp)[1:j]
          # Choose one mother street --------------------
          match_counter = jj = 1
          streetInd = vector(mode = "list", length = 77)
          for (w in 1:77) {streetInd[[w]] = c(-1) }
          w50 = rep(NA, j)
          close_ind = order(dist_temp)
          while(match_counter <= j) {
            temp = combinedMatchingSetupFix2[close_ind[jj], ]
            if(!(temp$indigo %in% streetInd[[temp$precinct]])) {
              w50[match_counter] = close_ind[jj]
              match_counter = match_counter + 1
              streetInd[[temp$precinct]] = append(streetInd[[temp$precinct]], temp$indigo)
            }
            jj = jj + 1
          }
          # --------------------------------------------

          null_dist = t_stat_area[w50]
          pval[ii] = mean(null_dist > stat_temp)

          ## Area -------------------------------------------------------------
          # streets_temp = sim_orig$streets1[ii] + sim_orig$streets2[ii]
          # ratio_temp = max(sim_orig$streets1[ii] / sim_orig$streets2[ii],
          #                  sim_orig$streets2[ii] / sim_orig$streets1[ii])
          # stat_temp = t_stat_streets_orig[ii]

          # dist_temp = sqrt(((streets_temp - (combinedMatchingSetupFix2$streets1 + combinedMatchingSetupFix2$streets2))^2/v1) +
          #                    ((ratio_temp - combinedMatchingSetupFix2$ratioStreet)^2 / v2))


          # # w50 = order(dist_temp)[1:j]
          # # Choose one mother street --------------------
          # match_counter = jj = 1
          # streetInd = vector(mode = "list", length = 77)
          # for (w in 1:77) {streetInd[[w]] = c(-1) }
          # w50 = rep(NA, j)
          # close_ind = order(dist_temp)
          # while(match_counter <= j) {
          #   temp = combinedMatchingSetupFix2[close_ind[jj], ]
          #   if(!(temp$indigo %in% streetInd[[temp$precinct]])) {
          #     w50[match_counter] = close_ind[jj]
          #     match_counter = match_counter + 1
          #     streetInd[[temp$precinct]] = append(streetInd[[temp$precinct]], temp$indigo)
          #   }
          #   jj = jj + 1
          # }
          # # --------------------------------------------

          # null_dist = t_stat_streets[w50]
          # pval[ii] = mean(null_dist > stat_temp)
        }
      }

      perc_pval = mean(pval < 0.05, na.rm=TRUE)
      perc_pval_match$perc_pval_less_05[row_num] = perc_pval
      p_val_df[row_num, ] = pval
      row_num = row_num + 1
    }
# }

save(p_val_df, file = paste0("../Output_tree/combination/p_val_df", k, ".dat"))
save(perc_pval_match, file = paste0("../Output_tree/combination/perc_pval_match", k, ".dat"))

# ---------------------------------------------------------------
# ------- Plotting everything
# ---------------------------------------------------------------

# load("../Output/sim_orig/p_vals_match_rel/perc_pval_match_1.dat")
# final_plot = perc_pval_match
# plot_mat = perc_pval_match
#
# for (i in 2:100) {
#     load(paste0("../Output/sim_orig/p_vals_match_rel/perc_pval_match_", i, ".dat"))
#     for(j in 1:3) {
#         for(k in 2:13) {
#             final_plot[[j]][[k]] = rbind(final_plot[[j]][[k]], perc_pval_match[[j]][[k]])
#             plot_mat[[j]][[k]] = cbind(plot_mat[[j]][[k]], perc_pval_match[[j]][[k]]$perc_pval_less_05)
#         }
#     }
# }
#
# folder_type = c("HotSpot", "Uniform", "Random")
#
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
