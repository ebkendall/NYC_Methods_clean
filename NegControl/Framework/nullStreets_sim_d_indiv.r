library("sp")
library("sf")
library("rgeos")
library("raster")

match_count <- c(250,150)

trialNum = 1
set.seed(trialNum)

load("../Data/indexList_MAIN.RData")


perc_pval_match = vector(mode = "list", length = 13)

p_val_df <- vector(mode = "list", length = 13)

for (k in 2:13) {

  print(k)
  
  load(paste0('../Output_tree/nullGridInfo/combinedMatchingSetup', k, ".dat"))
  load(paste0('../Output_tree/origGridInfo/sim_orig_', k, '.dat'))

  wMax_a = max(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))
  wMin_a = min(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))

  wMax_s = max(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))
  wMin_s = min(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))

  wMatchOk1 = which((combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) > wMin_a &
                     (combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) < wMax_a &
                     (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) > wMin_s &
                     (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) < wMax_s)
  
  wMatchOk2 = which(!is.na(combinedMatchingSetupFix$DATA$tStat))
  wMatchOk = intersect(wMatchOk1, wMatchOk2)
  # wMatchOk = which(!is.na(combinedMatchingSetupFix$DATA$tStat))
  
  combinedMatchingSetupFix2 = combinedMatchingSetupFix$DATA[wMatchOk,]
  
  v1 = sd(combinedMatchingSetupFix2$streets1 + combinedMatchingSetupFix2$streets2, na.rm=TRUE)^2
  v2 = sd(combinedMatchingSetupFix2$ratioStreet, na.rm=TRUE)^2

  row_num = 1
  perc_pval_match[[k]] = data.frame("num_match" = match_count,
                                    "perc_pval_less_05" = rep(NA, length(match_count)))
  p_val_df[[k]] = matrix(nrow = length(match_count), ncol = nrow(sim_orig$DATA))

  for(j in match_count) {
    print(paste0("Match Num: ", j))
    pval = rep(NA, nrow(sim_orig$DATA))

    for (ii in indexList_MAIN) {
      ## find matches
      area_temp = sim_orig$DATA$streets1[ii] + sim_orig$DATA$streets2[ii]
      ratio_temp = max(sim_orig$DATA$streets1[ii] / sim_orig$DATA$streets2[ii],
                       sim_orig$DATA$streets2[ii] / sim_orig$DATA$streets1[ii])
      
      dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$streets1 + combinedMatchingSetupFix2$streets2))^2/v1) +
                         ((ratio_temp - combinedMatchingSetupFix2$ratioStreet)^2 / v2))
      
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
      

      # tStats_temp = test_stats(gridPointValues, combinedMatchingSetupFix2, w50)
      null_dist = combinedMatchingSetupFix2$tStat[w50]

      # orig_temp = test_stats_orig(gridPointValues, sim_orig, ii)
      stat_temp = sim_orig$DATA$tStat[ii]

      test = density(null_dist, bw = "ucv")
      xx = test$x
      yy = test$y
      dx <- xx[2L] - xx[1L]
      C <- sum(yy) * dx
      
      p.unscaled <- sum(yy[xx >= stat_temp]) * dx
      p.scaled <- p.unscaled / C
      
      pval[ii] = p.scaled
      # pval[ii] = mean(null_dist >= stat_temp)
    }

    perc_pval = mean(pval < 0.05, na.rm=TRUE)
    perc_pval_match[[k]]$perc_pval_less_05[row_num] = perc_pval
    p_val_df[[k]][row_num, ] = pval
    row_num = row_num + 1
  }
}

save(p_val_df, file = paste0("../Output_tree/p_vals_match_rel/p_val_df_", trialNum, "_FINAL.dat"))
save(perc_pval_match, file = paste0("../Output_tree/p_vals_match_rel/perc_pval_match_", trialNum, "_FINAL.dat"))

# ---------------------------------------------------------------
# ------- Plotting everything
# ---------------------------------------------------------------
# load('../Output_tree/p_vals_match_rel/perc_pval_match_1_filt.dat')
# pdf('../Output_tree/Plots/pVal_num_match_FINAL_filt.pdf')
# par(mfrow=c(3,1))
# for (i in 2:13) {
#     pval = perc_pval_match[[i]]
#     # print(t(temp))
#     plot(pval$num_match, pval$perc_pval_less_05, main = paste0("pVal for B", i*100),
#          xaxt="none", xlab = "Perc. < 0.05 is ", ylim = c(0,0.15))
#     axis(1, seq(10,300,10), las=2)
#     abline(h=0.05, col = "red")
# }
# dev.off()

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