set.seed(2023)

match_count <- 560
load("../Data/indexList_MAIN.RData")

perc_pval_match = vector(mode = "list", length = 13)
p_val_df <- vector(mode = "list", length = 13)

for (k in 2:13) {

  print(k)
  
  load(paste0('../Output/nullGridInfo/combinedMatchingSetup', k, ".dat"))
  load(paste0('../Output/origGridInfo/sim_orig_', k, '.dat'))

  ## Now remove data points where these ratios are much different
  area_ratio = c(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))
  area_ratio[area_ratio < 1] = 1 / area_ratio[area_ratio < 1]
  wMax_a = max(area_ratio)
  wMin_a = min(area_ratio)
  
  street_ratio = c(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))
  street_ratio[street_ratio < 1] = 1 / street_ratio[street_ratio < 1]
  wMax_s = max(street_ratio)
  wMin_s = min(street_ratio)
  
  wRatioOk = which(combinedMatchingSetupFix$DATA$ratioArea > wMin_a &
                       combinedMatchingSetupFix$DATA$ratioArea < wMax_a & 
                       combinedMatchingSetupFix$DATA$ratioStreet > wMin_s &
                       combinedMatchingSetupFix$DATA$ratioStreet < wMax_s)
  
  combinedMatchingSetupFix2 = combinedMatchingSetupFix$DATA[wRatioOk,]
  
  v1 = sd(combinedMatchingSetupFix2$n_off_1 + combinedMatchingSetupFix2$n_off_2, na.rm=TRUE)^2
  non_zeros = which(combinedMatchingSetupFix2$n_off_1 != 0 & combinedMatchingSetupFix2$n_off_2 != 0)
  rat_off = rep(0, nrow(combinedMatchingSetupFix2))
  rat_off[non_zeros] = combinedMatchingSetupFix2$n_off_1[non_zeros] / combinedMatchingSetupFix2$n_off_2[non_zeros]
  rat_off[rat_off < 1 & rat_off != 0] = 1 / rat_off[rat_off < 1 & rat_off != 0]
  v2 = sd(rat_off, na.rm=TRUE)^2
  
  t_stat = abs(combinedMatchingSetupFix2$n_arr_1 - combinedMatchingSetupFix2$n_arr_2)
  t_stat_orig = abs(sim_orig$DATA$n_arr_1_prec - sim_orig$DATA$n_arr_2)
  
  row_num = 1
  perc_pval_match[[k]] = data.frame("num_match" = match_count,
                                    "perc_pval_less_05" = rep(NA, length(match_count)))
  p_val_df[[k]] = matrix(nrow = length(match_count), ncol = nrow(sim_orig$DATA))

  for(j in match_count) {
    print(paste0("Match Num: ", j))
    pval = rep(NA, nrow(sim_orig$DATA))

    for (ii in indexList_MAIN) {
      ## find matches
      off_temp = sim_orig$DATA$n_off_1_prec[ii] + sim_orig$DATA$n_off_2_prec[ii]
      if(sim_orig$DATA$n_off_1_prec[ii] == 0 | sim_orig$DATA$n_off_2_prec[ii] == 0) {
          ratio_temp = 0
      } else {
          ratio_temp = max(sim_orig$DATA$n_off_1_prec[ii] / sim_orig$DATA$n_off_2_prec[ii],
                           sim_orig$DATA$n_off_2_prec[ii] / sim_orig$DATA$n_off_1_prec[ii])  
      }
      
      stat_temp = t_stat_orig[ii]
      
      # print(paste0(ii, " -- t_stat: ", stat_temp, ", Ratio: ", ratio_temp))

      dist_temp = sqrt(((off_temp - (combinedMatchingSetupFix2$n_off_1 + combinedMatchingSetupFix2$n_off_2))^2/v1) +
                         ((ratio_temp - rat_off)^2 / v2))

      w50 = order(dist_temp)[1:j]
      
      null_dist = t_stat[w50]
      pval[ii] = mean(null_dist > stat_temp)
    }

    perc_pval = mean(pval < 0.05, na.rm=TRUE)
    perc_pval_match[[k]]$perc_pval_less_05[row_num] = perc_pval
    p_val_df[[k]][row_num, ] = pval
    row_num = row_num + 1
  }
}

save(p_val_df, file = paste0("../Output/p_vals_match_rel/p_val_df_new_stat_FINAL.dat"))
save(perc_pval_match, file = paste0("../Output/p_vals_match_rel/perc_pval_match_new_stat_FINAL.dat"))

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