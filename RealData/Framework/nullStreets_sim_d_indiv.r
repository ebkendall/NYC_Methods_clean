set.seed(2023)

match_count <- 560
load("../Data/indexList_MAIN.RData")

perc_pval_match = vector(mode = "list", length = 13)
p_val_df <- vector(mode = "list", length = 13)
debug_spatial_ind = vector(mode = 'list', length = 13)

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
  
  t_stat = abs(combinedMatchingSetupFix2$n_arr_1 / combinedMatchingSetupFix2$streets1
               - combinedMatchingSetupFix2$n_arr_2 / combinedMatchingSetupFix2$streets2)
  t_stat_orig = abs(sim_orig$DATA$n_arr_1_prec / sim_orig$DATA$streets1
                    - sim_orig$DATA$n_arr_2_prec / sim_orig$DATA$streets2)
  # t_stat = abs(combinedMatchingSetupFix2$n_arr_1 - combinedMatchingSetupFix2$n_arr_2)
  # t_stat_orig = abs(sim_orig$DATA$n_arr_1_prec - sim_orig$DATA$n_arr_2_prec)
  
  row_num = 1
  perc_pval_match[[k]] = data.frame("num_match" = match_count,
                                    "perc_pval_less_05" = rep(NA, length(match_count)))
  p_val_df[[k]] = matrix(nrow = length(match_count), ncol = nrow(sim_orig$DATA))
  debug_spatial_ind[[k]] = matrix(nrow = match_count, ncol = 164)

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

      dist_temp = sqrt(((off_temp - (combinedMatchingSetupFix2$n_off_1 + combinedMatchingSetupFix2$n_off_2))^2/v1) +
                         ((ratio_temp - rat_off)^2 / v2))

      w50 = order(dist_temp)[1:j]

      # Store the precinct and indigo to see how spatial variable we go
      debug_spatial_ind[[k]][,ii] = t_stat[w50]
      
      null_dist = t_stat[w50]
      pval[ii] = mean(null_dist > stat_temp)
      
      if(sum(is.nan(null_dist)) > 0) print(paste0(j, ", ", ii))
    }

    perc_pval = mean(pval < 0.05, na.rm=TRUE)
    perc_pval_match[[k]]$perc_pval_less_05[row_num] = perc_pval
    p_val_df[[k]][row_num, ] = pval
    row_num = row_num + 1
  }
}

save(p_val_df, file = "../Output/p_vals_match_rel/p_val_df_new_stat_FINAL.dat")
save(perc_pval_match, file = "../Output/p_vals_match_rel/perc_pval_match_new_stat_FINAL.dat")
save(debug_spatial_ind, file = "../Output/p_vals_match_rel/debug_spatial_ind.dat")

# dist_matrix = matrix(nrow = 13, ncol = 164)
# for(i in 2:length(debug_spatial_ind)) {
#     print(i)
#     temp = colMeans(debug_spatial_ind[[i]])
#     dist_matrix[i, ]  = temp
# }
# 
# dist_matrix_old = matrix(nrow = 13, ncol = 164)
# for(i in 2:length(debug_spatial_ind_old)) {
#     print(i)
#     temp = colMeans(debug_spatial_ind_old[[i]])
#     dist_matrix_old[i, ]  = temp
# }
