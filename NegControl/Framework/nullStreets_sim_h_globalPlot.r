load("../Data/indexList_MAIN.RData")

n_matches = 150

# Step 3 -----------------------------------------------------------------------

p_val_df = rep(NA, 13)

# whichMaxInfo = vector(mode = 'list', length = 13)

trialNum = 1
set.seed(trialNum)

load(paste0('../Output_tree/Global/global_t_stat_', trialNum,".dat"))

for(k in 2:13) {
  
  load(paste0('../Output_tree/origGridInfo/sim_orig_', k,".dat"))
  
  t_stat = max(sim_orig$DATA$t_stat_pval, na.rm = T)
  # w_max = which.max(na.omit(sim_orig$DATA$t_stat_pval))
  # print(paste0(k, ": ", t_stat))
  
  # whichMaxInfo[[k]] = rbind(whichMaxInfo[[k]], c(w_max, t_stat))
  
  test = density(global_t_stat[[k]]$max_t_stat, bw = "ucv")
  xx = test$x
  yy = test$y
  dx <- xx[2L] - xx[1L]
  C <- sum(yy) * dx
  
  p.unscaled <- sum(yy[xx >= t_stat]) * dx
  p.scaled <- p.unscaled / C
  
  # p_val_df[k] = mean(global_t_stat[[k]]$max_t_stat > t_stat)
  p_val_df[k] = p.scaled
  
}
print(p_val_df)
save(p_val_df, file = paste0("../Output_tree/Plots/global_p_values_",
                             n_matches, ".rda"))

# pdf(paste0("../Output_tree/Plots/global_", n_matches, "_new_total.pdf"))
# par(mfrow=c(2,2))
# for (i in 2:13) {
#   for(k in 1:4) {
#     print(paste0(i, " ", k))
#     pval = p_val_df[[i]][,k]
#     hist(pval, breaks = sqrt(length(pval)), main = paste0(save_type[k], ": pVal for B", i*100),
#          xlab = paste0("Perc. < 0.05 is ",  round(mean(pval < 0.05, na.rm=TRUE), 4)),
#          xlim=c(0,1))
#   }
# }

# for (i in 2:13) {
#   for(k in 1:4) {
#     plot(table(indexList_MAIN[whichMaxInfo[[k]][[i]][,1]]), 
#     ylab = "Freq", main = paste0("Max Obs TStat index: ", 
#     names(which.max(table(indexList_MAIN[whichMaxInfo[[k]][[i]][,1]]))),
#     "\n B:", i*100))
#   }
# }
# dev.off()
