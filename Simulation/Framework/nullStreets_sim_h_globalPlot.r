load("../Data/indexList_MAIN.RData")

# args = commandArgs(TRUE)
# n_matches = as.numeric(args[1])
n_matches = 150

save_type = c("HotSpot/", "Uniform/", "Random/", "Correlated/") 

# Step 3 -----------------------------------------------------------------------

p_val_df = vector(mode = "list", length = 13)

for(i in 2:13) {p_val_df[[i]] = data.frame("HotSpot"     = rep(NA,200),
                                           "Uniform"     = rep(NA,200),
                                           "Random"      = rep(NA,200),
                                           "Correlated"  = rep(NA,200))}

whichMaxInfo = vector(mode = "list", length = 4)
whichMaxInfo[[1]] = whichMaxInfo[[2]] = whichMaxInfo[[3]] = whichMaxInfo[[4]] = vector(mode = 'list', length = 13)

for (trialNum in 1:100) {
    print(trialNum)

    for (s_name in 1:4) {
        
        load(paste0('../Output_noWater/sim_orig/', save_type[s_name], "sim_master_", trialNum,".dat"))
        load(paste0('../Output_noWater/Global/', save_type[s_name], "global_t_stat_", trialNum,".dat"))
        
        for(k in 2:13) {
            
            t_stat = max(sim_master[[k]]$tStats_area, na.rm = T)
            w_max = which.max(na.omit(sim_master[[k]]$tStats_area))

            whichMaxInfo[[s_name]][[k]] = rbind(whichMaxInfo[[s_name]][[k]], 
                                          c(w_max, t_stat))
            
            test = density(global_t_stat[[k]]$max_t_stat, bw = "ucv")
            xx = test$x
            yy = test$y
            dx <- xx[2L] - xx[1L]
            C <- sum(yy) * dx

            p.unscaled <- sum(yy[xx >= t_stat]) * dx
            p.scaled <- p.unscaled / C
            
            # pval[ii] = p.scaled

            # p_val = mean(global_t_stat[[k]]$max_t_stat > t_stat)
            p_val_df[[k]][trialNum, s_name] = p.scaled
        }

    }
}

pdf(paste0("../Output_noWater/Plots/pValGlobal_max_", n_matches, "_test.pdf"))
par(mfrow=c(2,2))
for (i in 2:13) {
  for(k in 1:4) {
    print(paste0(i, " ", k))
    pval = p_val_df[[i]][,k]
    hist(pval, breaks = sqrt(length(pval)), main = paste0(save_type[k], ": pVal for B", i*100),
         xlab = paste0("Perc. < 0.05 is ",  round(mean(pval < 0.05, na.rm=TRUE), 4)),
         xlim=c(0,1))
  }
}

for (i in 2:13) {
  for(k in 1:4) {
    plot(table(indexList_MAIN[whichMaxInfo[[k]][[i]][,1]]), 
    ylab = "Freq", main = paste0("Max Obs TStat index: ", 
    names(which.max(table(indexList_MAIN[whichMaxInfo[[k]][[i]][,1]]))),
    "\n B:", i*100))
  }
}
dev.off()
