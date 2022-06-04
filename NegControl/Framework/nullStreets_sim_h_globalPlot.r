load("../Data/indexList_MAIN.RData")

test_stats_orig <- function(gridPointValues, sim_orig) {
    
    t_stat_df = data.frame("tStat_area" = c(-1),
                           "naive_pval" = c(-1),
                           "tStat_strt" = c(-1))

    rowInd = 1

    for (ii in indexList_MAIN) {
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
        rowInd = rowInd + 1
    }

    return(t_stat_df)
}

n_matches = 150

save_type = c("HotSpot/", "Uniform/", "Random/", "Correlated/") 

# Step 3 -----------------------------------------------------------------------

p_val_df = vector(mode = "list", length = 13)

for(i in 2:13) {p_val_df[[i]] = data.frame("HotSpot"     = rep(NA,1000),
                                           "Uniform"     = rep(NA,1000),
                                           "Random"      = rep(NA,1000),
                                           "Correlated"  = rep(NA,1000))}

whichMaxInfo = vector(mode = "list", length = 4)
whichMaxInfo[[1]] = whichMaxInfo[[2]] = whichMaxInfo[[3]] = whichMaxInfo[[4]] = vector(mode = 'list', length = 13)

for (trialNum in 1:1000) {
    set.seed(trialNum)
    print(trialNum)

    file_names <- c(paste0("../Data/Surfaces/gridPointValues_hotspot_", trialNum,".rda"),
                paste0("../Data/Surfaces/gridPointValues_uniform_", trialNum,".rda"),
                paste0("../Data/Surfaces/gridPointValues_cov_r_", trialNum,".rda"),
                paste0("../Data/Surfaces/gridPointValues_cov_c_", trialNum,".rda"))

    tau = 0.5  
# pdf(paste0("../Output_noWater/Plots/debuggingGlobal.pdf"))
# par(mfrow=c(2,2))
    for (s_name in 1:4) {
        
        load(paste0('../Output_noWater/sim_results/Global/', save_type[s_name], 
                    "global_t_stat_", trialNum,".dat"))

        load(file_names[s_name])
        gridPointValues = NULL

        if (s_name == 1) {gridPointValues = gridPointValues_hotspot * tau}
        else if (s_name == 2) {gridPointValues = gridPointValues_uniform}
        else if (s_name == 3) {gridPointValues = gridPointValues_cov_r}
        else if (s_name == 4) {gridPointValues = gridPointValues_cov_c_big}
        else {print("Incorrect input to start")}
        
        for(k in 2:13) {
            
            load(paste0('../Output_noWater/origGridInfo/sim_orig_', k,".dat"))

            t_stat_list = test_stats_orig(gridPointValues, sim_orig)

            t_stat = max(t_stat_list$tStat_area, na.rm = T)
            w_max = which.max(na.omit(t_stat_list$tStat_area))
            # print(paste0(k, ": ", t_stat))

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

            p_val_not = mean(global_t_stat[[k]]$max_t_stat > t_stat)
            p_val_df[[k]][trialNum, s_name] = p.scaled
            
            # hist(global_t_stat[[k]]$max_t_stat, breaks = round(sqrt(150)), 
            #      main = paste0(save_type[s_name], ": ", k),
            #      xlab = paste0("Kernel: ", p.scaled, ", NOT: ", p_val_not))
            # abline(v = t_stat, col = "red")
        }

    }
# dev.off()
}

save(p_val_df, file = paste0("../Output_noWater/Plots/global_p_values_",
                             n_matches, ".rda"))

pdf(paste0("../Output_noWater/Plots/global_", n_matches, "_new_total.pdf"))
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
