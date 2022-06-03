# Step Outline -----------------------------------------------------------------
# Step 1: save all the matches in the global_null list for each surface
# Step 2: iterate through and grab the max
# (next steps are in ...h_globalPlots.r)
# Step 3: get a p-value for each surface and each iterate for each buffer width
# Step 4: look at the distribution of p-values across all 100 iterates
# ------------------------------------------------------------------------------

load("../Data/indexList_MAIN.RData")

n_matches = 150
trialNum = 1
set.seed(trialNum)

`%notin%` <- Negate(`%in%`)

# Step 1 -----------------------------------------------------------------------

# Run this for the different surface types
global_null = vector(mode = "list", length = 13)


for (k in 2:13) {
    global_null[[k]] = matrix(nrow = 164, ncol = n_matches)
     
    print(k)
  
    load(paste0('../Output/nullGridInfo/combinedMatchingSetup', k, ".dat"))
    load(paste0('../Output/origGridInfo/sim_orig_', k, '.dat'))

    wMax_a = max(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))
    wMin_a = min(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))

    wMax_s = max(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))
    wMin_s = min(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))

    wMatchOk1 = which((combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) > wMin_a &
                        (combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) < wMax_a &
                        (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) > wMin_s &
                        (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) < wMax_s)

    wMatchOk2 = which(!is.na(combinedMatchingSetupFix$DATA$t_stat_pval))
    wMatchOk = intersect(wMatchOk1, wMatchOk2)
    
    combinedMatchingSetupFix2 = combinedMatchingSetupFix
    combinedMatchingSetupFix2$DATA = combinedMatchingSetupFix2$DATA[wMatchOk,]
    combinedMatchingSetupFix2$ARR_IND_1 = combinedMatchingSetupFix2$ARR_IND_1[wMatchOk]
    combinedMatchingSetupFix2$ARR_IND_2 = combinedMatchingSetupFix2$ARR_IND_2[wMatchOk]
    combinedMatchingSetupFix2$OFF_IND_1 = combinedMatchingSetupFix2$OFF_IND_1[wMatchOk]
    combinedMatchingSetupFix2$OFF_IND_2 = combinedMatchingSetupFix2$OFF_IND_2[wMatchOk]
    
    # =====================================================================

    tot_lengths = data.frame("arr1" = sapply(combinedMatchingSetupFix2$ARR_IND_1, length),
                           "arr2" = sapply(combinedMatchingSetupFix2$ARR_IND_2, length),
                           "off1" = sapply(combinedMatchingSetupFix2$OFF_IND_1, length),
                           "off2" = sapply(combinedMatchingSetupFix2$OFF_IND_2, length))
    tot_lengths[which(tot_lengths$off1 == 0 | tot_lengths$off2 == 0), ] = NA
    
    v1 = sd(tot_lengths$off1 + tot_lengths$off2, na.rm=TRUE)^2
    rat_off = tot_lengths$off1 / tot_lengths$off2
    rat_off[which(rat_off < 1)] = 1 / rat_off[which(rat_off < 1)]
    v2 = sd(rat_off, na.rm=TRUE)^2  

    # Need to compensate for 0s
    off_num = data.frame("off1" = sapply(sim_orig$OFF_IND_1, length),
                        "off2" = sapply(sim_orig$OFF_IND_2, length))
    off_num[which(off_num$off1 == 0 | off_num$off2 == 0), ] = 
        off_num[which(off_num$off1 == 0 | off_num$off2 == 0), ] + 1
    

    for(ii in indexList_MAIN) {
        off_temp = off_num$off1[ii] + off_num$off2[ii]
        ratio_temp = max(off_num$off1[ii] / off_num$off2[ii],
                        off_num$off2[ii] / off_num$off1[ii])

        dist_temp = sqrt(((off_temp - (tot_lengths$off1 + tot_lengths$off2))^2/v1) +
                            ((ratio_temp - rat_off)^2 / v2))
        
        stat_temp = sim_orig$DATA$t_stat_pval[ii]

        # w50 = order(dist_temp)[1:n_matches]

        # Choose one mother street --------------------
        match_counter = jj = 1
        streetInd = vector(mode = "list", length = 77)
        for (w in 1:77) {streetInd[[w]] = c(-1) }
        w50 = rep(NA, n_matches)
        close_ind = order(dist_temp)
        while(match_counter <= n_matches) {
          temp = combinedMatchingSetupFix2$DATA[close_ind[jj], ]
          if(!(temp$indigo %in% streetInd[[temp$precinct]])) {
            w50[match_counter] = close_ind[jj]
            match_counter = match_counter + 1
            streetInd[[temp$precinct]] = append(streetInd[[temp$precinct]], temp$indigo)
          }
          jj = jj + 1
        }
        # --------------------------------------------

        null_dist = combinedMatchingSetupFix2$DATA$t_stat_pval[w50]

        global_null[[k]][ii,] = null_dist
    }

}

save(global_null, file = paste0("../Output/Global/global_null_", trialNum, ".dat"))

# Step 2 -----------------------------------------------------------------------

load(paste0("../Output/Global/global_null_", trialNum, ".dat"))
global_t_stat <- vector(mode = "list", length = 13)

for(k in 2:13) {

    print(k)
    global_t_stat[[k]] = data.frame("max_t_stat" = rep(NA, n_matches),
                                    "max_loc" = rep(NA, n_matches))
    
    for (rep in 1:n_matches) {
    # This is the repetition to get the null distribution
        temp_loc = temp_max = c()
        myInd = 1
        for(ii in indexList_MAIN) {
            rand_ind = sample(c(1:n_matches), 1)

            temp_loc[myInd] = ii
            temp_max[myInd] = global_null[[k]][ii, rand_ind]
            myInd = myInd + 1
        }
        print(paste0(max(temp_max, na.rm = T), " ", which(temp_max == max(temp_max, na.rm = T))))
        global_t_stat[[k]][rep, 1] = max(temp_max, na.rm = T)
        global_t_stat[[k]][rep, 2] = temp_loc[which.max(temp_max)]
    }

}

save(global_t_stat, file = paste0("../Output/Global/global_t_stat_", trialNum, ".dat"))

