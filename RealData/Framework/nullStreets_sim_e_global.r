# Step Outline -----------------------------------------------------------------
# Step 1: save all the matches in the global_null list for each surface
# Step 2: iterate through and grab the max
# (next steps are in ...h_globalPlots.r)
# Step 3: get a p-value for each surface and each iterate for each buffer width
# Step 4: look at the distribution of p-values across all 100 iterates
# ------------------------------------------------------------------------------

load("../Data/indexList_MAIN.RData")

n_matches = 560
set.seed(2023)

`%notin%` <- Negate(`%in%`)

# Step 1 -----------------------------------------------------------------------

# Run this for the different surface types
global_null = vector(mode = "list", length = 13)

for (k in 2:13) {
    global_null[[k]] = matrix(nrow = 164, ncol = n_matches)
     
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
    
    # =====================================================================

    v1 = sd(combinedMatchingSetupFix2$n_off_1 + combinedMatchingSetupFix2$n_off_2, na.rm=TRUE)^2
    non_zeros = which(combinedMatchingSetupFix2$n_off_1 != 0 & combinedMatchingSetupFix2$n_off_2 != 0)
    rat_off = rep(0, nrow(combinedMatchingSetupFix2))
    rat_off[non_zeros] = combinedMatchingSetupFix2$n_off_1[non_zeros] / combinedMatchingSetupFix2$n_off_2[non_zeros]
    rat_off[rat_off < 1 & rat_off != 0] = 1 / rat_off[rat_off < 1 & rat_off != 0]
    v2 = sd(rat_off, na.rm=TRUE)^2
    
    t_stat = abs(combinedMatchingSetupFix2$n_arr_1 - combinedMatchingSetupFix2$n_arr_2)

    for(ii in indexList_MAIN) {
        ## find matches
        off_temp = sim_orig$DATA$n_off_1_prec[ii] + sim_orig$DATA$n_off_2_prec[ii]
        if(sim_orig$DATA$n_off_1_prec[ii] == 0 | sim_orig$DATA$n_off_2_prec[ii] == 0) {
            ratio_temp = 0
        } else {
            ratio_temp = max(sim_orig$DATA$n_off_1_prec[ii] / sim_orig$DATA$n_off_2_prec[ii],
                             sim_orig$DATA$n_off_2_prec[ii] / sim_orig$DATA$n_off_1_prec[ii])  
        }
        
        dist_temp = sqrt(((off_temp - (combinedMatchingSetupFix2$n_off_1 + combinedMatchingSetupFix2$n_off_2))^2/v1) +
                             ((ratio_temp - rat_off)^2 / v2))

        w50 = order(dist_temp)[1:n_matches]

        null_dist = t_stat[w50]

        global_null[[k]][ii,] = null_dist
    }

}

save(global_null, file = paste0("../Output/Global/global_null_FINAL.dat"))

# Step 2 -----------------------------------------------------------------------

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
        global_t_stat[[k]][rep, 1] = max(temp_max, na.rm = T)
        global_t_stat[[k]][rep, 2] = temp_loc[which.max(temp_max)]
    }

}

save(global_t_stat, file = paste0("../Output/Global/global_t_stat_FINAL.dat"))

