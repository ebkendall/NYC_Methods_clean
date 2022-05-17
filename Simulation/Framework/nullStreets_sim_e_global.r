# Step Outline -----------------------------------------------------------------
# Step 1: save all the matches in the global_null list for each surface
# Step 2: iterate through and grab the max
# (next steps are in ...h_globalPlots.r)
# Step 3: get a p-value for each surface and each iterate for each buffer width
# Step 4: look at the distribution of p-values across all 100 iterates
# ------------------------------------------------------------------------------

load("../Data/indexList_MAIN.RData")

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


save_type = c("HotSpot/", "Uniform/", "Random/", "Correlated/")

n_matches = 150
trialNum = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 1-1000
set.seed(trialNum)

`%notin%` <- Negate(`%in%`)

file_names <- c(paste0("../Data/Surfaces/gridPointValues_hotspot_", trialNum,".rda"),
                paste0("../Data/Surfaces/gridPointValues_uniform_", trialNum,".rda"),
                paste0("../Data/Surfaces/gridPointValues_cov_r_", trialNum,".rda"),
                paste0("../Data/Surfaces/gridPointValues_cov_c_", trialNum,".rda"))
tau = 0.5  
# Step 1 -----------------------------------------------------------------------

for (s_name in 1:4) {
    # Run this for the different surface types
    global_null = vector(mode = "list", length = 13)

    load(file_names[s_name])
    gridPointValues = NULL

    if (s_name == 1) {gridPointValues = gridPointValues_hotspot * tau}
    else if (s_name == 2) {gridPointValues = gridPointValues_uniform}
    else if (s_name == 3) {gridPointValues = gridPointValues_cov_r}
    else if (s_name == 4) {gridPointValues = gridPointValues_cov_c_big}
    else {print("Incorrect input to start")}

    for (k in 2:13) {
        # We need a global test for each buffer width
        print(paste0(s_name, " ", k))
        load(paste0("../Output_noWater/nullGridInfo/combinedMatchingSetup", k, ".dat"))
        load(paste0('../Output_noWater/origGridInfo/sim_orig_', k,".dat"))

        global_null[[k]] = matrix(nrow = max(indexList_MAIN), ncol = n_matches)
         
        # wMax_a = max(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))
        # wMin_a = min(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))
         
        # wMax_s = max(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))
        # wMin_s = min(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))
         
        # wMatchOk = which((combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) > wMin_a &
        #                    (combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) < wMax_a &
        #                    (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) > wMin_s &
        #                    (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) < wMax_s)
         
        # combinedMatchingSetupFix2 = combinedMatchingSetupFix[wMatchOk,]
        combinedMatchingSetupFix2 = combinedMatchingSetupFix

        # =====================================================================

        v1 = sd(combinedMatchingSetupFix2$DATA$area1 + combinedMatchingSetupFix2$DATA$area2, na.rm=TRUE)^2
        v2 = sd(combinedMatchingSetupFix2$DATA$ratioArea, na.rm=TRUE)^2

        for(ii in indexList_MAIN) {
            area_temp = sim_orig$DATA$area1[ii] + sim_orig$DATA$area2[ii]
            ratio_temp = max(sim_orig$DATA$area1[ii] / sim_orig$DATA$area2[ii],
                            sim_orig$DATA$area2[ii] / sim_orig$DATA$area1[ii])

            # MAKE FUNCTION
            orig_temp = test_stats_orig(gridPointValues, sim_orig, ii)
            stat_temp = orig_temp$tStat_area

            dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$DATA$area1 + combinedMatchingSetupFix2$DATA$area2))^2/v1) +
                                ((ratio_temp - combinedMatchingSetupFix2$DATA$ratioArea)^2 / v2))

            # w50 = order(dist_temp)[1:n_matches]

            # Choose one mother street --------------------
            match_count = jj = 1
            streetInd = vector(mode = "list", length = 77) 
            for (w in 1:77) {streetInd[[w]] = c(-1) }
            w50 = rep(NA, n_matches)
            close_ind = order(dist_temp)
            while(match_count <= n_matches) {
                temp = combinedMatchingSetupFix2$DATA[close_ind[jj], ]
                if(temp$indigo %notin% streetInd[[temp$precinct]]) {
                  w50[match_count] = close_ind[jj]
                  match_count = match_count + 1
                  streetInd[[temp$precinct]] = append(streetInd[[temp$precinct]], temp$indigo)
                }
                jj = jj + 1
            }
            # --------------------------------------------

            tStats_temp = test_stats(gridPointValues, combinedMatchingSetupFix2, w50)
            null_dist = tStats_temp$tStat_area

            global_null[[k]][ii,] = null_dist
        }

    }

    save(global_null, file = paste0("../Output_noWater/sim_results/Global/", save_type[s_name],
                                    "global_null_", trialNum, ".dat"))
}

# Step 2 -----------------------------------------------------------------------

for (s_name in 1:4) {

    load(paste0("../Output_noWater/sim_results/Global/", save_type[s_name], "global_null_", trialNum, ".dat"))
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
            global_t_stat[[k]][rep, 2] = 
                                temp_loc[which(temp_max == max(temp_max, na.rm = T))[1]]
        }

    }

    save(global_t_stat, file = paste0("../Output_noWater/sim_results/Global/", save_type[s_name],
                                    "global_t_stat_", trialNum, ".dat"))
}

