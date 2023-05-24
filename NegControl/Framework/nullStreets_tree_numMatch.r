library(sp); library(sf); 
library(rgeos); library(raster)
library(tidyverse); library(gridExtra)

match_count <- seq(20, 1000, by = 20)
load("../Data/indexList_MAIN.RData")

set.seed(10)

## --------------------------- Plot --------------------------------------------

for (k in 2:13) {

    print(k)

    load(paste0("../Output_tree/nullGridInfo/combinedMatchingSetup", k, ".dat"))

    load(paste0("../Output_tree/origGridInfo/sim_orig_", k,".dat"))

    ## Now remove data points where these ratios are much different
    # wRatioOk = which(combinedMatchingSetupFix$DATA$ratioArea / combinedMatchingSetupFix$DATA$ratioStreet < 1.4 &
    #                    combinedMatchingSetupFix$DATA$ratioArea / combinedMatchingSetupFix$DATA$ratioStreet > 1/1.4)
    wMax_a = max(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))
    wMin_a = min(na.omit(sim_orig$DATA$area1 / sim_orig$DATA$area2))
    
    wMax_s = max(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))
    wMin_s = min(na.omit(sim_orig$DATA$streets1 / sim_orig$DATA$streets2))
    
    wRatioOk = which((combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) > wMin_a &
                         (combinedMatchingSetupFix$DATA$area1 / combinedMatchingSetupFix$DATA$area2) < wMax_a &
                         (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) > wMin_s &
                         (combinedMatchingSetupFix$DATA$streets1 / combinedMatchingSetupFix$DATA$streets2) < wMax_s)

    combinedMatchingSetupFix2 = combinedMatchingSetupFix$DATA[wRatioOk,]
    # v1 = sd(combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2, na.rm=TRUE)^2
    # v2 = sd(combinedMatchingSetupFix2$ratioArea, na.rm=TRUE)^2
    
    v1 = sd(combinedMatchingSetupFix2$streets1 + combinedMatchingSetupFix2$streets2, na.rm=TRUE)^2
    v2 = sd(combinedMatchingSetupFix2$ratioStreet, na.rm=TRUE)^2

    row_num = 1
    perc_pval_match = data.frame("num_match" = match_count,
                                 "perc_pval_less_05" = rep(NA, length(match_count)))
    p_val_df = matrix(nrow = length(match_count), ncol = nrow(sim_orig$DATA))
    
    # t_stat_area = abs(combinedMatchingSetupFix2$count1 / combinedMatchingSetupFix2$area1 - combinedMatchingSetupFix2$count2 / combinedMatchingSetupFix2$area2)
    # t_stat_area_orig = abs(sim_orig$DATA$count1 / sim_orig$DATA$area1 - sim_orig$DATA$count2 / sim_orig$DATA$area2)
    
    t_stat_streets = abs(combinedMatchingSetupFix2$count1 - combinedMatchingSetupFix2$count2)
    t_stat_streets_orig = abs(sim_orig$DATA$count1 - sim_orig$DATA$count2)

    sim_orig = sim_orig$DATA
    
    for(j in match_count) {
      print(j)
      pval = rep(NA, nrow(sim_orig))

      for (ii in 1 : nrow(sim_orig)) {
        if (ii %in% indexList_MAIN) {
          ## find matches
          # ## Area -------------------------------------------------------------
          # area_temp = sim_orig$area1[ii] + sim_orig$area2[ii]
          # ratio_temp = max(sim_orig$area1[ii] / sim_orig$area2[ii],
          #                  sim_orig$area2[ii] / sim_orig$area1[ii])
          # stat_temp = t_stat_area_orig[ii]
          # 
          # dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2))^2/v1) +
          #                    ((ratio_temp - combinedMatchingSetupFix2$ratioArea)^2 / v2))
          # 
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
          # 
          # null_dist = t_stat_area[w50]
          # pval[ii] = mean(null_dist > stat_temp)

          ## Street ------------------------------------------------------------
          streets_temp = sim_orig$streets1[ii] + sim_orig$streets2[ii]
          ratio_temp = max(sim_orig$streets1[ii] / sim_orig$streets2[ii],
                           sim_orig$streets2[ii] / sim_orig$streets1[ii])
          stat_temp = t_stat_streets_orig[ii]

          dist_temp = sqrt(((streets_temp - (combinedMatchingSetupFix2$streets1 + combinedMatchingSetupFix2$streets2))^2/v1) +
                             ((ratio_temp - combinedMatchingSetupFix2$ratioStreet)^2 / v2))


          w50 = order(dist_temp)[1:j]
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
          # 
          # test = density(null_dist, bw = "ucv")
          # xx = test$x
          # yy = test$y
          # dx <- xx[2L] - xx[1L]
          # C <- sum(yy) * dx
          # 
          # p.unscaled <- sum(yy[xx > stat_temp]) * dx
          # p.scaled <- p.unscaled / C
          # 
          # pval[ii] = p.scaled
          null_dist = t_stat_streets[w50]
          pval[ii] = mean(null_dist > stat_temp)
        }
      }

          perc_pval = mean(pval < 0.05, na.rm=TRUE)
          perc_pval_match$perc_pval_less_05[row_num] = perc_pval
          p_val_df[row_num, ] = pval
          row_num = row_num + 1
    }

    # save(p_val_df, file = paste0("../Output_tree/combination/p_val_df", k, ".dat"))
    # save(perc_pval_match, file = paste0("../Output_tree/combination/perc_pval_match", k, ".dat"))
        
    save(p_val_df, file = paste0("../Output_tree/combination/p_val_df_street", k, ".dat"))
    save(perc_pval_match, file = paste0("../Output_tree/combination/perc_pval_match_street", k, ".dat"))
}



# Number of matches changing -----------------------------------------------------
# load('../Output_tree/p_vals_match_rel/perc_pval_match_1_BIG_MATCH.dat')
p = vector(mode = 'list', length = 12)
for(i in 2:13) {
    load(paste0('../Output_tree/combination/perc_pval_match_street', i, '.dat'))
    pval = perc_pval_match[1:50,]
    p[[i-1]] = ggplot(pval, aes( y=perc_pval_less_05, x=num_match)) + 
        geom_point(color = "red", size = 3) +
        geom_smooth(method = "loess", formula = y ~ x) +
        ggtitle(paste0("Matching's Effect on Type I Error (",i,"00 ft)")) +
        xlab("Number of Resampled Streets") + 
        ylab("Type I Error") +
        ylim(0,max(pval$perc_pval_less_05)) + 
        scale_x_continuous(breaks = pretty(pval$num_match, n = 10)) +
        geom_hline(yintercept=0.05, linetype="dashed", 
                   color = "black", size = 1.5) +
        theme(text = element_text(size=15))
    
}
pdf("numMatch_street_kern.pdf", onefile = T)
grid.arrange(p[[1]], p[[2]], ncol = 1, nrow = 2)
grid.arrange(p[[3]], p[[4]], ncol = 1, nrow = 2)
grid.arrange(p[[5]], p[[6]], ncol = 1, nrow = 2)
grid.arrange(p[[7]], p[[8]], ncol = 1, nrow = 2)
grid.arrange(p[[9]], p[[10]], ncol = 1, nrow = 2)
grid.arrange(p[[11]], p[[12]], ncol = 1, nrow = 2)
dev.off()

# j = 6
# load('../Output_tree/combination/perc_pval_match.dat')
# pval = perc_pval_match[[j]][1:100,]
# trees_numMatch = ggplot(pval, aes( y=perc_pval_less_05, x=num_match)) + 
#     geom_point(color = "red", size = 1) +
#     geom_smooth(method = "loess", formula = y ~ x) +
#     ggtitle(paste0("Matching's effect on type I error (",j,"00 ft)")) +
#     xlab("Number of resampled streets") + 
#     ylab("Type I error") +
#     ylim(0,max(pval$perc_pval_less_05)) + 
#     scale_x_continuous(breaks = pretty(pval$num_match, n = 10)) +
#     geom_hline(yintercept=0.05, linetype="dashed", 
#                color = "black", size = 1) +
#     theme(text = element_text(size=8))
# ggsave(filename = "Plots/trees_numMatch.png", plot = trees_numMatch, width = 1000, height = 500, units = "px")
