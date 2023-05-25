# ALL FIGURES ARE 1000 x 800

library(tidyverse, quietly = T)
library(gridExtra)

# Figure of Naive P-val  -------------------------------------------------------
load('../Output/origGridInfo/sim_orig_5.dat')
unadjPVal500 = data.frame("p" = na.omit(sim_orig$DATA$naive_pval))
realData_naive = ggplot(unadjPVal500, aes(x=p)) + 
                          geom_histogram(color="black", fill="white", bins = sqrt(nrow(sim_orig$DATA))) +
                          xlab("P-Values") + 
                          ylab("Frequency") + 
                          ggtitle("Histogram of p-values at buffer width 500 ft") + 
                          theme(text = element_text(size=8))
ggsave(filename = "Plots/realData_naive.png", plot = realData_naive, width = 1000, height = 800, units = "px")

load('../Output/origGridInfo/sim_orig_3.dat')
unadjPValTotal = data.frame(na.omit(sim_orig$DATA$naive_pval))
for (i in 4:13) {
  load(paste0('../Output/origGridInfo/sim_orig_', i, '.dat'))
  unadjPValTotal = cbind(unadjPValTotal, data.frame(na.omit(sim_orig$DATA$naive_pval)))
}
colnames(unadjPValTotal) = c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")

percRejection = data.frame("perc" = rep(1,11), "buff" = c(3:13))
for (i in 1:11) {
  percRejection[i,1] = sum(na.omit(unadjPValTotal[,i] < 0.05)) / sum(!is.na(unadjPValTotal[,i]))
}

realData_naive_total = ggplot(percRejection, aes(y=perc, x=buff)) + 
                          geom_bar(position="dodge", stat="identity") + 
                          ggtitle("Percentage of p-values less than 0.05") +
                          xlab("Buffer Width (100x in ft)") + 
                          ylab("Percent") +
                          ylim(0, 1) +
                          scale_x_continuous(breaks = pretty(percRejection$buff, n = 11)) +
                          theme(text = element_text(size=8))
ggsave(filename = "Plots/realData_naive_total.png", plot = realData_naive_total, width = 1000, height = 800, units = "px")

# Figure of Global results -----------------------------------------------------
load("../Data/indexList_MAIN.RData")

n_matches = 560

p_val_df = rep(NA, 13)

load('../Output/Global/global_t_stat_FINAL.dat')

for(k in 2:13) {
  
    load(paste0('../Output/origGridInfo/sim_orig_', k,".dat"))
    t_stat_orig = abs(sim_orig$DATA$n_arr_1_prec - sim_orig$DATA$n_arr_2_prec)
    
    t_stat = max(t_stat_orig, na.rm = T)
  
    p_val_df[k] = mean(global_t_stat[[k]]$max_t_stat > t_stat)
  
}
print(p_val_df)
globalPvals_new_2 = data.frame("buff" = 3:13,
                               "p" = p_val_df[3:13])
realData_global_total = ggplot(globalPvals_new_2, aes(y=p, x=buff)) + 
                      geom_bar(position="dodge", stat="identity") + 
                      ggtitle("P-Values for global test statistics") +
                      xlab("Buffer Width (100x in ft)") + 
                      ylab("P-Values") +
                      geom_hline(yintercept=0.05, linetype="dashed", 
                                 color = "red", size = 0.5) +
                      theme(text = element_text(size=8)) +
                      scale_x_continuous(breaks= 2:13)

ggsave(filename = "Plots/realData_global_total.png", plot = realData_global_total, width = 1000, height = 800, units = "px")

globalEmpDist = data.frame("five" = global_t_stat[[5]]$max_t_stat,
                           "nine" = global_t_stat[[9]]$max_t_stat,
                           "thirteen" = global_t_stat[[13]]$max_t_stat)
globalObsVal = data.frame("five" = 1, "nine" = 1, "thirteen" = 1)
ind = 1
for(k in c(5,9,13)) {
    load(paste0('../Output/origGridInfo/sim_orig_', k,".dat"))
    t_stat_orig = abs(sim_orig$DATA$n_arr_1_prec - sim_orig$DATA$n_arr_2_prec)
    
    t_stat = max(t_stat_orig, na.rm = T)
  
    globalObsVal[1,ind] = t_stat
    ind=ind+1
}

p1 = ggplot(globalEmpDist, aes(x=five)) + 
  geom_histogram(color="black", fill="white", bins = sqrt(nrow(globalEmpDist))) + 
  geom_vline(data=globalObsVal, aes(xintercept=globalObsVal$five[1], color="red"), size=1) + 
  ggtitle("Distribution of global test statistic (500 ft)") +
  xlab("Test Statistic") + 
  ylab("Frequency") +
  theme(legend.position="none", text = element_text(size=6))

p2 = ggplot(globalEmpDist, aes(x=nine)) + 
  geom_histogram(color="black", fill="white", bins = sqrt(nrow(globalEmpDist))) + 
  geom_vline(data=globalObsVal, aes(xintercept=globalObsVal$nine[1], color="red"), size=1) + 
  ggtitle("Distribution of global test statistic (900 ft)") +
  xlab("Test Statistic") + 
  ylab("Frequency") +
  theme(legend.position="none", text = element_text(size=6))

p3 = ggplot(globalEmpDist, aes(x=thirteen)) + 
  geom_histogram(color="black", fill="white", bins = sqrt(nrow(globalEmpDist))) + 
  geom_vline(data=globalObsVal, aes(xintercept=globalObsVal$thirteen[1], color="red"), size=1) + 
  ggtitle("Distribution of global test statistic (1300 ft)") +
  xlab("Test Statistic") + 
  ylab("Frequency") +
  theme(legend.position="none", text = element_text(size=6))

realData_global_sep = grid.arrange(p1, p2, p3, nrow = 3)
ggsave(filename = "Plots/realData_global_sep.png", plot = realData_global_sep, width = 1000, height = 800, units = "px")

# Figure of Individual results -----------------------------------------------------
perc_rejections_new = data.frame("orig" = c(1:13), "adjusted" = c(1:13))
load("../Output/p_vals_match_rel/p_val_df_new_stat_FINAL.dat")
for (i in 2:13) {
  load(paste0('../Output/origGridInfo/sim_orig_', i, '.dat'))
  p_orig = mean(sim_orig$DATA$naive_pval < 0.05, na.rm = T)
  p_new = mean(p_val_df[[i]][1,] < 0.05, na.rm = T)
  perc_rejections_new[i, ] = c(p_orig, p_new)
}
perc_rejections_new = perc_rejections_new[seq(3,13,by=2),]

buff = rep(seq(3,13,by=2), 2)
pValtype = c(rep("Naive", 6), rep("Corrected", 6))
yVal = c(perc_rejections_new[,1], perc_rejections_new[,2])
myData <- data.frame(buff,pValtype, yVal)

realData_pval_final = ggplot(myData, aes(fill=pValtype, y=yVal, x=buff)) + 
                          geom_bar(position="dodge", stat="identity") + 
                          ggtitle("Percent of p-values less than 0.05 (Arrest Data)") +
                          xlab("Buffer Width (100x in ft)") + 
                          ylab("Percent") +
                          scale_x_continuous(breaks = seq(3,13, by=2)) +
                          scale_y_continuous(breaks = pretty(myData$yVal, n = 10)) +
                          geom_hline(yintercept=0.05, linetype="dashed", 
                                     color = "black", size = 0.5) +
                          theme(text = element_text(size=8), legend.position="bottom",
                                  legend.title=element_blank(),
                                  legend.key.height= unit(1, 'mm'),
                                  legend.key.width= unit(4, 'mm'),
                                  legend.box.margin=margin(-10,-10,-10,-10)) +
                          scale_fill_discrete(name = "P-Value")

ggsave(filename = "Plots/realData_pval_final.png", plot = realData_pval_final, width = 1000, height = 800, units = "px")

# Updated P-Value histograms ---------------------------------------------------
load("../Output/p_vals_match_rel/p_val_df_new_stat_FINAL.dat")
p = vector(mode = 'list', length = 12)
for(i in 2:13) {
    adjPVal500 = data.frame("p" = na.omit(p_val_df[[i]][1,]))
    p[[i-1]] = ggplot(adjPVal500, aes(x=p)) + 
        geom_histogram(color="black", fill="white", bins = sqrt(144)) +
        xlab("P-Values") + 
        ylab("Frequency") + 
        ggtitle(paste0("Corrected p-values at buffer ",i,"00 ft")) + 
        theme(text = element_text(size=8))
}
pdf("Plots/correctedHist.pdf", onefile = T)
grid.arrange(p[[1]], p[[2]], ncol = 1, nrow = 2)
grid.arrange(p[[3]], p[[4]], ncol = 1, nrow = 2)
grid.arrange(p[[5]], p[[6]], ncol = 1, nrow = 2)
grid.arrange(p[[7]], p[[8]], ncol = 1, nrow = 2)
grid.arrange(p[[9]], p[[10]], ncol = 1, nrow = 2)
grid.arrange(p[[11]], p[[12]], ncol = 1, nrow = 2)
dev.off()

df_new = data.frame("p" = na.omit(p_val_df[[5]][1, ]))
realData_pval_500 = ggplot(df_new, aes(x=p)) + 
                        geom_histogram(color="black", fill="white", bins = sqrt(144)) +
                        xlab("P-Values") + 
                        ylab("Frequency") + 
                        ggtitle(paste0("Corrected p-values at buffer 500 ft")) + 
                        theme(text = element_text(size=8))
ggsave(filename = "Plots/realData_pval_500.png", plot = realData_pval_500, width = 1000, height = 800, units = "px")

# Newest plots to show importance of matching ----------------------------------
library(splines)
library(tidyverse)
k = 5
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

wMatchOk2 = which(!is.na(combinedMatchingSetupFix$DATA$t_stat_new))
wMatchOk = intersect(wMatchOk1, wMatchOk2)
# wMatchOk = which(!is.na(combinedMatchingSetupFix$DATA$t_stat_new))

combinedMatchingSetupFix2 = combinedMatchingSetupFix
combinedMatchingSetupFix2$DATA = combinedMatchingSetupFix2$DATA[wMatchOk,]
combinedMatchingSetupFix2$ARR_IND_1 = combinedMatchingSetupFix2$ARR_IND_1[wMatchOk]
combinedMatchingSetupFix2$ARR_IND_2 = combinedMatchingSetupFix2$ARR_IND_2[wMatchOk]
combinedMatchingSetupFix2$OFF_IND_1 = combinedMatchingSetupFix2$OFF_IND_1[wMatchOk]
combinedMatchingSetupFix2$OFF_IND_2 = combinedMatchingSetupFix2$OFF_IND_2[wMatchOk]

tot_lengths = data.frame("arr1" = sapply(combinedMatchingSetupFix2$ARR_IND_1, length),
                         "arr2" = sapply(combinedMatchingSetupFix2$ARR_IND_2, length),
                         "off1" = sapply(combinedMatchingSetupFix2$OFF_IND_1, length),
                         "off2" = sapply(combinedMatchingSetupFix2$OFF_IND_2, length))
tot_lengths[which(tot_lengths$off1 == 0 | tot_lengths$off2 == 0), ] = NA

rat_off = tot_lengths$off1 / tot_lengths$off2
rat_off[which(rat_off < 1)] = 1 / rat_off[which(rat_off < 1)]

tot_off = tot_lengths$off1 + tot_lengths$off2
t_stat_new = combinedMatchingSetupFix2$DATA$t_stat_new

set.seed(2022)
n=10000

plotting_info = data.frame("t_stat_new" = t_stat_new, "totLength" = tot_off, "ratioStreet" = rat_off)
plotting_info = plotting_info[!is.na(plotting_info$totLength), ]
sub_plot = plotting_info[sample(1:nrow(plotting_info), n), ]

# ratio of streets
nonLinMod = lm(t_stat_new ~ ns(ratioStreet, 4), data = sub_plot[,c("t_stat_new", "ratioStreet")])
nullMod = lm(t_stat_new ~ 1, data = data.frame(t_stat_new = sub_plot[,"t_stat_new"]))

gridPoints = seq(1, max(sub_plot$ratioStreet), length=n)
predictGrid = predict(nonLinMod, newdata=data.frame(ratioStreet=gridPoints))
df = data.frame(gridPoints = gridPoints, predictGrid = predictGrid,
                ratStreet = sub_plot$ratioStreet, t_stat_new = sub_plot$t_stat_new)

ggplot(data = df, aes(x = ratStreet, y = t_stat_new)) + 
  geom_point() +
  xlim(0,100) + 
  geom_line(aes(gridPoints, predictGrid), color = "red", size = 2) +
  xlab("Ratio of Crime") + 
  ylab("Test Statistic") + 
  ggtitle("Ratio of Crime's Effect on the Test Statistic") + 
  theme(text = element_text(size=24))

# plot(sub_plot$ratioStreet, sub_plot$t_stat_new, xlim = c(0,100), ylim = c(0,50))
# lines(gridPoints, predictGrid, col=2)
testResults = anova(nullMod, nonLinMod)
testResults

# total length

nonLinMod = lm(t_stat_new ~ ns(totLength, 4), data = sub_plot[,c("t_stat_new", "totLength")])
nullMod = lm(t_stat_new ~ 1, data = data.frame(t_stat_new = sub_plot[,"t_stat_new"]))

gridPoints = seq(1, max(sub_plot$totLength), length=n)
predictGrid = predict(nonLinMod, newdata=data.frame(totLength=gridPoints))

df = data.frame(gridPoints = gridPoints, predictGrid = predictGrid,
                totLength = sub_plot$totLength, t_stat_new = sub_plot$t_stat_new)

ggplot(data = df, aes(x = totLength, y = t_stat_new)) + 
  geom_point() +
  geom_line(aes(gridPoints, predictGrid), color = "red", size = 2) +
  xlab("Total Crime Count") + 
  ylab("Test Statistic") + 
  ggtitle("Total Crime Count's Effect on the Test Statistic") + 
  theme(text = element_text(size=24))

# plot(sub_plot$totLength, sub_plot$t_stat_new, ylim = c(0,50))
# lines(gridPoints, predictGrid, col=2)
testResults = anova(nullMod, nonLinMod)
testResults




