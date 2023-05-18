# ALL FIGURES ARE 1000 x 800

library(tidyverse, quietly = T)

# Figure of Naive P-val  -------------------------------------------------------
load('../Output/origGridInfo/sim_orig_5.dat')
unadjPVal500 = data.frame("p" = na.omit(sim_orig$DATA$naive_pval))
ggplot(unadjPVal500, aes(x=p)) + 
  geom_histogram(color="black", fill="white", bins = sqrt(nrow(sim_orig$DATA))) +
  xlab("P-Values") + 
  ylab("Frequency") + 
  ggtitle("Histogram of P-Values at Buffer Width 500 ft") + 
  theme(text = element_text(size=24))

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
# percRejection = percRejection[1:7,]

ggplot(percRejection, aes(y=perc, x=buff)) + 
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Percentage of P-Values less than 0.05") +
  xlab("Buffer Width (100x in ft)") + 
  ylab("Percent") +
  ylim(0, 1) +
  scale_x_continuous(breaks = pretty(percRejection$buff, n = 11)) +
  theme(text = element_text(size=24))

# Figure of Global results -----------------------------------------------------
library(gridExtra)
library(tidyverse)
load("../Data/indexList_MAIN.RData")

n_matches = 250

p_val_df = rep(NA, 13)

trialNum = 1
set.seed(trialNum)

load(paste0('../Output/Global/global_t_stat_', trialNum,"_FINAL.dat"))

for(k in 2:13) {
  
  load(paste0('../Output/origGridInfo/sim_orig_', k,".dat"))
  
  t_stat = max(sim_orig$DATA$t_stat_new, na.rm = T)
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
globalPvals_new_2 = data.frame("buff" = 3:13,
                               "p" = p_val_df[3:13])
ggplot(globalPvals_new_2, aes(y=p, x=buff)) + 
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("P-Values for Global Test Statistics") +
  xlab("Buffer Width (100x in ft)") + 
  ylab("P-Values") +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "red", size = 1.5) +
  theme(text = element_text(size=22)) +
  scale_x_continuous(breaks= 2:13)

globalEmpDist = data.frame("five" = global_t_stat[[5]]$max_t_stat,
                           "nine" = global_t_stat[[9]]$max_t_stat,
                           "thirteen" = global_t_stat[[13]]$max_t_stat)
globalObsVal = data.frame("five" = 1, "nine" = 1, "thirteen" = 1)
ind = 1
for(k in c(5,9,13)) {
  load(paste0('../Output/origGridInfo/sim_orig_', k,".dat"))
  
  globalObsVal[1,ind] = max(sim_orig$DATA$t_stat_new, na.rm = T)
  ind=ind+1
}

p1 = ggplot(globalEmpDist, aes(x=five)) + 
  geom_histogram(color="black", fill="white", bins = sqrt(nrow(globalEmpDist))) + 
  geom_vline(data=globalObsVal, aes(xintercept=globalObsVal$five[1], color="red"), size=1) + 
  ggtitle("Distribution of Global Test Statistic (Buffer 500 ft)") +
  xlab("Test Statistic") + 
  ylab("Frequency") +
  theme(legend.position="none", text = element_text(size=22))

p2 = ggplot(globalEmpDist, aes(x=nine)) + 
  geom_histogram(color="black", fill="white", bins = sqrt(nrow(globalEmpDist))) + 
  geom_vline(data=globalObsVal, aes(xintercept=globalObsVal$nine[1], color="red"), size=1) + 
  ggtitle("Distribution of Global Test Statistic (Buffer 900 ft)") +
  xlab("Test Statistic") + 
  ylab("Frequency") +
  theme(legend.position="none", text = element_text(size=22))

p3 = ggplot(globalEmpDist, aes(x=thirteen)) + 
  geom_histogram(color="black", fill="white", bins = sqrt(nrow(globalEmpDist))) + 
  geom_vline(data=globalObsVal, aes(xintercept=globalObsVal$thirteen[1], color="red"), size=1) + 
  ggtitle("Distribution of Global Test Statistic (Buffer 1300 ft)") +
  xlab("Test Statistic") + 
  ylab("Frequency") +
  theme(legend.position="none", text = element_text(size=22))

grid.arrange(p1, p2, p3, nrow = 3)

# Figure of Individual results -----------------------------------------------------
perc_rejections_new = data.frame("orig" = c(1:13), "adjusted" = c(1:13))
load("../Output/p_vals_match_rel/p_val_df_1_new_stat_FINAL.dat")
for (i in 2:13) {
  load(paste0('../Output/origGridInfo/sim_orig_', i, '.dat'))
  p_orig = mean(sim_orig$DATA$t_stat_pval < 0.05, na.rm = T)
  p_new = mean(p_val_df[[i]][1,] < 0.05, na.rm = T)
  perc_rejections_new[i, ] = c(p_orig, p_new)
}
perc_rejections_new = perc_rejections_new[seq(3,13,by=2),]

buff = rep(seq(3,13,by=2), 2)
pValtype = c(rep("Naive", 6), rep("Corrected", 6))
yVal = c(perc_rejections_new[,1], perc_rejections_new[,2])
myData <- data.frame(buff,pValtype, yVal)

ggplot(myData, aes(fill=pValtype, y=yVal, x=buff)) + 
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Percentage of P-Values less than 0.05 (Arrest Data)") +
  xlab("Buffer Width (100x in ft)") + 
  ylab("Percent") +
  scale_x_continuous(breaks = seq(3,13, by=2)) +
  scale_y_continuous(breaks = pretty(myData$yVal, n = 10)) +
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "black", size = 1.5) +
  theme(text = element_text(size=24), legend.position="bottom",
        legend.title=element_blank()) +
  scale_fill_discrete(name = "P-Value")

df_new = data.frame("p" = na.omit(p_val_df[[5]][1, ]))
ggplot(df_new, aes(x=p)) + 
  geom_histogram(color="black", fill="white", bins = 12) + 
  ggtitle("Histogram of Corrected P-Values at Buffer 500 ft") +
  xlab("Corrected P-Value") + 
  ylab("Frequency") +
  theme(text = element_text(size=24)) 

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




