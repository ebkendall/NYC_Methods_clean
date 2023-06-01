load('../Data/treesByPrec.RData')
load('../Data/tree_df.RData')
load('../Data/ind_prec_df.rda')
load('../Data/indexList_MAIN.RData')
load('../Data/nycSub.RData')
load('../Data/totalStreetBuffInfo_NEW.RData')

library(rgeos, quietly = T)
library(ggmap, quietly = T)
library(tidyverse, quietly = T)
library(gridExtra, quietly = T)

# Figure Trees on Streets ------------------------------------------------------
prec_num = 49

tree_sub = tree_df[which((tree_df$x_sp %in% treesByPrec[[prec_num]]$x) & (tree_df$y_sp %in% treesByPrec[[prec_num]]$y)), ]

png(filename = "Plots/treesAndStreets.png", width = 2000, height = 1000,
    units = "px", pointsize = 12, bg = "white", res = NA)
qmplot(longitude, latitude, data = tree_sub, alpha = I(.9), size = 1, colour = 'darkred', darken = .1, source = "osm") +
  theme(legend.position = "none")
dev.off()

# Figure of Naive P-val  -------------------------------------------------------
load('../Output_tree/origGridInfo/sim_orig_5.dat')
sim_orig$DATA$naive_pval[sim_orig$DATA$naive_pval == -1] = NA

unadjPVal200 = data.frame("p" = na.omit(sim_orig$DATA$naive_pval))
negControl_pval = ggplot(unadjPVal200, aes(x=p)) + 
                          geom_histogram(color="black", fill="white", bins = sqrt(nrow(sim_orig$DATA))) +
                          xlab("P-Values") + 
                          ylab("Frequency") + 
                          ggtitle("Histogram of p-values at buffer width 500 ft") + 
                          theme(text = element_text(size=8))
ggsave(filename = "Plots/negControl_pval.png", plot = negControl_pval, width = 1000, height = 800, units = "px")

load('../Output_tree/origGridInfo/sim_orig_3.dat')
sim_orig$DATA$naive_pval[sim_orig$DATA$naive_pval == -1] = NA
unadjPValTotal = data.frame(na.omit(sim_orig$DATA$naive_pval))
for (i in 4:10) {
  load(paste0('../Output_tree/origGridInfo/sim_orig_', i, '.dat'))
  sim_orig$DATA$naive_pval[sim_orig$DATA$naive_pval == -1] = NA
  unadjPValTotal = cbind(unadjPValTotal, data.frame(na.omit(sim_orig$DATA$naive_pval)))
}
colnames(unadjPValTotal) = c("3", "4", "5", "6", "7", "8", "9", "10")

percRejection = data.frame("perc" = rep(1,8), "buff" = c(3:10))
for (i in 1:8) {
  percRejection[i,1] = sum(na.omit(unadjPValTotal[,i] < 0.05)) / sum(!is.na(unadjPValTotal[,i]))
}
# percRejection = percRejection[1:7,]

negControl_pval_total = ggplot(percRejection, aes(y=perc, x=buff)) + 
                              geom_bar(position="dodge", stat="identity") + 
                              ggtitle("Percentage of p-values less than 0.05") +
                              xlab("Buffer Width (100x in ft)") + 
                              ylab("Percent") +
                              ylim(0, 1) +
                              scale_x_continuous(breaks = pretty(percRejection$buff, n = 8)) +
                              theme(text = element_text(size=8))
ggsave(filename = "Plots/negControl_pval_total.png", plot = negControl_pval_total, width = 1000, height = 800, units = "px")
print(percRejection)

# Number of matches changing -----------------------------------------------------
j = 3
load(paste0('../Output_tree/combination/perc_pval_match_street', j, '.dat'))
pval = perc_pval_match[1:60,]
trees_numMatch = ggplot(pval, aes( y=perc_pval_less_05, x=num_match)) + 
    geom_point(color = "red", size = 1) +
    geom_smooth(method = "loess", formula = y ~ x) +
    ggtitle(paste0("Matching's effect on type I error (",j,"00 ft)")) +
    xlab("Number of resampled streets") + 
    ylab("Type I error") +
    ylim(0,max(pval$perc_pval_less_05)) + 
    scale_x_continuous(breaks = pretty(pval$num_match, n = 10)) +
    geom_hline(yintercept=0.05, linetype="dashed", 
               color = "black", size = 1) +
    theme(text = element_text(size=8))
ggsave(filename = "Plots/trees_numMatch.png", plot = trees_numMatch, width = 1000, height = 500, units = "px")

# Figure of Individual results -----------------------------------------------------
perc_rejections_new = data.frame("orig" = c(1:13), "adjusted" = c(1:13))
load("../Output_tree/p_vals_match_rel/p_val_df_FINAL.dat")
for (i in 2:13) {
  load(paste0('../Output_tree/origGridInfo/sim_orig_', i, '.dat'))
  sim_orig$DATA$naive_pval[sim_orig$DATA$naive_pval == -1] = NA
  p_orig = mean(sim_orig$DATA$naive_pval < 0.05, na.rm = T)
  p_new = mean(p_val_df[[i]][1,] < 0.05, na.rm = T)
  perc_rejections_new[i, ] = c(p_orig, p_new)
}
perc_rejections_new = perc_rejections_new[3:10,]

buff = rep(3:10, 2)
pValtype = c(rep("Naive", 8), rep("Corrected", 8))
yVal = c(perc_rejections_new[,1], perc_rejections_new[,2])
myData <- data.frame(buff,pValtype, yVal)

trees_FINAL = ggplot(myData, aes(fill=pValtype, y=yVal, x=buff)) + 
                      geom_bar(position="dodge", stat="identity") + 
                      ggtitle("Percent of p-values less than 0.05 (Neg. Control)") +
                      xlab("Buffer width (100x in ft)") + 
                      ylab("Percent") +
                      scale_x_continuous(breaks=3:10) +
                      scale_y_continuous(breaks = pretty(myData$yVal, n = 10)) +
                      geom_hline(yintercept=0.05, linetype="dashed", 
                                 color = "black", size = 0.5) +
                      theme(text = element_text(size=8), legend.position="bottom",
                            legend.title=element_blank(),
                            legend.key.height= unit(1, 'mm'),
                            legend.key.width= unit(4, 'mm'),
                            legend.box.margin=margin(-10,-10,-10,-10)) +
                      scale_fill_discrete(name = "P-Value")
ggsave(filename = "Plots/trees_FINAL.png", plot = trees_FINAL, width = 1000, height = 800, units = "px")

# Updated P-Value histograms ---------------------------------------------------
load("../Output_tree/p_vals_match_rel/p_val_df_FINAL.dat")
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

adjPVal500 = data.frame("p" = na.omit(p_val_df[[5]][1,]))
trees_500_adj = ggplot(adjPVal500, aes(x=p)) + 
                        geom_histogram(color="black", fill="white", bins = sqrt(144)) +
                        xlab("P-Values") + 
                        ylab("Frequency") + 
                        ggtitle(paste0("Corrected p-values at buffer ",5,"00 ft")) + 
                        theme(text = element_text(size=8))
ggsave(filename = "Plots/trees_500_adj.png", plot = trees_500_adj, width = 1000, height = 800, units = "px")


