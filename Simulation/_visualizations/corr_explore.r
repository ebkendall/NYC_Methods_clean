library(tidyverse)
library(RColorBrewer)
library(gridExtra)


load("../Data/nycSub.RData")
load("../Data/gridWithin.rda")

plot_mat = vector(mode = "list", length = 50)

for (ii in 1:50) {
  
  load(paste0("../Data/Surfaces/gridPointValues_cov_c_", ii, ".rda"))
  
  # Correlated  -----------------------------------------------------------------
  plot_df = data.frame("x" = gridWithin@coords[,1],
                       "y" = gridWithin@coords[,2],
                       "CrimeCount" = gridPointValues_cov_c_big)
  
  nyc.map <- ggplot(plot_df, aes(x, y)) +
    geom_tile(aes(fill = CrimeCount)) + 
    scale_fill_gradient(low = "green", high = "red", breaks = c(1, 7), labels = c('Low', 'High')) +
    labs(title="Correlated Crime",
         x ="", y = "") +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position="bottom",
          text = element_text(size=35),
          legend.title = element_blank())
  plot_mat[[ii]] = nyc.map
}

for(ii in seq(1,48,by=4)) {
  name = paste0("Plots/corr_", ii, ".png")
  png(filename = name, width = 2000, height = 1000,
      units = "px", pointsize = 12, bg = "white", res = NA)
  grid.arrange(plot_mat[[ii]], plot_mat[[ii + 1]], 
               plot_mat[[ii + 2]], plot_mat[[ii + 3]], ncol=4, nrow =1)
  dev.off()
  
}
