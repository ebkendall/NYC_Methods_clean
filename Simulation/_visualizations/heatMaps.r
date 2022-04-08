library(tidyverse)
library(RColorBrewer)
library(gridExtra)

load("../Data/Surfaces/gridPointValues_cov_c_40.rda")
load("../Data/Surfaces/gridPointValues_cov_r_40.rda")
load("../Data/Surfaces/gridPointValues_hotspot_40.rda")
load("../Data/Surfaces/gridPointValues_uniform_40.rda")

load("../Data/nycSub.RData")
load("../Data/gridWithin.rda")

plot_mat = vector(mode = "list", length = 4)

# Uniform ---------------------------------------------------------------------
plot_df = data.frame("x" = gridWithin@coords[,1],
                     "y" = gridWithin@coords[,2],
                     "CrimeCount" = gridPointValues_uniform)

nyc.map <- ggplot(plot_df, aes(x, y)) +
  geom_tile(aes(fill = CrimeCount)) + 
  scale_fill_gradient(low = "green", high = "red", labels = c(' ')) +
  labs(title="Uniform Crime",
       x ="", y = "") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="bottom",
        text = element_text(size=35),
        legend.title = element_blank())

plot_mat[[1]] = nyc.map

# HotSpot ---------------------------------------------------------------------
plot_df = data.frame("x" = gridWithin@coords[,1],
                     "y" = gridWithin@coords[,2],
                     "CrimeCount" = gridPointValues_hotspot)

nyc.map <- ggplot(plot_df, aes(x, y)) +
  geom_tile(aes(fill = CrimeCount)) + 
  scale_fill_gradient(low = "green", high = "red", breaks = c(1, 7), labels = c('Low', 'High')) +
  labs(title="Hot Spot Crime",
       x ="", y = "") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="bottom",
        text = element_text(size=35),
        legend.title = element_blank())
plot_mat[[2]] = nyc.map

# Random  ---------------------------------------------------------------------
plot_df = data.frame("x" = gridWithin@coords[,1],
                     "y" = gridWithin@coords[,2],
                     "CrimeCount" = gridPointValues_cov_r)

nyc.map <- ggplot(plot_df, aes(x, y)) +
  geom_tile(aes(fill = CrimeCount)) + 
  scale_fill_gradient(low = "green", high = "red", breaks = c(1, 10), labels = c('Low', 'High')) +
  labs(title="Random Crime",
       x ="", y = "") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position="bottom",
        text = element_text(size=35),
        legend.title = element_blank())
plot_mat[[3]] = nyc.map

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
plot_mat[[4]] = nyc.map

png(filename = "Plots/heatMaps_4.png", width = 2000, height = 1000,
    units = "px", pointsize = 12, bg = "white", res = NA)
# pdf('Plots/heatMaps_3.pdf')
grid.arrange(plot_mat[[1]], plot_mat[[2]], plot_mat[[3]], plot_mat[[4]], ncol=4, nrow =1)
dev.off()
