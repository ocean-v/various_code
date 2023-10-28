# Loading libraries
library(tidyverse)
library(lidR)
library(raster)
library(rayshader)


# Loading LAS file 
## Data from OPEN NAGASAKI (https://opennagasaki.nerc.or.jp) by Nagasaki Prefectural.
## Preprocessed on CloudCompare
las <- readLAS("shiratake_combined_1.1_clean.las")


# Converting point cloud t raster
ras <- rasterize_canopy(las, res = 5, algorithm = p2r())


# Converting raster to tibble
ras_df <- ras %>% 
  as.data.frame(xy = TRUE) %>% 
  as_tibble()


# Creating ggplot obj
g <- ggplot(data = ras_df) + 
    geom_raster(aes(x = x, y = y, fill = Z)) + 
    MetBrewer::scale_fill_met_c(name = "Hokusai1", direction = -1, limits = c(0, 550)) + 
  labs(
    title = "Mt. Shiratake, Tsushima Island", 
    caption = "Data from OPEN NAGASAKI (https://opennagasaki.nerc.or.jp) by Nagasaki Prefectural.\n Image created by @pat_macro"
  ) + 
  theme(
    axis.title = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    panel.background = element_rect(fill = "#5b7e91", color = NA), 
    panel.grid = element_blank(), 
    plot.background = element_rect(fill = "#5b7e91", color = NA), 
    plot.title = element_text(color = "white"), 
    plot.caption = element_text(color = "white", size = 7.5), 
    legend.background = element_blank(), 
    legend.title = element_text(color = "white"), 
    legend.text = element_text(color = "white")
  )


# 3D plotting
plot_gg(
  g, multicore = TRUE, raytrace = TRUE, 
  width = ncol(ras) / 200, height = nrow(ras) / 200, 
  scale = 200, windowsize = c(1600, 900), 
  zoom = 0.55, phi = 45, theta = 30, 
  background = "#5b7e91"
)


# Creating snap shot
render_snapshot(filename = "rayshaderplot.png", clear = TRUE)
