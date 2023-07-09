# Loading libraries
library(tidyverse)
library(sf)
library(raster)
library(ggtext)
library(showtext)


# Loading font
font_add_google("Zen Maru Gothic", "Zen Maru Gothic")
showtext_auto()


# Loading data
tanabata_hiroshige <- stack("Shichūhan'ei_tanabata_matsuri_LOC_16325961983.jpg")


# Preparing data
tanabata_hiroshige_df <- tanabata_hiroshige %>% 
  as.data.frame(xy = TRUE)  %>% 
  as_tibble() %>% 
  rename(
    R = Shich.U.016B.han.ei_tanabata_matsuri_LOC_16325961983.1, 
    G = Shich.U.016B.han.ei_tanabata_matsuri_LOC_16325961983.2, 
    B = Shich.U.016B.han.ei_tanabata_matsuri_LOC_16325961983.3
  ) %>% 
  mutate(
    RGB = rgb(R / 255, G / 255, B / 255)
  )


# Plotting
plot_tanabata_2023 <- ggplot(data = tanabata_hiroshige_df) + 
  geom_point(aes(x = x, y = y, color = RGB), size = 0.9) + 
  scale_color_identity() + 
  coord_equal(expand = FALSE) + 
  labs(
    title = "<b>Japanese <span style='color:#316745;'>Tanabata</span> tradition</b>", 
    subtitle = "Hiroshige Utagawa's picture plotted by ggplot2", 
    caption = "Original image from Wikimedia Commons (https://commons.wikimedia.org/wiki/File:Shich%C5%ABhan%27ei_tanabata_matsuri_LOC_16325961983.jpg); Converted by @pat_macro"
  ) + 
  theme(
    axis.title = element_blank(), 
    axis.text = element_blank(), 
    panel.background = element_rect(fill = "#e9e4d4", color = NA), 
    panel.grid = element_blank(), 
    plot.background = element_rect(fill = "white", color = NA), 
    plot.title = element_textbox(family = "Zen Maru Gothic", size = 250), 
    plot.subtitle = element_text(family = "Zen Maru Gothic", size = 150),
    plot.caption = element_text(family = "Zen Maru Gothic", size = 30), 
    legend.position = "none"
  )

# Saving image
ggsave(plot_tanabata_2023, filename = "results/plot_tanabata_2023.png", width = 40, height = 60, units = "cm", dpi = 300)
