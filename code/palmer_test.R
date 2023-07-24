# Loading libraries
library(tidyverse)
library(palmerpenguins)
library(MetBrewer)


# Creating plot
g <- ggplot(data = penguins) + 
  geom_boxplot(aes(x = species, y = bill_length_mm), fill = NA) + 
  scale_y_continuous(breaks = seq(30, 60, 10), labels = seq(30, 60, 10), limits = c(30, 60)) + 
  xlab("Species") + 
  ylab("Bill length (mm)") + 
  labs(caption = "@pat_macro") + 
  theme(
    text = element_text(size = 15), 
    panel.background = element_rect(fill = NA, color = "black"), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(color = "black", linetype = 3), 
    legend.position = "none"
  )


# Saving image
ggsave(g, filename = "results/palmer_test_plot.png", width = 12, height = 10, units = "cm", dpi = 300)

