# Loading libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(showtext)
library(ggtext)


# Loading font
font_add_google("Josefin Sans", "Josefin Sans")
showtext_auto()


# Loading data
tokyo_data <- read_csv("data/0725_data.csv", skip = 5, col_names = c("Date", "Temp", "Temp_Quality", "Temp_Equality", "Humidity", "Humidity_Quality", "Humidity_Equality"))


# Transforming data
tokyo_data2 <- tokyo_data %>% 
  mutate(
    Year = year(Date), 
    Day_Month = str_c(month(Date), "/", str_pad(day(Date), width = 2, pad = "0")), 
    Hour = hour(Date), 
    Temperature_Humidity_Index = 0.81 * Temp + 0.01 * Humidity * (0.99 * Temp - 14.3) + 46.3
  ) %>% 
  filter(Day_Month == "7/25" & Year >= 1973)


# Setting theme
my_theme <- theme(
  text = element_text(family = "Josefin Sans", size = 80), 
  axis.title.x = element_blank(), 
  axis.ticks.x = element_blank(), 
  legend.direction = "vertical", 
  legend.background = element_blank(), 
  legend.box.background = element_blank(), 
  legend.key.height = unit(2, "cm"), 
  legend.title = element_blank(), 
  panel.background = element_blank(), 
  panel.grid.major.x = element_blank(), 
  panel.grid.major.y = element_line(size = 0.25, color = "#595857", linetype = 2)
)

ref_polygon <- tibble(
  x = rep(c(-1, 24, 24, -1), 6), 
  y = rep(c(60, 60, 65, 65), 6)  + rep(seq(0, 25, 5), each = 4), 
  col = rep(MetBrewer::met.brewer(name = "Hokusai1", n = 6, direction = -1), each = 4)
)

# Plot1
g_tokyo_hourly_THI <- ggplot(data = tokyo_data2 %>% drop_na(Temperature_Humidity_Index), aes(x = Hour, y = Temperature_Humidity_Index)) + 
  geom_polygon(data = ref_polygon, aes(x = x, y = y, fill = col), alpha = 0.8) + 
  geom_point(aes(color = Year)) + 
  geom_line(aes(color = Year, group = as.factor(Year)), linewidth = 1) + 
  scale_x_continuous(breaks = 0:23, labels = 0:23) + 
  scale_y_continuous(name = "Temperature (°C)", limits = c(60, 90), expand = expansion(mult = c(0, 0.01))) + 
  MetBrewer::scale_color_met_c(name = "Hokusai2", breaks = c(1973, seq(1975, 2020, 5), 2022), labels = c(1973, seq(1975, 2020, 5), 2022), limits = c(1973, 2022)) + 
  scale_fill_identity() + 
  labs(
    title = "Hourly weather changes in Tokyo on July 21st over the past 50 years", 
    caption = "$$ THI = 0.81\ \times\ Temperature\ +\ 0.01\ \times\ Relative\ Humidity\ \times\ (0.99\ \times\ Temperature\ -14.3)\ +\ 46.3$$ <br> Hourly temperature data from Japan Meteorological Agency (https://www.data.jma.go.jp/gmd/risk/obsdl/index.php); @pat_macro"
  ) + 
  my_theme + 
  theme(
    plot.title = element_textbox(family = "Josefin Sans", face = "bold", size = 120, lineheight = 0.4), 
    plot.caption = element_textbox(family = "Josefin Sans", face = "bold", size = 40, lineheight = 0.4)
  )


# Saving plot
ggsave(g_tokyo_hourly_THI, file = "results/g_tokyo_hourly_THI.png", width = 36, height = 36, dpi = 300, unit = "cm")

