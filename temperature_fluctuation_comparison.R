# Loading libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(showtext)


# Loading font
font_add_google("Roboto")


# Loading data
temp <- read_csv("data/data.csv", skip = 5, col_names = c("Date", "Temp", "Quality", "Equality"))


# Transforming data
temp2 <- temp %>% 
  mutate(
    Date = ymd_hms(Date), 
    Year = as.character(year(Date)), 
    Day_Month = str_c(month(Date), "/", str_pad(day(Date), width = 2, pad = "0"))
  ) %>% 
  filter(Day_Month != "4/26")


# Setting theme
my_theme <- theme(
  text = element_text(family = "Roboto", size = 11), 
  axis.title.x = element_blank(), 
  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
  axis.ticks.x = element_blank(), 
  legend.position = c(0.5, 0.95),
  legend.direction = "horizontal", 
  legend.background = element_blank(), 
  legend.box.background = element_blank(), 
  legend.title = element_blank(), 
  panel.background = element_blank(), 
  panel.grid.major.x = element_blank(), 
  panel.grid.major.y = element_line(size = 0.25, color = "#595857", linetype = 2)
)


# Plot1
g_temp <- ggplot(data = temp2) + 
  geom_point(aes(x = Year, y = Temp, color = Day_Month), alpha = 0.3) + 
  scale_y_continuous(name = "Temperature (°C)",limits = c(0, 35)) + 
  guides(color = guide_legend(nrow = 1)) + 
  my_theme + 
  theme(
    axis.text.x = element_blank()
  )


# Plot2
temp3 <- temp2 %>% 
  group_by(Year) %>% 
  summarise(dif.max_min = max(Temp, na.rm = TRUE) - min(Temp, na.rm = TRUE))

g_dif <- ggplot(data = temp3) + 
  geom_col(aes(x = Year, y = dif.max_min, fill = dif.max_min), alpha = 0.75) + 
  scale_y_continuous(name = "Difference between maximum and minimum \ntemperature in a focused period (°C)", limits = c(0, 22)) + 
  scale_fill_gradient(low = "#e8ecef", high = "#e95295", limits = c(0, 25)) + 
  my_theme + 
  theme(
    legend.key.height = unit(0.2, "cm")
  )


# Combining plots
g_temp_dif <- g_temp / g_dif + 
  plot_annotation(
    title = "Comparison of temperature fluctuations in 5 days (4/21 to 4/25) \nover the past 50 years in Tokyo", 
    caption = "Hourly temperature data from Japan Meteorological Agency (https://www.data.jma.go.jp/gmd/risk/obsdl/index.php); @pat_macro", 
    theme = theme(
      plot.title = element_text(family = "Roboto", face = "bold", size = 20), 
      plot.caption = element_text(family = "Roboto", face = "bold", size = 10)
    )
  )


# Saving plot
ggsave(g_temp_dif, file = "results/g_temp_dif.png", width = 24, height = 19, dpi = 300, unit = "cm")
