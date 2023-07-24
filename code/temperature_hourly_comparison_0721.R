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
tokyo_data <- read_csv("data/0721_data.csv", skip = 5, col_names = c("Date", "Temp", "Temp_Quality", "Temp_Equality", "Solar_radiation", "Solar_radiation_Quality", "Solar_radiation_Equality", "Humidity", "Humidity_Quality", "Humidity_Equality"))


# Transforming data
tokyo_data2 <- tokyo_data %>% 
  mutate(
    Year = year(Date), 
    Day_Month = str_c(month(Date), "/", str_pad(day(Date), width = 2, pad = "0")), 
    Hour = hour(Date)
  ) %>% 
  filter(Day_Month != "7/21")


# Setting theme
my_theme <- theme(
  text = element_text(family = "Josefin Sans", size = 60), 
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


# Plot1
g_tokyo_temp_hourly <- ggplot(data = tokyo_data2 %>% drop_na(Temp)) + 
  geom_path(aes(x = Hour, y = Temp, color = Year), linewidth = 0.5, alpha = 0.6) + 
  scale_y_continuous(name = "Temperature (°C)",limits = c(15, 40)) + 
  scale_color_viridis_c(option = "turbo", breaks = c(1973, seq(1975, 2020, 5), 2022), labels = c(1973, seq(1975, 2020, 5), 2022)) + 
  my_theme + 
  theme(
    axis.text.x = element_blank()
  )


# Plot2
g_tokyo_humidity_hourly <- ggplot(data = tokyo_data2 %>% drop_na(Humidity)) + 
  geom_path(aes(x = Hour, y = Humidity, color = Year), linewidth = 0.5, alpha = 0.6) + 
  scale_y_continuous(name = "Relative humidity (%)",limits = c(25, 100)) + 
  scale_color_viridis_c(option = "cividis", breaks = c(1973, seq(1975, 2020, 5), 2022), labels = c(1973, seq(1975, 2020, 5), 2022), direction = -1) + 
  my_theme + 
  theme(
    axis.text.x = element_blank()
  )


# Plot3
g_tokyo_radiation_hourly <- ggplot(data = tokyo_data2 %>% drop_na(Solar_radiation)) + 
  geom_path(aes(x = Hour, y = Solar_radiation, color = Year), linewidth = 0.5, alpha = 0.6) + 
  scale_x_continuous(breaks = 0:23, labels = 0:23) + 
  scale_y_continuous(name = "Solar radiation (MJ/m2)",limits = c(0, 3.5)) + 
  scale_color_viridis_c(option = "plasma", breaks = c(1989, seq(1990, 2020, 5), 2022), labels = c(1989, seq(1990, 2020, 5), 2022), direction = -1) + 
  my_theme


# Combining plots
g_tokyo_hourly <- g_tokyo_temp_hourly / g_tokyo_humidity_hourly / g_tokyo_radiation_hourly + 
  plot_annotation(
    title = "Hourly weather changes in Tokyo<br> on July 21st over the past 50 years", 
    caption = "Hourly temperature data from Japan Meteorological Agency (https://www.data.jma.go.jp/gmd/risk/obsdl/index.php); @pat_macro", 
    theme = theme(
      plot.title = element_textbox(family = "Josefin Sans", face = "bold", size = 120, lineheight = 0.25), 
      plot.caption = element_text(family = "Josefin Sans", face = "bold", size = 40)
    )
  )


# Saving plot
ggsave(g_tokyo_hourly, file = "results/g_tokyo_hourly.png", width = 30, height = 36, dpi = 300, unit = "cm")

