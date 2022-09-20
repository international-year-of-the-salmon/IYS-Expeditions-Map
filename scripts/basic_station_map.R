# Basic Sampling Station Maps

library(tidyverse)
library(here)
library(readxl)
library(mapdata)
library(gridExtra)

# Edit which vessels, years, or sampling event types you would like to plot
vessel <- c("TINRO", "NW", "Shimada", "Franklin", "Kaganovsky", "Legacy")
year <- c(2019, 2020, 2022)
#Could also be CTD or CTD-Rosette
event_types <- c("CTD")

iys_data <- read_excel(here("data", "IYS_Integrated_Data_Collection_V2.xlsx"), 
                       sheet = "Sampling_Events") |> 
  filter(vessel_name_abbr %in% vessel,
         year %in% year,
         event_type %in% event_types) |> 
  mutate(longitude_start_decdeg = ifelse(longitude_start_decdeg < 0,
                                         longitude_start_decdeg + 360,
                                         longitude_start_decdeg)
  )

pacific_map <- map_data("world2")

station_map <- ggplot() + geom_polygon(data = pacific_map, 
aes(x = long, y = lat, group = group), fill = 8, colour = "black") +
  coord_cartesian(xlim = c(170, 241), ylim = c(42, 63), expand = F) +
   scale_x_continuous(breaks = c(180, 200, 220, 240), 
                      labels = c("180°", "160°W", "140°W", "120°W")) +
   scale_y_continuous(breaks = c(45, 50, 55, 60), 
                      labels = c("45°N", "50°N", "55°N", "60°N")) +
  geom_point(data = iys_data, aes(x = longitude_start_decdeg, y = latitude_start_decdeg, 
                                  colour = vessel_name_abbr), size = 3) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "transparent"), panel.grid.minor = element_line(colour = "transparent")) + 
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("A) Sampling Stations") +
  theme(text=element_text(size=12)) +
  scale_shape_manual(values=c(8, 19))

station_map  
  
