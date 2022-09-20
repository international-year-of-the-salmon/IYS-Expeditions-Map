# Mapping actual stations by vessel for 2022
# Written by Caroline Graham with edits from Brett Johnson

library(tidyverse)
library(here)
library(mapdata)
library(gridExtra)

pacific_map <- map_data("world2")

#importing results data
results <- read.csv(here::here("Data_files", "2022_completed_stations_May42022.csv"))

#convert date column to date values (instead of character)
results$date <- as.Date(results$date)

#remove rows that do not have lat and lon values
results_no_NA <- results %>% 
  filter(!is.na(lat)) %>% 
  filter(!is.na(lon))

#fixing the lon values so they can be plotted on a scale from 0 to 360 degrees
results_no_NA$lon[results_no_NA$lon<0] = results_no_NA$lon[results_no_NA$lon<0] + 360

#make a column for total number of salmon caught
results_tot_salmon <- results_no_NA %>% 
  group_by(vessel_name, station_no) %>%
  dplyr::mutate(total_salmon = sum(pink, chum, sockeye, Chinook, coho, steelhead, na.rm = TRUE))

#make a column for if they caught salmon (yes or no)
results_TF_salmon <- results_tot_salmon %>% 
  mutate(salmon_caught = ifelse(total_salmon>0, 'yes', 'no'))

#sorting by fishing stations
fishing_stns_sort <- results_TF_salmon %>% 
  filter(Trawl == 'X' | Gillnet == 'X' | Longline == 'X')

#make a column for gear type
fishing_stns <- fishing_stns_sort %>% 
  mutate(gear_type = ifelse(Trawl=="X", 'trawl', 'gillnet & longline'))

#plotting fishing stations
fishing_stns_plot <- ggplot() + geom_polygon(data = pacific_map, aes(x= long, y = lat, group = group), fill = 8, color = "black") +
  coord_cartesian(xlim = c(170, 241), ylim = c(42, 63), expand = F) +
  scale_x_continuous(breaks = c(180, 200, 220, 240), labels = c("180°", "160°W", "140°W", "120°W")) +
  scale_y_continuous(breaks = c(45, 50, 55, 60), labels = c("45°N", "50°N", "55°N", "60°N")) +
  geom_point(data=fishing_stns, aes(x=lon, y=lat, color = vessel_name, shape = gear_type)) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "transparent"), panel.grid.minor = element_line(colour = "transparent")) + 
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("A) Fishing Stations") +
  theme(text=element_text(size=12)) +
  scale_shape_manual(values=c(8, 19)) +
  scale_color_manual(values=c("#000000", "#E69F00", "#56B4E9", "#CC79A7", "#0072B2"))
fishing_stns_plot

#save fishing stations plot
ggsave(here::here("Figures", "2022_fishing_stations.pdf"), fishing_stns_plot, width =320, height =200, units = "mm")

#sorting by CTD stations
ctd_stns <- results_TF_salmon %>% 
  filter(CTD =='X')

#plotting CTD stations
ctd_stns_plot <- ggplot() + geom_polygon(data = pacific_map, aes(x= long, y = lat, group = group), fill = 8, color = "black") +
  coord_cartesian(xlim = c(170, 241), ylim = c(42, 63), expand = F) +
  scale_x_continuous(breaks = c(180, 200, 220, 240), labels = c("180°", "160°W", "140°W", "120°W")) +
  scale_y_continuous(breaks = c(45, 50, 55, 60), labels = c("45°N", "50°N", "55°N", "60°N")) +
  #geom_point(data=station_data_2022, aes(x=lon, y=lat), size = 0.8, shape = 1) +
  geom_point(data=ctd_stns, aes(x=lon, y=lat, color = vessel_name)) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "transparent"), panel.grid.minor = element_line(colour = "transparent")) + 
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("B) CTD Stations") +
  theme(text=element_text(size=12)) +
  scale_color_manual(values=c("#000000", "#E69F00", "#CC79A7", "#0072B2"))
ctd_stns_plot

#plotting zooplankton gear stations
zoop_stns <- results_TF_salmon %>% 
  filter(Bongo =='X'| Juday == 'X' | Tucker == 'X')

#plotting zooplankton gear stations
zoop_stns_plot <- ggplot() + geom_polygon(data = pacific_map, aes(x= long, y = lat, group = group), fill = 8, color = "black") +
  coord_cartesian(xlim = c(170, 241), ylim = c(42, 63), expand = F) +
  scale_x_continuous(breaks = c(180, 200, 220, 240), labels = c("180°", "160°W", "140°W", "120°W")) +
  scale_y_continuous(breaks = c(45, 50, 55, 60), labels = c("45°N", "50°N", "55°N", "60°N")) +
  #geom_point(data=station_data_2022, aes(x=lon, y=lat), size = 0.8, shape = 1) +
  geom_point(data=zoop_stns, aes(x=lon, y=lat, color = vessel_name)) +
  theme_bw() + 
  theme(panel.grid.major = element_line(colour = "transparent"), panel.grid.minor = element_line(colour = "transparent")) + 
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("C) Zooplankton Stations") + 
  theme(text=element_text(size=12)) +
  scale_color_manual(values=c("#000000", "#E69F00", "#CC79A7", "#0072B2"))
zoop_stns_plot

#making multi-panel plot
grid.arrange(fishing_stns_plot, ctd_stns_plot, zoop_stns_plot)
completed_stations_multipanel <- grid.arrange(fishing_stns_plot, ctd_stns_plot, zoop_stns_plot, nrow = 3)
completed_stations_multipanel
ggsave(here::here("Figures", "2022_expedition_completed_stations.pdf"), completed_stations_multipanel, width =200, height =250, units = "mm")
