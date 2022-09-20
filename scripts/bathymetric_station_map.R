# Basic Stations Map with Batymetry

library(ggOceanMaps)
library(tidyverse)
library(readxl)
library(here)

# Edit which vessels, years, or sampling event types you would like to plot
vessel <- c("TINRO", "NW", "Shimada", "Franklin", "Kaganovsky", "Legacy")
sampling_year <- c(2022)
#event_types could include "Trawl", "CTD" or "CTD-Rosette"
event_types <- c("Trawl")



iys_data <- read_excel(here("data", "IYS_Integrated_Data_Collection_V2.xlsx"), 
                       sheet = "Sampling_Events") |> 
  filter(vessel_name_abbr %in% vessel,
         year %in% sampling_year,
         event_type %in% event_types) |> 
  rename("Vessel" = "vessel_name_abbr") |> 
  mutate(Vessel = ifelse(Vessel == "NW", "NW Explorer", Vessel))

basemap(data = iys_data,  rotate = TRUE, bathymetry = TRUE, lon.interval = 3) +
  guides(fill="none")+ # Removes bathymetry scale legend
  geom_spatial_point(data = iys_data, aes(x = longitude_start_decdeg,
                                          y = latitude_start_decdeg, 
                                          colour = Vessel,
                                          ))+
  annotation_scale(location = "br") + 
  annotation_north_arrow(location = "tr", which_north = "true")
