library(ggplot2)
library(sf)
library(ggmap)
library(ggspatial)
library(scales)
library(dplyr)
library(ggrepel)

periodical.cicada.data = (read.csv("data/observations-534967.csv"))

periodical.cicada.data <- periodical.cicada.data %>%
  select(latitude, longitude, common_name)

Cicada_sf <- st_as_sf(periodical.cicada.data, coords = c("longitude","latitude"), crs = 4326)

area <- c(left = min(periodical.cicada.data$longitude, na.rm = TRUE) -.05,
          bottom = min(periodical.cicada.data$latitude, na.rm = TRUE) -.05,
          right = max(periodical.cicada.data$longitude, na.rm = TRUE) +.05,
          top = max(periodical.cicada.data$latitude, na.rm = TRUE) +.05)

map <- get_stadiamap(area, zoom = 9, maptype = "stamen_terrain")

sites <- data.frame(
  site_name = c("UNC Chapel Hill", "Eno River", "Johnston Mill", "Prairie Ridge", "NC Botanical Garden"),
  latitude = c(35.90988, 36.07690, 35.99585, 35.80985, 35.89940),
  longitude = c(-79.04964, -79.00720, -79.05379, -78.71775, -79.03390))


sites_sf <- st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326)

ggmap(map) +
  geom_bin2d(data = periodical.cicada.data, aes(x = longitude, y = latitude), bins = 80, alpha = 0.6) +
  scale_fill_gradientn(colors = c("yellow", "orange", "red"), name = "Cicada Density") +
  geom_point(data = sites, aes(x = longitude, y = latitude), color = "black", size = 1)+ 
  geom_label_repel(data = sites, aes(x = longitude, y = latitude, label = site_name), 
                  color = "darkblue", size = 5, box.padding = 1.7) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(fill = "gray90"))
  

