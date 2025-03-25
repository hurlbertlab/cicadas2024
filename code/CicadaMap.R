library(ggplot2)
library(sf)
library(ggmap)
library(ggspatial)
library(scales)
library(dplyr)
library(ggrepel)

periodical.cicada.data = (read.csv("data/observations-534967.csv"))
fullDataset = read.csv("data/fullDataset_2024-08-17.csv")

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

bbox <- st_bbox(c(xmin = -80, xmax = -78.65, ymin = 35.3, ymax = 36.7), crs = 4326)
bbox_poly <- st_as_sfc(bbox)




ggmap(map) +
geom_sf(data = bbox_poly, aes(fill = "Cicada Distribution"), alpha = 0.5, inherit.aes = FALSE) +
geom_bin2d(data = periodical.cicada.data, aes(x = longitude, y = latitude, fill = "iNaturalist Observations"), bins = 100, alpha = 0.6) +
geom_point(data = sites, aes(x = longitude, y = latitude), color = "black", size = 3) + 
geom_label_repel(data = sites, aes(x = longitude, y = latitude, label = site_name), 
                   color = "black", size = 8, box.padding = 1.7) +
  labs(x = "Longitude", y = "Latitude", fill = "Legend") +
theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    legend.position = "bottom", 
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 15)) +
  scale_fill_manual(values = c("Cicada Distribution" = "maroon2", "iNaturalist Observations" = "yellow"))





EnoRiver <- filter(fullDataset, Name == "Eno River State Park", Year == 2024) %>%
  select(Name, Circle, Latitude, Longitude) %>%
  distinct(Circle, .keep_all = TRUE)

EnoRiver_sf <- st_as_sf(EnoRiver, coords = c("Longitude","Latitude"))

ERSP.area <- c(left = min(EnoRiver$Longitude, na.rm = TRUE) -.013,
          bottom = min(EnoRiver$Latitude, na.rm = TRUE) -.013,
          right = max(EnoRiver$Longitude, na.rm = TRUE) +.013,
          top = max(EnoRiver$Latitude, na.rm = TRUE) +.013)

Circles <- data.frame(
  Circle = c("Circle 1", "Circle 2", "Circle 3", "Circle 4", "Circle 5", "Circle 6", "Circle 7", "Circle 8"),
  latitude = c(36.0740347603, 36.0744491933, 36.0751177629, 36.0761118226, 36.0768227486, 36.0773659175, 36.0786444282, 36.0790969042),
  longitude = c(-79.0076410258, -79.0070628588, -79.0073105049, -79.0081902695, -79.0085027469, -79.0089654279, -79.0083854581, -79.0073614669))


Eno.map <- get_stadiamap(ERSP.area, zoom = 16, maptype = "stamen_terrain")

ggmap(Eno.map) +
  geom_point(data = Circles, aes(x = longitude, y = latitude), color = "black", pch = 19, size = 4, face = "bold") +
  geom_label_repel(data = Circles, aes(x = longitude, y = latitude, label = Circle), 
                   color = "black", size = 7, box.padding = 1.1) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
axis.title = element_text(size = 20, face = "bold"),
axis.text = element_text(size = 15))

