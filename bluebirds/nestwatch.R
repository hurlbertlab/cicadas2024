# load in packages
library(stringr)
library(sf)
library(dplyr)
library(tidyverse)
library(tmap)
# load in data
setwd("C:/git/cicadas2024/code")
nestwatch <- read.csv("nestwatch_summaries.csv") %>%
  filter(Species.Code == "easblu")
glimpse(nestwatch)
# County polygon
nc_counties <- read_sf("https://drive.google.com/uc?export=download&id=1hbaRC3hu6h9IMN4sNjY_BJhHyMTpMIT9")
nc_counties <- nc_counties %>% filter(County == "Orange")
glimpse(nc_counties)
# use library(sf) to make lat lon geometry in nestwatch, the st_to_sf function I believe,
# in arguments use crs = crs(polygonfromclass)
nestwatch_geom <- st_as_sf(nestwatch, coords = c('Longitude', 'Latitude'), crs = st_crs(nc_counties))
# and then use sf_join or sf_extract to extract the nestwatch GEOMETERIES that are within the polygon
nestwatch_county <- st_join(nestwatch_geom, nc_counties, join = st_within) %>%
  filter(County == "Orange")
#so, nestwatch may not yet be updated for 2024.
## This is the point where I realized I didn't really know what was going on and I decided to pivot
nc_nestwatch <- nestwatch %>%
  filter(Subnational.Code == "US-NC")
# Filter nestwatch data to include Eastern Bluebird nestboxes in NC recorded in 2024
# This is probably all of the Carol Woods observations?
nc_nestwatch <- nestwatch %>%
  filter(Subnational.Code == "US-NC",Species.Code == "easblu", Year == "2024")

# Nestboxes in NC 
nc_nestobs_plot <- plot(nc_nestwatch$Longitude,nc_nestwatch$Latitude)

# Orange County Polygon
orange_county <- nc_counties %>%
  filter(County == "Orange")

# Mapping I Guess???
map1 <- tm_graticules(nc_nestwatch) + tm_dots(col = "blue")
map2 <- tm_shape(orange_county) + tm_polygons(col = "lightblue")
map2 + map1 + tm_layout(title="Orange County Nestboxes 2024")
#see the github cicada2024/copperheads/code/snake-extract-county