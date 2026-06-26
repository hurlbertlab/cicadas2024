# Script for analyzing clay caterpillar predation
library(dplyr)
library(gsheet)
library(lubridate)
library(stringr)

# Cicada density index by site data
cicadaLevels = read.csv("data/cicada_noise_by_site_on_day143.csv") %>%
  mutate(Name = case_when(site == "eno" ~ "Eno River State Park",
                          site == "jmill" ~ "Triangle Land Conservancy - Johnston Mill Nature Preserve",
                          site == "ncbg" ~ "NC Botanical Garden",
                          site == "unc" ~ "UNC Chapel Hill Campus",
                          site == "pridge" ~ "Prairie Ridge Ecostation")) %>%
  select(Name, cicadaIndex)

mean_cicada_across_sites = mean(cicadaLevels$cicadaIndex)

# Read in clay caterpillar predation data
url = "https://docs.google.com/spreadsheets/d/1hi7iyi7xunriU2fvFpgNVDjO5ERML4IUgzTkQjLVYCo/edit?gid=0#gid=0"
#ForestCover = read.csv("data/ForestCover.csv")
#LandscapeCover = read.csv("data/sites_2022-09-19.csv")
#LandscapePrairie = read.csv("data/prairieridgeforest.csv")


dat_clay = gsheet2tbl(url) %>%
  mutate(DeployDate = as.Date(DeployDate, format = "%m/%d/%Y"),
         CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y"),
         year = str_extract(DeployDate, "\\d+"),
         # Floor to Monday of the sampling week, then subtract 1 day to get Sunday.
         # A sampling bout occurs during a week Monday-Friday, with the average
         # collection date across sites occurring on a Wednesday.
         # Sunday represents the average midpoint of caterpillar deployment
         # and is chosen as the day of year to represent predation activity.
         collect_sunday = floor_date(CollectionDate, unit = "week", week_start = 1) - days(1),
         bout_doy = yday(collect_sunday),
         Period = ifelse(bout_doy >= 121 & bout_doy < 160, "cicada", "post_cicada"),
         
  ) %>%
  
  # Assign sequential bout numbers within each year
  group_by(year) %>%
  mutate(
    # dense_rank correctly assigns the same bout number to all observations
    # sharing the same collect_sunday within a year, which is all sites
    # visited during the same sampling week
    Bout = dense_rank(collect_sunday)
  ) %>%
  ungroup() %>%
  left_join(cicadaLevels, by = 'Name') %>%
    # center cicada index values
  mutate(cicadaIndex_c = cicadaIndex - mean_cicada_across_sites)

# Center the bout_doy
bout_doy_center <- dat_clay %>%
  distinct(year, Bout, bout_doy) %>%
  pull(bout_doy) %>%
  mean()

cat("Bout doy centering value (Sunday-based):", round(bout_doy_center, 1), "\n")

dat_clay$bout_day_c = dat_clay$bout_doy - bout_doy_center

