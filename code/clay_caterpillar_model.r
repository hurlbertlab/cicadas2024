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


df = gsheet2tbl(url) %>%
  mutate(DeployDate = as.Date(DeployDate, format = "%m/%d/%Y"),
         CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y"),
         AdjustedDate = DeployDate + 4,    # centering date to middle of the week of deployment
         doy = yday(AdjustedDate),
         Period = ifelse(doy >= 121 & doy < 160, "cicada", "post_cicada"),
         year = str_extract(DeployDate, "\\d+"),
         # Floor each DeployDate to the Monday of its sampling week.
         # week_start = 1 specifies Monday as the first day of the week.
         # This produces the same Monday for all sites visited during the
         # same Mon-Fri window, regardless of which specific day each site
         # was visited, and regardless of year-to-year shifts in calendar dates.
         collect_monday = floor_date(CollectionDate, unit = "week", week_start = 1),
         
         # Day of year of the deployment Monday.
         # This is the common temporal reference assigned to ALL observations
         # within the same bout, overriding the site-specific doy values
         # that reflect consecutive-day site visits. Assigning the Monday doy
         # avoids the confound between site visitation order and apparent
         # temporal position discussed earlier.
         bout_doy = yday(collect_monday)
  ) %>%
  
  # Assign sequential bout numbers within each year
  group_by(year) %>%
  mutate(
    # dense_rank correctly assigns the same bout number to all observations
    # sharing the same deploy_monday within a year, which is all sites
    # visited during the same sampling week
    Bout = dense_rank(collect_monday)
  ) %>%
  ungroup() %>%
  left_join(cicadaLevels, by = 'Name') %>%
    # center cicada index values
  mutate(cicadaIndex_c = cicadaIndex - mean_cicada_across_sites)


