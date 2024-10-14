#############################
#
#############################

library(sf)
library(dplyr)
library(stringr)

#load in snake data
snake1 <- read.csv("copperheads/data/snakes/inat-snakes1.csv") %>%
  select(-place_county_name, -place_state_name, -place_country_name)
snake2 <- read.csv("copperheads/data/snakes/inat-snakes2.csv") %>%
  select(-observed_on_string, -sound_url,-oauth_application_id)
snake3 <- read.csv("copperheads/data/snakes/inat-snakes3.csv") %>%
  select(-observed_on_string, -sound_url,-oauth_application_id)
#combine snakes
snakes <- rbind(snake1,snake2)
snakes <- rbind(snakes,snake3) #445,000 rows
#rm
rm(snake1,snake2,snake3) 
#gbif download
#may need to be unzipped from "copperheads/data/snakes/inat-serpentes.zip" first
#snakes <- read.csv("copperheads/data/snakes/inat-serpentes.csv") #381,000 rows

#add in year information
snakes <- snakes %>%
  mutate(year = substr(observed_on, start = 1, stop = 4))

#load in copperheads
copperheads <- read.csv("copperheads/data/snakes/inat-copperheads.csv")
#confirm all copperheads in snake data
miss_copper <- anti_join(copperheads, snakes, by = "id") #about 3,801 copperhead observations that are NOT included.... where are they I wonder? let's exclude Texas because no cicadas in Texas, similarly we can exclude Florida
miss_copper <- miss_copper %>%
  mutate(txfl = str_detect(string = place_guess, pattern = "Texas|Florida|TX|FL")) %>%
  filter(txfl == FALSE) #great, that cuts it down by to 2522 obs

table(miss_copper$geoprivacy)
#ah, these are all records with obscured geoprivacy. Despite that do we have a lat/lon? Yes, but these aren't acurate enough to say where within the states these individuals were observed. The 41 observations with blank geoprivary (eg. not obscured or private), are below 30 latitude and not within cicada area anyway. Okay, confirmed that we have all the copperheads we want within the snakes df.

#load in cicada and county data
cicada <- st_read(dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb")

emergence_years <- read.csv("copperheads/data/cicada/cicada_emergence_years.csv")

#GBIF turn the snake lat/lon into a point eg. make geometry
#snakes_geom <- st_as_sf(snakes, coords = c('decimalLongitude', 'decimalLatitude'), crs = st_crs(cicada))

#iNat direct download turn the snake lat/lon into a point eg. make geometry
snakes_geom <- st_as_sf(snakes, coords = c('longitude', 'latitude'), crs = st_crs(cicada))

#overlay the snake points on the cicada counties, and extract which ST_CNTY_CODE each snake observation is part of. Not all snakes will have a ST_CNTY_CODE and these represent snake points that lie outside the boundaries of cicada broods
#let's try this way:
snakes_county <- 
  st_join(snakes_geom, cicada, join = st_within) %>%
  filter(!is.na(STATEFP))
#GBIF cool, so we have 178,000 snake observations within the area. 
#yes, the iNat download is an improvement. There are 239,000 observations

n_snake_obs_county <-
  snakes_county %>%
  group_by(ST_CNTY_CODE) %>%
  summarize(n_snake_obs = n())

n_copperhead <-
  snakes_county %>%
  filter(scientific_name == "Agkistrodon contortrix") %>%
  group_by(ST_CNTY_CODE, scientific_name) %>%
  summarize(n_copperhead = n())
#uh. oddly enough, ST_CNTY_CODE 37183, WAKE CO NC, has the highest number of copperhead observations? by almost a double amount?? why I wonder..... also, should probably have a filter for the same day/location/time to the hour but a different observer. Because eg. two people photo and upload a copperhead or other snake photo.

n_snake_obs_county_year <- 
  snakes_county %>%
  group_by(ST_CNTY_CODE, year) %>%
  summarize(n_snake_obs = n()) %>%
  st_drop_geometry() %>%
  ungroup()

n_copperhead_year <- 
  snakes_county %>%
  filter(scientific_name == "Agkistrodon contortrix") %>%
  group_by(ST_CNTY_CODE, scientific_name, year) %>%
  summarize(n_copperhead = n()) %>%
  st_drop_geometry() %>%
  ungroup()

snakes_year_county <- 
  left_join(n_snake_obs_county_year, n_copperhead_year, by = c("ST_CNTY_CODE", "year")) %>%
  mutate(perc_copper = n_copperhead/n_snake_obs,
         perc_copper = ifelse(is.na(perc_copper), 0, perc_copper))

write.csv(snakes_year_county, "copperheads/data/snakes/snakes_county_year.csv", row.names = FALSE)

plot(
  x = snakes_year_county$perc_copper,
  y = snakes_year_county$n_snake_obs,
  col = snakes_year_county$year
)

plot(
  x = snakes_year_county$perc_copper[snakes_year_county$n_snake_obs >20],
  y = snakes_year_county$n_snake_obs[snakes_year_county$n_snake_obs >20],
  col = snakes_year_county$year
)


#While we're here, let's also get some information about the brood regions.
#so, I need to join together the cicada shapefiles based on the brood
cicada_brood <- 
  cicada %>%
  group_by(BROOD_NAME, YEAR_NEXT_EMERGENCE, CYCLE) %>%
  summarize(SHAPE = st_union(SHAPE))
#hey also, these might need to be limited to cicada brood counties within the copperhead's range.
#yes, there are some portions of eg. Ohio where cicada are but where the copperhead range does not extend. 
#Hm. Yes, also the gbif download does not have ALL the inat observations, because
#on iNaturalist there are 18,198 research grade observations. Yet from the gbif download
#I only have 11,000. That actually does make a big difference in how much data
#there is per year and the amount of area that data covers. 

plot(cicada_brood$SHAPE)
#works

snakes_brood<- 
  st_join(snakes_geom, cicada_brood, join = st_within) %>%
  filter(!is.na(BROOD_NAME))
#same number of observations as snakes_county, perf. 
# 
# n_snake_obs_brood_year <- 
#   snakes_brood %>%
#   group_by(BROOD_NAME, year, YEAR_NEXT_EMERGENCE, CYCLE) %>%
#   summarize(n_snakes = n()) %>%
#   st_drop_geometry()
# 
# n_copperhead_brood_year <-
#   snakes_brood %>%
#   filter(scientific_name == "Agkistrodon contortrix") %>%
#   group_by(BROOD_NAME, year, scientific_name) %>%
#   summarize(n_copper = n()) %>%
#   st_drop_geometry()
# 
# snakes_year_brood <- 
#   left_join(n_snake_obs_brood_year, n_copperhead_brood_year, by = c("BROOD_NAME", "year")) %>%
#   ungroup() %>%
#   mutate(perc_copper = n_copper/n_snakes) %>%
#   mutate(last_emergence = case_when(
#     YEAR_NEXT_EMERGENCE > 2024 ~ (YEAR_NEXT_EMERGENCE-CYCLE),
#     YEAR_NEXT_EMERGENCE < 2024 ~ (YEAR_NEXT_EMERGENCE),
#     TRUE ~ 2024
#   )) %>%
#   mutate(emergence_year = ifelse(test = (year == last_emergence), 1, 0)) %>%
#   #let's filter to brood years that have a minimum number of snakes observations, let's say at least 20 snakes had to be seen within the whole brood area.
#   filter(n_snakes >= 20)
# 
# write.csv(snakes_year_brood, "copperheads/data/snakes/snakes_brood_year.csv")

#there's some way to mark the years that cicadas emerge based on brood years, because there are some broods that have emerged twice within the dataset and should be reflected
snakes_year_brood <- read.csv("copperheads/data/snakes/snakes_brood_year.csv") %>%
  dplyr::rows_update(emergence_year, )
  left_join(emergence_years, by = c("BROOD_NAME", "CYCLE" = "cycle"))

write.csv(snakes_year_brood, "copperheads/data/snakes/snakes_year_brood.csv", row.names = FALSE)

#plot 
my_bar <- boxplot(perc_copper ~ emergence_year,
        data = snakes_year_brood,
        main = "Variation in % Copperheads in Emergence vs Non-emergence Years",
        xlab = "Emergence Year (0 or 1)",
        ylab = "Percent of snake inat observations that are copperheads",
        col = c("lightblue", "lightgreen"))

text(
  x = c(1,2),
  y = c(.10,.10),
  labels = c(
    paste0("n = ", table(snakes_year_brood$emergence_year)[1]),
    paste0("n = ", table(snakes_year_brood$emergence_year)[2])))
