#--------------------------
# Filter snakes to 2019+ and find out if a spatial pattern
# exists 
#--------------------------

#load in snake data, this has already been filtered to only cicada counties
snakes_year_county <- read.csv("copperheads/data/snakes/snakes_county_year.csv") %>% 
  filter(year > 2018) #just 2019 onwards, once we've got good iNat adoption rates.
#m. yeah need some way to add if it's been an emergence year or not.@
#hm, okay not really jamming away at this today though, I think I'm going to go grade student papers for the afternoon

#load in cicada and county data
cicada <- st_read(dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb")

emergence_years <- read.csv("copperheads/data/cicada/cicada_emergence_years.csv")


c <- snakes_year_county %>% 
  left_join(emergence_years, by = c("year" = "emergence_year"))
