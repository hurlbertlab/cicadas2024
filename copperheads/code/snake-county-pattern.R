#--------------------------
# Filter snakes to 2019+ and find out if a spatial pattern
# exists 
#--------------------------

#libraries
library(dplyr)
library(sf)
library(assertthat)

#load in snake data, this has already been filtered to only cicada counties
snakes_year_county <- read.csv("copperheads/data/snakes/snakes_county_year.csv") %>% 
  filter(year > 2018) %>% #just 2019 onwards, once we've got good iNat adoption rates.
  mutate(emergence_year = NA,
         BROOD_NAME = NA,
         cycle = NA)

#load in cicada and county data
cicada <- st_read(dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb")

emergence_years <- read.csv("copperheads/data/cicada/cicada_emergence_years.csv") %>%
  #filter to only the broods with emergence 2019 onwards
  filter(!is.na(emergence_2019_through_2024))

#get the unique broods
unique_broods <- unique(emergence_years$BROOD_NAME)

c <- data.frame()

#for loop for assigning the right cicada brood name + emergence to every county
for(i in 1:length(unique_broods)) {
  #filter cicada to get the counties it happens in
  ibroodcounties <- cicada %>% 
    filter(BROOD_NAME == unique_broods[i]) 
  ibroodcounties <- unique(ibroodcounties$ST_CNTY_CODE)
  
  #filter emergence based on the broods to get the years it happens it
  iemergyear <- emergence_years %>%
    filter(BROOD_NAME == unique_broods[i]) %>%
    dplyr::select(BROOD_NAME, emergence_2019_through_2024, cycle)
  icycle <- unique(iemergyear$cycle)
  assertthat::assert_that(length(icycle) == 1)
  
  #based on county+year, add if it was an emergence year or not and what brood it was.
  snakes_year_county <- snakes_year_county %>%
    mutate(
      #brood name
      BROOD_NAME = 
        case_when(
          ST_CNTY_CODE %in% ibroodcounties & 
          year %in% iemergyear$emergence_2019_through_2024 ~ unique_broods[i],
          .default = BROOD_NAME),
      #emergence year
      emergence_year = case_when(
        ST_CNTY_CODE %in% ibroodcounties & year %in% iemergyear ~ 1,
        .default = emergence_year
      ),
      #cycle
      cycle = case_when(
        ST_CNTY_CODE %in% ibroodcounties & year %in% iemergyear ~  icycle,
        .default = cycle
      ) #end cycle case_when
    ) #end mutate
} #end for loop

# okay, so now emergence year is either 1 or NA... 
# and what we want at the end is a -3:+3, where emergence year is actually a 0
# and we ? remove counties where there's brood emerging at all or like.. ig those could be our controls.. "controls" is an interesting concept in a statistical model like this. What's the oak paper do
#Ya, oak paper doesn't bother about oaks outside of emergence areas.
  snakes_year_county <- snakes_year_county %>%
    #filter to only the counties where a brood emerges at all.
    group_by(ST_CNTY_CODE) %>%
    filter(any(!is.na(BROOD_NAME))) %>%
    ungroup() %>%
    #make emergence_year year 0
    mutate(emergence_year = case_when(emergence_year == 1 ~ 0,
                                      .default = emergence_year))
  
  temp_nbroods <- snakes_year_county %>%
    group_by(ST_CNTY_CODE) %>%
    filter(!is.na(BROOD_NAME)) %>%
    summarize(n_broods = length(unique(BROOD_NAME)))
  
  #testing
  temp_syc_2broods <- snakes_year_county %>% 
           filter(ST_CNTY_CODE %in% c(13241,17075,17183,18091,26023,26149,37193,47009,47155))
  
  for(a in 1:nrow(temp_nbroods)) {
    
    temp_syc <- snakes_year_county %>% 
      filter(ST_CNTY_CODE == temp_nbroods$ST_CNTY_CODE[a])
    
    if(temp_nbroods$n_broods[a] == 1) {
      
      yr_emergence <- max(temp_syc$year[temp_syc$emergence_year == 0], na.rm = TRUE)
      #the max doesn't mean anything, bc there's only one emergence year. Just removing the NAs
      
      temp_syc <- temp_syc %>%
        mutate(emergence_year = year - yr_emergence)
      
    } else if(temp_nbroods$n_broods[a] == 2) {
      
      #get the latest emergence year
      yr_emergence_one <- max(temp_syc$year[temp_syc$emergence_year == 0], na.rm = TRUE)
      #get the earliest emergence year
      yr_emergence_two <- min(temp_syc$year[temp_syc$emergence_year == 0], na.rm = TRUE) 
      #create two emergence_years, take the min, don't replace if emergence year is a 0
      temp_syc <- temp_syc %>%
        mutate(emone = year - yr_emergence_one,
               emtwo = year - yr_emergence_two,
               emergence_year = case_when(
                 emergence_year == 0 ~ emergence_year, #keep the same
                 emergence_year != 0 ~ pmin(emone, emtwo, na.rm = TRUE)
               )) %>%
       dplyr::select(-emone, -emtwo)
      
    } else {
      #error out
      assertthat::assert_that(1 == 2, msg = "There's a brood without 1 or 2 BROOD_NAMES (eg. 0 brood names or 3 brood names)")
    }
    
    #dplyr update rows
    snakes_year_county <- snakes_year_county %>%
      dplyr::rows_update(temp_syc, by = c("ST_CNTY_CODE", "year"))
    
    
  }
  
  #so the only issue we have is if multiple broods use the same counties in different years
  c <- snakes_year_county %>%
    group_by(ST_CNTY_CODE) %>%
    summarize(n_broods = length(unique(BROOD_NAME, na.rm = TRUE)))
  #okay, so it just needs to be the kind of thing where if there's more than one brood, time to emergence year ought to be calculated for both broods (as a temp variable) and then just take the smallest. easy-peasy
  #like get list of unique like, BROOD_NAMEs, filter out NA from that list, then run a if then for if there's 1 or 2 names.
  
  #and then add the geometry by ST_CNTY_CODE and let's heatmap max(perc_copper)? still the spatial-temporal aspect is much more important than JUST the spatial aspect but just for a fun little look c: 
  
  
  
  

#ah, you know, I suppose it doesn't even matter WHAT brood it is, in the 
#cases where multiple broods overlap. The important thing is that there is a brood
#not which brood specifically it is? Unless like, I want to look across a brood's range
#and look at broods independently?
#better mutate and then filter. And then bind-rows. I can make a dataset where I remove
#duplicate ST_CNTY_CODE years and otherwise then I can filter to each brood's counties
#and...ask..that question...but even THEN it matters when there's a brood. This analysis
#should look at either each COUNTY and ask if copperhead %s are higher in emergence years,
#OR, ACROSS A BROOD AREA, are those counties higher in emergence years. In which case again, why seperate out by which brood it's happening in? shouldn't matter.



snakes_year_county %>%
  filter(
    ST_CNTY_CODE %in% ibroodcounties & year %in% iemergyear
  )


#if not NA, then duplicate the row and fill in with new information for the latest brood

c <- snakes_year_county %>% 
  left_join(emergence_years, by = c("year" = "emergence_year"))

#no, filter to brood and then add true when years is %in% list of years for that brood.
