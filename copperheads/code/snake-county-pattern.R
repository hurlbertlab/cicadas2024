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
  #add new columns that we will fill in
  mutate(emergence_year = NA,
         BROOD_NAME = NA,
         cycle = NA)

#load in cicada and county data
cicada <- st_drop_geometry(st_read(dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb")) %>%
  dplyr::select(BROOD_CNTY_OCCURRENCE:NAME)

emergence_years <- read.csv("copperheads/data/cicada/cicada_emergence_years.csv")# %>%
  #filter to only the broods with emergence 2019 onwards
  #filter(!is.na(emergence_2019_through_2024))
  #okay so there are 5 broods. We ONLY care about the potential for double broods in counties if they are THESE broods. Double/triple/quad brood counties where there's only ONE..hm. No, because. there could be an emergence in 2018 and then again in 2019 and that affects predictions. Sucks to remove 161 counties tho. 

#connect each county with it's associated broods
county_broods <- cicada %>%
  left_join(emergence_years, by = "BROOD_NAME") %>%
  #we'll filter out counties with more than two broods. It gets too complicated
  filter(MULT_BROOD <= 2)




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

#every county that has only one brood needs to get that brood assigned.
#every county that has more than one brood needs to get BOTH of those broods assigned and then have emergence1 + emergence2 within the 2019-2024 time frame. 
#so. actually every county needs to just have two broods assigned to start with based on matching in the 2019/2024 time frame. 
#nah, ignore the time frame. What if we add new data this summer? Go back to even earlier stuff that Bella did with the nestboxes to add these in.


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
                                      .default = emergence_year)) %>%
    #add emergence years
    left_join(emergence_years, by = "BROOD_NAME")

# okay so the idea is create cicada year values for both emergence 1 and 2 and then use the minimum absolute value between the two to create a standardized cicada year value. 
  
  snakes_year_county <- snakes_year_county %>%
    #make new columns
    mutate(em1_cicada_year = NA,
           em2_cicada_year = NA) %>%
    #populate new columns
    mutate(em1_cicada_year = case_when(
      year == emergence_three ~ 0,
      FALSE ~ em1_cicada_year
    ),
    em2_cicada_year = case_when(
      year == emergence_four ~ 0,
      FALSE ~ em2_cicada_year
    ))
  
  # So now each emergence Year is either 0 or NA, for both emergence 1 and 2... 
  # So now, fill in NA with some integer that indicates how far away it is from an emergence year 
  # em1_cicada_year == Year - emergence_three
  snakes_year_county_C <- snakes_year_county %>%
    mutate(new_yar = year)
  
    mutate(em1_cicada_year = ifelse(is.na(em1_cicada_year) == TRUE, year - emergence_three, em1_cicada_year),
           em2_cicada_year = ifelse(is.na(em2_cicada_year) == TRUE, year - emergence_four, em2_cicada_year)) 
  
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
