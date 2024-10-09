#####################
# read in the cicada map, add county shapefiles, and save.
#####################

library(sf)
library(dplyr)

gdb_path = "copperheads/data/cicada/S_USA.Periodical_Cicada_Brood.gdb"
layer_name = "Periodical_Cicada_Brood"

cicada <- st_read(dsn = gdb_path) %>%
  dplyr::mutate(ST_CNTY_CODE = case_when(
    ST_CNTY_CODE ==  "09005" ~ "09150", 
    ST_CNTY_CODE == "09003" ~ "09130", 
    ST_CNTY_CODE == "09001" ~ "09110",
    ST_CNTY_CODE == "09007" ~ "09170",
    ST_CNTY_CODE == "09009" ~ "09190", 
    TRUE ~ ST_CNTY_CODE
  ))
"The Brood tabular information was designed to be joined to County boundaries and to create map services for the public."

#one-time process of adding county boundaries, no longer needed.
      #load in the county boundaries for the whole US
      #downloaded from: https://www.sciencebase.gov/catalog/item/5eaa545982cefae35a22231f
      #path = "copperheads/data/GovernmentUnits_National_GDB.gdb"
      #bigmap <- st_read(dsn = path, layer = "GU_CountyOrEquivalent")
      
      #c <- left_join(cicada, bigmap, by = c("ST_CNTY_CODE" = "STCO_FIPSCODE"))
      
      #save the updated cicada that now has county information added.
      #st_write(c, dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb", layer = "Periodical_Cicada_Brood_W_County", driver = "OpenFileGDB")
      
      #plot(c) #yep, works!
#doesn't look quite the same as from the oak paper, but I think it's an artifact of the crs. If that turns out to be wrong, then we need another county boundaries to work from.

#to load in and plot the new .gdb this created
cicada <- st_read(dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb")
plot(cicada$SHAPE) 

