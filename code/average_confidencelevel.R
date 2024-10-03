#creating data frame for average confidence level of each bird species
library(dplyr)

#sites = c("eno", "jmill", "ncbg", "pridge", "unc")

#confidence_levels = data.frame(site = "ex",
                               file = "ex",
                               start = 1,
                               end = 1, 
                               scientific_name = "ex", 
                               common_name = "ex",
                               confidence = 1)

#for (s in sites) {
  
  files = list.files(paste0("data/birdcallsdata"))
  
  for (f in files)
    file <- read.csv(f)
  
}
#need to join sheet with BirdNET results to sheet with call times on it
wd <- "Z:/Databases/CaterpillarsCount/AudioMoth/BirdNetResults"
setwd("Z:/Databases/CaterpillarsCount/AudioMoth/BirdNetResults")
confirmed_calls = read.csv(paste(wd,"/bird_calls_confirmed.csv", sep=""))
#confirmed_calls = read.csv("bird_calls_confirmed")
#need to create a read.csv line of code that will go through each excel sheet with results and rbinds them.


# Joining by two columns 
df = df1 %>%
  left_join(df2, by = c('Start', 'SpeciesCall'))

df = df1 %>%
  left_join(df2, by = c('Start' = 'StartTime', 
                        'SpeciesCall' = 'Species'))
