# Script for analyzing clay caterpillar predation

library(dplyr)
library(gsheet)
library(lubridate)

# new comment

url = "https://docs.google.com/spreadsheets/d/1hi7iyi7xunriU2fvFpgNVDjO5ERML4IUgzTkQjLVYCo/edit?gid=0#gid=0"

df = gsheet2tbl(url) %>%
  mutate(DeployDate = as.Date(DeployDate, format = "%m/%d/%Y"),
         CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y"))

birdPred = df %>%
  filter('Not_Found' != 1) %>%
  group_by(Name, DeployDate) %>%
  summarize(numBirdStrikes = sum(Bird),
            numClayCats = n(),
            pctBird = 100*numBirdStrikes/numClayCats) %>%
  mutate(jd = yday(DeployDate)+4)
    
  
plot(birdPred$jd[birdPred$Name == "Prairie Ridge Ecostation"], birdPred$pctBird[birdPred$Name == "Prairie Ridge Ecostation"], xlab = "Julian day", 
     ylab = "% Bird Strikes", ylim = c(0, 60), xlim = c(135, 185), type = 'b', 
     col = 'red', cex = 1, pch = 25, lwd = 2)

points(birdPred$jd[birdPred$Name == "NC Botanical Garden"], birdPred$pctBird[birdPred$Name == "NC Botanical Garden"], 
       type = 'b', col = 'green2', pch = 17, cex = 1, lwd = 2)

points(birdPred$jd[birdPred$Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve"], birdPred$pctBird[birdPred$Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve"], 
       type = 'b', col = 'darkgoldenrod', pch = 19, cex = 1, lwd = 2)

points(birdPred$jd[birdPred$Name == "Eno River State Park"], birdPred$pctBird[birdPred$Name == "Eno River State Park"], 
       type = 'b', col = 'magenta', pch = 21, cex = 1, lwd = 2)

points(birdPred$jd[birdPred$Name == "UNC Chapel Hill Campus"], birdPred$pctBird[birdPred$Name == "UNC Chapel Hill Campus"], 
       type = 'b', col = 'navy', pch = 18, cex = 1.25, lwd = 2)


legend("topright", legend = c("Prairie Ridge Ecostation", 
                             "NC Botanical Garden",
                             "Eno River State Park",
                             "UNC Chapel Hill Campus",
                             "Johnston Mill"),
       col = c("red", "green2", "magenta", "navy", "darkgoldenrod"),
       pch = c(25, 17, 21, 18, 19),
       lwd = 2,
       cex = 1)




















Strikes<- filter(df, Bird==1)

# Filtered Bird Strikes to get data on all bird strikes on clay caterpillars

NotFound<- filter(df, Not.Found==1)

# Filtered data to find the amount of clay caterpillars not found in field.

ERSP<- filter(Strikes, Name== 'Eno River State Park')
count(ERSP, CollectionDate)

UNC<- filter(Strikes, Name== 'UNC Chapel Hill Campus')
count(UNC, CollectionDate)

PR<- filter(Strikes, Name== 'Prairie Ridge Ecostation')
count(PR, CollectionDate)

JM<- filter(Strikes, Name== 'Triangle Land Conservancy - Johnston Mill Nature Preserve')
count(JM, CollectionDate)

NCBG<- filter(Strikes, Name== 'NC Botanical Garden')
count(NCBG, CollectionDate)

# Filtered for each site that had strike marks. Then counted for each site with Collection Date.

