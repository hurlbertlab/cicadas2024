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
  filter(Not.Found != 1) %>%
  group_by(Name, DeployDate) %>%
  summarize(numBirdStrikes = sum(Bird),
            numClayCats = n(),
            pctBird = 100*numBirdStrikes/numClayCats) %>%
  mutate(jd = yday(DeployDate))
    
  
plot(birdPred$jd[birdPred$Name == "Prairie Ridge Ecostation"], birdPred$pctBird[birdPred$Name == "Prairie Ridge Ecostation"], xlab = "Julian day", 
     ylab = "% Bird Strikes", ylim = c(0, 60), xlim = c(130, 185), type = 'b', 
     col = 'red', cex = 2, pch = 16, lwd = 3)

points(birdPred$jd[birdPred$Name == "NC Botanical Garden"], birdPred$pctBird[birdPred$Name == "NC Botanical Garden"], 
       type = 'b', col = 'aquamarine', pch = 17, cex = 2, lwd = 3, lty = 'dashed')

points(birdPred$jd[birdPred$Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve"], birdPred$pctBird[birdPred$Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve"], 
       type = 'b', col = 'darkgoldenrod2', pch = 11, cex = 2, lwd = 3)

points(birdPred$jd[birdPred$Name == "Eno River State Park"], birdPred$pctBird[birdPred$Name == "Eno River State Park"], 
       type = 'b', col = 'magenta', pch = 21, cex = 2, lwd = 3, lty = 'dotted')

points(birdPred$jd[birdPred$Name == "UNC Chapel Hill Campus"], birdPred$pctBird[birdPred$Name == "UNC Chapel Hill Campus"], 
       type = 'b', col = 'navy', pch = 6, cex = 2, lwd = 3, lty = 'dotdash')


legend("topright", legend = c("Prairie Ridge Ecostation", 
                             "NC Botanical Garden",
                             "Eno River State Park",
                             "UNC Chapel Hill Campus",
                             "Johnston Mill"),
       col = c("red", "aquamarine", "magenta", "navy", "darkgoldenrod2"),
       pch = c(16, 17, 21, 6, 11),
       lwd = 3,
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

