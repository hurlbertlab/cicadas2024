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
     col = '#CC79A7', cex = 1.5, pch = 15, lwd = 2)

points(birdPred$jd[birdPred$Name == "NC Botanical Garden"], birdPred$pctBird[birdPred$Name == "NC Botanical Garden"], 
       type = 'b', col = '#D55E00', pch = 17, cex = 1.5, lwd = 2)

points(birdPred$jd[birdPred$Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve"], birdPred$pctBird[birdPred$Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve"], 
       type = 'b', col = 'yellow3', pch = 18, cex = 1.5, lwd = 2)

points(birdPred$jd[birdPred$Name == "Eno River State Park"], birdPred$pctBird[birdPred$Name == "Eno River State Park"], 
       type = 'b', col = 'black', pch = 8, cex = 1.5, lwd = 2)

points(birdPred$jd[birdPred$Name == "UNC Chapel Hill Campus"], birdPred$pctBird[birdPred$Name == "UNC Chapel Hill Campus"], 
       type = 'b', col = '#0072B2', pch = 16, cex = 1.5, lwd = 2)


legend("topright", legend = c("Prairie Ridge Ecostation", 
                             "NC Botanical Garden",
                             "Eno River State Park",
                             "UNC Chapel Hill Campus",
                             "Johnston Mill"),
       col = c("#CC79A7", "#D55E00", "black", "#0072B2", "yellow3"),
       pch = c(15, 17, 8, 16, 18),
       lwd = 2,
       cex = 1)
