# Script for analyzing clay caterpillar predation
library(dplyr)
library(gsheet)
library(lubridate)

url = "https://docs.google.com/spreadsheets/d/1hi7iyi7xunriU2fvFpgNVDjO5ERML4IUgzTkQjLVYCo/edit?gid=0#gid=0"
ForestCover = read.csv("data/ForestCover.csv")
LandscapeCover = read.csv("data/sites_2022-09-19.csv")
LandscapePrairie = read.csv("data/prairieridgeforest.csv")

df = gsheet2tbl(url) %>%
  mutate(DeployDate = as.Date(DeployDate, format = "%m/%d/%Y"),
         CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y")) %>%
  mutate(AdjustedDate = DeployDate + 4,
         cicada_period = ifelse(AdjustedDate >= as.Date("2024-05-14") & AdjustedDate <= as.Date("2024-06-13"), 1, 0)) 

    
glm_bird_pred <- glm(Bird ~ Name + cicada_period, data = df, family = binomial)
summary(glm_bird_pred)


birdPred = df %>%
  filter('Not_Found' != 1) %>%
  group_by(Name, DeployDate) %>%
  summarize(numBirdStrikes = sum(Bird),
            numClayCats = n(),
            pctBird = 100 * numBirdStrikes / numClayCats) %>%
  mutate(AdjustedDate = DeployDate + 4)

x_min <- as.Date("2024-05-14")
x_max <- max(birdPred$AdjustedDate, na.rm = TRUE)

par(mar = c(6, 6, 5, 2))
plot(birdPred$AdjustedDate[birdPred$Name == "Prairie Ridge Ecostation"], 
     birdPred$pctBird[birdPred$Name == "Prairie Ridge Ecostation"], 
     xlab = "Date", ylab = "% Bird Strikes", ylim = c(0, 60), 
     xlim = c(x_min, x_max),
     xaxt = "n", 
     cex.axis = 2, cex.lab = 2, type = 'b', 
     col = 'black', cex = 2.7, pch = 8, lwd = 2)

axis.Date(1, at = seq(x_min, x_max, by = "12 days"), format = "%b %d", cex.axis = 2)

points(birdPred$AdjustedDate[birdPred$Name == "NC Botanical Garden"], 
       birdPred$pctBird[birdPred$Name == "NC Botanical Garden"], 
       type = 'b', col = '#D55E00', pch = 17, cex = 2.7, lwd = 2)

points(birdPred$AdjustedDate[birdPred$Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve"], 
       birdPred$pctBird[birdPred$Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve"], 
       type = 'b', col = '#CC79A7', pch = 18, cex = 2.7, lwd = 2)

points(birdPred$AdjustedDate[birdPred$Name == "Eno River State Park"], 
       birdPred$pctBird[birdPred$Name == "Eno River State Park"], 
       type = 'b', col = '#0072B2', pch = 16, cex = 2.7, lwd = 2)

points(birdPred$AdjustedDate[birdPred$Name == "UNC Chapel Hill Campus"], 
       birdPred$pctBird[birdPred$Name == "UNC Chapel Hill Campus"], 
       type = 'b', col = 'yellow3', pch = 15, cex = 2.7, lwd = 2)

legend("topright", legend = c("Prairie Ridge Ecostation", 
                              "NC Botanical Garden",
                              "Eno River State Park",
                              "UNC Chapel Hill Campus",
                              "Johnston Mill",
                              "Shaded = Cicadas Present"),
       col = c("black", "#D55E00", "#0072B2", "yellow3", "#CC79A7", NA),
       pch = c(8, 17, 16, 15, 18, NA),
       lwd = 2,
       cex = 1.7)

rect(as.Date("2024-05-14"), 0, as.Date("2024-06-13") + 4, 60, col = rgb(0.5, 0.5, 0.5, 0.25), border = NA)

