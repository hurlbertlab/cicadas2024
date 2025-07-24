# Script for analyzing clay caterpillar predation
library(dplyr)
library(gsheet)
library(lubridate)
library(stringr)

url = "https://docs.google.com/spreadsheets/d/1hi7iyi7xunriU2fvFpgNVDjO5ERML4IUgzTkQjLVYCo/edit?gid=0#gid=0"
ForestCover = read.csv("data/ForestCover.csv")
LandscapeCover = read.csv("data/sites_2022-09-19.csv")
LandscapePrairie = read.csv("data/prairieridgeforest.csv")

df = gsheet2tbl(url) %>%
  mutate(DeployDate = as.Date(DeployDate, format = "%m/%d/%Y"),
         CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y")) %>%
  mutate(AdjustedDate = DeployDate + 4,
         cicada_period = ifelse(AdjustedDate >= as.Date("2024-05-14") & AdjustedDate <= as.Date("2024-06-13"), 1, 0))%>%
  mutate(date = str_extract(AdjustedDate, pattern = "\\d+-\\d+-\\d+"))%>% 
  mutate(doy = case_when(
    str_detect(date, "-05") ~ 121 + as.numeric(substr(date, 9, 10)),
    str_detect(date, "-06") ~ 152 + as.numeric(substr(date, 9, 10)),
    str_detect(date, "-07") ~ 182 + as.numeric(substr(date, 9, 10))))%>%
  mutate(year = str_extract(DeployDate, "\\d+"))
eno_0516_circle1_clipped.mp3

glm(Bird ~ Name * cicada_period, data = df, family = binomial)

#created a glm to analyze whether cicadas present had an effect on bird predation
glm_bird_pred <- glm(Bird ~ Name + cicada_period, data = df, family = binomial)
summary(glm_bird_pred)

interaction_bird_glm <- glm(Bird ~ Name * cicada_period, data = df, family = binomial)
summary(interaction_bird_glm)

#created birdPred from df to find the percent of bird predation
birdPred = df %>%
  filter('Not_Found' != 1) %>%
  group_by(year, DeployDate, doy, AdjustedDate, Name) %>%
  summarize(numBirdStrikes = sum(Bird),
            numClayCats = n(),
            pctBird = 100 * numBirdStrikes / numClayCats)


#setting the min and max on dates for the bird predation graph
#x_min <- 130
#x_max <- max(birdPred$AdjustedDate, na.rm = TRUE)
#x_max <- 190

#plotted bird strikes for each site and shaded during cicadas
predation = function(x_min = 130, x_max = 190,
                     sites = c("eno", "jm", "pr", "ncbg", "unc", ...), yr = c(2024, 2025, ...),
                     c24 = '#CC79A7', c25 = '#0072B2',
                     mode = 1,
                     type = 'b',
                     color24,
                     color25,
                     shape24 = c(8, 17, 15, 16, 18),
                     shape25 = c(8, 17, 15, 16, 18),
                     shsize = 2,
                     axissize = 1,
                     labelsize = 1,
                     mainsize = 1,
                     main,
                     ...) {
  if(mode == 1){
    color24 <- c(c24, c24, c24, c24, c24)
    color25 <- c(c25, c25, c25, c25, c25)
    }
  if(mode == 2){
    c1 <- c("red", "#D55E00", "#0072B2", "yellow3", "#CC79A7")
    c2 <- c("red", "#D55E00", "#0072B2", "yellow3", "#CC79A7")
  }
  
  ifelse("eno" %in% sites, s1 <- "Eno River State Park", s1 <- 0)
  ifelse("jm" %in% sites, s2 <- "Triangle Land Conservancy - Johnston Mill Nature Preserve", s2 <- 0)
  ifelse("pr" %in% sites, s3 <- "Prairie Ridge Ecostation", s3 <- 0)
  ifelse("ncbg" %in% sites, s4 <- "NC Botanical Garden", s4 <- 0)
  ifelse("unc" %in% sites, s5 <- "UNC Chapel Hill Campus", s5 <- 0)
  site <- c(s1, s2, s3, s4, s5)

  plot(birdPred$doy[birdPred$Name == site[1] & birdPred$year == yr[1]], 
     birdPred$pctBird[birdPred$Name == site[1] & birdPred$year == yr[1]], 
     xlab = "Date", ylab = "% Bird Strikes", ylim = c(0, 50), 
     xlim = c(x_min, x_max),
     xaxt = "n", 
     cex.axis = axissize, cex.lab = labelsize, type = type, 
     col = color24[1], cex = shsize, pch = shape24[1], lwd = 2,
     main = main, cex.main = mainsize)
  for(i in 1:5){
    points(birdPred$doy[birdPred$Name == site[i] & birdPred$year == yr[1]], 
           birdPred$pctBird[birdPred$Name == site[i] & birdPred$year == yr[1]], 
           type = type, col = color24[i], pch = shape24[i], cex = shsize, lwd = 2)
    points(birdPred$doy[birdPred$Name == site[i] & birdPred$year == yr[2]], 
           birdPred$pctBird[birdPred$Name == site[i] & birdPred$year == yr[2]], 
           type = type, col = color25[i], pch = shape25[i], cex = shsize, lwd = 2)}

  axis.Date(1, at = seq(x_min, x_max, by = 12), format = "%b %d", cex.axis = 1)
}


##function for plotting caterpillar predation
#explanation, see presets below for actual use
predation(sites = c("eno"#, "jm", "pr", "ncbg", "unc" #"eno", AND/OR "jm", AND/OR "pr", AND/OR "ncbg", AND/OR "unc"
                    )
          , yr = c(2024, 2025)
          , mode = 1
          #mode 1 differentiates year by color, so if you want sites would then be differentiated by shape, or not like below
          #, c25 = 'red', c24 = 'black'
          #, shape24 = c(8, 8, 8, 8, 8)
          #, shape25 = c(17, 17, 17, 17, 17)
          #mode = 2 differentiates site by color, so you would probably change shape by year
          , axissize = 1,
          mainsize = 1,
          labelsize = 1,
          main = "2024-2025 Clay Caterpillar Predation"
          )
#preset for 5 site-separated panels
preset1 = function(sites, main){
    predation(sites = sites, yr = c(2024, 2025), mode = 1, type = 'b', shape24 = c(8, 8, 8, 8, 8), shape25 = c(17, 17, 17, 17, 17),
            axissize = 1, mainsize = 1, labelsize = 1, main = main)}
par(mfrow = c(2, 3), mar = c(4, 5, 4, 1))
preset1(sites = c("eno"), main = "Clay Caterpillar Predation at Eno")
#preset 1 legend
legend("topleft", legend = c(2024, 2025),
       col = c("#CC79A7", "#0072B2"),
       pch = c(8, 17),
       lwd = 2,
       cex = 1)
preset1(sites = c("jm"), main = "Johnston Mill")
preset1(sites = c("pr"), main = "Prairie Ridge")
preset1(sites = c("ncbg"), main = "NCBG")
preset1(sites = c("unc"), main = "UNC")




 ####old stuff

plot(birdPred$AdjustedDate[birdPred$Name == "Prairie Ridge Ecostation"],
     birdPred$pctBird[birdPred$Name == "Prairie Ridge Ecostation"],
     xlab = "Julian day", ylab = "% Bird Strikes", ylim = c(0, 60),
     xlim = c(135, 185), type = 'b', 
     col = 'red', cex = 1, pch = 25, lwd = 2)

points(birdPred$AdjustedDate[birdPred$Name == "NC Botanical Garden"], 
       birdPred$pctBird[birdPred$Name == "NC Botanical Garden"], 
       type = 'b', col = '#D55E00', pch = 17, cex = 2.7, lwd = 2)

points(birdPred$AdjustedDate[birdPred$Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve"], 
       birdPred$pctBird[birdPred$Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve"], 
       type = 'b', col = '#CC79A7', pch = 15, cex = 2.7, lwd = 2)

points(birdPred$AdjustedDate[birdPred$Name == "Eno River State Park"], 
       birdPred$pctBird[birdPred$Name == "Eno River State Park"], 
       type = 'b', col = '#0072B2', pch = 16, cex = 2.7, lwd = 2)

points(birdPred$AdjustedDate[birdPred$Name == "UNC Chapel Hill Campus"], 
       birdPred$pctBird[birdPred$Name == "UNC Chapel Hill Campus"], 
       type = 'b', col = 'yellow3', pch = 18, cex = 2.7, lwd = 2)

legend("topright", legend = c("Prairie Ridge", 
                              "NCBG",
                              "Eno River",
                              "UNC Campus",
                              "Johnston Mill",
                              "Shaded = Cicadas"),
       col = c("black", "#D55E00", "#0072B2", "yellow3", "#CC79A7", NA),
       pch = c(8, 17, 16, 18, 15, NA),
       lwd = 2,
       cex = 1.7)

rect(as.Date("2024-05-14"), 0, as.Date("2024-06-13") + 4, 60, col = rgb(0.5, 0.5, 0.5, 0.25), border = NA)

