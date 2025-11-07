#Plotting Predation as a function of caterpillar count
#x axis: meanDensityByWeek
#y axis: birdPred

# Functions for working with and analyzing Caterpillars Count! data
library(dplyr)
library(lubridate)
library(data.table)
library(gsheet)
library(maps)
library(sf)
library(stringr)

fullDataset = read.csv("data/fullDataset_2025-07-28.csv")

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

Mode = function(x){ 
  if (!is.numeric(x)) {
    stop("values must be numeric for mode calculation")
  }
  ta = table(x)
  tam = max(ta)
  mod = as.numeric(names(ta)[ta == tam])
  return(max(mod))
}

surveyData = # merged dataframe of Survey and arthropod Sighting tables for a single site
  ordersToInclude = 'caterpillar'      

minLength = 0        
jdRange = c(1,365)
outlierCount = 10000
plot = FALSE
plotVar = 'fracSurveys' # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
minSurveyCoverage = 0.8 
allDates = TRUE
new = TRUE
color = 'black'
allCats = TRUE

###################################################################################
meanCatDensityByWeek = function(.data,
                             ordersToInclude = 'caterpillar',
                             Site = unique(fullDataset$Name),
                             Yr = c(2020:2025),
                             minLength = 0,         
                             jdRange = c(1,365),
                             outlierCount = 10000,
                             minSurveyCoverage = 0.8, 
                             allDates = TRUE,
                             new = TRUE,
                             allCats = TRUE,
                             ...)                  

{
  surveyData = filter(.data, 
                      Name %in% Site,
                      Year %in% Yr)
    
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$Group)
  }
  
  numUniqueBranches = length(unique(surveyData$PlantFK))
  
  firstFilter = surveyData %>%
    filter(julianday >= jdRange[1], julianday <= jdRange[2]) %>%
    mutate(julianweek = 7*floor(julianday/7) + 4)
  
  effortByWeek = firstFilter %>%
    group_by(Year, julianweek) %>%
    summarize(nSurveyBranches = n_distinct(PlantFK),
              nSurveys = n_distinct(ID)) %>%
    mutate(modalBranchesSurveyed = Mode(5*ceiling(nSurveyBranches/5)),
           nSurveySets = nSurveys/modalBranchesSurveyed,
           modalSurveySets = Mode(round(nSurveySets)),
           okWeek = ifelse(nSurveySets/modalSurveySets >= minSurveyCoverage, 1, 0))
  
  if (allDates) {
    effortByWeek$okWeek = 1
  }
  
  if (!allCats) {
    secondFilter = firstFilter %>%
      filter(Hairy != 1, Tented != 1, Rolled != 1)
  } else {
    secondFilter = firstFilter
  }
  print(second)
  catCount = secondFilter %>%
    filter(Length >= minLength, 
           Group %in% ordersToInclude) %>%
    mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% 
    group_by(Year, julianweek) %>%
    summarize(totalCount = sum(Quantity2, na.rm = TRUE),
              numSurveysGTzero = length(unique(ID[Quantity > 0])),
              totalBiomass = sum(Biomass_mg, na.rm = TRUE)) %>% 
    right_join(effortByWeek, by = 'julianweek') %>%
    filter(okWeek == 1) %>%
    mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0) %>%
    mutate(meanDensity = totalCount/nSurveys,
           fracSurveys = 100*numSurveysGTzero/nSurveys,
           meanBiomass = totalBiomass/nSurveys) %>%
    arrange(julianweek) %>%
    data.frame()
  
  return(catCount)
}
##########################################################################

meanDensityByWeek = function(surveyData,
                             ordersToInclude = 'All',       
                             
                             minLength = 0,         
                             jdRange = c(1,365),
                             outlierCount = 10000,
                             plot = FALSE,
                             plotVar = 'fracSurveys', # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
                             minSurveyCoverage = 0.8, 
                             allDates = TRUE,
                             new = TRUE,
                             color = 'black',
                             allCats = TRUE,
                             ...)                  

{
  surveyData = filter(fullDataset, 
                      
                      Name== "Eno River State Park",
                      Year== 2024)
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$Group)
  }
  
  numUniqueBranches = length(unique(surveyData$PlantFK))
  
  firstFilter = surveyData %>%
    filter(julianday >= jdRange[1], julianday <= jdRange[2]) %>%
    mutate(julianweek = 7*floor(julianday/7) + 4)
  
  effortByWeek = firstFilter %>%
    group_by(julianweek) %>%
    summarize(nSurveyBranches = n_distinct(PlantFK),
              nSurveys = n_distinct(ID)) %>%
    mutate(modalBranchesSurveyed = Mode(5*ceiling(nSurveyBranches/5)),
           nSurveySets = nSurveys/modalBranchesSurveyed,
           modalSurveySets = Mode(round(nSurveySets)),
           okWeek = ifelse(nSurveySets/modalSurveySets >= minSurveyCoverage, 1, 0))
  
  if (allDates) {
    effortByWeek$okWeek = 1
  }
  
  if (!allCats) {
    secondFilter = firstFilter %>%
      filter(Hairy != 1, Tented != 1, Rolled != 1)
  } else {
    secondFilter = firstFilter
  }
  
  arthCount = secondFilter %>%
    filter(Length >= minLength, 
           Group %in% ordersToInclude) %>%
    mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% 
    group_by(julianweek) %>%
    summarize(totalCount = sum(Quantity2, na.rm = TRUE),
              numSurveysGTzero = length(unique(ID[Quantity > 0])),
              totalBiomass = sum(Biomass_mg, na.rm = TRUE)) %>% 
    right_join(effortByWeek, by = 'julianweek') %>%
    filter(okWeek == 1) %>%
    mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0) %>%
    mutate(meanDensity = totalCount/nSurveys,
           fracSurveys = 100*numSurveysGTzero/nSurveys,
           meanBiomass = totalBiomass/nSurveys) %>%
    arrange(julianweek) %>%
    data.frame()
  
  if (plot & new) {
    plot(arthCount$julianweek, arthCount[, plotVar], type = 'l', 
         col = color, las = 1, ...)
    points(arthCount$julianweek, arthCount[, plotVar], pch = 16, col = color, ...)
  } else if (plot & new==F) {
    points(arthCount$julianweek, arthCount[, plotVar], type = 'l', col = color, ...)
    points(arthCount$julianweek, arthCount[, plotVar], pch = 16, col = color, ...)
  }
  return(arthCount)
}

url = "https://docs.google.com/spreadsheets/d/1hi7iyi7xunriU2fvFpgNVDjO5ERML4IUgzTkQjLVYCo/edit?gid=0#gid=0"


df = gsheet2tbl(url) %>%
  mutate(DeployDate = as.Date(DeployDate, format = "%m/%d/%Y"),
         CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y")) %>%
  mutate(AdjustedDate = DeployDate + 4,
         cicada_period = ifelse(AdjustedDate >= as.Date("2024-05-14") & AdjustedDate <= as.Date("2024-06-13"), 1, 0))%>%
  mutate(date = str_extract(AdjustedDate, pattern = "\\d+-\\d+-\\d+"),
    year = as.integer(str_extract(DeployDate, "\\d+")),
    leapyear = leap_year(year),
    julianday = case_when(
    str_detect(date, "-05") ~ 120 + leapyear + as.numeric(substr(date, 9, 10)),
    str_detect(date, "-06") ~ 151 + leapyear + as.numeric(substr(date, 9, 10)),
    str_detect(date, "-07") ~ 181 + leapyear + as.numeric(substr(date, 9, 10))),
    julianweek = 7*floor(julianday/7) + 4)%>%
  mutate(year = as.integer(str_extract(DeployDate, "\\d+")))

birdPred = df %>%
  filter('Not_Found' != 1) %>%
  group_by(year, julianweek, Name, julianday) %>%
  summarize(numBirdStrikes = sum(Bird),
            numClayCats = n(),
            pctBird = 100 * numBirdStrikes / numClayCats)

###grouping weeks
catData <- fullDataset%>%
  filter(Group == 'caterpillar')%>%
  mutate(siteyrday = str_c(as.character(Name), as.character(Year), as.character(julianday), sep = ', '))

claywindow = function(syd = catData$siteyrday){
  cw = birdPred%>%
    mutate(secondwk = julianday+6, firstwk = julianday-7)%>%
    mutate(siteyrday = str_c(as.character(Name), as.character(year), as.character(julianday), sep = ', '))%>%
    select(Name, year, julianweek, julianday, firstwk, secondwk, pctBird, siteyrday)
  mutate(catData, clayweek = case_when(julianday >= firstwk && julianday <= secondwk ~ pctBird))
}  
claywindow()


qa = function(v){mutate(fullDataset, test = v + 1)}
qa(fullDataset$julianday)

biweek = fullDataset %>%
  group_by(Name)%>%
  mutate(clayweek = 
         
         
         <- vector(mode = "character", length = length(julianday)))%>%
  for(i in 1:length(julianday)){
    clayweek[i] = clayday[fullDataset$julianday[i]+7 >= clayday & clayday >= fullDataset$julianday[i]-7]}


fullDataset%>%
  filter(julianweek == | julianweek - 1 == )

test <- fullDataset %>%
  left_join(birdPred, by = c("julianweek", "Year" = "year", "Name" = "Name")) %>%
  #left_join but instead of julianweek you'll have julianweek = 'jwk-1' or whatever you call that variable, then
  filter(!is.na(pctBird))
#then also left_join by julian week -1? but you need to make a new variable in birdPred for that

#####################

catCount = data.frame()
for (site in c("Eno River State Park",
               "Triangle Land Conservancy - Johnston Mill Nature Preserve",
               "Prairie Ridge Ecostation",
               "NC Botanical Garden",
               "UNC Chapel Hill Campus")){
     
     for (year in 2023:2025) {

      siteyr = data.frame()
      siteyr = rbind(siteyr, filter(fullDataset, Name == site, Year == year))
      catCountSY <- meanDensityByWeek(surveyData = siteyr, ordersToInclude = 'caterpillar',
                                   minLength = 0,        
                                   jdRange = c(1,365),
                                   outlierCount = 10000,
                                   plot = FALSE,
                                   minSurveyCoverage = 0.8,
                                   allDates = TRUE,
                                   new = TRUE,
                                   color = 'black',
                                   allCats = TRUE)%>%
        select(julianweek, nSurveys, meanDensity, fracSurveys, meanBiomass) %>%
        mutate(Name = site,
               year = year)
      
      
      catCount = rbind(catCount, catCountSY)
     }
}

predvcount = right_join(catCount, birdPred, by = c('year', 'julianweek', 'Name'))

pvcspec = function(site, yr){
  Site <- c()
  if("eno" %in% site){Site <- c(Site, "Eno River State Park")}
  if("jm" %in% site){Site <- c(Site, "Triangle Land Conservancy - Johnston Mill Nature Preserve")}
  if("pr" %in% site){Site <- c(Site, "Prairie Ridge Ecostation")}
  if("ncbg" %in% site){Site <- c(Site, "NC Botanical Garden")}
  if("unc" %in% site){Site <- c(Site, "UNC Chapel Hill Campus")}
  if("any" %in% site){Site <- c("Eno River State Park",
                                "Triangle Land Conservancy - Johnston Mill Nature Preserve",
                                "Prairie Ridge Ecostation",
                                "NC Botanical Garden",
                                "UNC Chapel Hill Campus")}
    pvcplot = filter(predvcount, (Name %in% Site | Name %in% site), year %in% yr)%>%
      mutate(color = case_when(Name == "Eno River State Park" ~ "orange",
                               Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve" ~ "darkgreen",
                               Name == "Prairie Ridge Ecostation" ~ "purple",
                               Name == "NC Botanical Garden" ~ "blue",
                               Name == "UNC Chapel Hill Campus" ~ "lightblue"))%>%
      mutate(pch = case_when(year == 2024 ~ 1,
                             year == 2025 ~ 18))
    #print(pvcplot)
    par(mar = c(5, 6, 8, 2))
    l = lm(pctBird ~ fracSurveys, data = pvcplot, subset = )
    m = lm(pctBird ~ fracSurveys, data = pvcplot)
    plot(pvcplot$fracSurveys, pvcplot$pctBird,
     xlab = "Caterpillar Density", ylab = "Predation", ylim = c(0, 50), 
     xlim = c(0, 50),
     xaxt = "n", 
     #cex.axis = axissize, cex.lab = labelsize, type = type, 
     col = pvcplot$color, cex = 2, pch = pvcplot$pch,
     main = c(unique(pvcplot$Name), yr), cex.main = 1)
    axis(1, at = seq(0, 100, by = 10), las = 1)
    abline(l)
    summary(l)
}
pvcspec(site = c('pr'), yr = c(2024))


    