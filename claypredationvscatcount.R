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

fullDataset = read.csv("data/fullDataset_2024-08-17.csv")

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


url = "https://docs.google.com/spreadsheets/d/1hi7iyi7xunriU2fvFpgNVDjO5ERML4IUgzTkQjLVYCo/edit?gid=0#gid=0"


df = gsheet2tbl(url) %>%
  mutate(DeployDate = as.Date(DeployDate, format = "%m/%d/%Y"),
         CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y")) %>%
  mutate(AdjustedDate = DeployDate + 4,
         cicada_period = ifelse(AdjustedDate >= as.Date("2024-05-14") & AdjustedDate <= as.Date("2024-06-13"), 1, 0))%>%
  mutate(date = str_extract(AdjustedDate, pattern = "\\d+-\\d+-\\d+"),
    doy = case_when(
    str_detect(date, "-05") ~ 121 + as.numeric(substr(date, 9, 10)),
    str_detect(date, "-06") ~ 152 + as.numeric(substr(date, 9, 10)),
    str_detect(date, "-07") ~ 182 + as.numeric(substr(date, 9, 10))),
    julianweek = 7*floor(doy/7) + 4)%>%
  mutate(year = str_extract(DeployDate, "\\d+"))

birdPred = df %>%
  filter('Not_Found' != 1) %>%
  group_by(year, julianweek, Name) %>%
  summarize(numBirdStrikes = sum(Bird),
            numClayCats = n(),
            pctBird = 100 * numBirdStrikes / numClayCats)


totalcatCount <- meanCatDensityByWeek(.data = fullDataset, Yr = c(2024, 2025))
predvcount = totalcatCount %>%
  left_join(birdPred, by = julianweek)
  
merged = merge(totalcatCount, birdPred, by = 'julianweek', all.y = T)
  plot(merged$fracSurveys, merged$pctBird)
}
predvcount(totalcatCount)



output = data.frame()

for (site in c("Eno River State Park",
               "Triangle Land Conservancy - Johnston Mill Nature Preserve",
               "Prairie Ridge Ecostation",
               "NC Botanical Garden",
               "UNC Chapel Hill Campus")
  
  for (year in 2023:2025) {
    
    tmpData = filter(fullDataset, Name == site, Year == year)
    
    tmp = meanDensityByWeek(tmpData, ) %>%
      select(julianweek, nSurveys, meanDensity, fracSurveys, meanBiomass) %>%
      mutate(Site = site,
             Year = year)
    
    
    output = rbind(output, tmp)
    
    
    