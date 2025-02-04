# Functions for working with and analyzing Caterpillars Count! data
library(dplyr)
library(lubridate)
library(data.table)
library(gsheet)
library(maps)
library(sf)

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

#Cat data 2024
pr24 = filter(fullDataset, Name== "Prairie Ridge Ecostation", Year== 2024)
prCats24 = meanDensityByWeek(pr24, ordersToInclude = "caterpillar")

bg24 = filter(fullDataset, Name == "NC Botanical Garden", Year== 2024)
BGCats24 = meanDensityByWeek(bg24, ordersToInclude = "caterpillar")

UNC24 = filter(fullDataset, Name== "UNC Chapel Hill Campus", Year== 2024)
UNCcats24 = meanDensityByWeek(UNC24, ordersToInclude = "caterpillar")

JM24 = filter(fullDataset, Name== "Triangle Land Conservancy - Johnston Mill Nature Preserve", Year== 2024)
JMCats24 = meanDensityByWeek(JM24, ordersToInclude = "caterpillar")

ER24 = filter(fullDataset, Name == "Eno River State Park", Year== 2024)
ERCats24 = meanDensityByWeek(ER24, ordersToInclude = "caterpillar")

#Cat data 2023
pr23 = filter(fullDataset, Name== "Prairie Ridge Ecostation", Year== 2023)
prCats23 = meanDensityByWeek(pr23, ordersToInclude = "caterpillar")

bg23 = filter(fullDataset, Name == "NC Botanical Garden", Year== 2023)
BGCats23 = meanDensityByWeek(bg23, ordersToInclude = "caterpillar")

UNC23 = filter(fullDataset, Name== "UNC Chapel Hill Campus", Year== 2023)
UNCcats23 = meanDensityByWeek(UNC23, ordersToInclude = "caterpillar")

JM23 = filter(fullDataset, Name== "Triangle Land Conservancy - Johnston Mill Nature Preserve", Year== 2023)
JMCats23 = meanDensityByWeek(JM23, ordersToInclude = "caterpillar")

ER23 = filter(fullDataset, Name == "Eno River State Park", Year== 2023)
ERCats23 = meanDensityByWeek(ER23, ordersToInclude = "caterpillar")

#Cat Data 2022
pr22 = filter(fullDataset, Name== "Prairie Ridge Ecostation", Year== 2022)
prCats22 = meanDensityByWeek(pr22, ordersToInclude = "caterpillar")

bg22 = filter(fullDataset, Name == "NC Botanical Garden", Year== 2022)
BGCats22 = meanDensityByWeek(bg22, ordersToInclude = "caterpillar")

UNC22 = filter(fullDataset, Name== "UNC Chapel Hill Campus", Year== 2022)
UNCcats22 = meanDensityByWeek(UNC22, ordersToInclude = "caterpillar")

JM22 = filter(fullDataset, Name== "Triangle Land Conservancy - Johnston Mill Nature Preserve", Year== 2022)
JMCats22 = meanDensityByWeek(JM22, ordersToInclude = "caterpillar")

ER22 = filter(fullDataset, Name == "Eno River State Park", Year== 2022)
ERCats22 = meanDensityByWeek(ER22, ordersToInclude = "caterpillar")

#Cat data 2021
pr21 = filter(fullDataset, Name== "Prairie Ridge Ecostation", Year== 2021)
prCats21 = meanDensityByWeek(pr21, ordersToInclude = "caterpillar")

bg21 = filter(fullDataset, Name == "NC Botanical Garden", Year== 2021)
BGCats21 = meanDensityByWeek(bg21, ordersToInclude = "caterpillar")

UNC21 = filter(fullDataset, Name== "UNC Chapel Hill Campus", Year== 2021)
UNCcats21 = meanDensityByWeek(UNC21, ordersToInclude = "caterpillar")

JM21 = filter(fullDataset, Name== "Triangle Land Conservancy - Johnston Mill Nature Preserve", Year== 2021)
JMCats21 = meanDensityByWeek(JM21, ordersToInclude = "caterpillar")

ER21 = filter(fullDataset, Name == "Eno River State Park", Year== 2021)
ERCats21 = meanDensityByWeek(ER21, ordersToInclude = "caterpillar")


#this is the sites with years graphed 
quartz(width = 12, height = 8)

par(mfrow = c(2,3), mar= c(5, 5, 4, 2) + 0.1, 
    oma = c(0, 0, 2, 0))

######Cat graph Eno River
ER24 = filter(fullDataset, Name == "Eno River State Park", Year== 2024)
ERCats24 = meanDensityByWeek(ER24, ordersToInclude = "caterpillar", plot = TRUE, new = TRUE, color = 'black', ylim= c(0, 30), xlim= c(110, 220), ylab= "Percent of Surveys with Caterpillars", xlab= "Julian Day")

#no data for 2023
#ER23 = filter(fullDataset, Name == "Eno River State Park", Year== 2023)
#ERCats23 = meanDensityByWeek(ER23, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'purple')

ER22 = filter(fullDataset, Name == "Eno River State Park", Year== 2022)
ERCats22 = meanDensityByWeek(ER22, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'green3')

#no data for 2021
#ER21 = filter(fullDataset, Name == "Eno River State Park", Year== 2021)
#ERCats21 = meanDensityByWeek(ER21, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'red')

title("Eno River State Park")

legend("topleft", legend = c("2024", "2022"),
       col = c("black", "green3"),
       lty = 1,
       lwd = 2)

#######Cat graph UNC
UNC24 = filter(fullDataset, Name== "UNC Chapel Hill Campus", Year== 2024)
UNCcats24 = meanDensityByWeek(UNC24, ordersToInclude = "caterpillar", plot = TRUE, new = TRUE, color = 'black', ylim= c(0, 30), xlim= c(110, 220), ylab= "Percent of Surveys with Caterpillars", xlab= "Julian Day")

UNC23 = filter(fullDataset, Name== "UNC Chapel Hill Campus", Year== 2023)
UNCcats23 = meanDensityByWeek(UNC23, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'purple')

UNC22 = filter(fullDataset, Name== "UNC Chapel Hill Campus", Year== 2022)
UNCcats22 = meanDensityByWeek(UNC22, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'green3')

UNC21 = filter(fullDataset, Name== "UNC Chapel Hill Campus", Year== 2021)
UNCcats21 = meanDensityByWeek(UNC21, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'red')

title("UNC Chapel Hill")

legend("topleft", legend = c("2024", "2023", "2022", "2021"),
       col = c("black", "purple", "green3", "red"),
       lty = 1,
       lwd = 2)

#######Cat graph JM
JM24 = filter(fullDataset, Name== "Triangle Land Conservancy - Johnston Mill Nature Preserve", Year== 2024)
JMCats24 = meanDensityByWeek(JM24, ordersToInclude = "caterpillar", plot = TRUE, new = TRUE, color = 'black', ylim= c(0,30), xlim= c(110, 220), ylab= "Percent of Surveys with Caterpillars", xlab= "Julian Day")

#JM23 = filter(fullDataset, Name== "Triangle Land Conservancy - Johnston Mill Nature Preserve", Year== 2023')
#JMCats23 = meanDensityByWeek(JM23, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'purple')

JM22 = filter(fullDataset, Name== "Triangle Land Conservancy - Johnston Mill Nature Preserve", Year== 2022)
JMCats22 = meanDensityByWeek(JM22, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'green3')

#JM21 = filter(fullDataset, Name== "Triangle Land Conservancy - Johnston Mill Nature Preserve", Year== 2021)
#JMCats21 = meanDensityByWeek(JM21, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'red')

title("Johnston Mill")

legend("topleft", legend = c("2024", "2022"),
       col = c("black", "green3"),
       lty = 1,
       lwd = 2)

######Cat graph PR
pr24 = filter(fullDataset, Name== "Prairie Ridge Ecostation", Year== 2024)
prCats24 = meanDensityByWeek(pr24, ordersToInclude = "caterpillar", plot = TRUE, color = 'black', ylim= c(0,30), xlim= c(110, 220), ylab= "Percent of Surveys with Caterpillars", xlab= "Julian Day")

pr23 = filter(fullDataset, Name== "Prairie Ridge Ecostation", Year== 2023)
prCats23 = meanDensityByWeek(pr23, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'purple')

pr22 = filter(fullDataset, Name== "Prairie Ridge Ecostation", Year== 2022)
prCats22 = meanDensityByWeek(pr22, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'green3')

pr21 = filter(fullDataset, Name== "Prairie Ridge Ecostation", Year== 2021)
prCats21 = meanDensityByWeek(pr21, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'red')

title("Prairie Ridge")

legend("topleft", legend = c("2024", "2023", "2022", "2021"),
       col = c("black", "purple", "green3", "red"),
       lty = 1,
       lwd = 2)

#######Cat graph NCBG
bg24 = filter(fullDataset, Name == "NC Botanical Garden", Year== 2024)
BGCats24 = meanDensityByWeek(bg24, ordersToInclude = "caterpillar", plot = TRUE, new = TRUE, color = 'black', ylim= c(0,30), xlim= c(110, 220), ylab= "Percent of Surveys with Caterpillars", xlab= "Julian Day")

bg23 = filter(fullDataset, Name == "NC Botanical Garden", Year== 2023)
BGCats23 = meanDensityByWeek(bg23, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'purple')

bg22 = filter(fullDataset, Name == "NC Botanical Garden", Year== 2022)
BGCats22 = meanDensityByWeek(bg22, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'green3')

bg21 = filter(fullDataset, Name == "NC Botanical Garden", Year== 2021)
BGCats21 = meanDensityByWeek(bg21, ordersToInclude = "caterpillar", plot = TRUE, new = FALSE, color = 'red')

title("NC Botanical Garden")

legend("topleft", legend = c("2024", "2023", "2022", "2021"),
       col = c("black", "purple", "green3", "red"),
       lty = 1,
       lwd = 2)