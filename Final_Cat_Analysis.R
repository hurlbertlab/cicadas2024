library(dplyr)
library(lubridate)
library(data.table)
library(gsheet)
library(maps)
library(sf)
library(tidyr)

#Datasets used in project
LandscapeCover = read.csv("data/sites_2022-09-19.csv")
LandscapePrairie = read.csv("data/prairieridgeforest.csv")
ForestCover = read.csv("data/ForestCover.csv")
fullDataset = read.csv("data/fullDataset_2024-08-17.csv")
Cicadanoise = read.csv("data/inputated_values.csv")

# Function for substituting values based on a condition using dplyr:mutate
# Modification of dplyr's mutate function that only acts on the rows meeting a condition
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
# Function for calculating the mode of a series of values
Mode = function(x){ 
  if (!is.numeric(x)) {
    stop("values must be numeric for mode calculation")
  }
  ta = table(x)
  tam = max(ta)
  mod = as.numeric(names(ta)[ta == tam])
  return(max(mod))
}
# Function arguments
surveyData = # merged dataframe of Survey and arthropod Sighting tables for a single site
  ordersToInclude = 'caterpillar'       # which arthropod orders to calculate density for (codes)

minLength = 0         # minimum arthropod size to include 
jdRange = c(1,365)
outlierCount = 10000
plot = FALSE
plotVar = 'fracSurveys' # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
minSurveyCoverage = 0.8 # minimum proportion of unique survey branches examined per week in order to include the week as a data point
allDates = TRUE
new = TRUE
color = 'black'
allCats = TRUE

# Function for calculating and displaying arthropod phenology by week
meanDensityByWeek = function(surveyData, # merged dataframe of Survey and arthropodSighting tables for a single site
                             ordersToInclude = 'All', 
                             minLength = 0,   # minimum arthropod size to include 
                             jdRange = c(1,365),
                             outlierCount = 10000,
                             plot = FALSE,
                             plotVar = 'fracSurveys', # 'meanDensity'or 'fracSurveys'
                             minSurveyCoverage = 0.8, # minimum proportion of unique survey branches examined per week in order to include the week as a data point
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
    mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% #outlier counts replaced with 1
    group_by(julianweek) %>%
    summarize(totalCount = sum(Quantity2, na.rm = TRUE),
              numSurveysGTzero = length(unique(ID[Quantity > 0])),
              totalBiomass = sum(Biomass_mg, na.rm = TRUE)) %>% 
    right_join(effortByWeek, by = 'julianweek') %>%
    filter(okWeek == 1) %>%
    #next line replaces 3 fields with 0 if the totalCount is NA
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


#Cat data 2024////////////////
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


#Cat data 2023/////////////////
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


#Cat Data 2022///////////////////
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

#Cat data 2021//////////
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

######Cat graph Eno River////
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

#######Cat graph UNC////////////
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

#######Cat graph JM////////////////
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

######Cat graph PR/////////////////
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

#######Cat graph NCBG//////////
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

####example
#pHat<- example_fracsurveys
#nSurvs<- example$SUM_nsurveys[1]
#example_confidence_interval_min <- pHat-1.96*(pHat*(1-pHat)/nSurvs)^.5
#example_confidence_interval_max<- pHat+1.96*(pHat*(1-pHat)/nSurvs)^.5
#example_all_plot
#plot(x = 1, #we only have one point, this will represent out Pre-2024
     #y = pHat,
     #xlab = "",
     #ylab = "")
#segments(1, example_confidence_interval_min,
         #1, example_confidence_interval_max)


###########creating a for loop that will help with fracsurveys
frac_calculator <- function(
    site = "UNC Chapel Hill Campus", 
    df = fullDataset, 
    year_range = c(2021,2022,2023),
    julian_range = c(135:165)) {
  
  result <- df %>%
    filter(Name == site,
           Year %in% year_range,
           julianday %in% julian_range) %>%
    meanDensityByWeek(ordersToInclude = "caterpillar", plot = FALSE) %>%
    mutate(SUM_caterpillar_surveys = sum(numSurveysGTzero), #calculate fracsurveys 
           SUM_nsurveys = sum(nSurveys),
           truefrac = SUM_caterpillar_surveys/SUM_nsurveys) %>%
    
#add background information about year and before or after
    mutate(year_2024 = ifelse(2024 %in% year_range, TRUE, FALSE),
           during_cicada = ifelse(135 %in% julian_range, TRUE, FALSE),
           site= site) %>%
    select(SUM_caterpillar_surveys, SUM_nsurveys, truefrac, year_2024, during_cicada, site)
  
#all rows are the same, we just need one
  result <- result[1,]
#at end of function, give back result as the answer
  return(result)
}

#add everything together in one dataframe by creating a data frame
fracdataframe<- data.frame()

site_list <- c("UNC Chapel Hill Campus",
               "NC Botanical Garden",
               "Eno River State Park",
               "Prairie Ridge Ecostation",
               "Triangle Land Conservancy - Johnston Mill Nature Preserve")
for (site in site_list) {
  pre2024_cicada <- frac_calculator(site = site, year_range = 2021:2023, julian_range = 135:165)
  during2024_cicada <- frac_calculator(site = site, year_range = 2024, julian_range = 135:165)
  pre2024_nocicada <- frac_calculator(site = site, year_range = 2021:2023, julian_range = 166:365)
  during2024_nocicada <- frac_calculator(site = site, year_range = 2024, julian_range = 166:365)
  
  #add these to my dataframe!
  new_rows <- bind_rows(
    pre2024_cicada,
    during2024_cicada,
    pre2024_nocicada,
    during2024_nocicada
  ) #bind all four datasets together
  
  #add these rows to my existing dataframe, outside the for loop
  fracdataframe <- bind_rows(fracdataframe, new_rows)
  
  
}

fracdataframe <- fracdataframe %>%
  left_join(ForestCover %>% select(Name, forest_1km), by = c("site" = "Name")) 

Cicadanoise <- Cicadanoise %>%
  mutate(Name = case_when(
    site == "eno" ~ "Eno River State Park",
    site == "jmill" ~ "Triangle Land Conservancy - Johnston Mill Nature Preserve",
    site == "ncbg" ~ "NC Botanical Garden",
    site == "unc" ~ "UNC Chapel Hill Campus",
    site == "pridge" ~ "Prairie Ridge Ecostation"
  ))

fracdataframe <- fracdataframe %>%
  left_join(Cicadanoise %>% select(Name, calculated_mean_noise), by = c("site" = "Name"))

fracdataframe <- fracdataframe %>%
  mutate(cicada_present = ifelse(year_2024 == TRUE & during_cicada == TRUE, 1, 0))

write.csv(fracdataframe, "data/SitesCatFracDuringCicadas.csv", row.names= FALSE)



fracdiff <- fracdataframe %>%
  group_by(site, year_2024, forest_1km, calculated_mean_noise) %>%
  summarize(SUM_caterpillar_surveys = sum(SUM_caterpillar_surveys),
            SUM_nsurveys = sum(SUM_nsurveys),
            truefrac = SUM_caterpillar_surveys/SUM_nsurveys) %>%
  group_by(site) %>%
  mutate(truefracdiff = truefrac[year_2024 == 1] - truefrac[year_2024 == 0]) %>%
  distinct(site, truefracdiff, forest_1km, calculated_mean_noise)

lm_forest_frac_diff <- lm(truefracdiff ~ forest_1km, data = fracdiff)
summary(lm_forest_frac_diff)

lm_noise_frac_diff <- lm(truefracdiff ~ calculated_mean_noise, data = fracdiff)
summary(lm_noise_frac_diff)








#lm models //////////
#lm_cats_site <- lm(truefrac ~ year_2024, data = fracdataframe)
#summary(lm_cats_site)

#lm_cats_site_year <- lm(truefrac ~ year_2024 + site, data = fracdataframe)
#summary(lm_cats_site_year)

lm_cicada_effect <- lm(truefrac ~ year_2024 + site *during_cicada, data = fracdataframe)
summary(lm_cicada_effect)

lm_interaction <- lm(truefrac ~ cicada_present + site, data = fracdataframe)
summary(lm_interaction)

#lm_add <- lm(truefrac ~ cicada_present * site, data = fracdataframe)
#summary(lm_additive)

lm_forest_frac_diff <- lm(truefracdiff ~ forest_1km, data = fracdiff)
summary(lm_forest_frac_diff)

lm_noise_frac_diff <- lm(truefracdiff ~ calculated_mean_noise, data = fracdiff)
summary(lm_noise_frac_diff)

###############data plotted
#During cicada emergence
cicada_analysis <- fracdataframe %>%
  filter(during_cicada == 1) %>%
  group_by(site, year_2024) %>%
  summarize(mean_frac = mean(truefrac, na.rm = TRUE), 
            mean_frac = mean(truefrac, na.rm = TRUE),
            nSurvs = (SUM_nsurveys),  
            lower_ci = mean(truefrac, na.rm = TRUE) - 1.96 * sqrt((mean(truefrac, na.rm = TRUE) * (1 - mean(truefrac, na.rm = TRUE))) / sum(SUM_nsurveys)),
            upper_ci = mean(truefrac, na.rm = TRUE) + 1.96 * sqrt((mean(truefrac, na.rm = TRUE) * (1 - mean(truefrac, na.rm = TRUE))) / (SUM_nsurveys))
  ) %>%
  ungroup()

site_colors <- c(
  "UNC Chapel Hill Campus" = "purple",
  "NC Botanical Garden" = "blue",
  "Eno River State Park" = "orange",
  "Prairie Ridge Ecostation" = "red",
  "Triangle Land Conservancy - Johnston Mill Nature Preserve" = "black"
)
plot(0, 0, 
     xlim = c(0.5, 2.5), 
     ylim = c(0.00, 0.20),
     type = "n",
     xaxt = "n",
     yaxt = "n", 
     xlab = "",
     ylab = "% Surveys with Caterpillars",
     main = "During Cicada Emergence")
axis(1, at = c(1, 2), labels = c("Pre-2024", "2024"))

y_ticks <- seq(0, 0.2, by = 0.05) 
axis(2, at = y_ticks, labels = paste0(y_ticks * 100))

for (site in unique(cicada_analysis$site)) {
  site_data <- cicada_analysis[cicada_analysis$site == site, ]
  
  lines(c(1, 2), 
        site_data$mean_frac, 
        col = site_colors[site],
        lwd = 2)
  
  points(c(1, 2), 
         site_data$mean_frac, 
         col = site_colors[site],
         pch = 16,
         cex = 1.2)
  
  segments(c(1, 2), 
           site_data$lower_ci, 
           c(1, 2), 
           site_data$upper_ci, 
           col = site_colors[site],
           lwd = .7)  
  
  segments(c(1, 2) - 0.02, site_data$lower_ci,
           c(1, 2) + 0.02, site_data$lower_ci,  
           col = site_colors[site],
           lwd = .7)
  
  segments(c(1, 2) - 0.02, site_data$upper_ci, 
           c(1, 2) + 0.02, site_data$upper_ci, 
           col = site_colors[site],
           lwd = .7)
}
legend("topleft", 
       legend = names(site_colors),
       col = site_colors,
       lwd = 2,
       pch = 16,
       cex = 0.7)


#########After cicadas plot
cicada_analysis_nocicada <- fracdataframe %>%
  filter(during_cicada == 0) %>%
  group_by(site, year_2024) %>%
  summarize(
    mean_frac = mean(truefrac, na.rm = TRUE),
    nSurvs = (SUM_nsurveys),  # Total number of surveys
    lower_ci = mean(truefrac, na.rm = TRUE) - 1.96 * sqrt((mean(truefrac, na.rm = TRUE) * (1 - mean(truefrac, na.rm = TRUE))) / sum(SUM_nsurveys)),
    upper_ci = mean(truefrac, na.rm = TRUE) + 1.96 * sqrt((mean(truefrac, na.rm = TRUE) * (1 - mean(truefrac, na.rm = TRUE))) / (SUM_nsurveys))
  ) %>%
  ungroup()

site_colors <- c(
  "UNC Chapel Hill Campus" = "purple",
  "NC Botanical Garden" = "blue",
  "Eno River State Park" = "orange",
  "Prairie Ridge Ecostation" = "red",
  "Triangle Land Conservancy - Johnston Mill Nature Preserve" = "black"
)
plot(0, 0, 
     xlim = c(0.5, 2.5), 
     ylim = c(0.00, 0.20),
     type = "n",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "% Surveys with Caterpillars",
     main = "After Cicada Emergence")
axis(1, at = c(1, 2), labels = c("Pre-2024", "2024"))

y_ticks <- seq(0, 0.2, by = 0.05) 
axis(2, at = y_ticks, labels = paste0(y_ticks * 100))

for (site in unique(cicada_analysis_nocicada$site)) {
  site_data <- cicada_analysis_nocicada[cicada_analysis_nocicada$site == site, ]
  
  lines(c(1, 2), 
        site_data$mean_frac, 
        col = site_colors[site],
        lwd = 2)
  
  points(c(1, 2), 
         site_data$mean_frac, 
         col = site_colors[site],
         pch = 16,
         cex = 1.2)
  
  segments(c(1, 2), 
           site_data$lower_ci, 
           c(1, 2), 
           site_data$upper_ci, 
           col = site_colors[site],
           lwd = .7)  
  
  segments(c(1, 2) - 0.02, site_data$lower_ci,
           c(1, 2) + 0.02, site_data$lower_ci,  
           col = site_colors[site],
           lwd = .7)
  
  segments(c(1, 2) - 0.02, site_data$upper_ci, 
           c(1, 2) + 0.02, site_data$upper_ci, 
           col = site_colors[site],
           lwd = .7)
}
legend("topleft", 
       legend = names(site_colors), 
       col = site_colors, 
       lwd = 2, 
       pch = 16, 
       cex = 0.7)


####changing data to actual percent
fracdataframe <- fracdataframe %>%
  mutate(
    truefrac = truefrac * 100,
    calculated_mean_noise = calculated_mean_noise * 100,
    forest_1km = forest_1km * 100
  )

fracdiff <- fracdiff %>%
  mutate(
    truefracdiff = truefracdiff * 100,
    calculated_mean_noise = calculated_mean_noise * 100,
    forest_1km = forest_1km * 100
  )
#############plot of fracdiff with forest cover 
lm_frac_diff <- lm(truefracdiff ~ forest_1km, data = fracdiff)

site_colors <- c("red", "blue", "black", "purple", "orange3")
site_shapes <- c(16, 17, 8, 15, 18)  

names(site_colors) <- unique(fracdiff$site)
names(site_shapes) <- unique(fracdiff$site)

plot(fracdiff$forest_1km, fracdiff$truefracdiff, 
     type = "n",  
     xlab = "% Forest Cover", 
     ylab = "Difference in  % of Surveys with Caterpillars", 
     main = "Effect of Forest Cover on Caterpillar Abundance",
     xlim = range(fracdiff$forest_1km, na.rm = TRUE),
     ylim = range(fracdiff$truefracdiff, na.rm = TRUE)
)

for (site in unique(fracdiff$site)) {
  points(fracdiff$forest_1km[fracdiff$site == site], 
         fracdiff$truefracdiff[fracdiff$site == site],
         col = site_colors[site],
         pch = site_shapes[site],
         cex = 1.5)
}

abline(lm_frac_diff, col = "black", lwd = 2)

legend("topleft", legend = names(site_colors), 
       col = site_colors, 
       pch = site_shapes,
       cex = 0.68)
       


#####cicada volume graph
lm_Vol_diff <- lm(truefracdiff ~ calculated_mean_noise, data = fracdiff)

site_colors <- c("red", "blue", "black", "purple", "orange3")
site_shapes <- c(16, 17, 8, 15, 18)  

names(site_colors) <- unique(fracdiff$site)
names(site_shapes) <- unique(fracdiff$site)

plot(fracdiff$calculated_mean_noise, fracdiff$truefracdiff, 
     type = "n",  
     xlab = "% Mean Cicada Volume", 
     ylab = "Difference in  % of Surveys with Caterpillars", 
     main = "Caterpillar Abundance and Mean Cicada Volume Comparison",
     xlim = range(fracdiff$calculated_mean_noise, na.rm = TRUE),
     ylim = range(fracdiff$truefracdiff , na.rm = TRUE)
)

for (site in unique(fracdiff$site)) {
  points(fracdiff$calculated_mean_noise[fracdiff$site == site], 
         fracdiff$truefracdiff[fracdiff$site == site],
         col = site_colors[site],
         pch = site_shapes[site],
         cex = 1.5)
}

abline(lm_Vol_diff, col = "black", lwd = 2)

legend("topleft", legend = names(site_colors), 
       col = site_colors, 
       pch = site_shapes, 
       cex = 0.68)
