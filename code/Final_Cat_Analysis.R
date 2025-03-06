library(dplyr)
library(lubridate)
library(data.table)
library(gsheet)
library(maps)
library(sf)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(reshape2)

#Datasets
LandscapeCover = read.csv("data/sites_2022-09-19.csv")
LandscapePrairie = read.csv("data/prairieridgeforest.csv")
ForestCover = read.csv("data/ForestCover.csv")
fullDataset = read.csv("data/fullDataset_2024-08-17.csv")
Cicadanoise = read.csv("data/inputated_values.csv")
WeeklyCicadaNoise = read.csv("data/cicada_noise.csv")
NoisePredation <- read_excel("~/Downloads/NoisePredation.xlsx")

# Function for substituting values based on a condition using dplyr:mutate
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

meanDensityByWeek = function(surveyData, # merged dataframe of Survey and arthropodSighting tables for a single site
                             ordersToInclude = 'All', 
                             minLength = 0,   # minimum arthropod size to include 
                             jdRange = c(1,365),
                             outlierCount = 10000,
                             plot = FALSE,
                             plotVar = 'fracSurveys', # 'meanDensity'or 'fracSurveys'
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
  
  new_rows <- bind_rows(
    pre2024_cicada,
    during2024_cicada,
    pre2024_nocicada,
    during2024_nocicada
  ) 
  
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
  left_join(Cicadanoise %>% select(Name, calculated_mean_noise), by = c("site" = "Name")) %>%
  mutate(Caterpillars_Present = ifelse(truefrac > 0, 1, 0),
         year_2024 = ifelse(year_2024, 1, 0),
         during_cicada = ifelse(during_cicada, 1, 0),
         cicada_present = ifelse(year_2024 & during_cicada, 1,))
fracdataframe$site <- factor(fracdataframe$site)

fracdiff <- fracdataframe %>%
  group_by(site, year_2024, forest_1km, calculated_mean_noise) %>%
  summarize(SUM_caterpillar_surveys = sum(SUM_caterpillar_surveys),
            SUM_nsurveys = sum(SUM_nsurveys),
            truefrac = SUM_caterpillar_surveys/SUM_nsurveys) %>%
  group_by(site) %>%
  mutate(truefracdiff = truefrac[year_2024 == 1] - truefrac[year_2024 == 0]) %>%
  distinct(site, truefracdiff, forest_1km, calculated_mean_noise)


glm <- glm(truefrac ~ site + year_2024, data = fracdataframe, family = quasibinomial(link = "logit"))
summary(glm)

model1 <- glm(truefrac ~ site + year_2024 + site*year_2024, data = fracdataframe, family = quasibinomial(link = "logit"))
summary(model1)


#fracdataframe <- fracdataframe %>%
#  mutate(
#    truefrac = truefrac * 100,
#    calculated_mean_noise = calculated_mean_noise * 100,
#    forest_1km = forest_1km * 100
#  )

fracdiff <- fracdiff %>%
  mutate(
    truefracdiff = truefracdiff * 100,
    calculated_mean_noise = calculated_mean_noise * 100,
    forest_1km = forest_1km * 100
  )


#Dataset and filter for Clay Cat Predation and weekly cicada noise
url = "https://docs.google.com/spreadsheets/d/1hi7iyi7xunriU2fvFpgNVDjO5ERML4IUgzTkQjLVYCo/edit?gid=0#gid=0"
df = gsheet2tbl(url) %>%
  mutate(DeployDate = as.Date(DeployDate, format = "%m/%d/%Y"),
         CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y"))


#First Deployment//////////////
birdPred = df %>%
  filter('Not_Found' != 1) %>%
  group_by(Name, CollectionDate) %>%
  summarize(numBirdStrikes = sum(Bird),
            numClayCats = n(),
            pctBird = 100*numBirdStrikes/numClayCats) %>%
  mutate(jd = yday(CollectionDate)) 

final_data_1st_Deployment = WeeklyCicadaNoise %>%
  filter(jd >= 141 & jd <= 145) %>%
  mutate(Name = case_when(
    site == "eno" ~ "Eno River State Park",
    site == "jmill" ~ "Triangle Land Conservancy - Johnston Mill Nature Preserve",
    site == "ncbg" ~ "NC Botanical Garden",
    site == "pridge" ~ "Prairie Ridge Ecostation",
    site == "unc" ~ "UNC Chapel Hill Campus",
    TRUE ~ NA_character_  # For any unmatched values, assign NA
  )) %>%
  left_join(ForestCover, by = 'Name') %>% 
  select(Name, forest_1km, mean_noise)  %>%
  left_join(birdPred, by = "Name") %>%
  filter(jd >= 141 & jd <= 145)


#Second deployment of Clay Caterpillars####
final_data_2nd_Deployment = WeeklyCicadaNoise %>%
  filter(jd >= 155 & jd <= 159) %>%
  mutate(Name = case_when(
    site == "eno" ~ "Eno River State Park",
    site == "jmill" ~ "Triangle Land Conservancy - Johnston Mill Nature Preserve",
    site == "ncbg" ~ "NC Botanical Garden",
    site == "pridge" ~ "Prairie Ridge Ecostation",
    site == "unc" ~ "UNC Chapel Hill Campus",
    TRUE ~ NA_character_  # For any unmatched values, assign NA
  )) %>%
  left_join(ForestCover, by = 'Name') %>% 
  select(Name, forest_1km, mean_noise)  %>%
  left_join(birdPred, by = "Name") %>%
  filter(jd >= 155 & jd <= 159)


#All deployment data showing effect of each site for bird strikes and Cicada Volume Index
NoisePredation = NoisePredation %>%
  left_join(ForestCover %>% select(Name, forest_1km)) %>%
  group_by(Name) 


#lm models //////////
lm_forest_frac_diff <- lm(truefracdiff ~ forest_1km, data = fracdiff)
summary(lm_forest_frac_diff)

lm_noise_frac_diff <- lm(truefracdiff ~ calculated_mean_noise, data = fracdiff)
summary(lm_noise_frac_diff)

final_data_1st_Deployment_Forest <- lm(pctBird ~ forest_1km, data = final_data_1st_Deployment)
summary(final_data_1st_Deployment_Forest)

final_data_1st_Deployment_Noise <- lm(pctBird ~ mean_noise, data = final_data_1st_Deployment)
summary(final_data_1st_Deployment_Forest)

final_data_2nd_Deployment_Forest <- lm(pctBird ~ forest_1km, data = final_data_2nd_Deployment)
summary(final_data_2nd_Deployment_Forest)

final_data_2nd_Deployment_Noise <- lm(pctBird ~ mean_noise, data = final_data_2nd_Deployment)
summary(final_data_2nd_Deployment_Forest)

Mean_Noise_additive <- lm(pctBird ~ mean_noise + Name -1, data = NoisePredation)
summary(Mean_Noise_additive)


###############data plotted
par(mfrow = c(1, 2))

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
  "UNC Chapel Hill Campus" = "#0072B2",
  "NC Botanical Garden" = "#D55E00",
  "Eno River State Park" = "black",
  "Prairie Ridge Ecostation" = "#CC79A7",
  "Triangle Land Conservancy - Johnston Mill Nature Preserve" = "grey45"
)
plot(0, 0, 
     xlim = c(0.5, 2.5), 
     ylim = c(-0.005, 0.22),
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
       cex = 1,
       bty = "n")


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
  "UNC Chapel Hill Campus" = "#0072B2",
  "NC Botanical Garden" = "#D55E00",
  "Eno River State Park" = "black",
  "Prairie Ridge Ecostation" = "#CC79A7",
  "Triangle Land Conservancy - Johnston Mill Nature Preserve" = "grey45"
)
plot(0, 0, 
     xlim = c(0.5, 2.5), 
     ylim = c(-0.005, 0.22),
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
       cex = 1,
       bty = "n")


#############plot of fracdiff with forest cover 
lm_frac_diff <- lm(truefracdiff ~ forest_1km, data = fracdiff)

site_colors <- c("#0072B2", "#D55E00", "black", "#CC79A7", "yellow3")
site_shapes <- c(16, 17, 8, 15, 18)  

names(site_colors) <- unique(fracdiff$site)
names(site_shapes) <- unique(fracdiff$site)

plot(fracdiff$forest_1km, fracdiff$truefracdiff, 
     type = "n",  
     xlab = "% Forest Cover", 
     ylab = "Difference in  % of Surveys with Caterpillars",
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
       cex = 1,
       bty = "n")


#####cicada volume graph
lm_Vol_diff <- lm(truefracdiff ~ calculated_mean_noise, data = fracdiff)
summary_stats <- summary(lm_Vol_diff)

site_colors <- c("#0072B2", "#D55E00", "black", "#CC79A7", "yellow3")
site_shapes <- c(16, 17, 8, 15, 18)  

names(site_colors) <- unique(fracdiff$site)
names(site_shapes) <- unique(fracdiff$site)

plot(fracdiff$calculated_mean_noise, fracdiff$truefracdiff, 
     type = "n",  
     xlab = "Cicada Volume Index", 
     ylab = "Difference in  % of Surveys with Caterpillars",
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

legend("topleft",
       legend = names(site_colors), 
       col = site_colors,
       pch = site_shapes,
       cex = 1,
       bty = "n")


#Graph of each site on one plot //////////
site_colors <- c("#0072B2", "#D55E00", "black", "#CC79A7", "yellow3")
site_shapes <- c(16, 17, 8, 15, 18)  

names(site_colors) <- unique(NoisePredation$Name)
names(site_shapes) <- unique(NoisePredation$Name)

plot(NoisePredation$mean_noise, NoisePredation$pctBird,
     type = "n",  # Create empty plot to layer points later
     xlab = "Cicada Volume Index",
     ylab = "% Bird Predation")

for (site in unique(NoisePredation$Name)) {
  points(NoisePredation$mean_noise[NoisePredation$Name == site],
         NoisePredation$pctBird[NoisePredation$Name == site],
         col = site_colors[site],
         pch = site_shapes[site],
         cex = 1.5)
}

lm_overall <- lm(pctBird ~ mean_noise, data = NoisePredation)
abline(lm_overall, col = "gray50", lwd = 2, lty = 3)

for (site in unique(NoisePredation$Name)) {
  subset_data <- NoisePredation[NoisePredation$Name == site, ]
  lm_site <- lm(pctBird ~ mean_noise, data = subset_data)
  abline(lm_site, col = site_colors[site], lwd = 1.5)
}

legend("topright", legend = names(site_colors),
       col = site_colors, 
       pch = site_shapes, 
       cex = 1)





#Stacked bar graph for the strike marks
dfsum <- df %>%
  group_by(Name) %>%
  summarize(Bird = sum(Bird),
            Mammal = sum(Mammal),
            Arthropod = sum(Arthropod),
            Unidentified = sum(Unidentified)) %>%
  pivot_longer(cols = c("Bird","Mammal", "Arthropod", "Unidentified"), names_to = "Category", values_to = "Count") %>%
  mutate(
    Name = case_when(
      Name == "Triangle Land Conservancy - Johnston Mill Nature Preserve" ~ "Johnston Mill",
      Name == "UNC Chapel Hill Campus" ~ "UNC",
      Name == "Prairie Ridge Ecostation" ~ "Prairie Ridge",
      Name == "Eno River State Park" ~ "Eno River",
      Name == "NC Botanical Garden" ~ "NCBG",
    TRUE ~ Name)) 

my_colors <- c("orange", "skyblue", "lightgrey", "pink")
mytable <- xtabs(Count ~ Category + Name, data = dfsum)
mytable <- mytable[, order(mytable["Bird", ], decreasing = TRUE)]


Stacked_Bar <- barplot(
  mytable,
  beside = FALSE,
  col = my_colors,
  legend = rownames(mytable),
  names.arg = colnames(mytable),
  xlab = "Name",
  ylab = "Predation",
  main = "Clay Caterpillar Predation",
  ylim = c(0, 100)
)

totals <- colSums(mytable)
text(Stacked_Bar, totals, labels = totals, pos = 3)

stackHeights = apply(mytable, 2, cumsum)
stackLower = rbind(0, stackHeights[-nrow(stackHeights), ])
stackMidpoints = (stackHeights + stackLower) / 2

for (j in seq_len(ncol(mytable))) {
  for (i in seq_len(nrow(mytable))) {
    countValue <- mytable[i, j]
    if (countValue > 0) {
      text(Stacked_Bar[j], stackMidpoints[i, j], labels = countValue, cex=0.8)
    }
  }
}
