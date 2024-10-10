#creating data frame for average confidence level of each bird species
library(dplyr)
library(stringr)


sites = c("eno", "jmill", "ncbg", "pridge", "unc")

confidence_levels = read.csv(paste0("data/birdcallsdata/", "eno", "/", "EnoRiver_25m_0516.BirdNET.results.csv")) %>%
  mutate(Common.name = "ex") %>%
  mutate(site = "eno",
         resultsfile = "anything")

for(s in 1:length(sites)) {
  
 files <- list.files(paste0("data/birdcallsdata/", sites[s]))
  
  for(f in 1:length(files)) {
    temp_file <- read.csv(paste0("data/birdcallsdata/", sites[s], "/", files[f])) %>%
      mutate(site = sites[s],
             resultsfile = files[f])
    confidence_levels <- rbind(confidence_levels, temp_file)
  }
}

confidence_levels <- confidence_levels %>%
  filter(Common.name != "ex") %>%
  mutate(studysite = str_extract(resultsfile, pattern = "JMill|UNC|EnoRiver|NCBG|PRidge")) %>%
  mutate(distance = str_extract(resultsfile, pattern = "25m|50m")) %>%
  mutate(date = str_extract(resultsfile, pattern = "[0-9][0-9][0-9][0-9]"))
