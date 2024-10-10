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

confirmed_calls = read.csv("data/birdcalltimes/BirdCallsTimes.csv") %>%
  mutate(Date = recode(Date,
                      '516' = '0516',
                      '521' = '0521',
                      '618' = '0618',
                      '520' = '0520',
                      '617' = '0617',
                      '517' = '0517',
                      '524' = '0524',
                      '614' = '0614',
                      '522' = '0522',
                      '619' = '0619',
                      '523' = '0523',
                      '620' = '0620',
                      ),
         Location = str_remove(Location, ' '),
         Bird.Call = ifelse(Bird.Call == 'Eastern Wood Pewee', 'Eastern Wood-Pewee', Bird.Call))

birdnetresults = left_join(confirmed_calls, confidence_levels, by = c('Start.Time' = 'Start..s.', 'End.Time' = 'End..s.', 'Bird.Call' = 'Common.name', 'Date' = 'date', 'Location' = 'site', 'Distance' = 'distance'))

write.csv(birdnetresults,"data/birdnetresults.csv",row.names= FALSE)
