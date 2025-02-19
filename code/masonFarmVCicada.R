####################

### Comparing BirdNet Confidence to audiofiles from Mason Farm

####################

#First I need to quantify the sound present in the same band cicadas vocalize
library(warbleR)
library(tuneR)
library(seewave)
library(stringr)
library(dplyr)
library(tidyverse)

files = list.files(paste0("audiofiles/masonFarmAudio/audiofiles/25m/"))
  
for (f in files) {
    audiofile <-readWave(paste0("audiofiles/masonFarmAudio/audiofiles/25m/", f)) 
    audiofile_amp = seewave::spec(audiofile, flim = c(.97, 1.23))
    max_cicada_amplitude = max(audiofile_amp[audiofile_amp[,1] > .9 & audiofile_amp[,1] < 1.2, 2])
    peakfreq_audiofile = audiofile_amp[audiofile_amp[,2] == max_cicada_amplitude , 1]
    max_tone_amp = max(audiofile_amp[audiofile_amp[,1] > .78 & audiofile_amp[,1] < .90, 2])
    moving_average <- function(x, n = 5) {stats::filter(x, rep(1 / n, n), sides = 2) }
    runavg = moving_average(audiofile_amp[,2], n = 100)
    max_runavg = max(runavg[audiofile_amp[,1] > .9 & audiofile_amp[,1] < 1.2], na.rm = T)
    points(audiofile_amp[,1], runavg, type = 'l', col = 'salmon', lwd = 4)
    abline(v=c(1.0, 1.21), col = 'cyan')
    title(main = paste(f))
    
    # read in audiofile
    # figure out max amplitude
    
    tmp = data.frame(file = f,
                     max_cicada_amp = max_cicada_amplitude,
                     max_running_avg = max_runavg,
                     max_tone_amp = max_tone_amp,
                     sound_level = max_runavg/max_tone_amp) 
    
    sound_MasonFarm = rbind(sound_MasonFarm, tmp)
    
}

dev.off() 

sound_MasonFarm <- 
  sound_MasonFarm %>%
  filter(file != "ex")
sound_MasonFarm <- sound_MasonFarm[-c(2,4,5)]

#Now I will bind it with a file with the confidence levels for 25 Meters
confidence_masonFarm <- read.csv(paste0("data/masonFarm/BirdNET_CombinedTable_High.csv"))
masonFarmConf25m <- left_join(sound_MasonFarm, confidence_masonFarm, by = c('file' = 'File'))|>
  drop_na(max_running_avg)
write.csv(masonFarmConf25m, "data/masonFarmConf25m.csv")

#Now I compare them to cicada data 
#Need help on how to do linear regression and interaction plots with two different datasets/csv