#plotting temperature vs. frequency
#adding temperature to dataframe 
library(warbleR)
library(tuneR)
library(seewave)
library(stringr)
library(dplyr)

notes <- read.csv("data/CC!samplingnotes.csv") %>%
  filter(str_detect(date, "2024")) %>% 
  mutate(site = recode(site,
                         'ERSP' = 'eno',
                         'JM' = 'jmill',
                         '8892356' = 'ncbg',
                         '117' = 'pridge',
                         '1' = 'unc',)) %>%
  mutate(temp = case_when(
    str_detect(notes, "70s") ~ 72,
    str_detect(notes, "Mid 70s") ~ 75,
    str_detect(notes, "High 70's") ~ 78,
    str_detect(notes, "70s to 80s") ~ 75,
    str_detect(notes, "60's") ~ 62,
    str_detect(notes, "70F") ~ 72,
    str_detect(notes, "70's") ~ 72,
    str_detect(notes, "60's-70's") ~ 65,
    str_detect(notes, "80's") ~ 82,
    str_detect(notes, "70-80's") ~ 75, 
    str_detect(notes, "High 60's") ~ 68,
    str_detect(notes, "70's-80's") ~ 75,
    str_detect(notes, "Low 60's") ~ 62,
    str_detect(notes, "Mid 60's") ~ 65,
    str_detect(notes, "Low 80's") ~ 82, 
    str_detect(notes, "Mid 80's") ~ 85,
    str_detect(notes, "High 80's") ~ 88, 
    str_detect(notes, "Low 70's") ~ 72,
    str_detect(notes, "80's-90's") ~ 85,
    str_detect(notes, "Mid 90's") ~ 95,
    str_detect(notes, "Low 90's") ~ 92,
    str_detect(notes, "High 90's") ~ 98
  ))

write.csv(notes, "data/noteswtemp.csv", row.names = FALSE)

# creating data frame with frequencies of cicadas
#peakfreq_eno = eno_amp[eno_amp[,2] == max_cicada_amplitude_eno , 1]

sites = c("eno", "jmill", "ncbg", "pridge", "unc")

cicada_frequency = data.frame(site = "ex",
                           file = "ex",
                           max_cicada_freq = 1,
                           max_running_avg = 1)
for (s in sites) {
  
  files = list.files(paste0("audiofiles/", s))  
  
  for (f in files) {
    audiofile <-readMP3(paste0("audiofiles/", s, "/", f))
    audiofile_amp = seewave::spec(audiofile, flim = c(.97, 1.23))
    max_cicada_amplitude = max(audiofile_amp[audiofile_amp[,1] > .9 & audiofile_amp[,1] < 1.2, 2])
    peakfreq_audiofile = audiofile_amp[audiofile_amp[,2] == max_cicada_amplitude , 1]
    moving_average <- function(x, n = 5) { stats::filter(x, rep(1 / n, n), sides = 2) }
    runavg = moving_average(audiofile_amp[,2], n = 100)
    max_runavg = max(runavg[audiofile_amp[,2] > .9 & audiofile_amp[,2] < 1.2], na.rm = T)
    
    tmp = data.frame(site = s,
                     file = f,
                     max_cicada_freq = peakfreq_audiofile,
                     max_running_avg = max_runavg)
    cicada_frequency = rbind(cicada_frequency, tmp)
  }
}
cicada_frequency <- 
  cicada_frequency %>%
  filter(site != "ex")


mean(runavg, na.rm = TRUE)
write.csv(cicada_frequency,"data/cicada_frequency.csv",row.names= FALSE)

#joining cicada_frequency and temperature values for each day 
freq <- read.csv("data/cicada_frequency.csv")%>%
  filter(str_select)

#need to figure out how to get 