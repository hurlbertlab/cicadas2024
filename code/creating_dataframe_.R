library(tuneR)
library(warbleR)
library(seewave)
library(stringr)
library(dplyr)

sites = c("eno", "jmill", "ncbg", "pridge", "unc")


cicada_output = data.frame(site = "ex",
                    file = "ex",
                    max_cicada_amp = 1,
                    max_running_avg = 1,
                    max_tone_amp = 1,
                    sound_level = 1)
pdf(file = "figures/Cicada_Amplitudes.pdf")

for (s in sites) {
  
  files = list.files(paste0("audiofiles/", s))  
  
  for (f in files) {
    audiofile <-readMP3(paste0("audiofiles/", s, "/", f)) 
    audiofile_amp = spec(audiofile, flim = c(.97, 1.23))
    max_cicada_amplitude = max(audiofile_amp[audiofile_amp[,1] > .9 & audiofile_amp[,1] < 1.2, 2])
    peakfreq_audiofile = audiofile_amp[audiofile_amp[,2] == max_cicada_amplitude , 1]
    max_tone_amp = max(audiofile_amp[audiofile_amp[,1] > .78 & audiofile_amp[,1] < .90, 2])
    moving_average <- function(x, n = 5) { stats::filter(x, rep(1 / n, n), sides = 2) }
    runavg = moving_average(audiofile_amp[,2], n = 100)
    max_runavg = max(runavg[audiofile_amp[,1] > .9 & audiofile_amp[,1] < 1.2], na.rm = T)
    points(audiofile_amp[,1], runavg, type = 'l', col = 'salmon', lwd = 4)
    abline(v=c(1.0, 1.21), col = 'cyan')
    title(main = paste(s,f))

    # read in audiofile
    # figure out max amplitude
    
    tmp = data.frame(site = s,
                     file = f,
                     max_cicada_amp = max_cicada_amplitude,
                     max_running_avg = max_runavg,
                     max_tone_amp = max_tone_amp,
                     sound_level = max_cicada_amplitude/max_tone_amp) 
    
    cicada_output = rbind(cicada_output, tmp)
    
  }
}
dev.off() 

cicada_output <- 
  cicada_output %>%
  filter(site != "ex")


mean(runavg, na.rm = TRUE)
write.csv(cicada_output,"data/cicada_output.csv",row.names= FALSE)
