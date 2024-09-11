
sites = c("eno", "jmill", "ncbg", "pridge", "unc")


cicada_output = data.frame(site = NULL,
                    file = NULL,
                    max_cicada_amp = NULL,
                    max_tone_amp = NULL,
                    sound_level = NULL)

for (s in sites) {
  
  files = list.files(paste0("audiofiles/", s))  
  
  for (f in files) {
    
    audiofile <-readMP3(paste0("audiofiles/", s, "/", f)) 
    audiofile_amp = spec(audiofile, flim = c(.97, 1.23))
    max_cicada_amplitude = max(audiofile_amp[audiofile_amp[,1] > .9 & audiofile_amp[,1] < 1.2, 2])
    peakfreq_audiofile = audiofile_amp[audiofile_amp[,2] == max_cicada_amplitude , 1]
    max_tone_amp = max(audiofile_amp[audiofile_amp[,1] > .78 & audiofile_amp[,1] < .90, 2])
    moving_average <- function(x, n = 5) { stats::filter(x, rep(1 / n, n), sides = 2) }
    runavg = moving_average(eno_amp[,2], n = 100)
    max(runavg[eno_amp[,1] > .9 & eno_amp[,1] < 1.2], na.rm = T)
    points(eno_amp[,1], runavg, type = 'l', col = 'salmon', lwd = 4)
    abline(v=c(1.0, 1.21), col = 'cyan')
    # read in audiofile
    # figure out max amplitude
    
    tmp = data.frame(site = s,
                     file = f,
                     max_cicada_amp = max_cicada_amplitude,
                     max_tone_amp = max_tone_amp,
                     sound_level = max_cicada_amplitude/max_tone_amp) 
    
    output = rbind(cicada_output, tmp)
    
  }
}

