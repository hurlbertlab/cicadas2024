
sites = c("eno", "jmill", "ncbg", "pridge", "unc")


output = data.frame(site = NULL,
                    file = NULL,
                    max_cicada_amp = NULL,
                    max_tone_amp = NULL,
                    sound_level = NULL)

for (s in sites) {
  
  files = list.files(paste0("audiofiles/", s))  
  
  for (f in files[1:3]) {
    
    audiofile <-readMP3(paste0("audiofiles/", s, "/", f)) 
    audiofile_amp = spec(audiofile, flim = c(.97, 1.23))
    max_cicada_amplitude = max(audiofile_amp[audiofile_amp[,1] > .9 & audiofile_amp[,1] < 1.2, 2])
    peakfreq_audiofile = audiofile_amp[audiofile_amp[,2] == max_cicada_amplitude , 1]
    max_tone_amp = max(audiofile_amp[audiofile_amp[,1] > .78 & audiofile_amp[,1] < .90, 2])
    # read in audiofile
    # figure out max amplitude
    
    tmp = data.frame(site = s,
                     file = f,
                     max_cicada_amp = max_cicada_amplitude,
                     max_tone_amp = max_tone_amp,
                     sound_level = max_cicada_amplitude/max_tone_amp) 
    
    output = rbind(output, tmp)
    
  }
}


