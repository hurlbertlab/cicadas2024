
sites = c("EnoRiver", "JMill", "ncbg", "PRidge", "UNC")


output = data.frame(site = NULL,
                    file = NULL,
                    max_amp = NULL)

for (s in sites) {
  
  files = list.files(paste0("audiofiles/", s))  
  
  for (f in files[1:2]) {
    
    audiofile <-readMP3(f)
    audiofile_amp = spec(audiofile, flim = c(.97, 1.23))
    max_cicada_amplitude = max(audiofile_amp[audiofile_amp[,1] > .9 & audiofile_amp[,1] < 1.2, 2])
    peakfreq_audiofile = audiofile_amp[audiofile_amp[,2] == max_cicada_amplitude , 1]
    # read in audiofile
    # figure out max amplitude
    
    tmp = data.frame(site = s,
                     file = f,
                     max_amp = max_cicada_amplitude) 
    
    output = rbind(output, tmp)
    
  }
}


