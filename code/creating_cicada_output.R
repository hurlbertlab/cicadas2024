library(warbleR)
library(tuneR)
library(seewave)
library(stringr)
library(dplyr)

sites = c("eno", "jmill", "ncbg", "pridge", "unc")

cicada_output = data.frame(
  site = "ex",
  file = "ex",
  max_cicada_amp = 1,
  max_running_avg = 1,
  max_tone_amp = 1,
  sound_level = 1
)

# Function for centered moving-average smoothing
moving_average <- function(x, n = 5) {
  stats::filter(x, rep(1 / n, n), sides = 2)
}

# Save one plot per audio file
pdf(file = "figures/Cicada_Amplitudes.pdf")

for (s in sites) {
  
  # Get all audio files for the current site
  files = list.files(paste0("audiofiles/", s))
  
  for (f in files) {
    
    # Read the MP3 file
    audiofile <- readMP3(paste0("audiofiles/", s, "/", f))
    
    # Compute the frequency spectrum in the cicada-relevant range
    audiofile_amp = seewave::spec(audiofile, flim = c(.97, 1.23))
    
    # Maximum raw amplitude in the cicada band
    max_cicada_amplitude = max(
      audiofile_amp[audiofile_amp[,1] > .9 & audiofile_amp[,1] < 1.2, 2]
    )
    
    # Maximum amplitude in the nearby tone/reference band
    max_tone_amp = max(
      audiofile_amp[audiofile_amp[,1] > .78 & audiofile_amp[,1] < .90, 2]
    )
    
    # Smoothed spectrum
    runavg = moving_average(audiofile_amp[,2], n = 100)
    
    # Maximum smoothed amplitude in the cicada band
    max_runavg = max(
      runavg[audiofile_amp[,1] > .9 & audiofile_amp[,1] < 1.2],
      na.rm = TRUE
    )
    
    # Plot the smoothed spectrum
    points(audiofile_amp[,1], runavg, type = "l", col = "salmon", lwd = 4)
    abline(v = c(1.0, 1.21), col = "cyan")
    title(main = paste(s, f))
    
    # Store results for this file
    tmp = data.frame(
      site = s,
      file = f,
      max_cicada_amp = max_cicada_amplitude,
      max_running_avg = max_runavg,
      max_tone_amp = max_tone_amp,
      sound_level = max_runavg / max_tone_amp
    )
    
    cicada_output = rbind(cicada_output, tmp)
  }
}

dev.off()

# Remove the placeholder row
cicada_output <- cicada_output %>%
  filter(site != "ex")

# Write results to CSV
write.csv(cicada_output, "data/cicada_output.csv", row.names = FALSE)
