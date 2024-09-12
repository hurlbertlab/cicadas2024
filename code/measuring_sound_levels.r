library(tuneR)
library(warbleR)
library(seewave)
library(stringr)
library(dplyr)

# 

eno <- readMP3("audiofiles/eno/eno_0516_circle1_clipped.mp3")

# visualizing audiofiles with spectrogram (frequency on y and time on x)
spectro(eno, flim = c(0, 8))

# visualizing amplitude versus frequency
par(mfrow = c(1,1))
eno_amp = spec(eno, flim = c(0, 2))
abline(v=c(.9, 1.2), col = 'cyan')

# Peak cicada frequency
max_cicada_amplitude_eno = max(eno_amp[eno_amp[,1] > .9 & eno_amp[,1] < 1.2, 2])

peakfreq_eno = eno_amp[eno_amp[,2] == max_cicada_amplitude_eno , 1]

# Running average using stats::filter()
#  n is the moving window size
moving_average <- function(x, n = 5) { stats::filter(x, rep(1 / n, n), sides = 2) }

runavg = moving_average(eno_amp[,2], n = 10)

#testing 

# Eno River Circle 3 0516
eno2 <- readMP3("audiofiles/eno/eno_0516_circle3_clipped.mp3")
spectro(eno2, flim = c(0,8))
par(mfrow = c(1,1))
par(mar = c(5,5,2,2))
eno_amp = spec(eno2, flim = c(.97, 1.23))
max_cicada_amplitude_eno = max(eno_amp[eno_amp[,1] > .9 & eno_amp[,1] < 1.2, 2])
peakfreq_eno = eno_amp[eno_amp[,2] == max_cicada_amplitude_eno , 1]
moving_average <- function(x, n = 5) { stats::filter(x, rep(1 / n, n), sides = 2) }

runavg = moving_average(eno_amp[,2], n = 100)
max(runavg[eno_amp[,1] > .9 & eno_amp[,1] < 1.2], na.rm = T)

points(eno_amp[,1], runavg, type = 'l', col = 'salmon', lwd = 4)
abline(v=c(1.0, 1.21), col = 'cyan')
#moving_average <- function(x, n = 5) { stats::filter(x, rep(1 / n, n), sides = 2) }
#runavg = moving_average(audiofile_amp[,2], n = 100)
#max(runavg[audiofile_amp[,1] > .9 & eno_amp[,1] < 1.2], na.rm = T)
#points(audiofile_amp[,1], runavg, type = 'l', col = 'salmon', lwd = 4)
#abline(v=c(1.0, 1.21), col = 'cyan')
#how to save an image-> use pdf() or the export button 
#eno amp average amplitude for the cicadas frequencies in a specified band
#We will calculate the sound level by diving the max cicada amp by the max tone amp