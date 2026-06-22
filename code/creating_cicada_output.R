# This script extracts the amplitude of cicada calls from recordings over 
# multiple sites and dates. (There are multiple recordings per site-date.)

# Within the range of 0.9 - 1.2 kHz, a running 
# average of the sound amplitude by frequency spectrum is taken to smooth
# out spikes and the maximum amplitude within that frequency band is
# saved for each recording. The maximum amplitude of a reference tone
# played in each recording is also stored.


library(warbleR)
library(tuneR)
library(seewave)
library(stringr)
library(dplyr)
library(lubridate)

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

# Write results by site-circle-date to CSV
write.csv(cicada_output, "data/cicada_output_by_site-circle-date.csv", row.names = FALSE)




# Now calculate estimates of average cicada amplitude by site-date (averaging over circles).
# NOTE: The mean_noise column is the mean raw amplitude, as there were issues with 
# the attempt to standardize by tone sound level.

cicada_noise = read.csv(paste0("data/cicada_output_by_site-circle-date.csv"))  %>%
  mutate(date = str_extract(file, pattern = "[0-9][0-9][0-9][0-9]"))%>% 
  mutate(jd = case_when(
    str_starts(date, "05") ~ 121 + as.numeric(substr(date, 3, 4)),
    str_starts(date, "06") ~ 152 + as.numeric(substr(date, 3, 4)),
    str_starts(date, "07") ~ 182 + as.numeric(substr(date, 3, 4))))%>%
  group_by(site, jd) %>%
  mutate(std_dev = sd(max_running_avg)) %>%
  ungroup() %>%
  group_by(site, jd, std_dev)%>%
  summarize(mean_noise = mean(max_running_avg, na.rm =))

write.csv(cicada_noise, "data/cicada_noise_per_site-day.csv")


#######################################
#
# Plotting cicada noise by week
#
# function adds the standard deviation to a plot
add_standard_deviation <- function(df = cicada_noise,
                                   chosen_site = "eno", 
                                   color = "palevioletred4") {
  #filter to just the chosen site
  df <- df %>%
    filter(site == chosen_site)
  
  #add segments, mean +- standard deviation
  segments(df$jd, #x-position
           df$mean_noise - df$std_dev, #y-position
           df$jd,
           df$mean_noise + df$std_dev,
           col = color) 
  
}

plot(cicada_noise$jd[cicada_noise$site == "eno"], cicada_noise$mean_noise[cicada_noise$site == "eno"], xlab = "Julian Day", ylab = "Cicada Amplitude", ylim = c(0, .25), xlim = c(130,185), type = 'b', col = 'palevioletred4', cex = 1, pch = 25, lwd = 2)

add_standard_deviation()

points(cicada_noise$jd[cicada_noise$site == "ncbg"], cicada_noise$mean_noise[cicada_noise$site == "ncbg"], type = 'b', col = 'black', pch = 17, cex = 1, lwd = 2)

add_standard_deviation(chosen_site = "ncbg", color = 'black')

points(cicada_noise$jd[cicada_noise$site == "pridge"], cicada_noise$mean_noise[cicada_noise$site == "pridge"], type = 'b', col = 'steelblue3', pch = 17, cex = 1, lwd = 2)

add_standard_deviation(chosen_site = "pridge", color = 'steelblue3')

points(cicada_noise$jd[cicada_noise$site == "jmill"], cicada_noise$mean_noise[cicada_noise$site == "jmill"], type = 'b', col = 'springgreen1', pch = 17, cex = 1, lwd = 2)

add_standard_deviation(chosen_site = "jmill", color = 'springgreen1')

points(cicada_noise$jd[cicada_noise$site == "unc"], cicada_noise$mean_noise[cicada_noise$site == "unc"], type = 'b', col = 'cyan', pch = 17, cex = 1, lwd = 2)

add_standard_deviation(chosen_site = "unc", color = 'cyan')
legend("topright", legend = c("Eno River State Park", 
                              "NC Botanical Garden",
                              "Prarie Ridge Ecostation",
                              "Johnston Mill",
                              "UNC Chapel Hill Campus"),
       col = c("palevioletred4", "black", "steelblue3", "springgreen1", 'cyan'),
       pch = c(17, 17, 17, 17),
       lwd = 2,
       cex = 1)




# Creating an average site-level measure of cicada abundance based on site-level
# cicada amplitude interpolated for jd 143 which is during the peak cicada period
# but is also the first day for which there are cicada recordings for Prairie Ridge.
# Thus average cicada amplitude will be interpolated on that date for the 4 other sites.

# Keep only the date window of interest
# - exclude Prairie Ridge for now (since we don't need to interpolate it)
# - exclude one Eno observation at jd = 137 (instead, it will be interpolated btw 142 and 149)
cicada_period_data <- cicada_noise %>%
  filter(jd > 136, jd < 150) %>%
  filter(!(site == "eno" & jd == 137)) %>%
  filter(site != "pridge")

# Save Prairie Ridge rows separately and force them into the target-day format
pridgerow <- cicada_noise %>%
  filter(jd > 136, jd < 150, site == "pridge") %>%
  mutate(
    jd = 143,
    calculated_mean_noise = mean_noise
  ) %>%
  select(site, jd, calculated_mean_noise)

# Function to linearly interpolate mean_noise at a chosen day (x_new) for one site
get_imputed_value <- function(df, chosen_site, x_new = 143) {
  site_df <- df %>%
    filter(site == chosen_site) %>%
    arrange(jd)
  
  x1 <- min(site_df$jd)
  x2 <- max(site_df$jd)
  
  y1 <- site_df$mean_noise[site_df$jd == x1][1]
  y2 <- site_df$mean_noise[site_df$jd == x2][1]
  
  y_new <- y1 + (y2 - y1) * (x_new - x1) / (x2 - x1)
  
  return(y_new)
}

# Compute one imputed value per site at jd = 143
imputed_values <- cicada_period_data %>%
  distinct(site) %>%
  mutate(
    jd = 143,
    calculated_mean_noise = case_when(
      site == "eno"   ~ get_imputed_value(cicada_period_data, "eno",   143),
      site == "jmill" ~ get_imputed_value(cicada_period_data, "jmill", 143),
      site == "ncbg"  ~ get_imputed_value(cicada_period_data, "ncbg",  143),
      site == "unc"   ~ get_imputed_value(cicada_period_data, "unc",   143),
      TRUE ~ NA_real_
    )
  )

# Add Prairie Ridge back in
mean_site_values <- bind_rows(imputed_values, pridgerow)

# Write final output
write.csv(mean_site_values, "data/cicada_noise_by_site_on_day143.csv", row.names = FALSE)

# This file now contains one estimated cicada-noise value per site for jd = 143.
