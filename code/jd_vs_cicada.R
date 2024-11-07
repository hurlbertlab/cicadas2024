#Script plotting julian day versus cicada amplitude
library(dplyr)
library(stringr)
library(lubridate)

cicada_noise = read.csv(paste0("data/cicada_output.csv"))  %>%
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

#cicada_std_dev = read.csb(paste0(data/cicada_output.csv))
#cicada_dev <- cicada_std_dev %>%


plot(cicada_noise$jd[cicada_noise$site == "eno"], cicada_noise$mean_noise[cicada_noise$site == "eno"], xlab = "Julian Day", ylab = "Cicada Amplitude", ylim = c(0, .20), xlim = c(130,185), type = 'b', col = 'palevioletred4', cex = 1, pch = 25, lwd = 2)


#this function adds the standard deviation to a plot
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

add_standard_deviation()


segments(cicada_noise$jd[cicada_noise$site == "eno"],
         cicada_noise$mean_noise)

arrows(cicada_noise$jd[cicada_noise$site == "eno"], 
       cicada_noise$mean_noise[cicada_noise$site == "eno"] - cicada_output$std_dev[cicada_output$site == "eno"],
       cicada_noise$jd[cicada_noise$site == "eno"], 
       cicada_noise$mean_noise[cicada_noise$site == "eno"] + cicada_noise$std_dev[cicada_noise$site == "eno"],
       angle = 90, code = 3, length = 0.05, col = 'palevioletred4')

points(cicada_noise$jd[cicada_noise$site == "ncbg"], cicada_noise$mean_noise[cicada_noise$site == "ncbg"], type = 'b', col = 'thistle4', pch = 17, cex = 1, lwd = 2)

points(cicada_noise$jd[cicada_noise$site == "pridge"], cicada_noise$mean_noise[cicada_noise$site == "pridge"], type = 'b', col = 'steelblue3', pch = 17, cex = 1, lwd = 2)

points(cicada_noise$jd[cicada_noise$site == "jmill"], cicada_noise$mean_noise[cicada_noise$site == "jmill"], type = 'b', col = 'springgreen1', pch = 17, cex = 1, lwd = 2)

points(cicada_noise$jd[cicada_noise$site == "unc"], cicada_noise$mean_noise[cicada_noise$site == "unc"], type = 'b', col = 'cyan', pch = 17, cex = 1, lwd = 2)
legend("topright", legend = c("Eno River State Park", 
                              "NC Botanical Garden",
                              "Prarie Ridge Ecostation",
                              "Johnston Mill",
                              "UNC Chapel Hill Campus"),
       col = c("palevioletred4", "thistle4", "steelblue3", "springgreen1", 'cyan'),
       pch = c(17, 17, 17, 17),
       lwd = 2,
       cex = 1)
