#Script plotting julian day versus cicada amplitude
library(dplyr)
library(stringr)
library(lubridate)

cicada_noise = read.csv(paste0("data/cicada_output.csv"))  
cicada_noise<- cicada_noise%>%
  mutate(date = str_extract(file, pattern = "[0-9][0-9][0-9][0-9]"))%>% 
  mutate(jd = case_when(
    str_starts(date, "05") ~ 121 + as.numeric(substr(date, 3, 4)),
    str_starts(date, "06") ~ 152 + as.numeric(substr(date, 3, 4)),
    str_starts(date, "07") ~ 182 + as.numeric(substr(date, 3, 4))
  ))

#Plotting eno River    
plot(cicada_noise$jd[cicada_noise$site == "eno"], cicada_noise$sound_level[cicada_noise$site == "eno"], xlab = "Julian Day", ylab = "Cicada Amplitude", ylim = c(0, .7), xlim = c(130,185), type = 'b', col = 'red', cex = 1, pch = 25, lwd = 2)

points(cicada_noise$jd[cicada_noise$site == "ncbg"], cicada_noise$sound_level[cicada_noise$site == "ncbg"], type = 'b', col = 'magenta', pch = 17, cex = 1, lwd = 2)

points(cicada_noise$jd[cicada_noise$site == "pridge"], cicada_noise$sound_level[cicada_noise$site == "pridge"], type = 'b', col = 'blue', pch = 17, cex = 1, lwd = 2)

points(cicada_noise$jd[cicada_noise$site == "jmill"], cicada_noise$sound_level[cicada_noise$site == "jmill"], type = 'b', col = 'green', pch = 17, cex = 1, lwd = 2)

legend("topright", legend = c("Eno River State Park", 
                              "NC Botanical Garden",
                              "Prarie Ridge Ecostation",
                              "Johnston Mill"),
       col = c("red", "magenta", "blue", "green"),
       pch = c(17, 17, 17, 17),
       lwd = 2,
       cex = 1)
