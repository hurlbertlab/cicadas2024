# removing calls that occur below .3 
library(dplyr) 
library(ggplot2)
library(tidyr)
library(broom)
library(stringr)
lowHighConf <- read.csv("data/highestConf.csv")%>%
  filter(Confidence < 0.5 & Bird.Call == "Acadian Flycatcher" | Confidence < 0.5 & Bird.Call == "Blue-gray Gnatcatcher" | Confidence < 0.6 & Bird.Call == "Carolina Wren" | Confidence < 0.4 & Bird.Call == "Eastern Wood-Pewee"| Confidence < 0.5 & Bird.Call == "Yellow-billed Cuckoo"| Bird.Call == "Mourning Dove")

confTimes <- read.csv("data/birdnetresults.csv")%>%
  mutate(date = str_extract(resultsfile, pattern = "[0-9][0-9][0-9][0-9]"))%>% 
  mutate(jd = case_when(
    str_starts(date, "05") ~ 121 + as.numeric(substr(date, 3, 4)),
    str_starts(date, "06") ~ 152 + as.numeric(substr(date, 3, 4)),
    str_starts(date, "07") ~ 182 + as.numeric(substr(date, 3, 4))))

lowHighConfTimes <- left_join(lowHighConf, confTimes, by = c("Location", "Bird.Call", "Distance", "jd", "Confidence"))

  
allHighConf <- read.csv("data/highestConf.csv") %>%
  filter(Confidence > 0.3 & Bird.Call != "Mourning Dove" | Bird.Call == "Mourning Dove")


#making interaction plots excluding 
i_plot <- function(df= allHighConf,
                   bird = "Acadian Flycatcher"){
  df <- df %>%
    filter(Bird.Call == bird)
  
  df %>%
    ggplot()+
    aes(x = mean_noise, y = Confidence, color = Distance) + 
    geom_point(aes(shape = Location), size = 3)+ 
    stat_smooth(method = lm) +
    theme_minimal()+
    labs(x = "Mean Cicada Amplitude", y = "Confidence Score", title = paste("Mean Cicada Ampltidue vs Confidence Score for", bird))+
    theme(plot.title = element_text(hjust = 0.5))  
}
CW <- i_plot(bird = "Carolina Wren")
CW
ACFl <- i_plot(bird = "Acadian Flycatcher")
ACFl
BGGN <- i_plot(bird = "Blue-gray Gnatcatcher")
BGGN
EAWP <- i_plot(bird = "Eastern Wood-Pewee")
EAWP
MODO <- i_plot(bird = "Mourning Dove")
MODO
YBCU <- i_plot(bird = "Yellow-billed Cuckoo")
YBCU

