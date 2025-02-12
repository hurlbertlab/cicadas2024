####################

####Grouping by site for confidence levels #####
library(dplyr) 
library(ggplot2)
library(tidyr)
library(broom)

cicadas <- read.csv("data/birdsvcicada.csv")

i_plot <- function(df= cicadas,
                   bird = "Acadian Flycatcher",
                   site = "eno"){
  df <- df %>%
    filter(Bird.Call == bird, Location == site)
  
  df %>%
    ggplot()+
    aes(x = mean_noise, y = Confidence, color = Distance) + 
    geom_point()+ stat_smooth(method = lm) +
    theme_minimal()+
    labs(x = "Mean Cicada Amplitude", y = "Confidence Score", title = paste("Mean Cicada Ampltidue vs Confidence Score for", bird, "at", site))+
    theme(plot.title = element_text(hjust = 0.5))  
}

for (place in unique(cicadas$Location)) {
  CW <- i_plot(bird = "Carolina Wren", site = place)
  ACFL <- i_plot(bird = "Acadian Flycatcher", site = place)
  BGGN <- i_plot(bird = "Blue-gray Gnatcatcher", site = place)
  EAWP <- i_plot(bird = "Eastern Wood-Pewee", site =place)
  MODO <- i_plot(bird = "Mourning Dove", site =place)
  YBCU <- i_plot(bird = "Yellow-billed Cuckoo", site =place)
  print(CW)  # This will display the plot for each location
  print(ACFL)
  print(BGGN)
  print(EAWP)
  print(MODO)
  print(YBCU)
}

CW <- i_plot(bird = "Carolina Wren", site = "unc")
CW
ACFl <- i_plot(bird = "Acadian Flycatcher", site = "unc")
ACFl
BGGN <- i_plot(bird = "Blue-gray Gnatcatcher", site = "unc")
BGGN
EAWP <- i_plot(bird = "Eastern Wood-Pewee", site = "unc")
EAWP
MODO <- i_plot(bird = "Mourning Dove", site = "unc")
MODO
YBCU <- i_plot(bird = "Yellow-billed Cuckoo", site = "unc")
YBCU
