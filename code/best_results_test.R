#creating a script that extracts the single best recording from each site on each day of sampling
library(dplyr) 
library(ggplot2)

highestConf = read.csv(paste0("data/birdsvcicada.csv")) %>%
  group_by(Location, Bird.Call, Distance, jd, mean_noise) %>%
  summarize(Confidence = max(Confidence)) 

#this should give me a data frame of just the highest confidence scores for each species, at each location, on each sampling day  

#running linear regression 
#This function will create a linear regression model for each species modeling cicada amp vs their confidence interval 
lm_cicada <- function(df = highestConf,
                      chosen_species = "Blue-gray Gnatcatcher") {
  #filter to just chosen species 
  df <- df %>%
    filter(Bird.Call == chosen_species)
  #run linear model
  m1 <- lm(Confidence ~ Distance + mean_noise + Distance*mean_noise , data = df)
  plot(x = df$mean_noise, y = df$Confidence,
       xlab = "Mean Cicada Amplitude", 
       ylab = "Confidence Score",
       main = paste("Mean Cicada Amplitude vs Confidence Score for", chosen_species))
  r_squared <-summary(m1)$r.squared
  
  return(m1)
  
}

summary(m1)
m1$fitted.values

abline(m1, col = "blue")
text(x = min(.10), y = max(.71), labels = paste("R² =", round(r_squared, 3)), pos = 4, col = 'blue')

#tidy up the model
tidied <- tidy(m1) %>%
  pivot_longer(cols = term) %>%
  mutate(common_name = chosen_species) #add species column
write.csv(tidied, "data/results_confvspecies/resultsBGGN.csv")
m1 <- lm_cicada(highestConf, "Blue-Gray Gnatcatcher")

#now I want to model the interaction plots again
#modeling an interaction plot
i_plot <- function(df= highestConf,
                   bird = "Acadian Flycatcher"){
  df <- df %>%
    filter(Bird.Call == bird)
  
  df %>%
    ggplot()+
    aes(x = mean_noise, y = Confidence, color = Distance) + 
    geom_point()+ stat_smooth(method = lm) +
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

