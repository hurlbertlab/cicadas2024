#creating a script that extracts the single best recording from each site on each day of sampling

highestConf = read.csv(paste0("data/birdsvcicada.csv")) %>%
  group_by(Location, Bird.Call, Distance, jd, mean_noise) %>%
  summarize(max(Confidence))

#this should give me a data frame of just the highest confidence scores for each species, at each location, on each sampling day  

#now I want to model the interaction plots again
#modeling an interaction plot
i_plot <- function(df= highestConf,
                   bird = "Acadian Flycatcher"){
  df <- df %>%
    filter(Bird.Call == bird)
  
  df %>%
    ggplot()+
    aes(x = mean_noise, y = max(Confidence), color = Distance) + 
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