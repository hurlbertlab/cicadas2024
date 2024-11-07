#linear regression cicada v confidence for each species 
#need to first join birdnetconfidencelevels and cicada output so that I have the columns: species, confidence, circle, date, distance, cicada_runavg
library(dplyr)
library(stringr)
library(broom)
library(tidyr)
cicada <- read.csv('data/cicada_noise.csv')

birdnet <- read.csv('data/birdnetresults.csv')%>%
  select(-(1:2),-7, -(9:11))%>%
  mutate(jd = case_when(
    str_starts(Date, "5") ~ 121 + as.numeric(substr(Date, 2, 3)),
    str_starts(Date, "6") ~ 152 + as.numeric(substr(Date, 2, 3)),
    str_starts(Date, "7") ~ 182 + as.numeric(substr(Date, 2, 3))))

birdsvcicada = left_join(birdnet, cicada, by = c('Location'= 'site', 'jd' = 'jd'))%>%
  select(-7)

#################################
#This function will create a linear regression model for each species modeling cicada amp vs their confidence interval 
lm_cicada <- function(df = birdsvcicada,
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

m1 <- lm_cicada()
summary(m1)
m1$fitted.values

abline(m1, col = "blue")
text(x = min(.10), y = max(.71), labels = paste("R² =", round(r_squared, 3)), pos = 4, col = 'blue')

#tidy up the model
tidied <- tidy(m1) %>%
  pivot_longer(cols = term) %>%
  mutate(common_name = chosen_species) #add species column
write.csv(tidied, "data/results_confvspecies/AmpVYBCU.csv")
m1 <- lm_cicada(birdsvcicada, "Yellow-billed Cuckoo")
