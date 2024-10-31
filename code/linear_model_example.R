#generate fake temperature data and run a linear model with it.

library(dplyr)
library(stringr)

calls <- read.csv("data/cicada_output.csv") %>%
  mutate(temp = case_when(
    max_cicada_amp < .1 ~ 50,
    max_cicada_amp < .3 ~ 60,
    max_cicada_amp < .6 ~ 80,
    TRUE ~ 90
  ))

#run a linear model, effect of temperature on amplitude
m1 <- lm(max_cicada_amp ~ temp, data = calls)

summary(m1)
r2 <- .88

plot(x = calls$temp, y = calls$max_cicada_amp)
abline(m1)

