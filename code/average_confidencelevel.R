#creating data frame for average confidence level of each bird species

#need to join sheet with BirdNET results to sheet with call times on it
wd <- "Z:/Databases/CaterpillarsCount/AudioMoth/BirdNetResults"
setwd("Z:/Databases/CaterpillarsCount/AudioMoth/BirdNet Results")
confirmed_calls = read.csv(paste(wd,"bird_calls_confirmed.csv", sep=""))

#need to create a read.csv line of code that will go through each excel sheet with results and rbinds them.
