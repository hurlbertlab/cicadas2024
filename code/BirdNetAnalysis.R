###########################

##  Automating BirdNet  ##

###########################
library(reticulate)
library(birdnetR)

# Initialize the TFLite BirdNET Model
model <- birdnet_model_tflite("v2.4")
