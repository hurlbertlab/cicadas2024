###########################

##  Automating BirdNet  ##

###########################
library(reticulate)
library(birdnetR)

# Initialize the TFLite BirdNET Model
model <- birdnet_model_tflite()

# set min confidence and occurences 
min_confidence <- 0.5
min_occurrences <- 3

# Path to audiofiles
audio_path <- "audiofiles/eno/eno_0516_circle3_clipped.mp3"

# path to output 
output_dir <- "data/birdNetMasonFarm"

# Need to create a loop to go through each audiofile in each folder and do birdnet on it 
all_results <- data.frame()
results <- birdnet_analyze