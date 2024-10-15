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
    
    #May first is 122, so 0501 is 112, case_when if starts with 05, give it 121 + the second number eg. 15
    #June first is 153, case_when if it starts with 06, give it 152+ the second number eg. 15
    