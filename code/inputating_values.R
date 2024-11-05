#creating inputated value of missing prarie ridge day 

library(dplyr)
library(stringr)
library(lubridate)

cicada_noise = read.csv(paste0("data/cicada_output.csv"))  
cicada_noise<- cicada_noise%>%
  mutate(date = str_extract(file, pattern = "[0-9][0-9][0-9][0-9]"))%>% 
  mutate(jd = case_when(
    str_starts(date, "05") ~ 121 + as.numeric(substr(date, 3, 4)),
    str_starts(date, "06") ~ 152 + as.numeric(substr(date, 3, 4)),
    str_starts(date, "07") ~ 182 + as.numeric(substr(date, 3, 4))))%>%
  group_by(site, jd)%>%
  summarize(mean_noise = mean(max_running_avg, na.rm =))

algebra_inputation <- cicada_noise %>%
  filter(jd > 136,
         jd < 150) %>%
  filter(!(site == "eno" & jd == 137)) %>%
  filter(site != "pridge") #filtering out prarie ridge bc its the date we're imputing to

pridgerow <- cicada_noise %>%
  filter(jd > 136,
         jd < 150,
         site == "pridge") %>%
  mutate(x_new = 143,
         y_new = mean_noise) %>%
  select(-jd)

get_inputed_value <- function(df = algebra_inputation,
                              chosen_site = "eno",
                              x_new = 143) {
  
  df <- df %>%
    filter(site == chosen_site)
  
  x1 = min(df$jd)
  x2 = max(df$jd)
  y1 = df$mean_noise[df$jd == x1]
  y2 = df$mean_noise[df$jd == x2]
  
  y_new = y1 + (y2-y1) * (x_new - x1) / (x2 - x1)
  
  #return
  y_new
  
}


algebra_inputation$y_new <- 
  c(get_inputed_value(chosen_site = "eno"),
    get_inputed_value(chosen_site = "eno"),
    get_inputed_value(chosen_site = "jmill"),
    get_inputed_value(chosen_site = "jmill"),
    get_inputed_value(chosen_site = "ncbg"),
    get_inputed_value(chosen_site = "ncbg"),
    get_inputed_value(chosen_site = "unc"),
    get_inputed_value(chosen_site = "unc"))

#cleanup
algebra_inputation <- 
  algebra_inputation %>%
  mutate(x_new = 143) %>%
  distinct(site, y_new, x_new) %>%
  #add back in pridge
  bind_rows(pridgerow) %>%
  #give ynew and xnew better names
  rename(jd = x_new,
         calculated_mean_noise = y_new)
write.csv(algebra_inputation, "data/inputated_values.csv", row.names = FALSE)
#add back in pridge

#now you have all the values for date 143, which represents our "single value" of cicada noise that can characterize each site. 