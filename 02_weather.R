library(tidyverse)
library(lubridate)



# Read weather station keys -----------------------------------------------

weather_station_keys <- read_csv("Data/weather_station_key.txt")



# Read long-term weather data ---------------------------------------------

weather_long_term <- read_csv("Data/weather_long_term.txt", skip = 4) %>%
  left_join(weather_station_keys, by = c("station"="stationid")) %>%
  select(siteid, day:merra_srad) 

