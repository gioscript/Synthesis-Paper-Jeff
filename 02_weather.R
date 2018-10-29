library(readxl)
library(tidyverse)
library(lubridate)



# Read weather station keys -----------------------------------------------

weather_station_keys <- read_csv("Data/weather_station_key.txt")



# Read long-term weather data ---------------------------------------------

weather_long_term <- read_csv("Data/weather_long_term.txt", skip = 4) %>%
  left_join(weather_station_keys, by = c("station"="stationid")) %>%
  select(siteid, day:merra_srad) 
  # NOTE: some weather stations include several sites.
  #       MO2482 = {MUDS2, MUDS3_OLD, MUDS4}
  #       OH4189 = {HARDIN, HARDIN_NW}



# Read on-site weather data -----------------------------------------------

weather_on_site_unformated <- vector(mode = "list", length = length(weather_sheet_titles))

for (i in seq_along(weather_sheet_titles)) {
  # exclude sites that has no data or multiple on-site weather stations
  if (weather_sheet_titles[i]  %in% c("BATH_A Weather", "FAIRM Weather", "SERF_IA Weather")) {
    print(paste(weather_sheet_titles[i], "needs to be processed individually"))
  } else {
    print(weather_sheet_titles[i])
    # read column headings
    weather_sheet_column_heading = read_excel(paste0("Data/On-Site-Weather/", weather_sheet_titles[i] , ".xlsx"),
                                              sheet = "DAILY", n_max = 2) %>% names()
    # read data and add names to columns
    weather_on_site_unformated[[i]] = read_excel(paste0("Data/On-Site-Weather/", weather_sheet_titles[i] , ".xlsx"),
                                                 sheet = "DAILY", skip = 2, 
                                                 col_names = weather_sheet_column_heading,
                                                 col_types = c("date", rep("numeric", length(weather_sheet_column_heading) - 1)))
    # add column with site ID
    weather_on_site_unformated[[i]]$siteid = word(weather_sheet_titles[i])
  }
}
  

# read data from sites with multiple weather stations

# FAIRM >>>

# Eddy Covariance weather station 
weather_on_site_unformated_FAIRM_Eddy <-
  read_excel("Data/On-Site-Weather/FAIRM Weather.xlsx",
             sheet = "DAILY Eddy Covariance", skip = 2,
             col_names = c("Date", "Precipitation"))

# East weather station 
weather_on_site_unformated_FAIRM_East <-
  read_excel("Data/On-Site-Weather/FAIRM Weather.xlsx",
             sheet = "DAILY East", skip = 2,
             col_names = c("Date", "Precipitation"))

# Precipitation measurement overlaps in 2017
# according to site personal notes "East records seem more accurate"
weather_on_site_unformated_FAIRM <-
  weather_on_site_unformated_FAIRM_Eddy %>%
  filter(year(Date) < 2017) %>%
  bind_rows(weather_on_site_unformated_FAIRM_East) %>%
  mutate(siteid = "FAIRM")


# SERF_IA >>>

# Manual weather station 
weather_on_site_unformated_SERF_IA_Manual <-
  read_excel("Data/On-Site-Weather/SERF_IA Weather.xlsx",
             sheet = "MANUAL", skip = 2,
             col_names = c("Date", "Precipitation"))

# LevelRain weather station 
weather_on_site_unformated_SERF_IA_LevelRain <-
  read_excel("Data/On-Site-Weather/SERF_IA Weather.xlsx",
             sheet = "DAILY LevelRain", skip = 2,
             col_names = c("Date", "Precipitation"))

# ISU SM Network weather station 
weather_on_site_unformated_SERF_IA_ISUnetwork <-
  read_excel("Data/On-Site-Weather/SERF_IA Weather.xlsx",
             sheet = "DAILY ISUnetwork", skip = 2,
             col_names = c("Date", "Precipitation"))

# overlaping precipitation records were chosen according to Kristina Craft's recommendation (email from 2017-04-07)
weather_on_site_unformated_SERF_IA <-
  weather_on_site_unformated_SERF_IA_Manual %>%
  filter(year(Date) < 2012) %>%
  bind_rows(weather_on_site_unformated_SERF_IA_LevelRain %>%
              filter(year(Date) %in% c(2012, 2013))) %>%
  bind_rows(weather_on_site_unformated_SERF_IA_ISUnetwork %>%
              filter(year(Date) > 2013)) %>%
  mutate(siteid = "SERF_IA")


# combine on-site weather data for all sites
weather_on_site <-
  weather_on_site_unformated %>%
  bind_rows() %>%
  bind_rows(weather_on_site_unformated_FAIRM, weather_on_site_unformated_SERF_IA) %>%
  select(siteid, date = Date, precip_on_site = Precipitation) %>%
  mutate(date = as.Date(date)) 


# combine on-site and long-term weather station data
weather <- 
  weather_long_term %>%
  rename(date = day, precip_weather_station = precipmm) %>%
  filter(siteid %in% word(weather_sheet_titles)) %>%
  select(-doy, -merra_srad) %>%
  full_join(weather_on_site, by = c("siteid", "date")) %>%
  arrange(siteid, date) %>%
  mutate(precip_on_site = ifelse(is.na(precip_on_site), precip_weather_station, precip_on_site))



# Summarise weather data --------------------------------------------------

# calculate difference between monthly and 30-year--monthly-averages
weather_monthly <-
  weather %>%
  mutate(year = year(date),
         month = month(date, label = TRUE)) %>%
  # analysis does not include data from years > 2017
  filter(year < 2018) %>%
  # analysis does not include sites that are subirrigated
  filter(siteid != "FAIRM") %>%
  # calculate monthly precip
  group_by(siteid, year, month) %>%
  summarise_at(c("precip_on_site", "precip_weather_station"), sum) %>%
  # calculate 30-year monthly average (1986-2015)
  group_by(siteid, month) %>% 
  mutate(precip_30_year_monthly_ave = mean(precip_weather_station[between(year, 1986, 2015)])) %>%
  ungroup() %>%
  mutate(precip_diff = precip_on_site - precip_30_year_monthly_ave)




