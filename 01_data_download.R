library(googlesheets)
library(tidyverse)
library(readxl)



# Download Google Sheets --------------------------------------------------

# Download TD Site Metadata + History -------------------------------------

gap_site_history <- gs_key("1oZ2NEmoa0XHSGTWKaBLt0DJK1kIbpWO6iSZ0I2OE2gA")

# save site metadata on local drive
gs_download(from =  gap_site_history,
            to = "./Data/Site_History.xlsx")



# Download on-site weather data -------------------------------------------

# get list of all google sheets that ends with "Weather" in its title
weather_sheets_all <- gs_ls("Weather$")[1]

# get list of sites
read_xlsx("Data/Site_History.xlsx") %>%
  # remove first row containing units
  slice(-1) %>% 
  select(UniqueID, "Drainage Retention Practice") %>%
  # select only controlled drainage sites
  filter(`Drainage Retention Practice` == "Controlled Drainage") %>%
  mutate(sheet_title = paste(UniqueID, "Weather")) %>%
  # find sites form TD that have google sheets for weather
  inner_join(weather_sheets_all, by = "sheet_title") %>%
  .[["sheet_title"]] -> weather_sheet_titles

# save on-site weather files on local drive
for (i in seq_along(weather_sheet_titles)) {
  print(weather_sheet_titles[i])
  gs_download(from = gs_title(weather_sheet_titles[i]),
              to = paste0("Data/On-Site-Weather/", weather_sheet_titles[i], ".xlsx"),
              overwrite = TRUE)
  }



# Download on-site yield data ---------------------------------------------

# get list of all google sheets that ends with "Crop Yield Data" in its title
yield_sheets_all <- gs_ls("Crop Yield Data$")[1]

# get list of sites
read_xlsx("Data/Site_History.xlsx") %>%
  # remove first row containing units
  slice(-1) %>% 
  select(UniqueID, "Drainage Retention Practice") %>%
  # select only controlled drainage sites
  filter(`Drainage Retention Practice` == "Controlled Drainage") %>%
  mutate(sheet_title = paste(UniqueID, "Crop Yield Data")) %>%
  # find sites form TD that have google sheets for yield
  inner_join(yield_sheets_all, by = "sheet_title") %>%
  .[["sheet_title"]] -> yield_sheet_titles

# save on-site weather files on local drive
for (i in seq_along(yield_sheet_titles)) {
  print(yield_sheet_titles[i])
  gs_download(from = gs_title(yield_sheet_titles[i]),
              to = paste0("Data/Yield/", yield_sheet_titles[i], ".xlsx"),
              overwrite = TRUE)
}
