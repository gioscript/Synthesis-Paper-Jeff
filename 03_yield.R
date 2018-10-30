library(readxl)
library(tidyverse)
library(lubridate)



# Read crop yield data ----------------------------------------------------

yield_unformated <- vector(mode = "list", length = length(yield_sheet_titles))

for (i in seq_along(yield_sheet_titles)) {
  # exclude files with no cash crop yield data
  if (word(yield_sheet_titles[i]) %in% c("BATH_A", "FAIRM", "MUDS3_OLD", "WILKIN1", "WILKIN2", "UBWC")) {
    print(paste(word(yield_sheet_titles[i]), "is excluded from analysis"))
  } else {
    print(paste("Reading yield data for", word(yield_sheet_titles[i]), "..."))
    # read tab (sheet) names
    yield_sheet_tab_name = excel_sheets(paste0("Data/Yield/", yield_sheet_titles[i], ".xlsx"))
    # add sub-list for each year-tab inside the main list (corresponding to the sites)
    yield_unformated[[i]] = vector(mode = "list", length = length(yield_sheet_tab_name))
    for (j in seq_along(yield_sheet_tab_name)) {
      # ignore tab (sheet) for 2018
      if (yield_sheet_tab_name[j] == "2018") {
        print("Skipping crop year 2018")
      } else {
        # read column headings
        yield_sheet_column_heading = read_excel(paste0("Data/Yield/", yield_sheet_titles[i], ".xlsx"),
                                                sheet = j, n_max = 2) %>% names()
        # read data and add names to columns
        yield_unformated[[i]][[j]] = read_excel(paste0("Data/Yield/", yield_sheet_titles[i], ".xlsx"),
                                                sheet = j, 
                                                # 'range' helps to avoid problems with empty cells
                                                range = cell_limits(c(3, 1), 
                                                                    c(NA, length(yield_sheet_column_heading))),
                                                col_names = yield_sheet_column_heading,
                                                col_types = rep("text", length(yield_sheet_column_heading)))
        # add columns with siteids and crop year
        yield_unformated[[i]][[j]]$siteid = word(yield_sheet_titles[i])
        yield_unformated[[i]][[j]]$year = yield_sheet_tab_name[j]
      }
    }
  }
}


# combine yield data for all sites and years
yield <-
  yield_unformated %>% 
  combine() %>% 
  bind_rows() %>% 
  # select only yield data
  select(siteid, plotid = "Plot ID", year, n_treatment = "N-treatment Corn", matches("yield")) %>%
  # remove decimal point at the end of the plots with numeric names (101.0, etc.)
  mutate(plotid = str_replace(plotid, ".0$", "")) %>%
  # tranform to long table
  gather(key = varname, value = yield, -(siteid:n_treatment)) %>% 
  separate(varname, into = c("code", "crop"), sep = " ", remove = FALSE, extra = "drop") %>%
  mutate(yield = as.numeric(yield),
         year = as.numeric(year)) %>%
  filter(!is.na(yield)) %>%
  select(siteid, plotid, year, n_treatment, crop, varname, yield) %>%
  # separate sub-plot id from plot id for HICKS_B
  mutate(subplotid = ifelse(siteid == "HICKS_B", plotid, NA),
         plotid = ifelse(siteid == "HICKS_B", str_sub(plotid, 1, 2), plotid)) %>%
  select(siteid, plotid, subplotid, everything())



# Summarise crop yield data -----------------------------------------------

# combine yield data with plot dwm treatment
yield_with_dwm_treatment <-
  yield %>%
  # add dwm treatment
  left_join(plot_dwm_treatment, by = c("siteid", "plotid", "year")) %>%
  left_join(plot_identifier, by = c("siteid", "plotid")) %>%
  arrange(siteid, year, plotid) %>%
  select(siteid, plotid, subplotid, DWM, year, n_treatment, dwm_treatment, crop, yield) %>%
  # change "varies" with actual dwm treatment
  mutate(DWM = ifelse(DWM == "varies" & dwm_treatment == "CD", "controlled drainage", DWM),
         DWM = ifelse(DWM == "varies" & dwm_treatment == "FD", "free drainage", DWM)) %>%
  select(-ends_with("treatment")) %>%
  rename(dwm = DWM)


# add monthly precipitation
yield_with_dwm_and_precip <-
  weather_monthly %>%
  select(siteid, year, month, precip_30 = precip_30_year_monthly_ave, precip_diff) %>%
  filter(month %in% c("May", "Jun", "Jul", "Aug")) %>%
  gather(key = variable, value = value, precip_30:precip_diff) %>%
  unite(temp, month, variable) %>%
  spread(key = temp, value = value) %>%
  select(siteid, year, May_precip_30, May_precip_diff, Jun_precip_30, Jun_precip_diff,
         Jul_precip_30, Jul_precip_diff, Aug_precip_30, Aug_precip_diff) %>%
  right_join(yield_with_dwm_treatment, by = c("siteid", "year")) %>% 
  select(siteid, plotid, subplotid, dwm, year, crop, yield, everything())
  

# save data as CSV file
yield_with_dwm_and_precip %>%
  mutate_at(vars(yield:Aug_precip_diff), round, 1) %>%
  write_csv(paste0("Output/Data/yield_", Sys.Date(), ".csv"), na = "")
