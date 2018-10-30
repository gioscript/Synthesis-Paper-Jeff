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
yield_unformated %>% 
  combine() %>% 
  bind_rows() %>% 
  # mselect only yield data
  select(siteid, plotid = "Plot ID", year, n_treatment = "N-treatment Corn", matches("yield")) %>%
  gather(key = varname, value = value, -(siteid:n_treatment)) %>% 
  separate(varname, into = c("code", "crop", "defenition"), sep = " ", remove = FALSE, extra = "merge") %>%
  filter(str_detect(varname, "kg/ha") & crop != "Soybean" & !is.na(value))
  distinct(varname) 

# HICKS_B has plot and sub-plot level yield, hence different PlotID needs to be accounted


# after reading need to combine with plot treatment data