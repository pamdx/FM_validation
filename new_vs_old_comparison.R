rm(list = ls())

# Load necessary packages

library(tidyverse)
library(readxl)

theme_set(theme_bw()) # set simple theme for charts

source("functions.R")

# Load datasets

FM_old <- read_excel(choose.files(caption = "Please select the old employment database"))
FM_new <- read_excel(choose.files(caption = "Please select the new employment database"))

if (any(names(FM_new) != names(FM_old))) {
  stop("The old and new databases have different columns.")
}

FM_new_filtered <- FM_new %>%
  arrange("geographic_area", "OC2", "year", "OC3", "working_time", "sex") %>%
  #filter(is.na(flag) | flag != "X", OC2 != "Unspecified") %>%
  mutate(db = "new")

FM_old_filtered <- FM_old %>%
  arrange("geographic_area", "OC2", "year", "OC3", "working_time", "sex") %>%
  #filter(is.na(flag) | flag != "X", OC2 != "Unspecified") %>%
  mutate(db = "old") %>%
  filter(year %in% FM_new_filtered$year)

FM_new_vs_old <- FM_new_filtered %>%
  bind_rows(FM_old_filtered)

# Chart of global employment comparison

FM_new_vs_old %>%
  group_by(year, db) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(year, value, color = db)) +
  geom_line() +
  scale_y_continuous(labels = addUnits) + 
  scale_x_continuous(breaks = integer_breaks()) +
  labs(title = "Global employment: comparison of new and old databases")

FM_new_vs_old_diff <- FM_new_vs_old %>%
  group_by(year, db) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = db, values_from = value) %>%
  mutate(diff = new - old, percent_diff = round(((new/old) - 1) * 100, 2)) 

# Charts of sectorial employment comparison

for (i in unique(FM_new_vs_old$OC2)) {
  
  print(
    FM_new_vs_old %>%
      filter(OC2 == i) %>%
      group_by(year, db) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      ggplot(aes(year, value, color = db)) +
      geom_line() +
      scale_y_continuous(labels = addUnits) + 
      scale_x_continuous(breaks = integer_breaks()) +
      labs(title = paste(i, "employment: comparison of new and old databases"))
  )
  
}

# Charts of employment flag comparison

for (i in unique(FM_new_vs_old$flag)) {
  
  print(
    FM_new_vs_old %>%
      filter(ifelse(is.na(i), is.na(flag), flag == i)) %>%
      group_by(year, db) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      ggplot(aes(year, value, color = db)) +
      geom_line() +
      scale_y_continuous(labels = addUnits) + 
      scale_x_continuous(breaks = integer_breaks()) +
      labs(title = paste(i, "flag employment: comparison of new and old databases"))
  )
  
}

# Difference in number of countries/country-sector combinations

FM_new_vs_old %>%
  group_by(db) %>%
  summarise(number_countries = n_distinct(geographic_area))

FM_new_vs_old %>%
  group_by(db) %>%
  summarise(number_country_sector_comb = n_distinct(geographic_area, OC3))

country_sector_new <- unique(
  FM_new_vs_old %>%
  mutate(conc = paste(geographic_area, OC3, sep = "|")) %>%
  filter(db == "new") %>%
  pull(conc)
)

country_sector_old <- unique(
  FM_new_vs_old %>%
    mutate(conc = paste(geographic_area, OC3, sep = "|")) %>%
    filter(db == "old") %>%
    pull(conc)
)

if (length(setdiff(country_sector_new, country_sector_old)) > 0) {
  
  message("The following country/sector combinations are present in the new dataset but not in the old one:")
  setdiff(country_sector_new, country_sector_old)
  
}

if (length(setdiff(country_sector_old, country_sector_new)) > 0) {
  
  message("The following country/sector combinations are present in the old dataset but not in the new one:")
  setdiff(country_sector_old, country_sector_new)
  
}

country_sector_comp <- FM_new_vs_old %>%
  group_by(geographic_area, OC3, db) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = db, values_from = value) %>%
  filter(is.na(new) | is.na(old))

# Percentage of estimated data per country/country-sector combinations

FM_new_vs_old %>%
  group_by(geographic_area, flag, db) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = flag, values_from = value) %>%
  mutate(share_estimated = E/rowSums(select_if(., is.numeric), na.rm = TRUE)) %>%
  select(geographic_area, db, share_estimated) %>%
  pivot_wider(names_from = db, values_from = share_estimated) %>%
  mutate(difference_estimated = new - old) %>%
  arrange(desc(difference_estimated))

FM_new_vs_old %>%
  group_by(geographic_area, OC2, flag, db) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = flag, values_from = value) %>%
  mutate(share_estimated = E/rowSums(select_if(., is.numeric), na.rm = TRUE)) %>%
  select(geographic_area, OC2, db, share_estimated) %>%
  pivot_wider(names_from = db, values_from = share_estimated) %>%
  mutate(difference_estimated = new - old) %>%
  arrange(desc(difference_estimated))
