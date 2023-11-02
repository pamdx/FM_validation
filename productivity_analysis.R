rm(list = ls())

# Load necessary packages

library(tidyverse)

theme_set(theme_bw()) # set simple theme for charts

source("functions.R")

# Load datasets

FM_new <- read_csv("C:/Users/pierr/Desktop/MOD3_consolidation/FM_DB_imputed_1995-2020.csv")
prod <- readRDS("C:/Users/pierr/Desktop/emputator/emputator/inputs/PROD.rds")

# Combine datasets

FM_new_filtered <- FM_new %>%
  filter(is.na(flag) | flag != "X", OC2 %in% unique(prod$OC2)) %>%
  group_by(geographic_area, OC2, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(conc = paste(geographic_area, OC2, year, sep = "|"))

productivity <- FM_new_filtered %>%
  left_join(prod %>% mutate(conc = paste(country, OC2, year, sep = "|"))) %>%
  select(geographic_area, OC2, year, value, prod_value) %>%
  mutate(productivity = prod_value/value)

# Check for situations where there is production but no employment

prod_noemp <- productivity %>%
  filter((prod_value > 0) & (is.na(value) | value == 0)) 

# Check for situations where there is employment but no production

emp_noprod <- productivity %>%
  filter((is.na(prod_value) | prod_value == 0) & (value > 0)) 

# Visualize global productivity evolution per sector

for (i in unique(productivity$OC2)) {
  print(
    productivity %>%
      filter(OC2 == i) %>%
      group_by(year) %>%
      summarise(value = sum(value, na.rm = TRUE), prod_value = sum(prod_value, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(avg_prod = prod_value/value) %>%
      ggplot(aes(x = year, y = avg_prod)) +
      geom_line() +
      scale_y_continuous(labels = addUnits) + 
      scale_x_continuous(breaks = integer_breaks()) +
      labs(title = paste(i, "employment: global average productivity by year")) 
  )
}

# Identify outliers at the row level

sum_stat_productivity <- productivity %>%
  select(-c(value,prod_value)) %>%
  group_by(OC2, year) %>%
  summarise(median_prod = median(productivity, na.rm = TRUE), 
            mean_prod = mean(productivity, na.rm = TRUE), 
            q1_prod = quantile(productivity, probs = 0.25, na.rm = TRUE),
            q3_prod = quantile(productivity, probs = 0.75, na.rm = TRUE)) %>%
  mutate(IQR = q3_prod - q1_prod) %>%
  mutate(upper_outlier_threshold = q3_prod + 1.5 * IQR) %>%
  mutate(lower_outlier_threshold = q1_prod - 1.5 * IQR) %>%
  mutate(conc = paste(OC2, year, sep = "|"))

outliers_identification <- productivity %>%
  mutate(conc = paste(OC2, year, sep = "|")) %>%
  left_join(sum_stat_productivity) %>%
  select(-conc) %>%
  mutate(upper_outlier = productivity > upper_outlier_threshold) %>%
  mutate(lower_outlier = productivity < lower_outlier_threshold)

if (sum(outliers_identification$upper_outlier, na.rm = TRUE) > 0) {
  
  message(paste("There are", paste(sum(outliers_identification$upper_outlier, na.rm = TRUE), collapse = ""), "upper productivity outliers. This represents", paste(round(sum(outliers_identification$upper_outlier, na.rm = TRUE)/nrow(outliers_identification), 2), collapse = ""), "percent of the data."))
  
}

# Visualize outliers per sector

for (i in unique(productivity$OC2)) {
  
  print(
    productivity %>%
      filter(OC2 == i) %>%
      ggplot(aes(year, productivity, group = year)) +
      geom_boxplot() +
      scale_y_continuous(labels = addUnits) + 
      scale_x_continuous(breaks = integer_breaks()) +
      labs(title = paste(i, "employment: productivity outliers by year"))
  )
}

# Boxplot of productivities per year

for (i in unique(productivity$OC2)) {
  
  y_limit <- max(
    sum_stat_productivity %>% 
      filter(OC2 == i) %>% 
      pull(upper_outlier_threshold)
  )
  
  print(
    productivity %>%
      filter(OC2 == i) %>%
      ggplot(aes(year, productivity, group = year)) +
      geom_boxplot() +
      stat_summary(fun.y = mean, geom = "point", shape = 4, size = 4) +
      scale_y_continuous(labels = addUnits) + 
      scale_x_continuous(breaks = integer_breaks()) +
      labs(title = paste(i, "employment: boxplots of productivities by year"), 
           subtitle = "The crosses indicate yearly means.",
           caption = "Y axis trucated to focus on ±1.5*IQR") +
      coord_cartesian(ylim = c(0, y_limit))
  )
}

# Visualize distribution of sectorial productivities by year

library(ggridges)

for (i in unique(productivity$OC2)) {
  
  x_limit <- max(
    sum_stat_productivity %>% 
      filter(OC2 == i) %>% 
      pull(upper_outlier_threshold)
  )
  
  print(
    productivity %>% filter(!is.na(productivity), !is.infinite(productivity)) %>%
      filter(OC2 == i) %>%
      ggplot(aes(x = productivity, y = year, group = year)) +
      geom_density_ridges() +
      theme_ridges() +
      scale_x_continuous(labels = addUnits, limits=c(0, x_limit)) + 
      scale_y_continuous(breaks = integer_breaks()) +
      labs(title = paste(i, "employment: distribution of productivities by year"),
           caption = "Y axis trucated to focus on ±1.5*IQR")
  )
}

# Table to check most productive countries for each sector/year combination

productivity %>%
  select(-c(value,prod_value)) %>%
  mutate(conc = paste(OC2, year, sep = "|")) %>%
  select(-c(OC2, year)) %>%
  pivot_wider(names_from = conc, values_from = productivity)

# Charts of top 10 most productive countries for each sector

most_productive <- productivity %>%
  select(-c(value,prod_value)) %>%
  group_by(geographic_area, OC2) %>%
  summarise(avg_productivity = mean(productivity, na.rm = TRUE)) %>%
  ungroup()
  # pivot_wider(names_from = year, values_from = productivity) %>%
  # mutate(avg_prod = rowMeans(select_if(., is.numeric), na.rm = TRUE)) %>%
  # select(geographic_area, OC2, avg_prod, everything()) %>%
  # pivot_longer(cols = 4:last_col(), names_to = "year", values_to = "productivity")
  
for (i in unique(productivity$OC2)) {
  
  most_productive_countries <- most_productive %>%
    filter(OC2 == i, !is.na(avg_productivity), !is.infinite(avg_productivity)) %>%
    arrange(desc(avg_productivity)) %>%
    slice(1:10) %>%
    pull(geographic_area)
  
  print(
    productivity %>%
      filter(OC2 == i, geographic_area %in% most_productive_countries) %>%
      select(-c(OC2, value, prod_value)) %>%
      ggplot(aes(x = year, y = productivity)) +
      geom_line(aes(group = geographic_area, color = geographic_area)) +
      labs(title = paste("Top 10 countries for", tolower(i), "productivity"),
           caption = "Top 10 selection based on the average productivity for the period.") 
  )
  
}
  
  
