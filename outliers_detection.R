library(anomalize)
library(tidyverse)
library(tibbletime)
library(tidyquant)

# fetch data
data <- read_csv("C:/Users/pierr/Desktop/FM_data_final/OECD_data_final.csv", )

# subset and aggregate

country <- "Chile"
sector <- "Aquaculture"

subset <- data %>%
  filter(geographic_area == country, OC2 == sector) %>%
  group_by(OC2, year) %>%
  summarise(value = sum(value))

subset$year <- as.Date(paste(subset$year, 12, 31, sep = "-"))

# anomalize 
anomalized <- subset %>% 
  time_decompose(value) %>%
  anomalize(remainder) %>%
  time_recompose()

# plot data with anomalies
anomalized %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) + labs(title = paste(country, "anomalies"))