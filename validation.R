rm(list = ls())

# Load necessary packages

library(validate)
library(tidyverse)

# Parameters

options(scipen = 999)
current_dispatch <- 2020

# Load datasets

FM_new <- read_csv(choose.files())

# Load list of country codes and define landlocked countries

ll <- c('004','020','031','040','051','064','068','072','108','112','140','148','203','231','336','348','398','417','418','426','438','442','454','466','496','498','524','562','600','646','674','688','703','716','728','748','756','762','795','800','807','854','860','894') # define list of landlocked countries by UN m49 code

country_codes <- read_csv("https://raw.githubusercontent.com/openfigis/RefData/gh-pages/country/CL_FI_COUNTRY_ITEM.csv") %>%
  filter(!(Name_En == "Netherlands Antilles" & Start_Year == 1900 & End_Year == 1985)) %>% # Remove second entry for Netherlands Antilles, which leads to duplication of rows for that country
  mutate(landlocked = UN_Code %in% ll) %>%
  select(Name_En, Start_Year, End_Year, landlocked)

# Join data with country info

FM_new <- FM_new %>%
  left_join(country_codes, by = c("geographic_area" = "Name_En"))

if (nrow(FM_new[is.na(FM_new$Start_Year),]) > 0) {
  print(unique(FM_new$geographic_area[is.na(FM_new$Start_Year)]))
  stop("Missing country mapping")
}

# Load validation rules

rules <- validator(.file = "rules.R")

# Perform data validation

output <- confront(FM_new, rules, raise = 'all')

summary(output)

violated_rules <- unique(summary(output)[summary(output)$fails > 0,]$name)

if (length(violated_rules) > 0) {
  
  warning(paste("The following rules were violated:", paste(violated_rules, collapse = ", ")))
  
  View(filter(summary(output), fails > 0))
  
}

plot(output)

fails <- violating(FM_new, output[17])
