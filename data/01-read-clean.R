# initial clean of the data
library(tidyverse)
library(naniar)

housing <- read_csv("data/housing-data/raw/Melbourne_housing_FULL.csv") %>%
  janitor::clean_names() %>%
  rename(region_name = regionname,
         property_count = propertycount)

glimpse(dat_melb)

write_csv(housing,
          path = "data/housing-data/housing.csv")
