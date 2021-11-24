
# Load libraries
library(tidyverse)
library(lubridate)

hotspots <- read.csv(here::here("data/VIC_hotspots_raw.csv"))
memberships <- read.csv(here::here("data/VIC_hotspots_after_clustering.csv"))

hotspots$fire_id <- memberships$fire_id
rm(memberships)

# select the ignition points
temp <- hotspots %>%
  group_by(fire_id) %>%
  summarise(hour_id = min(hour_id)) %>%
  ungroup()

hotspots_joined <- left_join(temp, hotspots)

hotspots_joined2 <- hotspots_joined %>%
  group_by(fire_id) %>%
  summarise(lon = mean(lon), lat = mean(lat), time = mean(as.POSIXct(date))) %>%
  ungroup()

# transform time to datetime
hotspots_joined2$time <- as.Date(as.POSIXct(hotspots_joined2$time, origin = '1970-01-01', tz = "GMT"), tz = "GMT")

# rename fire_id to id
hotspots_joined2 <- rename(hotspots_joined2, id = fire_id)


write_csv(hotspots_joined2, "data/predict_x_new.csv")
