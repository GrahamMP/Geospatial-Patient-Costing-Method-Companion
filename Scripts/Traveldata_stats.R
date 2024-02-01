library(dplyr)
library(tidyverse)

# get data
travel_pop <- read_csv("data/Geospatial Data/CHSA-to-Hosp-distances_population.csv")

# rename for convenience
names(travel_pop)[names(travel_pop) == 'Street distance (km)'] <- "dist"
names(travel_pop)[names(travel_pop) == 'Street duration'] <- "dur"

# remove one CHSA with na distance value
travel_pop <- travel_pop %>%
  drop_na(dist)

# Weighted mean and sd for distance
dist_mean_sd <-
  travel_pop %>%
  group_by(HA) %>%
  summarise(weighted_mean = weighted.mean(dist,Population),
            weighted_sd = radiant.data::weighted.sd(dist,Population))

# Weighted mean and sd for duration
dur_mean_sd <-
  travel_pop %>%
  group_by(HA) %>%
  summarise(weighted_mean = weighted.mean(dur,Population),
            weighted_sd = radiant.data::weighted.sd(dur,Population))

# Summary stats for distance
dist_summary <-
  travel_pop %>%
  group_by(HA) %>%
  summarise(min = min(dist),
            q1 = quantile(dist, 0.25),
            mean = mean(dist),
            q3 = quantile(dist, 0.75),
            max = max(dist))

# Summary stats for duration
dur_summary <-
  travel_pop %>%
  group_by(HA) %>%
  summarise(min = min(dur),
            q1 = quantile(dur, 0.25),
            mean = mean(dur),
            q3 = quantile(dur, 0.75),
            max = max(dur))

