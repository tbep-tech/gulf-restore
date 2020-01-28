# find closest point  -----------------------------------------------------

library(tbeptools)
library(sf)
library(tidyverse)
library(sp)

data(reststat)
data(epcdata)

prj <- CRS("+proj=utm +zone=17. +datum=WGS84")

# restoration sites
rest <- reststat %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
  as('Spatial') %>% 
  spTransform(prj) %>% 
  data.frame %>% 
  select(-optional) %>% 
  rename(
    lon = coords.x1, 
    lat = coords.x2
  )

# water quality stations, filter station 6 for example
wqsta <- epcdata %>% 
  select(epchc_station, Longitude, Latitude) %>% 
  unique %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
  filter(epchc_station == 6) %>% 
  as('Spatial') %>% 
  spTransform(prj) %>% 
  data.frame %>% 
  select(-optional) %>% 
  rename(
    id = epchc_station,
    lon = coords.x1, 
    lat = coords.x2
  )

# get distances, sort, select closest
dist <- rbind(wqsta[, c('lon', 'lat')], rest[, c('lon', 'lat')]) %>%
  mutate(
  ) %>% 
  dist(method = 'euclidean') %>% 
  as.matrix %>% 
  .[-1, 1] %>%
  data.frame(
    id = rest[, c('id')],
    dist = ., stringsAsFactors = F
  ) %>%
  arrange(dist) %>% 
  .[1, ]

