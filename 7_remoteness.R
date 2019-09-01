# Captured a given suburb's remoteness score by intersecting with the remoteness map

# [0] ---- Load packages ----

library(readr)
library(tidyverse)
library(rdrop2)
library(feather)
library(httpuv)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(sf)
library(RColorBrewer)

# [1] ---- Load the remoteness data and 2016 shapefile ----

nsw_2016 <- read_sf("data/shapefiles/2016") %>%
  filter(STE_CODE16 == 1)

nsw_remoteness <- read_sf("data/remoteness") %>%
  filter(STE_CODE16 == 1)

# [2] ---- Intersect the data ----

remoteness_intersection <- nsw_2016 %>%
  st_intersection(nsw_remoteness) %>%
  mutate(area = st_area(geometry)) %>%
  select(SSC_CODE16,SSC_NAME16,STE_CODE16,STE_NAME16,AREASQKM16,area) %>%
  st_drop_geometry()

remoteness_intersection_2 <- nsw_remoteness %>%
  st_intersection(nsw_2016)

# How to get the maximum

highest <- remoteness_intersection %>%
  group_by(SSC_CODE16,SSC_NAME16) %>%
  mutate(rank = rank(desc(area))) %>%
  filter(rank == 1)
