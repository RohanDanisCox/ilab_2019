# Obtaining Green space scores

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

# [1] ---- Can calculate straight from unique suburb information ----

  suburbs <- readRDS("data/created/suburbs.rds")
  
# [2] ---- Calculate Greenscore ----  
  
  suburb_no_na <- suburbs %>%
    mutate_if(is.numeric , replace_na, replace = 0)

  suburb_green_ratio <- suburb_no_na %>%
    mutate(suburb_green_ratio = round(Parkland_area_sqkm/ (Residential_area_sqkm + Commercial_area_sqkm + Industrial_area_sqkm + Other_area_sqkm),4)) %>%
    mutate(suburb_green_ratio = case_when(is.infinite(suburb_green_ratio) ~ 1400,
                                          TRUE ~ suburb_green_ratio)) %>%
    select(suburb_code,suburb_green_ratio)
  
  check <- suburb_green_ratio %>%
    filter(suburb_green_ratio >10)
  
  sa2_green_ratio <- suburb_no_na %>%
    group_by(sa2_maincode) %>%
    summarise(Parkland_area_sqkm = sum(Parkland_area_sqkm),
              Residential_area_sqkm = sum(Residential_area_sqkm),
              Commercial_area_sqkm = sum(Commercial_area_sqkm),
              Industrial_area_sqkm = sum(Industrial_area_sqkm),
              Other_area_sqkm = sum(Other_area_sqkm)) %>%
    mutate(sa2_green_ratio = round(Parkland_area_sqkm/ (Residential_area_sqkm + Commercial_area_sqkm + Industrial_area_sqkm + Other_area_sqkm),4)) %>%
    mutate(sa2_green_ratio = case_when(is.infinite(sa2_green_ratio) ~ 800,
                                          TRUE ~ sa2_green_ratio)) %>%
    select(sa2_maincode,sa2_green_ratio)

  green_score <- suburb_no_na %>%
    left_join(suburb_green_ratio, by = "suburb_code") %>%
    left_join(sa2_green_ratio, by = "sa2_maincode") %>%
    mutate(green_score = (0.5*suburb_green_ratio) + (0.5*sa2_green_ratio),
           green_score_decile = ntile(green_score,10)) %>%
    select(suburb_code,suburb_name,green_score,green_score_decile)

# [3] ---- Save off Greenscore ----
  
  write_rds(green_score,"data/created/green_score.rds")
  