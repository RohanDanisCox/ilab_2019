# Combining all the data

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

# [1] ---- Load the data files ----

  suburb_2016 <- readRDS("data/created/suburbs.rds")
  
  correspondence <- readRDS("data/created/correspondence.rds")
  
  crime_score <- readRDS("data/created/crime_score.rds")
  
  education_score <- readRDS("data/created/education_score.rds")
  
  green_score <- readRDS("data/created/green_score.rds")
  
  seifa_scores <- readRDS("data/created/seifa_scores.rds")
  
  class(suburb_2016$suburb_code)
  class(correspondence$suburb_code)
  class(education_score$suburb_code) # this should be numeric
  class(green_score$suburb_code) 
  class(seifa_scores$suburb_code) # this should be numeric
  
  
  
# [2] ---- Combine all the data ----
  
  crime <- crime_score %>%
    select(suburb_name,year,crime_score = log_crime_score)
  
  education <- education_score %>%
    select(suburb_code,suburb_name,education_score)
  
  green <- green_score %>%
    select(suburb_code,suburb_name, green_score)
  
  seifa <- seifa_scores
  
  all <- suburb_2016 %>%
    select(1:2) %>%
    left_join(crime, by = "suburb_name") %>%
    left_join(education, by = c("suburb_code", "suburb_name"))
    
  
    
  