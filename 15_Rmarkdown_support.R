# RMarkdown File Support

# [0] ---- Load packages ----

  library(readr)
  library(tidyverse)
  library(rdrop2)
  library(httpuv)
  library(sp)
  library(rgdal)
  library(rgeos)
  library(maptools)
  library(ggmap)
  library(broom)
  library(sf)
  library(RColorBrewer)
  #library(mapview)
  library(leaflet)
  library(leafpop)
  library(pryr)

# [1] ---- Obtain data ----

  master <- readRDS("data/created/master.rds") %>%
    filter(year >= 2000)
  
  data <- master %>%
    select(suburb_code,suburb_name,year,sa2_name,sa3_name,sa4_name,gccsa_name,suburb_area_sqkm,
           violent_crime,dasg_crime,log_crime_score, # Crime
           education_score, # education
           green_score_decile, # green space
           usual_resident_population,working_age_proportion,senior_citizen_proportion, # Demographics 
           confirmed_journeys,public_transport_proportion,motor_vehicle_proportion, bicycle_walking_proportion, # Transport
           confirmed_dwellings, house_and_semi_proportion, unit_proportion, dwelling_density, # Dwellings
           seifa_econ_disadvantage, seifa_econ_adv_disadv, # SEIFA
           seifa_econ_resources, seifa_education_occupation, # SEIFA
           median_land_value,median_land_value_per_sqm, # Land Values
           aria_overall, aria_education, aria_health, aria_shopping, aria_public_transport, aria_financial_postal, # ARIA
           house_median = house_median_suburb, unit_median = apartment_median_suburb, land_median_suburb, annual_turnover) # Property Prices

  index <- data %>%
    select(suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name,year,house_median) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name) %>%
    mutate(base_price = first(house_median)) %>%
    ungroup() %>%
    filter(!is.na(base_price))
  
  na_check <- index %>%
    filter(is.na(house_median)) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    summarise(n = n()) %>%
    ungroup() #%>%
    #filter(n > 1)
  
  index_1 <- index %>%
    anti_join(na_check, by = c("suburb_name", "sa2_name", "sa3_name", "sa4_name","gccsa_name"))
  
  index_2 <- index_1 %>%
    mutate(index = (house_median / base_price) * 100)
  
  ggplot(index_2,aes(x = year, y = index, group = suburb_name, colour = gccsa_name)) + 
    geom_line(alpha = 0.2)
  
  index_3 <- index_2 %>%
    group_by(year) %>%
    mutate(mean_nsw = mean(house_median)) %>%
    ungroup() %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    mutate(base_nsw = first(mean_nsw)) %>% 
    ungroup() %>%
    mutate(nsw_control_index = (house_median / base_nsw) *100)

  ggplot(index_3,aes(x = year, y = nsw_control_index, group = suburb_name, colour = gccsa_name)) + 
    geom_line(alpha = 0.3)
  
  