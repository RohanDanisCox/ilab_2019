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
  library(ggrepel)
  library(randomForest)

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
  
  # Save off the data
  
  
# [2] ---- Start working with the data ----

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
    mutate(index = (house_median / base_price) * 100) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    mutate(top = last(index))
  
  # Graph of all suburbs - highlighting top few
  ggplot(index_2,aes(x = year, y = index, group = suburb_name, colour = gccsa_name)) + 
    geom_line(alpha = 0.2) +
    geom_text(data = index_2 %>% 
                group_by(suburb_name) %>%
                arrange(desc(year)) %>%
                slice(1) %>%
                filter(index > 900),
              aes(x = year + 0.2, label = suburb_name, hjust = 0)) + 
    expand_limits(x = max(index_2$year) + 3)
  
  # Graph of all suburbs - highlighting top few - Using ggrepel instead
  ggplot(index_2,aes(x = year, y = index, group = suburb_name, colour = gccsa_name)) + 
    geom_line(alpha = 0.2) +
    geom_text_repel(data = index_2 %>% 
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1) %>%
                      filter(index > 800),
                    aes(label = suburb_name)) + 
    expand_limits(x = max(index_2$year) + 2) +
    ggtitle("Everywhere Makes Money") +
    theme_minimal()+
    theme(legend.position="bottom")
  
  # Graph of just top 10 suburbs in greater sydney
  ggplot(data = index_2 %>% 
           filter(gccsa_name == "Greater Sydney") %>% 
           arrange(desc(top)) %>%
           head(200),
         aes(x = year, y = index, group = suburb_name, colour = sa4_name)) + 
    geom_line(alpha = 0.2) +
    geom_text_repel(data = index_2 %>% 
                      filter(gccsa_name == "Greater Sydney") %>% 
                      arrange(desc(top)) %>%
                      head(200) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name)) + 
    expand_limits(x = max(index_2$year) + 3)
  
  # Graph of just top 10 suburbs outside sydney
  ggplot(data = index_2 %>% 
           filter(gccsa_name == "Rest of NSW") %>% 
           arrange(desc(top)) %>%
           head(200),
         aes(x = year, y = index, group = suburb_name, colour = sa4_name)) + 
    geom_line(alpha = 0.2) +
    geom_text_repel(data = index_2 %>% 
                      filter(gccsa_name == "Rest of NSW") %>% 
                      arrange(desc(top)) %>%
                      head(200) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name)) + 
    expand_limits(x = max(index_2$year) + 3)
  
  # Trying to create the previous 2 graphs but faceted
  ggplot(data = index_2 %>%
           arrange(desc(top)) %>%
           head(400),
         aes(x = year, y = index, group = suburb_name, colour = sa4_name)) + 
    geom_line(alpha = 0.2) +
    facet_wrap( ~ gccsa_name) +
    geom_text_repel(data = index_2 %>% 
                      arrange(desc(top)) %>%
                      head(400) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name)) + 
    expand_limits(x = max(index_2$year) + 3)
  
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
  
  ###### I THINK I WANT TO BE CONTROLLING FOR YEAR BEFORE DOING REGRESSIONS
  
  index_4 <- index_3 %>%
    mutate(control = house_median / mean_nsw) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    mutate(base_control = first(control)) %>% 
    ungroup() %>%
    mutate(control_index = (control / base_control) *100)
  
  ggplot(index_4,aes(year,control_index, group = suburb_name)) + 
    geom_line(alpha = 0.2) +
    geom_text(data = index_4 %>% 
                group_by(suburb_name) %>%
                arrange(desc(year)) %>%
                slice(1),
              aes(x = year + 0.2, label = suburb_name, hjust = 0)) + 
    expand_limits(x = max(index_4$year) + 3)
  
  saveRDS(index_4,"rmarkdown/rmarkdown_data.rds")
  
# [3] ---- A bit of modelling ---- 
  
  ## Add back nsw_control_index to data 
  
  data_2 <- index_3 %>%
    select(suburb_name,year,nsw_control_index) %>%
    left_join(data)
  
  # Simple linear model
  simple_linear_model <- lm(data = data_2,formula = nsw_control_index ~ year +
                              log_crime_score + 
                              green_score_decile +
                              education_score + 
                              usual_resident_population + 
                              dwelling_density + 
                              seifa_econ_disadvantage + 
                              seifa_econ_adv_disadv + 
                              seifa_econ_resources + 
                              seifa_education_occupation + 
                              median_land_value_per_sqm + 
                              working_age_proportion)
  
  summary(simple_linear_model)
  
  # Random Forest
  
  random_forest_data <- data_2 %>%
    select(3,2,12:17,19:21,23:29)
  
  base_random_forest <- randomForest(data = random_forest_data,
                                     y = random_forest_data[,1],
                                     x = random_forest_data[,2:18])
    
    ?randomForest
  
  
  # to package up
  
  rmarkdown::render_site(input = "rmarkdown")
  