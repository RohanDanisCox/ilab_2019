# Visualisations

# Suggested sites
# https://rstudio.github.io/leaflet/
# https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/vignettes/vspd-base-shiny.md 
# https://geocompr.robinlovelace.net/adv-map.html

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
  library(mapview)
  library(leaflet)
  library(leafpop)

# [1] ---- Suburb_Investigation App ----

# [1a] ---- Obtain Data and Add to Data File ----

  # Obtain choice options - distinct suburb names to choose from
  master <- readRDS("data/created/master.rds")
  
  choices <- master %>%
    distinct(suburb_name,sa4_name,sa3_name,sa2_name)
  
  saveRDS(choices, "suburb_investigation_app/data/choices.rds")

  # Obtain map file
  nsw <- read_sf("data/shapefiles/2016") %>%
    filter(STE_CODE16 == 1)
  
  suburbs <- readRDS("data/created/suburbs.rds")
  
  suburbs1 <- suburbs %>%
    select(suburb_code,suburb_name,sa3_code,sa3_name,sa4_code,sa4_name)
  
  nsw1 <- nsw %>% 
    mutate(suburb_code = as.numeric(SSC_CODE16),
           suburb_name = SSC_NAME16,
           area = AREASQKM16) %>%
    select(suburb_code,suburb_name,area)
  
  map <- nsw1 %>%
    left_join(suburbs1) %>%
    select(suburb_code,suburb_name,sa3_code,sa3_name,sa4_code,sa4_name,area)
  
  saveRDS(map, "suburb_investigation_app/data/map.rds")
  
  # Obtaining the data files 
  
  # The master file should be only data points that matter
  suburb_data <- master %>%
    filter(!gccsa_code %in% c(19499,19799))
  
  suburb_data_ready <- suburb_data %>%
    select(suburb_code,
           suburb_name,
           year,
           sa2_name,
           sa3_name,
           sa4_name,
           gccsa_name,
           Crime = log_crime_score, 
           Education = education_score, 
           Green_Space = green_score_decile,
           Census_Population = confirmed_population, 
           Usual_Resident_Population = usual_resident_population,
           Working_Age = working_age_proportion,
           Senior_Citizens = senior_citizen_proportion, 
           Journeys_to_Work = confirmed_journeys,
           Journey_to_Work_by_Public_Transport = public_transport_proportion, 
           Journey_to_Work_by_Motor_Vehicle = motor_vehicle_proportion,
           Journey_to_Work_by_Bicycle_or_Walking = bicycle_walking_proportion,
           Number_of_Dwellings = confirmed_dwellings, 
           Proportion_of_House = house_and_semi_proportion,
           Proportion_of_Units = unit_proportion,
           SEIFA_Socio_Economic_Disadvantage = relative_socio_economic_disadvantage_index,
           SEIFA_Socio_Economic_Advantage_Disadvantage = relative_socio_economic_adv_disadv_index,
           SEIFA_Economic_Resources = economic_resources_index,
           SEIFA_Education_and_Occupation = education_and_occupation_index,
           Number_of_Properties = number_of_properties,
           Median_Land_Value = median_land_value,
           Median_Land_Value_Per_Sq_M = median_land_value_per_sqm,
           ARIA_Overall_Services = aria_overall,
           ARIA_Education_Services = aria_education,
           ARIA_Health_Services = aria_health,
           ARIA_Shopping_Services = aria_shopping,
           ARIA_Public_Transport_Services = aria_public_transport,
           ARIA_Financial_Postal_Services = aria_financial_postal,
           House_Median = house_median_suburb,
           Apartment_Median = apartment_median_suburb,
           Land_Median = land_median_suburb,
           Dwelling_Density = dwelling_density,
           Annual_Turnover = annual_turnover,
           Proportion_of_Annual_Turnover = annual_turnover_proportion)
  
  saveRDS(suburb_data_ready, "suburb_investigation_app/data/suburb_data.rds")
  
  nsw_data <- suburb_data %>%
    group_by(year) %>%
    summarise(Crime = weighted.mean(log_crime_score, usual_resident_population, na.rm = TRUE), 
              Education = weighted.mean(education_score, usual_resident_population, na.rm = TRUE), 
              Green_Space = weighted.mean(green_score, usual_resident_population, na.rm = TRUE),
              Census_Population = weighted.mean(confirmed_population, usual_resident_population, na.rm = TRUE), 
              Usual_Resident_Population = weighted.mean(usual_resident_population, usual_resident_population, na.rm = TRUE),
              Working_Age = weighted.mean(working_age_proportion, usual_resident_population, na.rm = TRUE),
              Senior_Citizens = weighted.mean(senior_citizen_proportion, usual_resident_population, na.rm = TRUE), 
              Journeys_to_Work = weighted.mean(confirmed_journeys, usual_resident_population, na.rm = TRUE),
              Journey_to_Work_by_Public_Transport = weighted.mean(public_transport_proportion, usual_resident_population, na.rm = TRUE), 
              Journey_to_Work_by_Motor_Vehicle = weighted.mean(motor_vehicle_proportion, usual_resident_population, na.rm = TRUE),
              Journey_to_Work_by_Bicycle_or_Walking = weighted.mean(bicycle_walking_proportion, usual_resident_population, na.rm = TRUE),
              Number_of_Dwellings = weighted.mean(confirmed_dwellings, usual_resident_population, na.rm = TRUE), 
              Proportion_of_House = weighted.mean(house_and_semi_proportion, usual_resident_population, na.rm = TRUE),
              Proportion_of_Units = weighted.mean(unit_proportion, usual_resident_population, na.rm = TRUE),
              SEIFA_Socio_Economic_Disadvantage = weighted.mean(relative_socio_economic_disadvantage_index, usual_resident_population, na.rm = TRUE),
              SEIFA_Socio_Economic_Advantage_Disadvantage = weighted.mean(relative_socio_economic_adv_disadv_index, usual_resident_population, na.rm = TRUE),
              SEIFA_Economic_Resources = weighted.mean(economic_resources_index, usual_resident_population, na.rm = TRUE),
              SEIFA_Education_and_Occupation = weighted.mean(education_and_occupation_index, usual_resident_population, na.rm = TRUE),
              Number_of_Properties = weighted.mean(number_of_properties, usual_resident_population, na.rm = TRUE),
              Median_Land_Value = weighted.mean(median_land_value, usual_resident_population, na.rm = TRUE),
              Median_Land_Value_Per_Sq_M = weighted.mean(median_land_value_per_sqm, usual_resident_population, na.rm = TRUE),
              ARIA_Overall_Services = weighted.mean(aria_overall, usual_resident_population, na.rm = TRUE),
              ARIA_Education_Services = weighted.mean(aria_education, usual_resident_population, na.rm = TRUE),
              ARIA_Health_Services = weighted.mean(aria_health, usual_resident_population, na.rm = TRUE),
              ARIA_Shopping_Services = weighted.mean(aria_shopping, usual_resident_population, na.rm = TRUE),
              ARIA_Public_Transport_Services = weighted.mean(aria_public_transport, usual_resident_population, na.rm = TRUE),
              ARIA_Financial_Postal_Services = weighted.mean(aria_financial_postal, usual_resident_population, na.rm = TRUE),
              House_Median = weighted.mean(house_median_suburb, usual_resident_population, na.rm = TRUE),
              Apartment_Median = weighted.mean(apartment_median_suburb, usual_resident_population, na.rm = TRUE),
              Land_Median = weighted.mean(land_median_suburb, usual_resident_population, na.rm = TRUE),
              Dwelling_Density = weighted.mean(dwelling_density, usual_resident_population, na.rm = TRUE),
              Annual_Turnover = weighted.mean(annual_turnover, usual_resident_population, na.rm = TRUE),
              Proportion_of_Annual_Turnover = weighted.mean(annual_turnover_proportion, usual_resident_population, na.rm = TRUE))
  
  saveRDS(nsw_data, "suburb_investigation_app/data/nsw_data.rds")
  
  sa4_data <- suburb_data %>%
    group_by(sa4_name,year) %>%
    summarise(Crime = weighted.mean(log_crime_score, usual_resident_population, na.rm = TRUE), 
              Education = weighted.mean(education_score, usual_resident_population, na.rm = TRUE), 
              Green_Space = weighted.mean(green_score, usual_resident_population, na.rm = TRUE),
              Census_Population = weighted.mean(confirmed_population, usual_resident_population, na.rm = TRUE), 
              Usual_Resident_Population = weighted.mean(usual_resident_population, usual_resident_population, na.rm = TRUE),
              Working_Age = weighted.mean(working_age_proportion, usual_resident_population, na.rm = TRUE),
              Senior_Citizens = weighted.mean(senior_citizen_proportion, usual_resident_population, na.rm = TRUE), 
              Journeys_to_Work = weighted.mean(confirmed_journeys, usual_resident_population, na.rm = TRUE),
              Journey_to_Work_by_Public_Transport = weighted.mean(public_transport_proportion, usual_resident_population, na.rm = TRUE), 
              Journey_to_Work_by_Motor_Vehicle = weighted.mean(motor_vehicle_proportion, usual_resident_population, na.rm = TRUE),
              Journey_to_Work_by_Bicycle_or_Walking = weighted.mean(bicycle_walking_proportion, usual_resident_population, na.rm = TRUE),
              Number_of_Dwellings = weighted.mean(confirmed_dwellings, usual_resident_population, na.rm = TRUE), 
              Proportion_of_House = weighted.mean(house_and_semi_proportion, usual_resident_population, na.rm = TRUE),
              Proportion_of_Units = weighted.mean(unit_proportion, usual_resident_population, na.rm = TRUE),
              SEIFA_Socio_Economic_Disadvantage = weighted.mean(relative_socio_economic_disadvantage_index, usual_resident_population, na.rm = TRUE),
              SEIFA_Socio_Economic_Advantage_Disadvantage = weighted.mean(relative_socio_economic_adv_disadv_index, usual_resident_population, na.rm = TRUE),
              SEIFA_Economic_Resources = weighted.mean(economic_resources_index, usual_resident_population, na.rm = TRUE),
              SEIFA_Education_and_Occupation = weighted.mean(education_and_occupation_index, usual_resident_population, na.rm = TRUE),
              Number_of_Properties = weighted.mean(number_of_properties, usual_resident_population, na.rm = TRUE),
              Median_Land_Value = weighted.mean(median_land_value, usual_resident_population, na.rm = TRUE),
              Median_Land_Value_Per_Sq_M = weighted.mean(median_land_value_per_sqm, usual_resident_population, na.rm = TRUE),
              ARIA_Overall_Services = weighted.mean(aria_overall, usual_resident_population, na.rm = TRUE),
              ARIA_Education_Services = weighted.mean(aria_education, usual_resident_population, na.rm = TRUE),
              ARIA_Health_Services = weighted.mean(aria_health, usual_resident_population, na.rm = TRUE),
              ARIA_Shopping_Services = weighted.mean(aria_shopping, usual_resident_population, na.rm = TRUE),
              ARIA_Public_Transport_Services = weighted.mean(aria_public_transport, usual_resident_population, na.rm = TRUE),
              ARIA_Financial_Postal_Services = weighted.mean(aria_financial_postal, usual_resident_population, na.rm = TRUE),
              House_Median = weighted.mean(house_median_suburb, usual_resident_population, na.rm = TRUE),
              Apartment_Median = weighted.mean(apartment_median_suburb, usual_resident_population, na.rm = TRUE),
              Land_Median = weighted.mean(land_median_suburb, usual_resident_population, na.rm = TRUE),
              Dwelling_Density = weighted.mean(dwelling_density, usual_resident_population, na.rm = TRUE),
              Annual_Turnover = weighted.mean(annual_turnover, usual_resident_population, na.rm = TRUE),
              Proportion_of_Annual_Turnover = weighted.mean(annual_turnover_proportion, usual_resident_population, na.rm = TRUE))
  
  saveRDS(sa4_data, "suburb_investigation_app/data/sa4_data.rds")
  
  sa3_data <- suburb_data %>%
    group_by(sa3_name,year) %>%
    summarise(Crime = weighted.mean(log_crime_score, usual_resident_population, na.rm = TRUE), 
              Education = weighted.mean(education_score, usual_resident_population, na.rm = TRUE), 
              Green_Space = weighted.mean(green_score, usual_resident_population, na.rm = TRUE),
              Census_Population = weighted.mean(confirmed_population, usual_resident_population, na.rm = TRUE), 
              Usual_Resident_Population = weighted.mean(usual_resident_population, usual_resident_population, na.rm = TRUE),
              Working_Age = weighted.mean(working_age_proportion, usual_resident_population, na.rm = TRUE),
              Senior_Citizens = weighted.mean(senior_citizen_proportion, usual_resident_population, na.rm = TRUE), 
              Journeys_to_Work = weighted.mean(confirmed_journeys, usual_resident_population, na.rm = TRUE),
              Journey_to_Work_by_Public_Transport = weighted.mean(public_transport_proportion, usual_resident_population, na.rm = TRUE), 
              Journey_to_Work_by_Motor_Vehicle = weighted.mean(motor_vehicle_proportion, usual_resident_population, na.rm = TRUE),
              Journey_to_Work_by_Bicycle_or_Walking = weighted.mean(bicycle_walking_proportion, usual_resident_population, na.rm = TRUE),
              Number_of_Dwellings = weighted.mean(confirmed_dwellings, usual_resident_population, na.rm = TRUE), 
              Proportion_of_House = weighted.mean(house_and_semi_proportion, usual_resident_population, na.rm = TRUE),
              Proportion_of_Units = weighted.mean(unit_proportion, usual_resident_population, na.rm = TRUE),
              SEIFA_Socio_Economic_Disadvantage = weighted.mean(relative_socio_economic_disadvantage_index, usual_resident_population, na.rm = TRUE),
              SEIFA_Socio_Economic_Advantage_Disadvantage = weighted.mean(relative_socio_economic_adv_disadv_index, usual_resident_population, na.rm = TRUE),
              SEIFA_Economic_Resources = weighted.mean(economic_resources_index, usual_resident_population, na.rm = TRUE),
              SEIFA_Education_and_Occupation = weighted.mean(education_and_occupation_index, usual_resident_population, na.rm = TRUE),
              Number_of_Properties = weighted.mean(number_of_properties, usual_resident_population, na.rm = TRUE),
              Median_Land_Value = weighted.mean(median_land_value, usual_resident_population, na.rm = TRUE),
              Median_Land_Value_Per_Sq_M = weighted.mean(median_land_value_per_sqm, usual_resident_population, na.rm = TRUE),
              ARIA_Overall_Services = weighted.mean(aria_overall, usual_resident_population, na.rm = TRUE),
              ARIA_Education_Services = weighted.mean(aria_education, usual_resident_population, na.rm = TRUE),
              ARIA_Health_Services = weighted.mean(aria_health, usual_resident_population, na.rm = TRUE),
              ARIA_Shopping_Services = weighted.mean(aria_shopping, usual_resident_population, na.rm = TRUE),
              ARIA_Public_Transport_Services = weighted.mean(aria_public_transport, usual_resident_population, na.rm = TRUE),
              ARIA_Financial_Postal_Services = weighted.mean(aria_financial_postal, usual_resident_population, na.rm = TRUE),
              House_Median = weighted.mean(house_median_suburb, usual_resident_population, na.rm = TRUE),
              Apartment_Median = weighted.mean(apartment_median_suburb, usual_resident_population, na.rm = TRUE),
              Land_Median = weighted.mean(land_median_suburb, usual_resident_population, na.rm = TRUE),
              Dwelling_Density = weighted.mean(dwelling_density, usual_resident_population, na.rm = TRUE),
              Annual_Turnover = weighted.mean(annual_turnover, usual_resident_population, na.rm = TRUE),
              Proportion_of_Annual_Turnover = weighted.mean(annual_turnover_proportion, usual_resident_population, na.rm = TRUE))
  
  saveRDS(sa3_data, "suburb_investigation_app/data/sa3_data.rds")

# [1b] ---- Testing Objects ----

  haberfield <- map %>%
    filter(sa4_name == "Sydney - Inner West") %>%
    mutate(fill = case_when(suburb_name == "Haberfield" ~ "suburb",
                            suburb_name != "Haberfield" & sa3_name == "Strathfield - Burwood - Ashfield" ~ "sa3",
                            TRUE ~ "No"))
    
  ggplot() +
    geom_sf(data = haberfield, aes(fill = fill)) +
    scale_fill_manual(values = c("white","pink","red"), guide = FALSE)
  
  haberfield <- master %>%
    filter(suburb_name == "Haberfield")
  
  ggplot(haberfield,aes(x = year,y = house_median_suburb)) +
    geom_line(colour = "blue", linetype = "dashed") + 
    geom_line(data = haberfield,mapping = aes(x = year, y = house_median_nsw),colour = "red",linetype = "dotted") + 
    theme_minimal(base_size = 16) +
    scale_color_identity(name = "Model fit",
                         breaks = c("black", "red", "blue"),
                         labels = c("Linear", "Quadratic", "Cubic"),
                         guide = "legend")
  
  ggplot(haberfield,aes(x = year,y = house_median_suburb,colour = "black")) +
    geom_line(linetype = "dashed") + 
    geom_line(data = haberfield,mapping = aes(x = year, y = house_median_nsw, colour = "red"),linetype = "dotted") + 
    theme_minimal(base_size = 16) +
    scale_color_identity(name = "Geography",
                         breaks = c("black", "red"),
                         labels = c("Suburb", "NSW"),
                         guide = "legend") + 
    labs(Title = "Variable over time", x = "Year", y = as.character()) 

  # [2] ---- Leaflet Investigation App ----
  
  # [2a] ---- Obtain Data and Add to Data File ----

  nsw <- read_sf("data/shapefiles/2016") %>%
    filter(STE_CODE16 == 1) %>%
    filter(!SSC_CODE16 %in% c(19494,19797,12387)) %>%
    mutate(SSC_CODE16 = as.numeric(SSC_CODE16)) %>%
    select(suburb_code = SSC_CODE16,suburb_name = SSC_NAME16)
  
  master <- readRDS("data/created/master.rds")
  
  suburb_data <- master %>%
    filter(!gccsa_code %in% c(19499,19799))
  
  suburb_subset <- suburb_data %>%
    select(suburb_code,suburb_name,year, log_crime_score,education_score) %>%
    filter(year == 2019)
  
  map <- nsw %>%
    left_join(suburb_subset, by = c("suburb_code", "suburb_name")) %>%
    select(suburb_code,suburb_name,log_crime_score,education_score)
  
  other_map <- readRDS("mapview_investigation_app/data/map.rds")
  
  # [2b] ---- Testing Objects ----

  leaflet(nsw) %>%
    addTiles() %>%
    setView(146.9211,-32.2532, zoom = 5.5)
  
  leaflet(map) %>%
    addTiles() %>%
    setView(146.9211,-32.2532, zoom = 5.5) %>%
    addPolygons()
  
  mapview(map, zcol= c("log_crime_score","education_score"))
  
  mapview(map) + 
   mapview(zcol= c("log_crime_score","education_score"))
  
  mapview(other)
  
# [2] ---- Mapview Investigation App ----
  
# [2a] ---- Obtain Data and Add to Data File ----
  
  nsw <- read_sf("data/shapefiles/2016") %>%
    filter(STE_CODE16 == 1) %>%
    filter(!SSC_CODE16 %in% c(19494,19797,12387)) %>%
    mutate(SSC_CODE16 = as.numeric(SSC_CODE16)) %>%
    select(suburb_code = SSC_CODE16,suburb_name = SSC_NAME16)
  
  master <- readRDS("data/created/master.rds")
  
  suburb_data <- master %>%
    filter(!gccsa_code %in% c(19499,19799))
  
  suburb_subset <- suburb_data %>%
    select(suburb_code,suburb_name,year, log_crime_score,education_score) %>%
    filter(year == 2019)
  
  map <- nsw %>%
    left_join(suburb_subset, by = c("suburb_code", "suburb_name")) %>%
    select(suburb_code,suburb_name,log_crime_score,education_score)
  
  other_map <- readRDS("mapview_investigation_app/data/map.rds")
  
# [2b] ---- Testing Objects ----
  
  leaflet(nsw) %>%
    addTiles() %>%
    setView(146.9211,-32.2532, zoom = 5.5)
  
  leaflet(map) %>%
    addTiles() %>%
    setView(146.9211,-32.2532, zoom = 5.5) %>%
    addPolygons()
  
  mapview(map, zcol= c("log_crime_score","education_score"))
  
  mapview(map) + 
    mapview(zcol= c("log_crime_score","education_score"))
  
  mapview(other)
  
  saveRDS(map, "mapview_investigation_app/data/other_map.rds")
