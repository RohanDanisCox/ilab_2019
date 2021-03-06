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
  #library(mapview)
  library(leaflet)
  library(leafpop)
  library(pryr)

# [1] ---- Suburb_Investigation App ----

# [1a] ---- Obtain Data and Add to Data File ----

  # Obtain choice options - distinct suburb names to choose from
  master <- readRDS("data/created/master.rds")
  
  choices <- master %>%
    distinct(suburb_name,sa4_name,sa3_name,sa2_name)
  
  saveRDS(choices, "suburb_investigation_app/data/choices.rds")

  # Obtain map file
  nsw <- read_sf("data/shapefiles/2016") %>%
    filter(STE_CODE16 == 1) %>%
    filter(!SSC_CODE16 %in% c(19494,19797,12387))
  
  suburbs <- readRDS("data/created/suburbs.rds")
  
  suburbs1 <- suburbs %>%
    select(suburb_code,suburb_name,sa3_code,sa3_name,sa4_code,sa4_name,gccsa_name)
  
  nsw1 <- nsw %>% 
    mutate(suburb_code = as.numeric(SSC_CODE16),
           suburb_name = SSC_NAME16,
           area = AREASQKM16) %>%
    select(suburb_code,suburb_name,area)
  
  map <- nsw1 %>%
    left_join(suburbs1) %>%
    select(suburb_code,suburb_name,sa3_code,sa3_name,sa4_code,sa4_name,gccsa_name) 
  
  saveRDS(map,"data/created/maps.rds")
  
  ## Simplify map
  
  simple_map <- map %>%
    st_simplify(preserveTopology = TRUE,dTolerance = 0.0001)
  
  object_size(map)
  object_size(simple_map)
  
  saveRDS(simple_map, "suburb_investigation_app/data/map.rds")
  
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
           Violent_Crime = violent_crime,
           DASG_Crime = dasg_crime,
           Crime = log_crime_score, 
           Education = education_score, 
           Green_Score = green_score,
           Green_Decile = green_score_decile,
           Usual_Resident_Population = usual_resident_population,
           Working_Age = working_age_proportion,
           Senior_Citizens = senior_citizen_proportion, 
           Journeys_to_Work = confirmed_journeys,
           Journey_to_Work_by_Public_Transport = public_transport_proportion, 
           Journey_to_Work_by_Motor_Vehicle = motor_vehicle_proportion,
           Journey_to_Work_by_Bicycle_or_Walking = bicycle_walking_proportion,
           Number_of_Dwellings = confirmed_dwellings, 
           Proportion_of_Houses = house_and_semi_proportion,
           Proportion_of_Units = unit_proportion,
           SEIFA_Socio_Economic_Disadvantage = seifa_econ_disadvantage,
           SEIFA_Socio_Economic_Advantage_Disadvantage = seifa_econ_adv_disadv,
           SEIFA_Economic_Resources = seifa_econ_resources,
           SEIFA_Education_and_Occupation = seifa_education_occupation,
           Number_of_Properties = number_of_properties,
           Median_Land_Value = median_land_value,
           Median_Land_Value_Per_Sq_M = median_land_value_per_sqm,
           ARIA_Overall_Services = aria_overall,
           ARIA_Education_Services = aria_education,
           ARIA_Health_Services = aria_health,
           ARIA_Shopping_Services = aria_shopping,
           ARIA_Public_Transport_Services = aria_public_transport,
           ARIA_Financial_Postal_Services = aria_financial_postal,
           Median_House_Price = house_median_suburb,
           Median_Apartment_Price = apartment_median_suburb,
           Land_Median = land_median_suburb,
           Dwelling_Density = dwelling_density,
           Annual_Turnover = annual_turnover,
           Proportion_of_Annual_Turnover = annual_turnover_proportion) %>%
    mutate(Education = case_when(year <=2015 ~ NA_real_,
                                 TRUE ~ Education),
           Green_Score = case_when(year <=2015 ~ NA_real_,
                                   TRUE ~ Green_Score),
           Green_Decile = case_when(year <=2015 ~ NA_integer_,
                                   TRUE ~ Green_Decile))
  
  summary(suburb_data_ready$Senior_Citizens)
  
  saveRDS(suburb_data_ready, "suburb_investigation_app/data/suburb_data.rds")
  
  nsw_data <- suburb_data_ready %>%
    group_by(year) %>%
    summarise(Violent_Crime = weighted.mean(Violent_Crime, Usual_Resident_Population, na.rm = TRUE),
              DASG_Crime = weighted.mean(DASG_Crime, Usual_Resident_Population, na.rm = TRUE),
              Crime = weighted.mean(Crime, Usual_Resident_Population, na.rm = TRUE), 
              Education = weighted.mean(Education, Usual_Resident_Population, na.rm = TRUE), 
              Green_Score = weighted.mean(Green_Score, Usual_Resident_Population, na.rm = TRUE),
              Green_Decile = weighted.mean(Green_Decile, Usual_Resident_Population, na.rm = TRUE),
              Working_Age = weighted.mean(Working_Age, Usual_Resident_Population, na.rm = TRUE),
              Senior_Citizens = weighted.mean(Senior_Citizens, Usual_Resident_Population, na.rm = TRUE), 
              Journeys_to_Work = weighted.mean(Journeys_to_Work, Usual_Resident_Population, na.rm = TRUE),
              Journey_to_Work_by_Public_Transport = weighted.mean(Journey_to_Work_by_Public_Transport, Usual_Resident_Population, na.rm = TRUE), 
              Journey_to_Work_by_Motor_Vehicle = weighted.mean(Journey_to_Work_by_Motor_Vehicle, Usual_Resident_Population, na.rm = TRUE),
              Journey_to_Work_by_Bicycle_or_Walking = weighted.mean(Journey_to_Work_by_Bicycle_or_Walking, Usual_Resident_Population, na.rm = TRUE),
              Number_of_Dwellings = weighted.mean(Number_of_Dwellings, Usual_Resident_Population, na.rm = TRUE), 
              Proportion_of_Houses = weighted.mean(Proportion_of_Houses, Usual_Resident_Population, na.rm = TRUE),
              Proportion_of_Units = weighted.mean(Proportion_of_Units, Usual_Resident_Population, na.rm = TRUE),
              SEIFA_Socio_Economic_Disadvantage = weighted.mean(SEIFA_Socio_Economic_Disadvantage, Usual_Resident_Population, na.rm = TRUE),
              SEIFA_Socio_Economic_Advantage_Disadvantage = weighted.mean(SEIFA_Socio_Economic_Advantage_Disadvantage, Usual_Resident_Population, na.rm = TRUE),
              SEIFA_Economic_Resources = weighted.mean(SEIFA_Economic_Resources, Usual_Resident_Population, na.rm = TRUE),
              SEIFA_Education_and_Occupation = weighted.mean(SEIFA_Education_and_Occupation, Usual_Resident_Population, na.rm = TRUE),
              Number_of_Properties = weighted.mean(Number_of_Properties, Usual_Resident_Population, na.rm = TRUE),
              Median_Land_Value = weighted.mean(Median_Land_Value, Usual_Resident_Population, na.rm = TRUE),
              Median_Land_Value_Per_Sq_M = weighted.mean(Median_Land_Value_Per_Sq_M, Usual_Resident_Population, na.rm = TRUE),
              ARIA_Overall_Services = weighted.mean(ARIA_Overall_Services, Usual_Resident_Population, na.rm = TRUE),
              ARIA_Education_Services = weighted.mean(ARIA_Education_Services, Usual_Resident_Population, na.rm = TRUE),
              ARIA_Health_Services = weighted.mean(ARIA_Health_Services, Usual_Resident_Population, na.rm = TRUE),
              ARIA_Shopping_Services = weighted.mean(ARIA_Shopping_Services, Usual_Resident_Population, na.rm = TRUE),
              ARIA_Public_Transport_Services = weighted.mean(ARIA_Public_Transport_Services, Usual_Resident_Population, na.rm = TRUE),
              ARIA_Financial_Postal_Services = weighted.mean(ARIA_Financial_Postal_Services, Usual_Resident_Population, na.rm = TRUE),
              Median_House_Price = weighted.mean(Median_House_Price, Usual_Resident_Population, na.rm = TRUE),
              Median_Apartment_Price = weighted.mean(Median_Apartment_Price, Usual_Resident_Population, na.rm = TRUE),
              Land_Median = weighted.mean(Land_Median, Usual_Resident_Population, na.rm = TRUE),
              Dwelling_Density = weighted.mean(Dwelling_Density, Usual_Resident_Population, na.rm = TRUE),
              Annual_Turnover = weighted.mean(Annual_Turnover, Usual_Resident_Population, na.rm = TRUE),
              Proportion_of_Annual_Turnover = weighted.mean(Proportion_of_Annual_Turnover, Usual_Resident_Population, na.rm = TRUE),
              Usual_Resident_Population = weighted.mean(Usual_Resident_Population, Usual_Resident_Population, na.rm = TRUE))
  
  saveRDS(nsw_data, "suburb_investigation_app/data/nsw_data.rds")
  
# [1b] ---- Testing Objects ----
  
  map_subset <- map %>%
    filter(suburb_name %in% c("Haberfield","Broken Hill"))
  
  top <- map_subset %>%
    filter(suburb_name == "Broken Hill") %>%
    st_centroid(geometry)
  top_lat <- top$geometry[[1]][1]
  top_lng <- top$geometry[[1]][2]

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
    select(suburb_code,suburb_name,year,sa2_name,sa3_name,sa4_name,suburb_area_sqkm,
           violent_crime,dasg_crime,log_crime_score, # Crime
           education_score, # education
           green_score,green_score_decile, # green space
           usual_resident_population,working_age_proportion,senior_citizen_proportion, # Demographics 
           confirmed_journeys,public_transport_proportion,motor_vehicle_proportion, bicycle_walking_proportion, # Transport
           confirmed_dwellings, house_and_semi_proportion, unit_proportion, dwelling_density, # Dwellings
           seifa_econ_disadvantage, seifa_econ_adv_disadv, # SEIFA
           seifa_econ_resources, seifa_education_occupation, # SEIFA
           median_land_value,median_land_value_per_sqm, # Land Values
           aria_overall, aria_education, aria_health, aria_shopping, aria_public_transport, aria_financial_postal, # ARIA
           house_median_suburb, apartment_median_suburb, land_median_suburb, annual_turnover, # Property Prices
           ) %>%
    filter(year == 2019)
  
  map <- nsw %>%
    left_join(suburb_subset, by = c("suburb_code", "suburb_name")) 
  
  map_data <- map %>%
    st_drop_geometry()
  
  check_na <- map %>%
    st_drop_geometry() %>%
    select(7:40) %>%
    map_df(~(data.frame(na_count = sum(is.na(.x)))),
           .id = "variable")
  
  ## Trying to simplify object
  
  simple_map <- map %>%
    st_simplify(preserveTopology = TRUE,dTolerance = 0.001)
  
  object_size(map)
  object_size(simple_map)
 
  saveRDS(simple_map,"leaflet_investigation_app/data/simple_map.rds")
  saveRDS(map_data,"leaflet_investigation_app/data/map_data.rds")
  
  names(suburb_subset)
  # [2b] ---- Testing Objects ----

# [3] ---- Similarity Investigation App ----  
  # [3a] ---- Obtain Data and Add to Data File ----
  
  nsw <- read_sf("data/shapefiles/2016") %>%
    filter(STE_CODE16 == 1) %>%
    filter(!SSC_CODE16 %in% c(19494,19797,12387)) %>%
    mutate(SSC_CODE16 = as.numeric(SSC_CODE16)) %>%
    select(suburb_code = SSC_CODE16,suburb_name = SSC_NAME16)
  
  master <- readRDS("data/created/master.rds")
  
  suburb_data <- master %>%
    filter(!gccsa_code %in% c(19499,19799))
  
  suburb_subset <- suburb_data %>%
    select(suburb_code,suburb_name,year,sa2_name,sa3_name,sa4_name,suburb_area_sqkm,
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
           house_median_suburb, apartment_median_suburb, land_median_suburb, annual_turnover) # Property Prices
  
  select_suburb_subset <- suburb_subset %>%
    filter(year == 2019)
  
  comparison_suburb_subset <- suburb_subset %>%
    filter(year >= 2006)
  
  map <- nsw %>%
    left_join(select_suburb_subset, by = c("suburb_code", "suburb_name")) 
  
  select_scaled_data <- select_suburb_subset %>%
    select(2,4:39) %>%
    mutate_if(is.numeric,scale)
  
  comparison_scaled_data <- comparison_suburb_subset %>%
    select(2:39) %>%
    mutate(year = as.character(year)) %>%
    mutate_if(is.numeric,scale) %>%
    mutate(year = as.numeric(year))

  select_scaling_data <- select_suburb_subset %>%
    select(7:39) %>%
    map_df(~(data.frame(min = min(.x, na.rm = TRUE),
                        max = max(.x, na.rm = TRUE),
                        mean = mean(.x,na.rm = TRUE),
                        sd = sd(.x, na.rm = TRUE),
                        na_count = sum(is.na(.x)))),
           .id = "variable")
  
  comparison_scaling_data <- comparison_suburb_subset %>%
    select(7:39) %>%
    map_df(~(data.frame(min = min(.x, na.rm = TRUE),
                        max = max(.x, na.rm = TRUE),
                        mean = mean(.x,na.rm = TRUE),
                        sd = sd(.x, na.rm = TRUE),
                        na_count = sum(is.na(.x)))),
           .id = "variable")
    
  ## Trying to simplify object
  
  simple_map <- map %>%
    st_simplify(preserveTopology = TRUE,dTolerance = 0.0002)
  
  simple_data <- simple_map %>%
    st_drop_geometry()
  
  object_size(map)
  object_size(simple_map)
  
  saveRDS(simple_map,"similarity_app/data/simple_map.rds")
  saveRDS(comparison_suburb_subset,"similarity_app/data/unscaled_data.rds")
  saveRDS(select_scaled_data,"similarity_app/data/select_scaled_data.rds")
  saveRDS(select_scaling_data,"similarity_app/data/select_scaling_data.rds")
  saveRDS(comparison_scaled_data,"similarity_app/data/comparison_scaled_data.rds")
  saveRDS(comparison_scaling_data,"similarity_app/data/comparison_scaling_data.rds")
  
  # [2b] ---- Testing Objects ----
  
  map_presentation <- map %>%
    set_names(~ str_replace_all(.,"_"," ") %>%
                str_to_title()) %>%
    rename(geometry = Geometry)
    
# [4] ---- Data Sources ---- 
    
    data_sources <- tibble(`Data Set` = c("Property Sale Information",
                          "Property Land Information",
                          "NSW Suburb Boundaries (ESRI Shapefile)",
                          "Australia ASGS Digital Boundaries (ESRI Shapefile)",
                          "Recorded Crime by Offence",
                          "SEIFA",
                          "Census",
                          "NSW Government Schools",
                          "NSW Non-Government Schools",
                          "Mesh Blocks",
                          "ASGS/ASGC Geographic Correspondences",
                          "Metro ARIA"),
           Source = c('<a href="https://valuation.property.nsw.gov.au/embed/propertySalesInformation/">NSW Valuer General</a>',
                      '<a href="https://valuation.property.nsw.gov.au/embed/propertySalesInformation/">NSW Valuer General</a>',
                      '<a href="https://data.gov.au/dataset/ds-dga-bdcf5b09-89bc-47ec-9281-6b8e9ee147aa/details?q=PSMA%20administrative%20boundaries">PSMA Australia Limited</a>',
                      '<a href="https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument">Australian Bureau of Statistics (ABS)</a>',
                      '<a href="https://www.bocsar.nsw.gov.au/Pages/bocsar_datasets/Datasets-.aspx">Bureau of Crime Statistics and Research (BOCSAR)</a>',
                      '<a href="https://www.abs.gov.au/ausstats/abs@.nsf/mf/2033.0.55.001">Australian Bureau of Statistics (ABS)</a>',
                      '<a href="https://www.abs.gov.au/websitedbs/censushome.nsf/home/tablebuilder?opendocument&amp;navpos=240">Australian Bureau of Statistics (ABS) - Table Builder</a>',
                      '<a href="https://data.aurin.org.au/dataset/nsw-govt-de-nsw-school-locations-2016-na">NSW Department of Education</a>',
                      '<a href="https://data.cese.nsw.gov.au/data/dataset/nsw-non-government-school-locations-and-descriptions">NSW Department of Education</a>',
                      '<a href="https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument">Australian Bureau of Statistics (ABS)</a>',
                      '<a href="https://data.gov.au/dataset/ds-dga-23fe168c-09a7-42d2-a2f9-fd08fbd0a4ce/details?q=ASGS%202016%20correspondence">data.gov.au</a>',
                      '<a href="https://data.aurin.org.au/dataset/ua-hcmpr-adh-hcmpr-sa1-metro-aria-2014-australia-sa1">Hugo Centre for Migration and Population Research at the University of Adelaide</a>'),
           License = c("Creative Commons Attribution 4.0",
                       "Creative Commons Attribution 4.0",
                       "Creative Commons Attribution 2.5",
                       "Creative Commons Attribution 4.0",
                       "Creative Commons Attribution 2.5",
                       "Creative Commons Attribution 2.5",
                       "Creative Commons Attribution 3.0",
                       "Creative Commons Attribution 3.0",
                       "Creative Commons Attribution 2.5",
                       "Creative Commons Attribution 2.5",
                       "Creative Commons Attribution 2.5",
                       "Creative Commons Attribution-NonCommercial 4.0"))
    
    saveRDS(data_sources,"similarity_app/data/data_sources.rds")
    saveRDS(data_sources,"leaflet_investigation_app/data/data_sources.rds")
    
    
# [5] ---- Garbage ----
    
    sa4_data <- suburb_data %>%
      group_by(sa4_name,year) %>%
      summarise(Violent_Crime = weighted.mean(violent_crime, usual_resident_population, na.rm = TRUE),
                DASG_Crime = weighted.mean(dasg_crime, usual_resident_population, na.rm = TRUE),
                Crime = weighted.mean(log_crime_score, usual_resident_population, na.rm = TRUE), 
                Education = weighted.mean(education_score, usual_resident_population, na.rm = TRUE), 
                Green_Score = weighted.mean(green_score, usual_resident_population, na.rm = TRUE),
                Green_Decile = weighted.mean(green_score_decile, usual_resident_population, na.rm = TRUE),
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
                SEIFA_Socio_Economic_Disadvantage = weighted.mean(seifa_econ_disadvantage, usual_resident_population, na.rm = TRUE),
                SEIFA_Socio_Economic_Advantage_Disadvantage = weighted.mean(seifa_econ_adv_disadv, usual_resident_population, na.rm = TRUE),
                SEIFA_Economic_Resources = weighted.mean(seifa_econ_resources, usual_resident_population, na.rm = TRUE),
                SEIFA_Education_and_Occupation = weighted.mean(seifa_education_occupation, usual_resident_population, na.rm = TRUE),
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
      summarise(Violent_Crime = weighted.mean(violent_crime, usual_resident_population, na.rm = TRUE),
                DASG_Crime = weighted.mean(dasg_crime, usual_resident_population, na.rm = TRUE),
                Crime = weighted.mean(log_crime_score, usual_resident_population, na.rm = TRUE), 
                Education = weighted.mean(education_score, usual_resident_population, na.rm = TRUE), 
                Green_Score = weighted.mean(green_score, usual_resident_population, na.rm = TRUE),
                Green_Decile = weighted.mean(green_score_decile, usual_resident_population, na.rm = TRUE),
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
                SEIFA_Socio_Economic_Disadvantage = weighted.mean(seifa_econ_disadvantage, usual_resident_population, na.rm = TRUE),
                SEIFA_Socio_Economic_Advantage_Disadvantage = weighted.mean(seifa_econ_adv_disadv, usual_resident_population, na.rm = TRUE),
                SEIFA_Economic_Resources = weighted.mean(seifa_econ_resources, usual_resident_population, na.rm = TRUE),
                SEIFA_Education_and_Occupation = weighted.mean(seifa_education_occupation, usual_resident_population, na.rm = TRUE),
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
    
    test <- readRDS("similarity_app/data/select_scaled_data.rds")
    
    select_scaler <- readRDS("similarity_app/data/select_scaling_data.rds") %>%
      filter(variable %in% c("suburb_area_sqkm","log_crime_score","education_score","green_score_decile",
                             "usual_resident_population","working_age_proportion","senior_citizen_proportion",
                             "public_transport_proportion","motor_vehicle_proportion","bicycle_walking_proportion",
                             "house_and_semi_proportion","unit_proportion","dwelling_density",
                             "seifa_econ_disadvantage","seifa_econ_adv_disadv",
                             "seifa_econ_resources","seifa_education_occupation",
                             "median_land_value_per_sqm","house_median_suburb","apartment_median_suburb"))
    
    comparison_scaled_data <- test %>%
      select(suburb_name,sa2_name,sa3_name,sa4_name,year,suburb_area_sqkm,log_crime_score,education_score,green_score_decile,
             usual_resident_population,working_age_proportion,senior_citizen_proportion,
             public_transport_proportion,motor_vehicle_proportion,bicycle_walking_proportion,
             house_and_semi_proportion,unit_proportion,dwelling_density,
             seifa_econ_disadvantage,seifa_econ_adv_disadv,
             seifa_econ_resources,seifa_education_occupation,
             median_land_value_per_sqm,house_median_suburb,apartment_median_suburb)  
    
    test_1 <- comparison_scaled_data %>%
      select(median_land_value_per_sqm) %>%
      t()
    
    
    scaled_values <- comparison_scaled_data %>%
      filter(year == 2019) %>%
      filter(suburb_name == "Haberfield")
    
    other_suburbs <- comparison_scaled_data %>%
      filter(year == 2019) %>%
      filter(suburb_name != "Haberfield")
    
    na_count <- other_suburbs %>%
        mutate(na_count = rowSums(is.na(other_suburbs))) %>%
        select(na_count)
      
    new <- tibble(new_values = c(1000,3,3,2,
                                 2000,0.5,0.2,0.1,
                                 0.5,0.5,0.3,
                                 0.3,200,1000,1000,
                                 1000,1000,30,700000,
                                 600000)) %>%
      cbind(select_scaler) %>%
      mutate(scaled_value = (new_values - mean) / sd) %>%
      select(scaled_value) %>% 
      rownames_to_column %>%
      gather(variable, value, -rowname) %>% 
      spread(rowname, value) %>%
      select(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`)
    
    mtcars %>% 
      rownames_to_column %>%
      gather(variable, value, -rowname) %>% 
      spread(rowname, value)
    
    dist <- as.data.frame(rdist_na(scaled_values[,6:25],other_suburbs[,6:25])) %>%
        rename(distance = V1) #%>%
        #cbind(na_count) %>%
        #mutate(similarity = round(1/(1+(distance/(20-na_count))),4)) %>%
        #select(distance,similarity)
    
    dist <- as.data.frame(rdist_na(new[,2:21],other_suburbs[,6:25])) %>%
      rename(distance = V1) #%>%
    #cbind(na_count) %>%
    #mutate(similarity = round(1/(1+(distance/(20-na_count))),4)) %>%
    #select(distance,similarity)
    
    
    rdist_na <- function(X,Y){
      if (!is.matrix(X)) 
        X = as.matrix(X)
      if (!is.matrix(Y)) 
        Y = as.matrix(Y)
      distances <- matrix(pdist(X,Y)@dist, ncol=nrow(X), byrow = TRUE)
      #count NAs
      na.count <- sapply(1:nrow(X),function(i){rowSums(is.na(Y) | is.na(X[i,]))})
      #scaling to number of cols
      distances * sqrt(ncol(X)/(ncol(X) - na.count))
    }
    
    