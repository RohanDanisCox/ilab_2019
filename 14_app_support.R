library(tidyr)

## APP support

  master <- readRDS("data/created/master.rds")
  
  choices <- master %>%
    distinct(suburb_name,sa4_name,sa3_name,sa2_name)
  
  saveRDS(choices, "suburb_investigation_app/data/choices.rds")

map <- readRDS("data/nsw_sa4.rds")

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
  geom_line(colour = "blue") + 
  geom_line(data = haberfield,mapping = aes(x = year, y = house_median_nsw,colour = "red"))



# Building the data files which will be used for the suburb_investigator_app

  #The master file should be only data points that matter
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
           Green_Space = green_score,
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
           SEIFA_Relative_Socio_Economic_Disadvantage = relative_socio_economic_disadvantage_index)

  saveRDS(suburb_data_ready, "suburb_investigation_app/data/suburb_data.rds")

  nsw_data <- suburb_data %>%
    group_by(year) %>%
    summarise(Crime = mean(log_crime_score, na.rm = TRUE), 
              Education = mean(education_score, na.rm = TRUE), 
              Green_Space = mean(green_score, na.rm = TRUE),
              Census_Population = mean(confirmed_population, na.rm = TRUE), 
              Usual_Resident_Population = mean(usual_resident_population, na.rm = TRUE),
              Working_Age = mean(working_age_proportion, na.rm = TRUE),
              Senior_Citizens = mean(senior_citizen_proportion, na.rm = TRUE), 
              Journeys_to_Work = mean(confirmed_journeys, na.rm = TRUE),
              Journey_to_Work_by_Public_Transport = mean(public_transport_proportion, na.rm = TRUE), 
              Journey_to_Work_by_Motor_Vehicle = mean(motor_vehicle_proportion, na.rm = TRUE),
              Journey_to_Work_by_Bicycle_or_Walking = mean(bicycle_walking_proportion, na.rm = TRUE),
              Number_of_Dwellings = mean(confirmed_dwellings, na.rm = TRUE), 
              Proportion_of_House = mean(house_and_semi_proportion, na.rm = TRUE),
              Proportion_of_Units = mean(unit_proportion, na.rm = TRUE),
              SEIFA_Relative_Socio_Economic_Disadvantage = mean(relative_socio_economic_disadvantage_index, na.rm = TRUE))

  saveRDS(nsw_data, "suburb_investigation_app/data/nsw_data.rds")


names(master)


# Fixing choices

