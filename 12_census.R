# Combining all the data

# [0] ---- Load packages ----

library(readxl)
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

  # Age

  suburb_age_2006 <- read_xlsx("data/census/age_by_suburb_5_year_2006.xlsx", skip = 8) %>%
    select(suburb_name_2006 = 2,3:24) %>%
    filter(suburb_name_2006 != "State Suburbs" & suburb_name_2006 != "Total") %>%
    filter(!is.na(`0-4 years`))
  
  suburb_age_2011 <- read_xlsx("data/census/age_by_suburb_5_year_2011.xlsx", skip = 8) %>%
    select(suburb_name_2011 = 2,3:24) %>%
    filter(suburb_name_2011 != "State Suburbs" & suburb_name_2011 != "Total") %>%
    filter(!is.na(`0-4 years`))
  
  suburb_age_2016 <- read_xlsx("data/census/age_by_suburb_5_year_2016.xlsx", skip = 8) %>%
    select(suburb_name_2016 = 2,3:24) %>%
    filter(suburb_name_2016 != "State Suburbs" & suburb_name_2016 != "Total") %>%
    filter(!is.na(`0-4 years`))

# Journey to work
  
  journey_to_work_2006 <- read_xls("data/census/mtwp_2006.xls", skip = 8) %>%
    select(suburb_name_2006 = 2,3:239) %>%
    filter(suburb_name_2006 != "State Suburbs" & suburb_name_2006 != "Total") %>%
    filter(!is.na(Train))
  
  journey_to_work_2011 <- read_xls("data/census/mtwp_2011.xls", skip = 8) %>%
    select(suburb_name_2011 = 2,3:239) %>%
    filter(suburb_name_2011 != "State Suburbs" & suburb_name_2011 != "Total") %>%
    filter(!is.na(Train))
  
  journey_to_work_2016 <- read_xls("data/census/mtwp_2016.xls", skip = 8) %>%
    select(suburb_name_2016 = 2,3:239) %>%
    filter(suburb_name_2016 != "State Suburbs" & suburb_name_2016 != "Total") %>%
    filter(!is.na(Train))
  
  # Dwellings
  
  dwellings_2006 <- read_xlsx("data/census/dwellings_2006.xlsx", skip = 8) %>%
    select(suburb_name_2006 = 2,3:15) %>%
    filter(suburb_name_2006 != "State Suburbs" & suburb_name_2006 != "Total") %>%
    filter(!is.na(`Separate house`))

  dwellings_2011 <- read_xlsx("data/census/dwellings_2011.xlsx", skip = 8) %>%
    select(suburb_name_2011 = 2,3:15) %>%
    filter(suburb_name_2011 != "State Suburbs" & suburb_name_2011 != "Total") %>%
    filter(!is.na(`Separate house`))

  dwellings_2016 <- read_xlsx("data/census/dwellings_2016.xlsx", skip = 8) %>%
    select(suburb_name_2016 = 2,3:15) %>%
    filter(suburb_name_2016 != "State Suburbs" & suburb_name_2016 != "Total") %>%
    filter(!is.na(`Separate house`))

# [2] ---- Write functions to get measures ----
  
  age_bands <- function(df) {
    df %>%
      mutate(confirmed_population = select(.,2:22) %>%
             rowSums(na.rm = TRUE)) %>%
      mutate(working_age_total = select(.,5:14) %>%
             rowSums(na.rm = TRUE)) %>%
      mutate(senior_citizen_total = select(.,15:22) %>%
             rowSums(na.rm = TRUE)) %>%
      mutate(working_age_proportion = working_age_total / confirmed_population,
             senior_citizen_proportion = senior_citizen_total / confirmed_population) %>%
      select(1,24,27:28)
  }

  methods_of_travel <- function(df){
    all_forms <- c(2:233)
    predominantly_public_transport <- c(2:5,13:46,68:197)
    predominantly_motor_vehicle <- c(6:10,47:66,198:232)
    bicycle_walking<- c(11,233)
    df %>%
      mutate(confirmed_journeys = select(.,all_forms) %>%
               rowSums(na.rm = TRUE)) %>%
      mutate(public_transport_total = select(.,predominantly_public_transport) %>%
               rowSums(na.rm = TRUE)) %>%
      mutate(motor_vehicle_total = select(.,predominantly_motor_vehicle) %>%
               rowSums(na.rm = TRUE)) %>%
      mutate(bicycle_walking_total = select(.,bicycle_walking) %>%
               rowSums(na.rm = TRUE)) %>%
      mutate(public_transport_proportion = public_transport_total/confirmed_journeys,
             motor_vehicle_proportion = motor_vehicle_total/confirmed_journeys,
             bicycle_walking_proportion = bicycle_walking_total/confirmed_journeys) %>%
      select(1,239,243:245)
  }
  
  dwelling_type <- function(df){
    df %>%
      mutate(confirmed_dwellings = select(.,2:11) %>%
               rowSums(na.rm = TRUE)) %>%
      mutate(house_and_semi_total = select(.,2:4) %>%
               rowSums(na.rm = TRUE)) %>%
      mutate(unit_total = select(.,5:8) %>%
               rowSums(na.rm = TRUE)) %>%
      mutate(other_total = select(.,9:11) %>%
               rowSums(na.rm = TRUE)) %>%
      mutate(house_and_semi_proportion = house_and_semi_total/confirmed_dwellings,
             unit_proportion = unit_total/confirmed_dwellings,
             other_proportion = other_total/confirmed_dwellings) %>%
      select(1,15,19:21)
  }

# [3] ---- Building the measures ----
  
  suburb_ages_2006 <- age_bands(suburb_age_2006) %>% mutate(year = 2006)
  suburb_ages_2011 <- age_bands(suburb_age_2011) %>% mutate(year = 2011)
  suburb_ages_2016 <- age_bands(suburb_age_2016) %>% mutate(year = 2016)
  
  method_of_travel_2006 <- methods_of_travel(journey_to_work_2006) %>% mutate(year = 2006)
  method_of_travel_2011 <- methods_of_travel(journey_to_work_2011) %>% mutate(year = 2011)
  method_of_travel_2016 <- methods_of_travel(journey_to_work_2016) %>% mutate(year = 2016)
  
  dwelling_type_2006 <- dwelling_type(dwellings_2006) %>% mutate(year = 2006)
  dwelling_type_2011 <- dwelling_type(dwellings_2011) %>% mutate(year = 2011)
  dwelling_type_2016 <- dwelling_type(dwellings_2016) %>% mutate(year = 2016)
  
# [3] ---- Combine ----
  
  census_measures_2006 <- suburb_ages_2006 %>%
    left_join(method_of_travel_2006, by = c("suburb_name_2006", "year")) %>%
    left_join(dwelling_type_2006, by = c("suburb_name_2006", "year"))

  census_measures_2011 <- suburb_ages_2011 %>%
    left_join(method_of_travel_2011, by = c("suburb_name_2011", "year")) %>%
    left_join(dwelling_type_2011, by = c("suburb_name_2011", "year"))
  
  census_measures_2016 <- suburb_ages_2016 %>%
    left_join(method_of_travel_2016, by = c("suburb_name_2016", "year")) %>%
    left_join(dwelling_type_2016, by = c("suburb_name_2016", "year"))
  
  correspondence <- readRDS("data/created/correspondence.rds")
  names(correspondence)
  
  suburb_correspondence_2006 <- correspondence %>%
    select(1:4)
  suburb_correspondence_2011 <- correspondence %>%
    select(1:2,6:7)
  suburb_correspondence_2016 <- correspondence %>%
    select(1:2)
  
  census_measures_2006_ready <- census_measures_2006 %>%
    left_join(suburb_correspondence_2006, by = "suburb_name_2006") %>%
    select(14:15,5,2:4,6:13) %>%
    distinct()
  
  census_measures_2011_ready <- census_measures_2011 %>%
    left_join(suburb_correspondence_2011, by = "suburb_name_2011") %>%
    select(14:15,5,2:4,6:13) %>%
    distinct()
  
  census_measures_2016_ready <- census_measures_2016 %>%
    left_join(suburb_correspondence_2016, by = c("suburb_name_2016" = "suburb_name")) %>%
    select(14,suburb_name = 1,5,2:4,6:13) %>%
    distinct()
  
  census_measures_raw <- census_measures_2006_ready %>%
    bind_rows(census_measures_2011_ready) %>%
    bind_rows(census_measures_2016_ready) %>%
    arrange(suburb_code, year)
 
# [4] ---- Allowing for amalgamated suburbs ----
  
  amalgamated_suburb <- correspondence %>%
    select(1:4) %>% 
    group_by(suburb_code,suburb_name) %>%
    mutate(n = n(),
           amalgamated = case_when(n>1 ~ TRUE,
                                TRUE ~ FALSE)) %>%
    filter(amalgamated == TRUE) %>%
    select(1:2,6) %>%
    distinct()
  
  amalgamated_census <- census_measures_raw %>%
    left_join(amalgamated_suburb, by = c("suburb_code", "suburb_name")) %>%
    filter(amalgamated == TRUE) %>%
    group_by(suburb_code,suburb_name,year) %>%
    mutate(combined_population = sum(confirmed_population),
           individual_population = confirmed_population) %>%
    group_by(suburb_code,suburb_name,year) %>%
    summarise(confirmed_population = sum(individual_population),
              working_age_proportion = weighted.mean(working_age_proportion,individual_population/combined_population),
              senior_citizen_proportion = weighted.mean(senior_citizen_proportion,individual_population/combined_population),
              confirmed_journeys = sum(confirmed_journeys),
              public_transport_proportion = weighted.mean(public_transport_proportion,individual_population/combined_population),
              motor_vehicle_proportion = weighted.mean(motor_vehicle_proportion,individual_population/combined_population),
              bicycle_walking_proportion = weighted.mean(bicycle_walking_proportion,individual_population/combined_population),
              confirmed_dwellings = sum(confirmed_dwellings),
              house_and_semi_proportion = weighted.mean(house_and_semi_proportion,individual_population/combined_population),
              unit_proportion = weighted.mean(unit_proportion,individual_population/combined_population),
              other_proportion = weighted.mean(other_proportion,individual_population/combined_population))
              
  census_measures <- census_measures_raw %>%
    anti_join(amalgamated_suburb, by = c("suburb_code", "suburb_name")) %>%
    bind_rows(amalgamated_census) %>%
    arrange(suburb_code,year)
  
# [3] ---- Save off as artefact ----
  
  write_rds(census_measures,"data/created/census_scores.rds")
    