# Education

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

# [1] ---- Load the NSW school data and 2016 shapefile ----

  gov_schools <- read_sf("data/education/gov_schools") 
  
  non_gov_schools <- read_sf("data/education/nongov_schools") 
  
  nsw_2016 <- read_sf("data/shapefiles/2016") %>%
    filter(STE_CODE16 == 1) %>%
    select(suburb_code = SSC_CODE16,
           suburb_name = SSC_NAME16,
           area = AREASQKM16)
  
  names(nsw_2016)
  
# [2] ---- Clean the Data----
  
  gov_schools1 <- gov_schools %>%
    mutate(sector = "Government",
           denomination = NA_character_) %>%
    select(sector,
           school_name = X2school_na,
           street,
           date_opened = X3date_1st_,
           sex = X8school_ge,
           denomination,
           school_type = X11school_s,
           selective = X21selectiv,
           level_of_schooling = X22level_of,
           postcode,
           years = X27school_s)
  
  non_gov_schools1 <- non_gov_schools %>%
    mutate(date_opened = NA_character_,
           school_type = NA_character_,
           selective = NA_character_,
           years = NA_character_,
           postcode = as.character(postcode)) %>%
    select(sector,
           school_name = X0school_na,
           street,
           date_opened,
           sex = X2school_ge,
           denomination = X6school_af,
           school_type,
           selective,
           level_of_schooling = X7level_of_,
           postcode,
           years)
  
  schools_data <- gov_schools1 %>%
    rbind(non_gov_schools1) %>%
    mutate(years = case_when(level_of_schooling == "Central/Community School" ~ "K-12",
                                   level_of_schooling == "Combined" ~ "K-12",
                                   level_of_schooling == "Primary School" ~ "K-6",
                                   level_of_schooling == "Primary" ~ "K-6",
                                   level_of_schooling == "Secondary School" ~ "7-12",
                                   level_of_schooling == "Secondary" ~ "7-12",
                                   level_of_schooling == "Environmental Education Centre" ~ "Special",
                                   level_of_schooling == "Other School" ~ "Special",
                                   level_of_schooling == "Special" ~ "Special",
                                   level_of_schooling == "Infants School" ~ "Special",
                                   level_of_schooling == "Schools for Specific Purposes" ~ "Special")) %>%
    mutate(sex = case_when(sex == "Boys" ~ "Male",
                           sex == "Girls" ~ "Female",
                           sex == "Co-ed" ~ "Co-ed",
                           sex == "Coed" ~ "Co-ed",
                           sex == "Male" ~ "Male",
                           sex == "Female" ~ "Female"))
  
# [3] ---- Intersect the data ----  
  
  nsw_schools_data <- schools_data %>%
    st_join(nsw_2016)
  
# [4] ---- Create the suburb proxy score ----  
  
  nsw_schools_data1 <- nsw_schools_data %>%
    st_drop_geometry() %>%
    #group_by(suburb_code,suburb_name) %>%
    mutate(girls_primary = case_when((sex == "Female" | sex == "Co-ed") & 
                                                   (years == "K-6" | years == "K-12") ~ TRUE,
                                                 TRUE ~ FALSE),
           boys_primary = case_when((sex == "Male" | sex == "Co-ed") & 
                                               (years == "K-6" | years == "K-12") ~ TRUE,
                                             TRUE ~ FALSE),
           girls_secondary = case_when((sex == "Female" | sex == "Co-ed") & 
                                               (years == "7-12" | years == "K-12") ~ TRUE,
                                             TRUE ~ FALSE),
           boys_secondary = case_when((sex == "Male" | sex == "Co-ed") & 
                                                  (years == "7-12" | years == "K-12") ~ TRUE,
                                                TRUE ~ FALSE),
           special = case_when(years == "Special" ~ TRUE,
                                        TRUE ~ FALSE)) %>%
    group_by(suburb_code,suburb_name) %>%
    summarise(coverage_girls_primary = sum(girls_primary),
              coverage_boys_primary = sum(boys_primary),
              coverage_girls_secondary = sum(girls_secondary),
              coverage_boys_secondary= sum(boys_secondary),
              coverage_special = sum(special)) %>%
    ungroup() %>%
    mutate(coverage_girls_primary = case_when(coverage_girls_primary > 0 ~ 1,
                                              TRUE ~ 0),
           coverage_boys_primary = case_when(coverage_boys_primary > 0 ~ 1,
                                              TRUE ~ 0),
           coverage_girls_secondary = case_when(coverage_girls_secondary > 0 ~ 1,
                                              TRUE ~ 0),
           coverage_boys_secondary = case_when(coverage_boys_secondary > 0 ~ 1,
                                              TRUE ~ 0),
           coverage_special = case_when(coverage_special > 0 ~ 1,
                                               TRUE ~ 0)) %>%
    mutate(education_score = coverage_girls_primary + 
             coverage_boys_primary + 
             coverage_girls_secondary + 
             coverage_boys_secondary +
             coverage_special) %>%
    select(1,2,8)
              
# [5] ---- Add a score for the Sa2 ----
  
  suburb_2016 <- readRDS("data/created/suburbs.rds")
  
  suburb_education <- suburb_2016 %>%
    mutate(suburb_code = as.character(suburb_code)) %>%
    left_join(nsw_schools_data1, by = c("suburb_code", "suburb_name")) %>%
    mutate(education_score = case_when(is.na(education_score) ~ 0,
                                       TRUE ~ education_score)) %>%
    group_by(sa2_name) %>%
    mutate(sa2_education_mean = mean(education_score)) %>%
    ungroup() %>%
    mutate(suburb_code = as.numeric(suburb_code)) %>%
    mutate(education_score = education_score + sa2_education_mean) %>%
    select(suburb_code,suburb_name,education_score)

# [5] ---- Save off Education score ----  
  
  write_rds(suburb_education,"data/created/education_score.rds")
