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
  
  census_scores <- readRDS("data/created/census_scores.rds")
  
# [2] ---- Getting the years right ----
  year <- tibble(year = 1990:2019)
  
  suburbs <- suburb_2016 %>%
    select(1:2)
  
  suburbs <- crossing(suburbs, year)

# [3] ---- Handling Census scores----
  
  # Create a function to interpolate scores
  interpolate_scores_census <- function(df,var) {
    use_var <- enquo(var)
    name <- quo_name(use_var)
    data <- df %>%
      select(suburb_code,suburb_name,year,!!use_var) %>%
      arrange(suburb_code,year) %>%
      group_by(suburb_code,suburb_name) %>%
      mutate(grouped_id = row_number()) %>%
      spread(key = year,value = !!use_var) %>%
      select(1,2, y2006 = 4,y2011 = 5, y2016 = 6) %>%
      group_by(suburb_code,suburb_name) %>%
      mutate(y2006 = max(y2006, na.rm = TRUE),
             y2011 = max(y2011, na.rm = TRUE),
             y2016 = max(y2016, na.rm = TRUE)) %>%
      ungroup() %>%
      distinct() %>%
      mutate(y2007 = ((y2006 * 4) + (y2011 * 1))/5,
             y2008 = ((y2006 * 3) + (y2011 * 2))/5,
             y2009 = ((y2006 * 2) + (y2011 * 3))/5,
             y2010 = ((y2006 * 1) + (y2011 * 4))/5,
             y2012 = ((y2011 * 4) + (y2016 * 1))/5,
             y2013 = ((y2011 * 3) + (y2016 * 2))/5,
             y2014 = ((y2011 * 2) + (y2016 * 3))/5,
             y2015 = ((y2011 * 1) + (y2016 * 4))/5,
             y2017 = case_when(is.infinite(y2011) ~ y2016,
                               TRUE ~ (y2016 - y2015) + y2016),
             y2018 = case_when(is.infinite(y2011) ~ y2016,
                               TRUE ~ (y2016 - y2015)*2 + y2016),
             y2019 = case_when(is.infinite(y2011) ~ y2016,
                               TRUE ~ (y2016 - y2015)*3 + y2016)) %>%
      gather(key = "year", value = !!use_var, -c(suburb_code,suburb_name)) %>%
      mutate(year = str_replace(year,"y","")) %>%
      mutate(year = as.numeric(year)) %>%
      mutate(!!name := case_when(is.infinite(!!use_var) ~ NA_real_,
                                                   TRUE ~ !!use_var)) %>%
      arrange(suburb_code,year)
  }
  
  ## CENSUS
  names(census_scores)
  
  a <- interpolate_scores_census(census_scores,confirmed_population)
  b <- interpolate_scores_census(census_scores,working_age_proportion)
  c <- interpolate_scores_census(census_scores,senior_citizen_proportion)
  d <- interpolate_scores_census(census_scores,confirmed_journeys)
  e <- interpolate_scores_census(census_scores,public_transport_proportion)
  f <- interpolate_scores_census(census_scores,motor_vehicle_proportion)
  g <- interpolate_scores_census(census_scores,bicycle_walking_proportion)
  h <- interpolate_scores_census(census_scores,confirmed_dwellings)
  i <- interpolate_scores_census(census_scores,house_and_semi_proportion)
  j <- interpolate_scores_census(census_scores,unit_proportion)
  k <- interpolate_scores_census(census_scores,other_proportion)

  census <- a %>%
    left_join(b,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(c,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(d,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(e,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(f,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(g,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(h,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(i,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(j,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(k,by = c("suburb_code", "suburb_name", "year"))
  
 
  # [4] ---- Handling Seifa scores ----
  
  # Create a function to interpolate scores
  interpolate_scores_seifa <- function(df,var) {
    use_var <- enquo(var)
    name <- quo_name(use_var)
    data <- df %>%
      select(suburb_code,suburb_name,year,!!use_var) %>%
      arrange(suburb_code,year) %>%
      group_by(suburb_code,suburb_name) %>%
      mutate(grouped_id = row_number()) %>%
      spread(key = year,value = !!use_var) %>%
      select(1,2,y2001 = 4, y2006 = 5,y2011 = 6, y2016 = 7) %>%
      group_by(suburb_code,suburb_name) %>%
      mutate(y2001 = max(y2001, na.rm = TRUE),
             y2006 = max(y2006, na.rm = TRUE),
             y2011 = max(y2011, na.rm = TRUE),
             y2016 = max(y2016, na.rm = TRUE)) %>%
      ungroup() %>%
      distinct() %>%
      mutate(y2002 = ((y2001 * 4) + (y2006 * 1))/5,
             y2003 = ((y2001 * 3) + (y2006 * 2))/5,
             y2004 = ((y2001 * 2) + (y2006 * 3))/5,
             y2005 = ((y2001 * 1) + (y2006 * 4))/5,
             y2007 = ((y2006 * 4) + (y2011 * 1))/5,
             y2008 = ((y2006 * 3) + (y2011 * 2))/5,
             y2009 = ((y2006 * 2) + (y2011 * 3))/5,
             y2010 = ((y2006 * 1) + (y2011 * 4))/5,
             y2012 = ((y2011 * 4) + (y2016 * 1))/5,
             y2013 = ((y2011 * 3) + (y2016 * 2))/5,
             y2014 = ((y2011 * 2) + (y2016 * 3))/5,
             y2015 = ((y2011 * 1) + (y2016 * 4))/5,
             y2017 = case_when(is.infinite(y2011) ~ y2016,
                               TRUE ~ (y2016 - y2015) + y2016),
             y2018 = case_when(is.infinite(y2011) ~ y2016,
                               TRUE ~ (y2016 - y2015)*2 + y2016),
             y2019 = case_when(is.infinite(y2011) ~ y2016,
                               TRUE ~ (y2016 - y2015)*3 + y2016)) %>%
      gather(key = "year", value = !!use_var, -c(suburb_code,suburb_name)) %>%
      mutate(year = str_replace(year,"y","")) %>%
      mutate(year = as.numeric(year)) %>%
      mutate(!!name := case_when(is.infinite(!!use_var) ~ NA_real_,
                                 TRUE ~ round(!!use_var,1))) %>%
      arrange(suburb_code,year)
  }

  names(seifa_scores)
  
  usual_res_pop <- interpolate_scores_seifa(seifa_scores,usual_resident_population)
  rel_dis <- interpolate_scores_seifa(seifa_scores, relative_socio_economic_disadvantage_index)
  rel_adv_dis<- interpolate_scores_seifa(seifa_scores, relative_socio_economic_adv_disadv_index)
  eco_res <- interpolate_scores_seifa(seifa_scores, economic_resources_index)
  edu_occ <- interpolate_scores_seifa(seifa_scores, education_and_occupation_index)
  
  seifa <- usual_res_pop %>%
    left_join(rel_dis,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(rel_adv_dis,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(eco_res,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(edu_occ,by = c("suburb_code", "suburb_name", "year")) %>%
    mutate(usual_resident_population = round(usual_resident_population,0))

# [5] ---- Combine all the data ----
  
  crime <- crime_score %>%
    select(suburb_code,suburb_name,year,crime_score = log_crime_score)
  
  education <- suburbs %>%
    left_join(education_score, by = c("suburb_code", "suburb_name")) %>%
    select(suburb_code,suburb_name,year,education_score)
  
  green <- suburbs %>%
    left_join(green_score, by = c("suburb_code", "suburb_name")) %>%
    select(suburb_code,suburb_name,year, green_score)
  
  all <- suburbs %>%
    left_join(crime, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(education, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(green, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(seifa, by = c("suburb_code", "suburb_name", "year"))
  