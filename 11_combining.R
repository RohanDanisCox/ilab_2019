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
  
  land_values <- readRDS("data/created/land_values.rds")
  
  annual_turnover <- readRDS("data/created/annual_turnover.rds")
  
  metro_aria_scores <- readRDS("data/created/metro_aria.rds") 
  
  metro_aria_year <- metro_aria_scores %>%
    mutate(year = 2014) %>%
    bind_rows(metro_aria_scores) %>%
    mutate(year = case_when(is.na(year) ~ 2015,
                            TRUE ~ year)) %>%
    bind_rows(metro_aria_scores) %>%
    mutate(year = case_when(is.na(year) ~ 2016,
                            TRUE ~ year)) %>%
    bind_rows(metro_aria_scores) %>%
    mutate(year = case_when(is.na(year) ~ 2017,
                            TRUE ~ year)) %>%
    bind_rows(metro_aria_scores) %>%
    mutate(year = case_when(is.na(year) ~ 2018,
                            TRUE ~ year)) %>%
    bind_rows(metro_aria_scores) %>%
    mutate(year = case_when(is.na(year) ~ 2019,
                            TRUE ~ year))
  
  master_sales_by_year_raw <- readRDS("data/created/master_sales_by_year.rds") 
  
# [2] ---- Getting the years right ----
  year <- tibble(year = 1990:2019)
  
  suburbs_distinct <- suburb_2016 %>%
    select(1:2)
  
  suburbs_raw <- crossing(suburbs_distinct, year) %>%
    arrange(suburb_code,year)

# [3] ---- Handling Census & SEIFA information scores----
  
  # Add suburb correspondence quality to decide whether it is fair to extrapolate
  
  suburbs <- correspondence %>%
    select(1,2,5,8) %>%
    mutate(can_extrapolate = case_when(quality_indicator_2011 %in% c("Good","Acceptable") ~ 1,
                                  TRUE ~ 0)) %>%
    select(1,2,5) %>%
    distinct() %>%
    right_join(suburbs_raw, by = c("suburb_code", "suburb_name")) %>%
    select(1,2,4,3)

  # Build a function ------- LAG CALCULATION VERSION - slower but cleaner code

  interpolate_extrapolate_lag <- function(df,var){
    quo_var <- enquo(var)
    df_names <- append("valid",names(df),after = length(df)) # for joining later
    
    # Identify whether the suburb has enough valid values to interpolate
    data <- suburbs %>%
      left_join(df, by = c("suburb_code", "suburb_name", "year")) %>%
      group_by(suburb_code,suburb_name) %>%
      mutate(valid = sum(!is.na(!!quo_var))) %>%
      ungroup()
    
    # Interpolate values and then use the value change between 2015 and 2016 to extrapolate until 2019 if allowable
    data_incl <- data %>%
      filter(valid > 1) %>%
      group_by(suburb_code,suburb_name) %>%
      arrange(year) %>%
      mutate(interpolate_value=approx(x = year,y = !!quo_var,xout = year)$y) %>%
      mutate(extrapolate_value_change = interpolate_value - lag(interpolate_value)) %>%
      mutate(extrapolate_value = case_when(year == 2017 ~ lag(interpolate_value)+lag(extrapolate_value_change),
                                           year == 2018 ~ lag(interpolate_value,2) + (lag(extrapolate_value_change,2)*2),
                                           year == 2019 ~ lag(interpolate_value,3) + (lag(extrapolate_value_change,3)*3),
                                           TRUE ~ NA_real_)) %>% 
      mutate(extrapolate_value = case_when(year == 2017 & can_extrapolate == 0 ~ lag(interpolate_value),
                                           year == 2018 & can_extrapolate == 0 ~ lag(interpolate_value,2),
                                           year == 2019 & can_extrapolate == 0 ~ lag(interpolate_value,3),
                                           TRUE ~ extrapolate_value)) %>% 
      mutate(extrapolate_value = case_when(extrapolate_value < 0 ~ 0,
                                           TRUE ~ extrapolate_value)) %>%
      ungroup()
    
    # Use the 2016 value for all years up to 2019
    data_excl <- data %>%
      filter(valid == 1) %>%
      group_by(suburb_code,suburb_name) %>%
      arrange(year) %>%
      mutate(value = case_when(year %in% c(2016,2017,2018,2019) ~ mean(!!quo_var,na.rm = TRUE),
                               TRUE ~ NA_real_)) %>%
      ungroup()
    
    # combine and filter the data
    final_data <- data %>%
      left_join(data_incl, by = df_names) %>%
      left_join(data_excl, by = df_names) %>%
      mutate(!!quo_var := case_when(valid == 1 ~ value,
                                    year > 2016 ~ extrapolate_value,
                                    year <= 2016 ~ interpolate_value)) %>%
      select(suburb_code,suburb_name, year, !!quo_var)
  }
  
  ## Census data - as C
  
  c1 <- interpolate_extrapolate_lag(census_scores,confirmed_population)
  c2 <- interpolate_extrapolate_lag(census_scores,working_age_proportion)
  c3 <- interpolate_extrapolate_lag(census_scores,senior_citizen_proportion)
  c4 <- interpolate_extrapolate_lag(census_scores,confirmed_journeys)
  c5 <- interpolate_extrapolate_lag(census_scores,public_transport_proportion)
  c6 <- interpolate_extrapolate_lag(census_scores,motor_vehicle_proportion)
  c7 <- interpolate_extrapolate_lag(census_scores,bicycle_walking_proportion)
  c8 <- interpolate_extrapolate_lag(census_scores,confirmed_dwellings)
  c9 <- interpolate_extrapolate_lag(census_scores,house_and_semi_proportion)
  c10 <- interpolate_extrapolate_lag(census_scores,unit_proportion)
  c11 <- interpolate_extrapolate_lag(census_scores,other_proportion)
  
  census <- c1 %>%
    left_join(c2,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(c3,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(c4,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(c5,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(c6,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(c7,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(c8,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(c9,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(c10,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(c11,by = c("suburb_code", "suburb_name", "year"))
  
 ## SEIFA data - as S
  
  s1 <- interpolate_extrapolate_lag(seifa_scores, usual_resident_population)
  
  # Need usual_resident_population to be extended back in time in order allow for weighted means calculations
  s1_2 <- s1 %>%
    nest(-c(suburb_code,suburb_name)) %>%
    mutate(fit = map(data, ~ lm(usual_resident_population ~ year, data = .x)), 
           prediction = map2(fit,data,predict)) %>% 
    unnest(prediction,data) %>%
    mutate(prediction = prediction)
  
  s1_3 <- s1_2 %>%
    mutate(usual_resident_population = case_when(is.na(usual_resident_population) & prediction > 0 ~ prediction,
                                                 is.na(usual_resident_population) & prediction <= 0 ~ 0,
                                                 TRUE ~ usual_resident_population)) %>%
    select(suburb_code,suburb_name,year,usual_resident_population)
  
  s2 <- interpolate_extrapolate_lag(seifa_scores, relative_socio_economic_disadvantage_index)
  s3 <- interpolate_extrapolate_lag(seifa_scores, relative_socio_economic_adv_disadv_index)
  s4 <- interpolate_extrapolate_lag(seifa_scores, economic_resources_index)
  s5 <- interpolate_extrapolate_lag(seifa_scores, education_and_occupation_index)
  
  seifa <- s1_3 %>%
    left_join(s2,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(s3,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(s4,by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(s5,by = c("suburb_code", "suburb_name", "year")) %>%
    mutate(usual_resident_population = round(usual_resident_population,0))

# [5] ---- Combine suburbs with the remaining scores ----
  
  crime <- crime_score
  
  education <- suburbs %>%
    left_join(education_score, by = c("suburb_code", "suburb_name")) %>%
    select(suburb_code,suburb_name,year,education_score)
  
  green <- suburbs %>%
    left_join(green_score, by = c("suburb_code", "suburb_name")) %>%
    select(suburb_code,suburb_name,year, green_score, green_score_decile)
  
  metro_aria <- suburbs %>%
    left_join(metro_aria_year, by = c("suburb_code", "suburb_name", "year")) %>%
    select(-can_extrapolate)
  
  sales_by_year <- suburbs %>%
    left_join(master_sales_by_year_raw, by = c("suburb_code", "suburb_name", "year")) %>%
    select(-can_extrapolate)
  
  #Add back useful stuff to suburbs
  suburbs_add_back <- suburbs %>%
    left_join(suburb_2016, by = c("suburb_code", "suburb_name")) %>%
    select(1:18)
  
# [6] ---- Save / Load Ready Data ----
  
  # Save off now that ready to combine
  write_rds(suburbs_add_back,"data/created/ready_to_combine/suburbs.rds")
  write_rds(crime,"data/created/ready_to_combine/crime.rds")
  write_rds(education,"data/created/ready_to_combine/education.rds")
  write_rds(green,"data/created/ready_to_combine/green.rds")
  write_rds(census,"data/created/ready_to_combine/census.rds")
  write_rds(seifa,"data/created/ready_to_combine/seifa.rds")
  write_rds(land_values,"data/created/ready_to_combine/land_values.rds")
  write_rds(annual_turnover,"data/created/ready_to_combine/annual_turnover.rds")
  write_rds(metro_aria,"data/created/ready_to_combine/metro_aria.rds")
  write_rds(sales_by_year,"data/created/ready_to_combine/sales_by_year.rds")
  
  # Load ready data
  suburbs <- readRDS("data/created/ready_to_combine/suburbs.rds")
  crime <- readRDS("data/created/ready_to_combine/crime.rds")
  education <- readRDS("data/created/ready_to_combine/education.rds")
  green <- readRDS("data/created/ready_to_combine/green.rds")
  census <- readRDS("data/created/ready_to_combine/census.rds")
  seifa <- readRDS("data/created/ready_to_combine/seifa.rds")
  land_values <- readRDS("data/created/ready_to_combine/land_values.rds")
  annual_turnover <- readRDS("data/created/ready_to_combine/annual_turnover.rds")
  metro_aria <- readRDS("data/created/ready_to_combine/metro_aria.rds")
  sales_by_year <- readRDS("data/created/ready_to_combine/sales_by_year.rds")
  
# [7] ---- Creating the master file ----
  
  master_raw <- suburbs %>%
    left_join(crime, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(education, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(green, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(census, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(seifa, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(land_values, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(annual_turnover, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(metro_aria, by = c("suburb_code", "suburb_name", "year")) %>%
    left_join(sales_by_year, by = c("suburb_code", "suburb_name", "year"))
  
  write_rds(master_raw,"data/created/master_raw.rds")
  master_raw <- readRDS("data/created/master_raw.rds")
  
  # Adding in extra measures based on combinations of existing measures
  master_raw_1 <- master_raw %>%
    mutate(dwelling_density = confirmed_dwellings/suburb_area_sqkm,
           annual_turnover = case_when(is.na(Apartment_number_of_sales) & 
                                         is.na(House_number_of_sales) ~ NA_real_,
                                       !is.na(Apartment_number_of_sales) & 
                                         is.na(House_number_of_sales) ~ Apartment_number_of_sales,
                                       is.na(Apartment_number_of_sales) & 
                                         !is.na(House_number_of_sales) ~ House_number_of_sales,
                                       !is.na(Apartment_number_of_sales) & 
                                         !is.na(House_number_of_sales) ~ Apartment_number_of_sales + House_number_of_sales),
           annual_turnover_proportion = annual_turnover / confirmed_dwellings,
           median_house_price_less_land_value = House_median - Land_median)
    
  write_rds(master_raw_1,"data/created/master.rds")
  master <- readRDS("data/created/master.rds")
  
  master_bio <- master %>% 
    map_df(~(data.frame(n_distinct = n_distinct(.x),
                        class = class(.x),
                        na_count = sum(is.na(.x)))),
           .id = "variable")
  
  summary(master)
  
  # Save off to dropbox
  drop_upload("data/created/master.rds", path = "ilab2019/master")

# [XX] ---- Extra Redundant Functions ----
  
  # Census interpolate and extrapolate - LINEAR MODEL VERSION
  
  interpolate_extrapolate_lm <- function(df,var){
    quo_var <- enquo(var)
    df_names <- append("valid",names(df),after = length(df)) # for joining later
    
    # Identify whether the suburb has enough valid values to interpolate
    data <- suburbs %>%
      left_join(census_scores, by = c("suburb_code", "suburb_name", "year")) %>%
      group_by(suburb_code,suburb_name) %>%
      mutate(valid = sum(!is.na(!!quo_var))) %>%
      ungroup()
    
    # Interpolate values and then fit a linear model to each suburb and use this to extrapolate beyond 2016
    data_incl <- data %>%
      filter(valid > 1) %>%
      group_by(suburb_code,suburb_name) %>%
      arrange(year) %>%
      mutate(interpolate_value=approx(x = year,y = !!quo_var,xout = year)$y) %>%
      ungroup() %>%
      nest(-c(suburb_code,suburb_name)) %>%
      mutate(fit = map(data, ~ lm(interpolate_value ~ year, data = .x)), # Fitting a simple linear model to the interpolated values by year
             prediction = map2(fit,data,predict)) %>% # Get predictions based on this model for all years
      unnest(prediction,data) %>%
      mutate(prediction = prediction)
    
    # Use the 2016 value for all years up to 2019
    data_excl <- data %>%
      filter(valid == 1) %>%
      group_by(suburb_code,suburb_name) %>%
      arrange(year) %>%
      mutate(value = case_when(year %in% c(2016,2017,2018,2019) ~ mean(!!quo_var,na.rm = TRUE),
                               TRUE ~ NA_real_)) %>%
      ungroup()
    
    # combine and filter the data
    final_data <- data %>%
      left_join(data_incl, by = df_names) %>%
      left_join(data_excl, by = df_names) %>%
      mutate(!!quo_var := case_when(valid == 1 ~ value,
                                    year > 2016 ~ prediction,
                                    year <= 2016 ~ interpolate_value)) %>%
      select(suburb_code,suburb_name, year, !!quo_var)
  }
  
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
  