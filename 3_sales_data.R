# Sales Data

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
  library(readxl)
  library(lubridate)
  library(ggplot2)
  library(forcats)
  #library(data.table)
  #library(furrr)
  #library(rbenchmark)
  library(parallel)
  library(multidplyr)

# [1] ---- Load Data ----
  
  # Get file path to each file
  sales_files <- paste0("data/sales/",list.files(path = "data/sales"))

  # Load all the sale value data into memory
  sales_data <- sapply(sales_files,readRDS)
  
  # Split the old and new sales data
  old_sales_data <- sales_data[[1]]
  new_sales_data_raw <- sales_data[2:25]
  new_sales_data <- new_sales_data_raw %>%
    bind_rows()
  
  # Save off the new sales data
  saveRDS(new_sales_data, "data/sales/new_sales_data.rds")
  
  # Upload to dropbox
  drop_upload("data/sales/new_sales_data.rds","ilab2019/sale_value")
  
  # Can start from here now
  land_value <- readRDS("data/land_value/land_value.rds")
  old_sales_data <- readRDS("data/sales/old_sales_data.rds")
  new_sales_data <- readRDS("data/sales/new_sales_data.rds")

# [2] ---- Combine old sales data with new data ----
  str(old_sales_data)
  str(new_sales_data)
  
  old_zones <- read_xlsx("data/sales/zoning.xlsx", sheet = 1) %>%
    select(zone_code = Zone,
           zone_description = `Code Name`)
  
  new_zones <- read_xlsx("data/sales/zoning.xlsx", sheet = 2) %>%
    mutate(zoning = case_when(zoning == "Unknown" ~ zone_code,
                              TRUE ~ zoning))
  
  old_sales_data1 <- old_sales_data %>%
    left_join(old_zones, by = "zone_code") %>%
    mutate(sale_counter = NA_real_,
           download_date = NA_real_,
           property_name = NA_real_,
           settlement_date = as.Date(NA),
           zoning = NA_character_,
           nature_of_property = NA_character_,
           primary_purpose = NA_character_,
           strata_lot_number = NA_character_,
           sale_code =NA_character_,
           interest_of_sale = NA_character_,
           dealing_number =  NA_character_) %>%
    select(record_type,district_code,download_date,source,valuation_number,property_id,sale_counter,property_name,
           property_id,unit_number,house_number,street_name,locality,post_code,area,area_type,dimensions,land_description,
           contract_date,purchase_price,settlement_date,zoning,zone_code,zone_description,nature_of_property,primary_purpose,strata_lot_number,
           comp_code,vendor_name,purchaser_name,sale_code,interest_of_sale,dealing_number)
  
  new_sales_data1 <- new_sales_data %>%
    left_join(new_zones, by = "zoning") %>%
    mutate(source = NA_character_,
           valuation_number = NA_character_,
           land_description = NA_character_,
           dimensions = NA_character_,
           vendor_name = NA_character_,
           purchaser_name = NA_character_,
           property_id = as.numeric(property_id)) %>%
    select(record_type,district_code,download_date,source,valuation_number,property_id,sale_counter,property_name,
         property_id,unit_number,house_number,street_name,locality,post_code,area,area_type,dimensions,land_description,
         contract_date,purchase_price,settlement_date,zoning,zone_code, zone_description,nature_of_property,primary_purpose,strata_lot_number,
         comp_code,vendor_name,purchaser_name,sale_code,interest_of_sale,dealing_number)

  sales_data <- bind_rows(old_sales_data1,new_sales_data1)

# [3] ---- Data Understanding ----
  
  # Get the unique suburbs and counts for each data set - roughly 1500 sales have unique suburb names - errors most likely
  sales_unique_suburbs <- sales_data %>%
    group_by(locality,post_code) %>%
    summarise(sales = n()) %>%
    filter(sales > 2)
  
  # There are 64k sales missing a suburb - mostly old and look like they are missing other information
  sales_missing_suburb <- sales_data %>%
    filter(is.na(locality))
  
  # no missing suburbs in the land values
  land_value_unique_suburbs <- land_value %>%
    group_by(locality,post_code) %>%
    summarise(properties = n())
  
# [4] ---- Try to match suburb names to suburb base file ----
    
  sales_unique_suburbs_1 <- sales_unique_suburbs %>%
    mutate(lower_case = tolower(locality)) %>%
    mutate(title_case = tools::toTitleCase(lower_case))
  
  land_value_unique_suburbs_1 <- land_value_unique_suburbs  %>%
    mutate(lower_case = tolower(locality)) %>%
    mutate(title_case = tools::toTitleCase(lower_case))
  
  
  # Try matching with suburbs 
  
  suburbs <- readRDS("data/created/suburbs.rds")
  suburb_crime <- read_csv("data/created/suburb_match_crime.csv")
  
  sales_suburbs <- sales_unique_suburbs_1 %>%
    left_join(suburb_crime, by = c("title_case" = "suburb_crime")) %>% 
    mutate(suburb_base = case_when(is.na(suburb_base) ~ "missing_sales_suburb",
                                   TRUE ~ suburb_base)) %>%
    select(suburb_base,sales_locality = locality, sales_post_code = post_code)
  
  land_value_suburbs <- land_value_unique_suburbs_1 %>%
    left_join(suburb_crime, by = c("title_case" = "suburb_crime")) %>%
    mutate(suburb_base = case_when(is.na(suburb_base) ~ "missing_land_value_suburb",
                                   TRUE ~ suburb_base)) %>%
    select(suburb_base,land_value_locality = locality, land_value_post_code = post_code)
  
  suburbs_1 <- suburbs %>%
    select(1:2) %>%
    full_join(sales_suburbs, by = c("suburb_name" = "suburb_base")) %>%
    full_join(land_value_suburbs, by = c("suburb_name" = "suburb_base"))
  
  write_csv(suburbs_1,"data/created/suburb_match_sales.csv")
  
  drop_upload("data/created/suburb_match_sales.csv","ilab2019/")

# [5] ---- Download cleaned suburb file ----
  
  matching_suburbs <- read_xlsx("data/matching suburbs.xlsx", sheet = 1)
  
  sales_match <- matching_suburbs %>%
    select(suburb_code,suburb_name,locality = sales_locality,post_code = sales_post_code) %>%
    mutate(post_code = as.numeric(post_code)) %>%
    filter(!is.na(locality)) %>%
    distinct()
  
  saveRDS(sales_match,"data/created/sales_match.rds")
  
  land_value_match <- matching_suburbs %>%
    select(suburb_code,suburb_name,locality = land_value_locality,post_code = land_value_post_code) %>%
    mutate(post_code = as.numeric(post_code)) %>%
    filter(!is.na(locality)) %>%
    distinct()
  
  saveRDS(land_value_match,"data/created/land_value_match.rds")
  
# [6] ---- Matching up to data and saving off ----
  
  sales_match <- readRDS("data/created/sales_match.rds")
  land_value_match <- readRDS("data/created/land_value_match.rds")
  
  sales_data1 <- sales_data %>%
    left_join(sales_match, by = c("locality","post_code"))
  
  land_value1 <- land_value %>%
    left_join(land_value_match, by = c("locality","post_code"))
  
  saveRDS(sales_data1,"data/sales/matched_sales.rds")
  saveRDS(land_value1,"data/land_value/matched_land_value.rds")
  
  
# [7] ---- Check whether it is worth matching instead with the 2018 Locality data file ---- 
  
  correspondence <- readRDS("data/created/correspondence.rds") %>%
    select(suburb_code_correspondence = 1, suburb_name_correspondence = 2, locality = 9, post_code = 10)
  
  matched_sales_loc <- matched_sales %>%
    filter(is.na(suburb_code))
    inner_join(correspondence, by = c("locality","post_code")) 
    
  # Can't find any of these so doesn't look like it adds any value
  
# [8] ---- Can start from here now for sales ----  
  
  matched_sales <- readRDS("data/sales/matched_sales.rds")
  
# [9] ---- What are the useful variables? ---- 
  
  matched_sales_bio <- matched_sales %>% map_df(~(data.frame(n_distinct = n_distinct(.x),
                                                             class = class(.x),
                                                             na_count = sum(is.na(.x)))),
                                                .id = "variable")
 
# [10] ---- Filter Sales down to only those that are useful ----   
  
  sales <- matched_sales %>%
    select(-c(record_type,download_date,source,sale_counter,property_name,dimensions,land_description,
              vendor_name,purchaser_name,sale_code,interest_of_sale,comp_code,dealing_number)) %>%
    filter(is.na(area_type) | area_type %in% c("H","M"),
           is.na(nature_of_property) | nature_of_property %in% c("3","R","V")) %>%
    mutate(district_code = as.numeric(district_code),
           valuation_number = as.numeric(valuation_number),
           area_type = fct_explicit_na(case_when(is.na(area_type) ~ NA_character_,
                                           TRUE ~ area_type)),
           area = case_when(area_type == "H" ~ area *10000,
                            TRUE ~ area),
           nature_of_property = fct_explicit_na(case_when(nature_of_property == "3" ~ "Other",
                                                    nature_of_property == "R" ~ "Residence",
                                                    nature_of_property == "V" ~ "Vacant",
                                                    is.na(nature_of_property) ~ NA_character_)),
           strata_lot_number = as.numeric(strata_lot_number),
           zoning = fct_explicit_na(case_when(is.na(zoning) ~ NA_character_,
                                        TRUE ~ zoning)),
           zone_code = fct_explicit_na(case_when(is.na(zone_code) ~ NA_character_,
                                        TRUE ~ zone_code)),
           zone_description = fct_explicit_na(case_when(is.na(zone_description) ~ NA_character_,
                                           TRUE ~ zone_description)),
           property_type = case_when(is.na(strata_lot_number) & is.na(unit_number) ~ "House",
                                     TRUE ~ "Apartment"),
           property_type = case_when(zone_code == "R" & area > 10000 ~ "Land",
                                     TRUE ~ property_type),
           property_type = fct_explicit_na(case_when(property_type == "House" & 
                                                       (str_detect(primary_purpose,"FARM") |
                                                          str_detect(primary_purpose,"LAND") | 
                                                          str_detect(primary_purpose,"RURAL") |
                                                          primary_purpose %in% c("ACREAGE")) ~ "Land",
                                                     TRUE ~ property_type))) %>%
    select(-area_type)
  
  sales_1 <- sales %>%
    filter(purchase_price > 1000) %>%
    filter(!(is.na(house_number) & is.na(property_id))) %>%
    filter(contract_date > as.Date("1990/01/01") & contract_date < as.Date("2019/07/01")) %>% 
    filter(!(area == "(Missing)" & zone_code == "(Missing)" & zoning == "(Missing)" & is.na(primary_purpose))) %>%
    distinct() 
  
  sales_2 <- sales_1 %>%
    mutate(year = floor_date(contract_date,"year"),
           quarter = ceiling_date(contract_date,"quarter"),
           month = floor_date(contract_date,"month"))
  
  filter_out <- sales_2 %>%
    filter(nature_of_property == "Other") %>%
    filter(!str_detect(primary_purpose,"RESIDENTIAL")&
             !str_detect(primary_purpose,"HOUSE") &
             !str_detect(primary_purpose,"FARM") &
             !str_detect(primary_purpose,"HOME") &
             !str_detect(primary_purpose,"RURAL") & 
             !str_detect(primary_purpose,"FLAT") &
             !str_detect(primary_purpose,"UNIT") & 
             !str_detect(primary_purpose,"VILLA") &
             !str_detect(primary_purpose,"RESIDENCE") &
             !str_detect(primary_purpose,"APARTMENT") &
             !str_detect(primary_purpose,"RENTAL") &
             !str_detect(primary_purpose,"GRAZING") &
             !str_detect(primary_purpose,"DWELLING") &
             !str_detect(primary_purpose,"COTTAGE")) 
  
  sales_3 <- sales_2 %>% 
    anti_join(filter_out) 
  
  sales_4 <- sales_3 %>%
    filter(!(is.na(primary_purpose) & str_detect(zone_description,"Industrial"))) %>%
    filter(!(is.na(primary_purpose) & str_detect(zone_description,"Business"))) %>%
    filter(!(is.na(primary_purpose) & str_detect(zone_description,"Commercial")))
  
  sales_5 <- sales_4 %>%
    group_by(unit_number,house_number,street_name,suburb_name,contract_date,purchase_price) %>%
    mutate(packaged_sale_n = n()) %>%
    ungroup()
  
  sales_6 <- sales_5 %>%
    mutate(packaged_sale = case_when(packaged_sale_n >1 ~ "Yes",
                                     TRUE ~ "No"))
  
  sales_7 <- sales_6 %>%
    mutate(new_price = case_when(purchase_price > 2000000 & packaged_sale == "Yes" ~ purchase_price/packaged_sale_n,
                                      TRUE ~ NA_real_)) 
  
  sales_8 <- sales_7 %>%
    filter(!(packaged_sale == "Yes" & property_type %in% c("House","Apartment") & new_price > 5000000)) 
  
  sales_9 <- sales_8 %>%
    mutate(purchase_price = case_when(is.na(new_price)~purchase_price,
                                      TRUE ~ new_price)) %>%
    select(-new_price)
  
  cleaned_sales <- sales_9 %>%
    filter(!(contract_date < as.Date("2002-01-01") &
              purchase_price > 25000000 & 
              property_type %in% c("House","Apartment"))) %>%
    filter(!(contract_date >= as.Date("2002-01-01") &
             purchase_price > 100000000 & 
             property_type %in% c("House","Apartment"))) %>%
    filter(!(is.na(suburb_code)))
  
  cleaned_sales_bio <- cleaned_sales %>% map_df(~(data.frame(n_distinct = n_distinct(.x),
                                                             class = class(.x),
                                                             na_count = sum(is.na(.x)))),
                                                .id = "variable")

  saveRDS(cleaned_sales,"data/created/cleaned_sales.rds")
  
# [11] ---- Start here with cleaned sales ---- 
  
  cleaned_sales <- readRDS("data/created/cleaned_sales.rds")
 
# [11] ---- Calculate a rolling median ----  
  
# [11a] ---- Method 2 - duplicate the dataset across time windows and then group_by time window when calculating median ----
  sales_subset <- cleaned_sales %>%
    select(quarter,contract_date, suburb_code, suburb_name, purchase_price,property_type) 
  
  df_1 <- sales_subset %>%
    mutate(target_date_window = quarter)
  
  df_2 <- sales_subset %>%
    mutate(target_date_window = quarter + months(3))
  
  df_3 <- sales_subset %>%
    mutate(target_date_window = quarter + months(6))
  
  df_big <- bind_rows(df_1,df_2,df_3)
  
  table(df_big$property_type)
  
# [11b] ---- Try using Multiplyr ----   
  
  # Set number of cores into clusters
  cores <- detectCores()
  cluster <- new_cluster(cores)

  # 
  df_clustered <- df_big %>% 
    group_by(suburb_code,suburb_name,target_date_window,property_type) %>% 
    partition(cluster)
  df_clustered
  
  median_9m_window <- df_clustered %>%
    summarise(median = median(purchase_price),
              number_of_sales = n()) %>%
    collect()

  glebe_check <- median_9m_window %>%
    filter(suburb_name == "Glebe (NSW)") %>%
    filter(property_type != "Land")
  
  ggplot(glebe_check,aes(target_date_window,median,colour = property_type)) +
    geom_line()

  write_rds(median_9m_window,"data/created/median_9m_window.rds")
  
# [12] ---- Get volume of sales per year to use for turnover statistics ----
  
  annual_turnover_subset <- cleaned_sales %>%
    select(suburb_code,suburb_name,year,contract_date,purchase_price,property_type) 
    
  annual_turnover_clustered <- annual_turnover_subset %>%
    group_by(suburb_code,suburb_name,year,property_type) %>% 
    partition(cluster)
  
  annual_turnover_clustered
  
  annual_turnover <- annual_turnover_clustered %>%
    summarise(number_of_sales = n(),
              median = median(purchase_price)) %>%
    collect() %>%
    ungroup() %>%
    mutate(year = year(year))
  
  annual_turnover_1 <- annual_turnover %>%
    gather(variable, value, -(suburb_code:property_type)) %>%
    unite(temp, property_type, variable) %>%
    spread(temp, value)

  saveRDS(annual_turnover_1,"data/created/annual_turnover.rds")
  
# [13] ---- Can start from here for land values ----  
  
  matched_land_value <- readRDS("data/land_value/matched_land_value.rds")
  
  matched_land_bio <- matched_land_value %>% map_df(~(data.frame(n_distinct = n_distinct(.x),
                                                             class = class(.x),
                                                             na_count = sum(is.na(.x)))),
                                                .id = "variable")
  
  table(matched_land_value$area_type)
  glimpse(matched_land_value)
  
  # Filter down to what is useful
  
  land_value <- matched_land_value %>%
    select(-c(property_name,property_description,basis_2012,authority_2012,basis_2013,authority_2013,
              basis_2014,authority_2014,basis_2015,authority_2015,basis_2016,authority_2016,
              basis_2017,authority_2017,basis_2018,authority_2018)) %>%
    filter(is.na(area_type) | area_type %in% c("H","M")) %>%
    mutate(area_type = fct_explicit_na(case_when(is.na(area_type) ~ NA_character_,
                                                 TRUE ~ area_type)),
           area = case_when(area_type == "H" ~ area *10000,
                            TRUE ~ area)) %>%
    select(-area_type) %>%
    select(26:27,1:25) %>%
    filter(!is.na(area))
  
  land_value_1 <- land_value %>%
    gather(key = "year", value = "land_value",c(land_value_2012,land_value_2013,land_value_2014,land_value_2015,
                                                land_value_2016,land_value_2017,land_value_2018)) %>%
    mutate(year = as.numeric(str_replace(year,"land_value_",""))) %>%
    select(-c(base_date_2018:base_date_2012))
  
  land_value_2 <- land_value_1 %>%
    distinct() %>%
    filter(!is.na(land_value)) %>%
    mutate(land_value_per_sqm = land_value/area)
  
  saveRDS(land_value_2,"data/created/land_value_cleaned.rds")
  
  # [14] ---- Start here with cleaned land_values ---- 
  
  land_value_cleaned <- readRDS("data/created/land_value_cleaned.rds")
  
  land_value_subset <- land_value_cleaned %>%
    select(suburb_code, suburb_name, year, land_value, land_value_per_sqm) 
  
  cores <- detectCores()
  cluster <- new_cluster(cores)
  
  # 
  land_value_clustered <- land_value_subset %>% 
    group_by(suburb_code,suburb_name,year) %>% 
    partition(cluster)
  land_value_clustered
  
  land_values <- land_value_clustered %>%
    summarise(number_of_properties = n(),
              median_land_value = median(land_value),
              median_land_value_per_sqm = median(land_value_per_sqm)) %>%
    collect()
  
  write_rds(land_values,"data/created/land_values.rds")
  
#### ---- Below here are all things I tried but are now redundant ----
  
  # [11b] ---- Method 1 - Create a subsetting function and iterate through using purrr ----
  df <- cleaned_sales %>%
    distinct(quarter,suburb_name,property_type) %>%
    select(suburbs = suburb_name, property = property_type, end_date = quarter) %>%
    mutate(start_date = end_date - months(6))

  #subset the data 
  method1 <- cleaned_sales %>%
    select(quarter,contract_date, suburb_name, purchase_price,property_type) 

  # Build a function
  roll_median <- function(suburbs,property,end_date,start_date) {
    median <- method1 %>%
      filter(suburb_name == suburbs & 
               property_type == property &
               contract_date <= end_date & contract_date > start_date) %>%
      summarise(median = median(purchase_price,na.rm = TRUE))
    as.integer(median)
  }
  
  # Try on subset 
  
  df1 <- df[200000:200050,]
  
  system.time(df_roll_median <- df1 %>%
                mutate(median = pmap(df1,roll_median)))
  
  system.time(df_roll_groups <- df1 %>%
                mutate(median = pmap(df1,roll_subset)))
  
  pmap(df1,roll_median)
  
  ##### Try with Furrr ######
  
  future::plan(multiprocess)
  
  system.time(map_dbl(1:4, function(x){
    Sys.sleep(1)
    x^2
  }))
  
  system.time(future_map_dbl(1:4, function(x){
    Sys.sleep(1)
    x^2
  }))
  
  system.time(df_roll_median_future <- df1 %>%
                mutate(median = future_pmap(df1,roll_median)))
  
  future_pmap(df1,roll_median,.progress = TRUE)
  
  #### Try with DATA.TABLE ####
  
  dt_roll_median <- function(dates,suburbs,property) {
    filter <- med_data_dt[suburb_name == suburbs &
                         property_type == property &
                         contract_date <= dates &
                         contract_date > (dates-months(6))]
    median(filter[,purchase_price])
  }
  
  dt_roll_subset <- function(suburbs,property,end_date,start_date) {
    med_data_dt[suburb_name == suburbs &
                            property_type == property &
                            contract_date <= end_date &
                            contract_date > start_date]
  }
  
  med_data_dt <- setDT(med_data)
  
  df_dt <- setDT(df)
  
  df_dt_1 <- df_dt[200000:200050]
  
  system.time(check <- pmap(df_dt_1,dt_roll_median))
  
  system.time(check <- pmap(df_dt_1,dt_roll_subset))

# [12] ---- Dwelling density ---- 
  
  # This would definitely be more accurate to get from Census data
  dwellings_per_suburb <- matched_land_value %>%
    group_by(suburb_code,suburb_name) %>%
    summarise(total_dwellings = n()) 
  
# [13] ---- Relative Turnover per year ---- 
  
  sales_per_year <- cleaned_sales %>%
    group_by(year,suburb_name) %>%
    summarise(sales = n())
  
# TRASH
  
  # This takes quite a long time - roughly 2hours for the land_median alone which is by far the smallest
  # Could be worth trying to use dtplyr or Multiplyr
  
  saveRDS(land_median,"data/created/land_median.rds")
  houses <- df_big %>%
    filter(property_type == "House")
  
  apartments <- df_big %>% 
    filter(property_type == "Apartment")
  
  land <- df_big %>%
    filter(property_type == "Land")
  
  get_median <- function (df){
    df %>%
      group_by(target_date_window,suburb_name,property_type) %>%
      summarise(median = median(purchase_price),
                number_of_sales = n())
  } 
  
  house_median <- get_median(houses) 
  apartment_median <- get_median(apartments)
  land_median <- get_median(land)
  
