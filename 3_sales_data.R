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
  
  old_sales_data1 <- old_sales_data %>%
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
           contract_date,purchase_price,settlement_date,zoning,zone_code,nature_of_property,primary_purpose,strata_lot_number,
           comp_code,vendor_name,purchaser_name,sale_code,interest_of_sale,dealing_number)
  
  new_sales_data1 <- new_sales_data %>%
    mutate(source = NA_character_,
           valuation_number = NA_character_,
           land_description = NA_character_,
           dimensions = NA_character_,
           zone_code = NA_character_,
           vendor_name = NA_character_,
           purchaser_name = NA_character_,
           property_id = as.numeric(property_id)) %>%
    select(record_type,district_code,download_date,source,valuation_number,property_id,sale_counter,property_name,
         property_id,unit_number,house_number,street_name,locality,post_code,area,area_type,dimensions,land_description,
         contract_date,purchase_price,settlement_date,zoning,zone_code,nature_of_property,primary_purpose,strata_lot_number,
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

  