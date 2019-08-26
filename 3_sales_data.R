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
  library(data.table)
  library(furrr)
  library(rbenchmark)
  library(parallel)

no_of_cores = detectCores()

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
  
# [7] ---- Can start from here now ----  
  
  matched_land_value <- readRDS("data/land_value/matched_land_value.rds")
  matched_sales <- readRDS("data/sales/matched_sales.rds")
  
  matched_sales_1 <- matched_sales %>%
    group_by(suburb_code,suburb_name) %>%
    summarise(n = n(),
              average = mean(purchase_price,na.rm = TRUE),
              median = median(purchase_price,na.rm = TRUE))
  
  matched_land_value_1 <- matched_land_value %>%
    group_by(suburb_code,suburb_name) %>%
    summarise(n = n(),
              average_2018 = mean(land_value_2018, na.rm = TRUE),
              median = median(land_value_2018,na.rm = TRUE))
  
# [8] ---- What are the useful variables? ---- 
  
  matched_sales_bio <- matched_sales %>% map_df(~(data.frame(n_distinct = n_distinct(.x),
                                                             class = class(.x))),
                                                .id = "variable")
  
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
                                                     TRUE ~ property_type)))
  sales_1 <- sales %>%
    filter(purchase_price > 0) %>%
    filter(contract_date > as.Date("1990/01/01") & contract_date < as.Date("2019/07/01")) %>% 
    filter(!(area == "(Missing)" & zone_code == "(Missing)" & zoning == "(Missing)" & is.na(primary_purpose))) %>%
    distinct() 
  
  sales_2 <- sales_1 %>%
    mutate(year = floor_date(contract_date,"year"),
           quarter = floor_date(contract_date,"quarter"),
           month = floor_date(contract_date,"month"))
  
# [9] ---- How to calculate a moving median ----  
  
  ## Another way is to create a massive dataset and then subset - this works and is probably the most effecient..
  
  df_1 <- med_data %>%
    mutate(current_quarter = quarter)
  
  df_2 <- med_data %>%
    mutate(current_quarter = quarter - months(3))
  
  df_3 <- med_data %>%
    mutate(current_quarter = quarter - months(6))
  
  df_big <- bind_rows(df_1,df_2,df_3)
  
  
  small_df <- df_big %>%
    filter(str_detect(suburb_name, "^A"))
  
  system.time(check <- small_df %>%
    group_by(current_quarter,suburb_name,property_type) %>%
    summarise(median = median(purchase_price),
              n = n()))
  
  check2 <- check %>%
    filter(suburb_name == "Abbotsford (NSW)") %>%
    filter(property_type == "House")
  
  ggplot(check, aes(x = current_quarter, y = median)) +
    geom_point()
  
  check3 <- med_data %>%
    filter(suburb_name == "Abbotsford (NSW)") %>%
    filter(property_type == "House") %>%
    filter(contract_date <= as.Date("2019-04-01") & 
             contract_date > as.Date("2018-07-01")) %>%
    summarise(median = median(purchase_price))
  
  
  # Get the distinct calculation dataframe
  df <- sales_2 %>%
    distinct(quarter,suburb_name,property_type) %>%
    select(suburbs = suburb_name, property = property_type, end_date = quarter) %>%
    mutate(start_date = end_date - months(6))

  #subset the data 
  med_data <- sales_2 %>%
    select(quarter,contract_date, suburb_name, purchase_price,property_type) 
  
  # split the data?? 
  split_df <- df %>%
    split()
  
  # Build a function
  roll_median <- function(suburbs,property,end_date,start_date) {
    median <- med_data %>%
      filter(suburb_name == suburbs & 
               property_type == property &
               contract_date <= end_date & contract_date > start_date) %>%
      summarise(median = median(purchase_price,na.rm = TRUE))
    as.integer(median)
  }
  
  roll_subset <- function(suburbs,property,end_date,start_date) {
    df <- med_data %>%
      filter(suburb_name == suburbs & 
               property_type == property &
               contract_date <= end_date & contract_date > start_date)
  }
  

  # subset 
  
  df1 <- df[200000:200050,]
  
  system.time(df_roll_median <- df1 %>%
                mutate(median = pmap(df1,roll_median)))
  
  system.time(df_roll_groups <- df1 %>%
                mutate(median = pmap(df1,roll_subset)))
  
  pmap(df1,roll_median)
  
  452699/100*30
  135809/60/60/24
  
  ?split
  
  
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
  
  # DATA.TABLE attempt
  
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

  # Can then easily apply the map function with mutate e.g.
  df_roll_median <- df %>%
    mutate(median = pmap(df,roll_median))
  
  quarters <- sales_2 %>%
    distinct(quarter) %>%
    filter(quarter > as.Date("2015/01/01")) # this line is just for testing
  
  suburbs <- sales_2 %>%
    distinct(suburb_name) %>%
    filter(suburb_name < "B") # this line is just for testing
  
  property_type <- sales_2 %>%
    distinct(property_type)
  
  params <- tibble::tribble(
    ~ date, ~ suburb, ~ property_type,
    as.Date("2018-10-01"), "Haberfield", "House",
    as.Date("2019-01-01"), "Armidale", "House",
    as.Date("2019-04-01"), "Abercrombie",  "House",
  )
    
  roll_median <- function(quarter,suburb_name,property_type) {
    median <- sales_2 %>%
      select(quarter,contract_date, suburb_name, purchase_price,property_type) %>% # will need to stick property type back in at some stage
      filter(suburb_name == suburb_name) %>%
      filter(property_type == property_type) %>%
      filter(contract_date <= quarter & contract_date > quarter-months(6)) %>%
      summarise(median = median(purchase_price,na.rm = TRUE))
    as.double(median)
  }

  check <- params %>%
    mutate(median = pmap_dbl(params,roll_median))

  
# [8] ---- Dwelling density ---- 
  
  # This would definitely be more accurate to get from Census data
  dwellings_per_suburb <- matched_land_value %>%
    group_by(suburb_code,suburb_name) %>%
    summarise(total_dwellings = n()) 
  
# [8] ---- Relative Turnover per year ---- 
  
  # probably want to create some grouping variables year, quarter, month etc
  matched_sales_2 <- matched_sales %>%
    mutate(year = floor_date(contract_date,"year"),
           quarter = floor_date(contract_date,"quarter"),
           month = floor_date(contract_date,"month"))
  
  # This would definitely be more accurate to get from Census data
  sales_per_year <- matched_sales_2 %>%
    group_by(year) %>%
    summarise(sales = n()) %>%
    filter(year >= 1989-01-01)
  
  # visualise distribution - something is definitely wrong in hurstville
  ggplot(sales_per_year, aes(x = year, y=sales)) +
    geom_line()

  summary(matched_sales$contract_date)
  summary(matched_sales$settlement_date)
    
  # Investigate hurstville 
  
  hurstville <- matched_sales_2 %>%
    filter(suburb_name == "Hurstville")
    
  hurstville_1 <- hurstville
    group_by(street_name, year) %>%
    summarise(n = n())
  
  hurstville_2 <- hurstville %>%
    filter(street_name %in% c("FOREST RD","PEARL ST")) %>%
    group_by(house_number,street_name) %>%
    summarise(n = n())
  
  hurstville_3 <- hurstville %>%
    filter(street_name %in% c("FOREST RD","PEARL ST")) %>%
    filter(house_number %in% c("1 B","458","460"))
  
  hurstville_4 <- hurstville_3 %>%
    group_by(house_number,street_name,strata_lot_number) %>%
    summarise(n = n())
  
  hurstville_5 <- hurstville_3 %>%
    group+
    
  hurstville_land <- matched_land_value %>%
    filter(suburb_name == "Hurstville") %>%
    filter(street_name %in% c("FOREST RD","PEARL ST")) 
  
  hurstville_delete <- hurstville %>%
    select(-sale_counter) %>%
    distinct()
    
  overall_ignore_sales_counter <- matched_sales %>%
    select(-sale_counter) %>%
    distinct()
  
  matched_sales <- overall_ignore_sales_counter
  
  data %>% 
    group_by(month=floor_date(date, "month")) %>%
    summarize(summary_variable=sum(value))
  
  
# TRASH
  
  
  
  sales_2 <- sales_1 %>%
    filter(is.na(area)) %>%
    filter(property_type == "House") %>%
    filter(zone_code == "(Missing)") %>%
    count(year)
  
  
  
  table(sales$property_type,sales$zone_code)
  table(sales$primary_purpose)
  
  summary <- sales %>%
    group_by(primary_purpose) %>%
    summarise(n = n()) %>%
    filter(n >50)
  
  
  sales %>%
    filter(primary_purpose == "APARTMENT") %>%
    count(property_type)
  summarise(property_type)
  
  sales %>% 
    filter(str_detect(primary_purpose,"RURAL")) %>%
    count(primary_purpose) %>%
    arrange(desc(n))
  
  check <- sales %>%
    filter(area > 1000000) %>%
    filter(purchase_price < 100000)
  
  
  
  table(sales$property_type)
  
  test <- sales %>%
    filter(unit_flag == "Apartment" & property_type == "House")
  
  test2 <- sales %>%
    filter(unit_flag == "House" & property_type == "Apartment")
  summary(sales)
  
  select(distict_code, valuation_number,property_id,unit_number,house_number,street_name,locality,
         post_code,area,area_type,
         
         
         
         # Nature of property
         
         summary(matched_land_value)
         glimpse(matched_sales)
         summary(matched_sales)
         summary(sales)
         
         table(matched_sales$zoning)
        
  