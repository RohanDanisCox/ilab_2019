# File to import all data off of the NSW Valuer General's website into discrete files ready for use.

# [0] ---- Load packages ----

  library(readr)
  library(tidyverse)
  library(feather)

# [1] ---- Define functions ----

  # Old method 

  to_feather_old <- function(path,output) {
    data_path <- path
    files <- dir(path = data_path,pattern = "*.DAT")
    files %>%
      map(~ read_delim(file.path(data_path, .),
                       quote = "", #this was a pain to find out - basically it won't look for a quote marker (default is " or ')
                       delim = ";",
                       col_names = c("record_type","district_code","source","valuation_number",
                                     "property_id","unit_number","house_number","street_name",
                                     "locality","post_code","contract_date","purchase_price",
                                     "land_description","area","area_type","dimensions",
                                     "comp_code","zone_code","vendor_name","purchaser_name","unknown_a",
                                     "unknown_b"),
                       col_types = cols(
                         record_type = col_character(),
                         district_code = col_double(),
                         source = col_character(),
                         valuation_number = col_character(),
                         property_id = col_double(),
                         unit_number = col_character(),
                         house_number = col_character(),
                         street_name = col_character(),
                         locality = col_character(),
                         post_code = col_double(),
                         contract_date = col_date("%d/%m/%Y"),
                         purchase_price = col_double(),
                         land_description = col_character(),
                         area = col_double(),
                         area_type = col_character(),
                         dimensions = col_character(),
                         comp_code = col_character(),
                         zone_code = col_character(),
                         vendor_name = col_character(),
                         purchaser_name = col_character(),
                         unknown_a = col_character(),
                         unknown_b = col_character()),
                       skip = 1)) %>%
      reduce(bind_rows) %>%
      write_feather(output)
  }
  
  # New method
  
  to_feather_new <- function(year) {
    data_path <- paste0("Data/new/",year)
    files <- dir(path = data_path,pattern = "*.DAT")
    files %>%
      map(~ read_delim(file.path(data_path, .),
                       delim = ";",
                       col_names = c("record_type","district_code","property_id","sale_counter",
                                     "download_date","property_name","unit_number","house_number",
                                     "street_name","locality","post_code","area","area_type","contract_date",
                                     "settlement_date","purchase_price","zoning","nature_of_property",
                                     "primary_purpose","strata_lot_number","comp_code","sale_code",
                                     "interest_of_sale","dealing_number","unknown_a"),
                       col_types = cols(
                         record_type = col_character(),
                         district_code = col_double(),
                         property_id = col_character(),
                         sale_counter = col_double(),
                         download_date = col_double(), # may need to change this
                         property_name = col_double(),
                         unit_number = col_character(),
                         house_number = col_character(),
                         street_name = col_character(),
                         locality = col_character(),
                         post_code = col_double(),
                         area = col_double(),
                         area_type = col_character(),
                         contract_date = col_date("%Y%m%d"),
                         settlement_date = col_date("%Y%m%d"),
                         purchase_price = col_double(),
                         zoning = col_character(),
                         nature_of_property = col_character(),
                         primary_purpose = col_character(),
                         strata_lot_number = col_character(),
                         comp_code = col_character(),
                         sale_code = col_character(),
                         interest_of_sale = col_character(),
                         dealing_number = col_character(),
                         unknown_a = col_character()),
                       skip = 1)) %>%
      bind_rows() %>%
      filter(record_type == "B") %>%
      write_feather(paste0("new_data_",year,".feather"))
    }
  to_feather_2019 <- function(month) {
    data_path <- paste0("Data/new/2019/",month)
    files <- dir(path = data_path,pattern = "*.DAT")
    files %>%
      map(~ read_delim(file.path(data_path, .),
                       delim = ";",
                       col_names = c("record_type","district_code","property_id","sale_counter",
                                     "download_date","property_name","unit_number","house_number",
                                     "street_name","locality","post_code","area","area_type","contract_date",
                                     "settlement_date","purchase_price","zoning","nature_of_property",
                                     "primary_purpose","strata_lot_number","comp_code","sale_code",
                                     "interest_of_sale","dealing_number","unknown_a"),
                       col_types = cols(
                         record_type = col_character(),
                         district_code = col_double(),
                         property_id = col_character(),
                         sale_counter = col_double(),
                         download_date = col_double(), # may need to change this
                         property_name = col_double(),
                         unit_number = col_character(),
                         house_number = col_character(),
                         street_name = col_character(),
                         locality = col_character(),
                         post_code = col_double(),
                         area = col_double(),
                         area_type = col_character(),
                         contract_date = col_date("%Y%m%d"),
                         settlement_date = col_date("%Y%m%d"),
                         purchase_price = col_double(),
                         zoning = col_character(),
                         nature_of_property = col_character(),
                         primary_purpose = col_character(),
                         strata_lot_number = col_character(),
                         comp_code = col_character(),
                         sale_code = col_character(),
                         interest_of_sale = col_character(),
                         dealing_number = col_character(),
                         unknown_a = col_character()),
                       skip = 1)) %>%
      bind_rows() %>%
      filter(record_type == "B") %>%
      write_feather(paste0("new_data_2019_",month,".feather"))
  }
  
  # Land value data
  
  to_feather_land_value <- function(year) {
    data_path <- paste0("Data/land_value/",year)
    year_1 <- as.numeric(year)-1
    year_2 <- year_1-1
    year_3 <- year_2-1
    year_4 <- year_3-1
    year_5 <- year_4-1
    files <- dir(path = data_path,pattern = "*.csv")
    files %>%
      map(~ read_csv(file.path(data_path, .),
                     col_names = c("district_code","district_name","property_id","property_type",
                                   "property_name","unit_number","house_number","street_name",
                                   "locality","post_code","property_description","zone_code",
                                   "area","area_type",
                                   paste0("base_date_",year_1),paste0("land_value_",year_1),
                                   paste0("authority_",year_1),paste0("basis_",year_1),
                                   paste0("base_date_",year_2),paste0("land_value_",year_2),
                                   paste0("authority_",year_2),paste0("basis_",year_2),
                                   paste0("base_date_",year_3),paste0("land_value_",year_3),
                                   paste0("authority_",year_3),paste0("basis_",year_3),
                                   paste0("base_date_",year_4),paste0("land_value_",year_4),
                                   paste0("authority_",year_4),paste0("basis_",year_4),
                                   paste0("base_date_",year_5),paste0("land_value_",year_5),
                                   paste0("authority_",year_5),paste0("basis_",year_5),"unknown"),
                     col_types = "dcdccccccdccdccdcccdcccdcccdcccdccl",
                     skip = 1)) %>%
      bind_rows() %>%
      select(-unknown)
  }
  
# [2] ---- Import Data ----
  
  # Old method 
  
  to_feather_old("Data/old","old_data.feather")
  
  # New method
  
  years <- as.character(2001:2018)
  sapply(years,to_feather_new)
  
  months <- c("Jan","Feb","Mar","Apr","May","Jun")
  sapply(months,to_feather_2019)
  
  # Land value data

  lv_2019 <- to_feather_land_value("2019")
  lv_2018 <- to_feather_land_value("2018")
  lv_2017 <- to_feather_land_value("2017")
  
# [3] ---- Simplify Land Value Data ----
  
  land_value <- lv_2019 %>%
    full_join(lv_2018) %>%
    full_join(lv_2017)
  
  write_feather(land_value,"land_value.feather")

  