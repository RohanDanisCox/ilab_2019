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
                         district_code = col_integer(),
                         source = col_character(),
                         valuation_number = col_character(),
                         property_id = col_integer(),
                         unit_number = col_character(),
                         house_number = col_character(),
                         street_name = col_character(),
                         locality = col_character(),
                         post_code = col_integer(),
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

  to_feather_old("Data/old","old_data.feather")
  
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
                         district_code = col_integer(),
                         property_id = col_character(),
                         sale_counter = col_integer(),
                         download_date = col_integer(), # may need to change this
                         property_name = col_integer(),
                         unit_number = col_character(),
                         house_number = col_character(),
                         street_name = col_character(),
                         locality = col_character(),
                         post_code = col_integer(),
                         area = col_double(),
                         area_type = col_character(),
                         contract_date = col_date("%Y%m%d"),
                         settlement_date = col_date("%Y%m%d"),
                         purchase_price = col_integer(),
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
                         district_code = col_integer(),
                         property_id = col_character(),
                         sale_counter = col_integer(),
                         download_date = col_integer(), # may need to change this
                         property_name = col_integer(),
                         unit_number = col_character(),
                         house_number = col_character(),
                         street_name = col_character(),
                         locality = col_character(),
                         post_code = col_integer(),
                         area = col_double(),
                         area_type = col_character(),
                         contract_date = col_date("%Y%m%d"),
                         settlement_date = col_date("%Y%m%d"),
                         purchase_price = col_integer(),
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
  
  years <- as.character(2001:2018)
  
  sapply(years,to_feather_new)
  
  months <- c("Jan","Feb","Mar","Apr","May","Jun")

  sapply(months,to_feather_2019)