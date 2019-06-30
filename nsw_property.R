# NSW Property

# [0] ---- Load packages ----

library(readr)
library(tidyverse)
library(rdrop2)
library(feather)

install.packages("httpuv")
library(httpuv)

# [1] ---- Load data ----

drop_auth()
drop_dir()

drop_download("ilab2019/2017.feather")

data_2017 <- read_feather("2017.feather")



data_path <- "Data/1990-2000"
files <- dir(path = data_path,pattern = "*.DAT")

year_1990_2000 <- files %>%
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
  reduce(bind_rows)

write_feather(year_1990_2000,"year_1990_2000.feather")



data_path <- "Data/2017"
files <- dir(path = data_path,pattern = "*.DAT")

p2017 <- files %>%
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
                   skip = 1)) 


p2017_clean <- bind_rows(p2017) %>%
  filter(record_type == "B")

write_feather(p2017_clean,"2017.feather")


p2017 <- read_delim("Data/2017/*.DAT", delim = ";", skip = 1)
?read_delim

?dir

p2017 <- read_delim("Data/2017/001_SALES_DATA_NNME_01052017.DAT",
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
                    skip = 1) %>%
  filter(record_type == "B")


