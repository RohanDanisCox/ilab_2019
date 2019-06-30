year_1990 <- read_delim("ARCHIVE_SALES_1990.DAT",
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
                   skip = 1) 


test <- year_1990 %>%
  filter(post_code == 2045)

install.packages('tidyverse')
library(tidyverse)
