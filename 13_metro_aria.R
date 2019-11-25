# Adding in metro aria

# [0] ---- Load packages ----

  library(readr)
  library(readxl)
  library(tidyverse)
  library(rdrop2)
  library(feather) # I don't think this will be used
  library(httpuv)
  library(sp)
  library(rgdal)
  library(rgeos)
  library(maptools)
  library(ggmap) - # probably won't use this
  library(broom) # broom might already be in tidyverse
  library(sf)
  library(RColorBrewer)
  library(leaflet)
  library(leafpop)

# [1] ---- Load correspondence file data ----
  
  mapping_2011_ss1_2016_raw <- read_xlsx("data/suburb_comparison/cg_sa1_2011_ssc_2016.xlsx", sheet = 4, skip = 7,
                                     col_names = c("sa1_maincode",
                                                   "sa1_7digit",
                                                   "suburb_code_2016",
                                                   "suburb_name_2016",
                                                   "ratio",
                                                   "percentage"),
                                     col_types = c("numeric","numeric","numeric","text","numeric","numeric")) %>%
    filter(suburb_code_2016 < 20000)
  
  metro_aria_raw <- read_csv("data/metro_aria/metro aria (sl1).csv", skip = 1,
                             col_names = c("shopping_category","overall_category","public_transport",
                                           "ucl_code","education","ucl_name","overall","shopping","education_category",
                                           "id","health_category","health","bounded_by","public_transport_category",
                                           "financial_postal_category","financial_postal","sa1_maincode")) %>%
    select(10,17,13,7,2,5,9,12,11,8,1,3,14,16,15) %>%
    mutate(overall = case_when(overall == 1 ~ 5,
                               overall == 2 ~ 4,
                               overall == 3 ~ 3,
                               overall == 4 ~ 2,
                               overall == 5 ~ 1),
           education = case_when(education == 1 ~ 5,
                               education == 2 ~ 4,
                               education == 3 ~ 3,
                               education == 4 ~ 2,
                               education == 5 ~ 1),
           health = case_when(health == 1 ~ 5,
                               health == 2 ~ 4,
                               health == 3 ~ 3,
                               health == 4 ~ 2,
                               health == 5 ~ 1),
           shopping = case_when(shopping == 1 ~ 5,
                               shopping == 2 ~ 4,
                               shopping == 3 ~ 3,
                               shopping == 4 ~ 2,
                               shopping == 5 ~ 1),
           public_transport = case_when(public_transport == 1 ~ 5,
                               public_transport == 2 ~ 4,
                               public_transport == 3 ~ 3,
                               public_transport == 4 ~ 2,
                               public_transport == 5 ~ 1),
           financial_postal = case_when(financial_postal == 1 ~ 5,
                               financial_postal == 2 ~ 4,
                               financial_postal == 3 ~ 3,
                               financial_postal == 4 ~ 2,
                               financial_postal == 5 ~ 1))
  
# [2] ---- Identify most suitable conversion ----

  mapping_2011_ss1_2016 <- mapping_2011_ss1_2016_raw %>%
    group_by(sa1_maincode) %>%
    mutate(rank = rank(desc(ratio))) %>%
    filter(rank == 1) %>%
    ungroup() 

# [3] ---- Match the data ----
  
  metro_aria <- metro_aria_raw %>%
    left_join(mapping_2011_ss1_2016, by = "sa1_maincode") %>%
    filter(!is.na(suburb_code_2016)) %>%
    rename(suburb_code = suburb_code_2016, suburb_name = suburb_name_2016) %>%
    group_by(suburb_code, suburb_name) %>%
    summarise(aria_overall = mean(overall),
              aria_education = mean(education),
              aria_health = mean(health),
              aria_shopping = mean(shopping),
              aria_public_transport = mean(public_transport),
              aria_financial_postal = mean(financial_postal))
      
# [4] ---- Save off metro aria file ----
  
  write_rds(metro_aria,"data/created/metro_aria.rds")

