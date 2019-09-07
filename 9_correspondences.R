# Establish the best mapping of correspondence between data sources

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

# [1] ---- Load correspondence files files ----

  mapping_2018_2016_raw <- read_xlsx("data/suburb_comparison/cg_loc_2018_ssc_2016.xlsx", sheet = 4, skip = 7,
                                  col_names = c("suburb_name_2018",
                                                "state_2018",
                                                "post_code_2018",
                                                "suburb_code_2016",
                                                "suburb_name_2016",
                                                "ratio",
                                                "percentage"),
                                  col_types = c("text","text","numeric","text","text","numeric","numeric"))

  mapping_2011_2016_raw <- read_xls("data/suburb_comparison/cg_ssc_2011_ssc_2016.xls", sheet = 4, skip = 7,
                                  col_names = c("suburb_code_2011",
                                                "suburb_name_2011",
                                                "suburb_code_2016",
                                                "suburb_name_2016",
                                                "ratio",
                                                "percentage"),
                                  col_types = c("text","text","text","text","numeric","numeric"))

  mapping_2006_2016_raw <- read_xlsx("data/suburb_comparison/cg_ssc_2006_ssc_2016.xlsx", sheet = 4, skip = 7,
                                   col_names = c("suburb_code_2006",
                                                 "suburb_name_2006",
                                                 "suburb_code_2016",
                                                 "suburb_name_2016",
                                                 "ratio",
                                                 "percentage"),
                                   col_types = c("text","text","text","text","numeric","numeric"))
  
# [2] ---- Identify most suitable conversion ----

  mapping_2018_2016 <- mapping_2018_2016_raw %>%
    group_by(suburb_name_2018,post_code_2018) %>%
    mutate(rank = rank(desc(ratio))) %>%
    filter(rank == 1) %>%
    ungroup() %>%
    mutate(suburb_code_2016 = as.numeric(suburb_code_2016)) %>%
    filter(suburb_code_2016 <20000)
  
  mapping_2011_2016 <- mapping_2011_2016_raw %>%
    group_by(suburb_code_2011,suburb_name_2011) %>%
    mutate(rank = rank(desc(ratio))) %>%
    filter(rank == 1) %>%
    ungroup() %>%
    mutate(suburb_code_2016 = as.numeric(suburb_code_2016)) %>%
    filter(suburb_code_2016 <20000)
  
  mapping_2006_2016 <- mapping_2006_2016_raw %>%
    group_by(suburb_code_2006,suburb_name_2006) %>%
    mutate(rank = rank(desc(ratio))) %>%
    filter(rank == 1) %>%
    ungroup() %>%
    mutate(suburb_code_2016 = as.numeric(suburb_code_2016)) %>%
    filter(suburb_code_2016 <20000)

# [3] ---- Load suburb base data of 2016 census ----

  suburb_2016 <- readRDS("data/created/suburbs.rds")

# [4] ---- Combine into Correspondence File ----

  correspondence <- suburb_2016 %>%
    left_join(mapping_2006_2016, by = c("suburb_code" = "suburb_code_2016", "suburb_name" = "suburb_name_2016")) %>%
    left_join(mapping_2011_2016, by = c("suburb_code" = "suburb_code_2016", "suburb_name" = "suburb_name_2016")) %>%
    left_join(mapping_2018_2016, by = c("suburb_code" = "suburb_code_2016", "suburb_name" = "suburb_name_2016")) %>%
    select(1,2,46,47,51,52,56,58,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,22,24,26,28,30,32,34,36,38,40,42,44) %>%
    arrange(suburb_code)
  
# [5] ---- Save off correspondence file ----

  write_rds(correspondence,"data/created/correspondence.rds")
