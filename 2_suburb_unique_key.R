# Build out a unique key of suburbs. 
# This is going to form the base nucleus of suburb data which all the remaining data points will be joined to

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

# [1] ---- Import mesh block data ----

  mesh_block_category <- read_csv("data/meshblocks/mesh_block_category.csv")
  mesh_block_suburb <- read_csv("data/meshblocks/mesh_block_suburb.csv")

# [2] ---- Aggregate mesh block data into suburb data ----

  mesh_block_raw <- mesh_block_category %>%
    left_join(mesh_block_suburb, by = c("MB_CODE_2016", "STATE_CODE_2016", "STATE_NAME_2016", "AREA_ALBERS_SQKM"))
  
  meshblocks <- mesh_block_raw %>%
    select(mb_code = MB_CODE_2016,
           mb_category = MB_CATEGORY_NAME_2016,
           area_sqkm = AREA_ALBERS_SQKM,
           suburb_code = SSC_CODE_2016,
           suburb_name = SSC_NAME_2016,
           sa1_maincode = SA1_MAINCODE_2016,
           sa1_7code = SA1_7DIGITCODE_2016,
           sa2_maincode = SA2_MAINCODE_2016,
           sa2_5code = SA2_5DIGITCODE_2016,
           sa2_name = SA2_NAME_2016,
           sa3_code = SA3_CODE_2016,
           sa3_name = SA3_NAME_2016,
           sa4_code = SA4_CODE_2016,
           sa4_name = SA4_NAME_2016,
           gccsa_code = GCCSA_CODE_2016,
           gccsa_name = GCCSA_NAME_2016,
           state_code = STATE_CODE_2016,
           state_name = STATE_NAME_2016)
           
  suburb_base <- meshblocks %>%
    group_by(suburb_code) %>%
    mutate(suburb_area_sqkm = sum(area_sqkm),
           total_meshblocks = n()) %>% 
    ungroup() %>%
    select(4:20) %>%
    distinct(suburb_code,.keep_all = TRUE)
  
  add_category_info <- meshblocks %>%
    group_by(suburb_code,mb_category) %>%
    summarise(meshblocks = n(),
              area_sqkm = sum(area_sqkm)) %>%
    gather(variable, value, -(suburb_code:mb_category)) %>%
    unite(temp, mb_category, variable) %>%
    spread(temp, value)
  
  suburbs <- suburb_base %>%
    left_join(add_category_info, by = "suburb_code")

# [3] ---- Write out the suburb data for use ----
  
  write_rds(suburbs,path = "data/created/suburbs.rds")

