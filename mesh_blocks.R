# Working with Mesh Blocks - this information could be useful for green spaces

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

# [1] ---- Import mesh_block data ----

  mesh_block_category <- read_csv("Data/ABS/mesh_block_category.csv")
  mesh_block_suburb <- read_csv("Data/ABS/mesh_block_suburb.csv")

  
  table(mesh_block_category$GCCSA_NAME_2016)
  
  table(mesh_block_category$MB_CATEGORY_NAME_2016)
# [2] ---- Calculate Green Score (Parkland & Include Primary Industries )

  mesh_block <- mesh_block_category %>%
    left_join(mesh_block_suburb, by = c("MB_CODE_2016", "STATE_CODE_2016", "STATE_NAME_2016", "AREA_ALBERS_SQKM")) #%>%
    #select(MB_CODE_2016,MB_CATEGORY_NAME_2016,SSC_NAME_2016,AREA_ALBERS_SQKM, SA2_NAME_2016,SA3_NAME_2016)
  
  sa2_mesh_block <- mesh_block %>%
    group_by(SA2_NAME_2016) %>%
    mutate(total_area_sa2 = sum(AREA_ALBERS_SQKM)) 
  
  sa2_mesh_block_1 <- sa2_mesh_block %>%
    group_by(SA2_NAME_2016, MB_CATEGORY_NAME_2016, total_area_sa2) %>%
    summarise(area_sa2 = sum(AREA_ALBERS_SQKM))
  
  sa2_mesh_block_2 <- sa2_mesh_block_1 %>%
    filter(MB_CATEGORY_NAME_2016 == "Parkland" | MB_CATEGORY_NAME_2016 == "Primary Production") %>%
    group_by(SA2_NAME_2016) %>%
    mutate(green_area_sa2 = sum(area_sa2)) %>%
    mutate(green_proportion_sa2 = green_area_sa2/total_area_sa2) %>%
    select(SA2_NAME_2016,green_proportion_sa2) %>%
    distinct()
  
  suburb_mesh_block <- mesh_block %>%
    group_by(SSC_NAME_2016) %>%
    mutate(total_area_suburb = sum(AREA_ALBERS_SQKM)) 
  
  suburb_mesh_block_1 <- suburb_mesh_block %>%
    group_by(SSC_NAME_2016, MB_CATEGORY_NAME_2016, total_area_suburb) %>%
    summarise(area_suburb = sum(AREA_ALBERS_SQKM))
  
  suburb_mesh_block_2 <- suburb_mesh_block_1 %>%
    filter(MB_CATEGORY_NAME_2016 == "Parkland" | MB_CATEGORY_NAME_2016 == "Primary Production") %>%
    group_by(SSC_NAME_2016) %>%
    mutate(green_area_suburb = sum(area_suburb)) %>%
    mutate(green_proportion_suburb = green_area_suburb/total_area_suburb) %>%
    select(SSC_NAME_2016,green_proportion_suburb) %>%
    distinct()
  
  mesh_block_1 <- mesh_block %>%
    left_join(suburb_mesh_block_2) %>%
    left_join(sa2_mesh_block_2) %>%
    mutate(green_proportion_suburb = case_when(is.na(green_proportion_suburb) ~ 0,
                                               TRUE ~ green_proportion_suburb),
           green_proportion_sa2 = case_when(is.na(green_proportion_sa2) ~ 0,
                                            TRUE ~ green_proportion_sa2)) %>%
    mutate(green_score = green_proportion_suburb * 0.5 + green_proportion_sa2 * 0.5) %>%
    mutate(green_score_decile = ntile(green_score,10))

# [3] ---- Investigate scores
  
  mesh_block_check <- mesh_block_1 %>%
    distinct(SSC_NAME_2016, .keep_all = TRUE) %>%
    filter(GCCSA_NAME_2016 == "Greater Sydney")
    
