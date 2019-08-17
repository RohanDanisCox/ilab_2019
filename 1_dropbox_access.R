# Access Data from Dropbox

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

# [1] ---- Set Up Dropbox ----

  # These have been used to create drop_tokens on home computer and transferred to GCP.
  #token <- drop_auth()
  #saveRDS(token, "DropToken.RDS")
  
  drop_auth(new_user = FALSE, rdstoken = "DropToken.RDS")
  directories <- drop_dir("/ilab2019")
  
# [2] ---- Define function to download data from Dropbox ----

  # Define function to download files
  my_download <- function(path,local_path) {
    drop_download(path = path,local_path = local_path,overwrite = TRUE)
  }
  
# [3] ---- Define the paths ----
  
  # Property sales data
  sales_path <- drop_dir(path = "ilab2019/sale_value") %>%
    select(path_lower) %>%
    as_vector()
  
  # Land value data
  land_value_path <- drop_dir(path = "ilab2019/land_value") %>%
    select(path_lower) %>%
    as_vector()
  
  # ABS Mesh Blocks from 2016
  mesh_block_path <- drop_dir(path = "ilab2019/mesh_blocks") %>%
    select(path_lower) %>%
    as_vector()
  
  # Government School Data
  gov_schools_path <- drop_dir(path = "ilab2019/education/gov_schools") %>%
    select(path_lower) %>%
    as_vector()
  
  gov_schools2_path <- drop_dir(path = "ilab2019/education") %>%
    select(path_lower) %>%
    filter(str_detect(path_lower,".csv")) %>%
    as_vector()
  
  # Non-Government School Data
  nongov_schools_path <- drop_dir(path = "ilab2019/education/nongov_schools") %>%
    select(path_lower) %>%
    as_vector()
  
  # School catchment data
  school_catch_path <- drop_dir(path = "ilab2019/education/school_catchment") %>%
    select(path_lower) %>% 
    as_vector()
  
  # PSMA NSW Shapefile 2019
  shapefile_2019_path <- drop_dir(path = "ilab2019/suburb_shapefiles/suburb_shapefile_2019") %>%
    select(path_lower) %>%
    as_vector()
  
  # PSMA NSW Shapefile 2018
  shapefile_2018_path <- drop_dir(path = "ilab2019/suburb_shapefiles/suburb_shapefile_2018") %>%
    select(path_lower) %>%
    as_vector()
  
  # ABS NSW Shapefile 2016
  shapefile_2016_path <- drop_dir(path = "ilab2019/suburb_shapefiles/suburb_shapefile_2016") %>%
    select(path_lower) %>%
    as_vector()
  
  # AUS Remoteness Shapefile
  remoteness_area_path <- drop_dir(path = "ilab2019/remoteness") %>%
    select(path_lower) %>%
    as_vector()
  
  # Crime data
  crime_path <- drop_dir(path = "ilab2019/crime") %>%
    select(path_lower) %>%
    as_vector()
  
  # Seifa data
  seifa_path <- drop_dir(path = "ilab2019/seifa") %>%
    select(path_lower) %>%
    as_vector()
  
  # Suburb comparison file - 2011 to 2016
  suburb_comparison <- drop_dir(path = "ilab2019/suburb_comparison_2011_2016") %>%
    select(path_lower) %>%
    as_vector()
  
  # Transport location data
  transport_path <- drop_dir(path = "ilab2019/transport") %>%
    select(path_lower) %>%
    as_vector()
  
  # Metro Aria data
  metro_aria_path <- drop_dir(path = "ilab2019/metro_aria") %>%
    select(path_lower) %>%
    as_vector()
  
# [3] ---- Download the data ----  
  
  # Property sales data
  sapply(sales_path,my_download,local_path = "data/sales")
  
  # Land value data
  sapply(land_value_path,my_download,local_path = "data/land_value") 
  
  # ABS Mesh Blocks from 2016
  sapply(mesh_block_path,my_download,local_path = "data/meshblocks") 
  
  # School Data
  sapply(gov_schools_path,my_download,local_path = "data/education/gov_schools") 
  sapply(gov_schools2_path,my_download,local_path = "data/education") 
  sapply(nongov_schools_path,my_download,local_path = "data/education/nongov_schools") 
  sapply(school_catch_path,my_download,local_path = "data/education/school_catch") 
  
  # NSW Suburb Shapefiles
  sapply(shapefile_2019_path,my_download,local_path = "data/shapefiles/2019") 
  sapply(shapefile_2018_path,my_download,local_path = "data/shapefiles/2018") 
  sapply(shapefile_2016_path,my_download,local_path = "data/shapefiles/2016") 
  
  # AUS Remoteness Shapefile
  sapply(remoteness_area_path,my_download,local_path = "data/remoteness") 
  
  # Crime data
  sapply(crime_path,my_download,local_path = "data/crime") 
  
  # Seifa data
  sapply(seifa_path,my_download,local_path = "data/seifa") 
  
  # Suburb comparison file - 2011 to 2016
  sapply(suburb_comparison,my_download,local_path = "data/suburb_comparison") 
  
  # Transport location data
  sapply(transport_path,my_download,local_path = "data/transport") 
  
  # Metro Aria data
  sapply(metro_aria_path,my_download,local_path = "data/metro_aria") 
  