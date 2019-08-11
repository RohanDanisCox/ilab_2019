# Access Data

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
  drop_dir()
  
# [2] ---- Download sale_value data from Dropbox ----

  # Define function to download all sale value RDS files
  download_sales <- function(path) {
    drop_download(path = path,local_path = "sale_value/",overwrite = TRUE)
  }
  
  # Get the paths
  sales_path <- drop_dir(path = "ilab2019/sale_value") %>%
    select(path_lower) %>%
    as_vector()
  
  # Download all the sale value data
  sapply(sales_path,download_sales)
  
  # Reset sales path to local machine
  new_sales_path <- sales_path %>%
    str_replace("/ilab2019/","")
  
  # Load all the sale value data into memory
  sales_data <- sapply(new_sales_path,readRDS)
  
  # Split the old and new sales data
  old_sales_data <- sales_data[[1]]
  new_sales_data_raw <- sales_data[2:25]
  new_sales_data <- new_sales_data_raw %>%
    bind_rows()
  
  # Save off the new sales data
  saveRDS(new_sales_data, "sale_value/new_sales_data.rds")
  
  # Upload to dropbox
  drop_upload("sale_value/new_sales_data.rds","ilab2019/sale_value")
  
# [3] ---- Download land_value data from Dropbox ----
  
  drop_download("ilab2019/land_value/land_value.rds","land_value/land_value.rds",overwrite = TRUE)
  
  #Upload to memory
  land_value <- readRDS("land_value/land_value.rds")
  
# [4] ---- Download NSW mesh_blocks data from Dropbox ----
  
  drop_download("ilab2019/mesh_blocks/mesh_block_category.csv","mesh_blocks/mesh_block_category.csv",overwrite = TRUE)
  drop_download("ilab2019/mesh_blocks/mesh_block_suburb.csv","mesh_blocks/mesh_block_suburb.csv",overwrite = TRUE)
  
# [4] ---- Extra work ----
  
  ### Need to write a function to upload files to dropbox using drop_upload
  
  ### Need to write a function to delete all data files using unlink - or 
  ### if the data itself is not costly which it may very well not be then leave it in there.
  
  
# [4] ---- Download Shapefiles ----
  
  ### Work with shape files - https://data.gov.au/dataset/ds-dga-91e70237-d9d1-4719-a82f-e71b811154c6/details
  
  # Define function to download all sale value RDS files
  download_shape <- function(path,local_path) {
    drop_download(path = path,local_path = local_path,overwrite = TRUE)
  }
  
  # Get the paths
  gov_schools_path <- drop_dir(path = "ilab2019/education/gov_schools") %>%
    select(path_lower) %>%
    as_vector()
  
  nongov_schools_path <- drop_dir(path = "ilab2019/education/nongov_schools") %>%
    select(path_lower) %>%
    as_vector()
  
  school_catch_path <- drop_dir(path = "ilab2019/education/school_catchment") %>%
    select(path_lower) %>%
    as_vector()
  
  # Download all the sale value data
  sapply(gov_schools_path,download_shape,local_path = "education/gov_schools")
  sapply(nongov_schools_path,download_shape,local_path = "education/nongov_schools")
  sapply(school_catch_path,download_shape,local_path = "education/school_catch")
  
  
  
  drop_download("ilab2019/NSW_LOCALITY_POLYGON_shp/NSW_LOCALITY_POLYGON_shp.shx")
  drop_download("ilab2019/NSW_LOCALITY_POLYGON_shp/NSW_LOCALITY_POLYGON_shp.shx")
  
  ## Using the shape files
  
  shp <- readOGR("NSW_LOCALITY_POLYGON_shp")
  map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
  map

  ## Can get a map from Google if we want
  
  register_google(key = "") # Need to log in at this address and copy key https://console.cloud.google.com/apis/credentials?project=ilab2019-245222&folder&organizationId
  
  mapImage <- get_map(location = c(lon = 147, lat = -33),
                      color = "color",
                      #source = "osm",
                      # maptype = "terrain",
                      zoom = 6)
  
  ggmap(mapImage)+
    geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
  
  
  ## But for now it probably makes more sense to leave it out - no need to run up the cost
  
  
  
  check <- fortify(shp)
  
  broom <- sp_tidi
  
  ggplot() +
    geom_polygon(data = check, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
  
  map + 
    theme_void() + 
    coord_map(xlim = c(-2, 2),ylim = c(50, 51))
  
  # This seems to pull out the data in the SHP file
  shp_data <- shp@data
  
  # Unsure what this does
  check <- fortify(shp)
  
  
  ### Need to figure out how to use broom to get a tidy table that I can use instead of fortify
  ?broom
  ?sp_tidiers
  
  
  ### 
  