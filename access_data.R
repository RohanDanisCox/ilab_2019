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

# [1] ---- Load data ----

  # These have been used to create drop_tokens on home computer and transferred to GCP.
  #token <- drop_auth()
  #saveRDS(token, "DropToken.RDS")

  drop_auth(new_user = FALSE, rdstoken = "DropToken.RDS")
  
  drop_dir()
  
  drop_download("ilab2019/2017.feather",overwrite = TRUE)
  
  data_2017 <- read_feather("2017.feather")

  unlink("2017.feather")
    
  
  
  ### Need to write a function to upload files to dropbox using drop_upload
  
  ### Need to write a function to delete all data files using unlink - or 
  ### if the data itself is not costly which it may very well not be then leave it in there.
  
  
  ### Work with shape files - https://data.gov.au/dataset/ds-dga-91e70237-d9d1-4719-a82f-e71b811154c6/details
  
  drop_download("ilab2019/NSW_LOCALITY_POLYGON_shp/NSW_LOCALITY_POLYGON_shp.shx")
  
  
  ## Using the shape files
  
  shp <- readOGR("NSW_LOCALITY_POLYGON_shp")
  map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

  ## Can get a map from Google if we want
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
  
  
  ?get_map
  ?register_google
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
  