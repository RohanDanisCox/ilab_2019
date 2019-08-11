# Visualisations

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

# [1] ---- Download Shapefiles ----

  ### Work with shape files - https://data.gov.au/dataset/ds-dga-91e70237-d9d1-4719-a82f-e71b811154c6/details

  drop_download("ilab2019/NSW_LOCALITY_POLYGON_shp/NSW_LOCALITY_POLYGON_shp.shx")
  
  ## Using the shape files
  
  shp <- readOGR("NSW_LOCALITY_POLYGON_shp")
  sf <- read_sf("NSW_LOCALITY_POLYGON_shp")
  
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
  
  
# [1] ---- Deconstruct into XX ----
  
  check <- fortify(shp)
  
# [2] ---- Visualise with ggplot2 ----
  
  ggplot(sf)+
    geom_sf()

  
  haberfield <- sf %>%
    filter(NSW_LOCA_2 == "HABERFIELD")
  
  glenroy <- sf %>%
    filter(NSW_LOCA_2 == "GLENROY")
  
  glenmore <- sf %>%
    filter(NSW_LOCA_2 == "GLENMORE")
  
  
  ggplot(glenmore)+
    geom_sf()
  
  
  just_data <- sf %>%
    st_set_geometry(NULL)
  
  summary(just_data)

  check <- just_data %>%
    filter(NSW_LOCA_2 == "GLENROY")
  
  just_data_1 <- just_data %>%
    mutate(SSC_NAME_2016 = tolower(NSW_LOCA_2))
  
  mesh <- mesh_block_check %>%
    mutate(SSC_NAME_2016 = tolower(SSC_NAME_2016))
  
  try <- just_data_1 %>%
    left_join(mesh)
  
  sf_1 <- sf %>%
    mutate(SSC_NAME_2016 = tolower(NSW_LOCA_2))
  
  try_2 <- sf_1 %>%
    left_join(mesh)
  
  try_3 <- try_2 %>%
    filter(GCCSA_NAME_2016 == "Greater Sydney")
  
  ggplot(try_2)+
    geom_sf(mapping = aes(fill=green_score_decile))
  
  mypalette<-brewer.pal(11,"RdYlGn")
  
  mapview(try_2, zcol="green_score_decile",col.regions = mypalette)
  

    
  

?left_join
# [2] ---- Trying mapview ---- # needed to add Cairo
  
  install.packages("mapview")
  
  install.packages("gdtools")
  
  library(mapview)
  library(devtools)
  
  devtools::install_github('r-lib/later')
  
  mapview(school_catch)
  
  install.packages("devtools")
  
# [2] ---- Trying schools ----
  
  gov_schools <- read_sf("education/gov_schools")
  nongov_schools <- read_sf("education/nongov_schools")
  school_catch <- read_sf("education/school_catch")
  
  ggplot(nongov_schools)+
    geom_sf()
  
  test <- gov_schools %>%
    filter(X28town_sub == "Haberfield")
  
  ggplot(test)+
    geom_sf()
  
  ggplot(school_catch)+
    geom_sf()
  
  
  