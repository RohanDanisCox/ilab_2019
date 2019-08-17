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
  library(leaflet)
  library(leafpop)

# [1] ---- Grab 2016 Shapefile ----

  sf <- read_sf("data/shapefiles/2016")
  
  nsw <- sf %>%
    filter(STE_CODE16 == 1)
  
  greenscore <- readRDS("data/created/green_score.rds")
  suburbs <- readRDS("data/created/suburbs.rds")
  
  sydney_only <- suburbs %>%
    filter(gccsa_code == "1GSYD")
  
  nsw_green_score <- nsw %>%
    left_join(greenscore, by = c("SSC_NAME16" = "suburb_name"))
  
  mypalette<-brewer.pal(11,"RdYlGn")
  
  sydney_green_score <- nsw_green_score %>%
    semi_join(sydney_only, by = c("SSC_NAME16" = "suburb_name"))
  
  mapview(sydney_green_score, zcol="green_score_decile",col.regions = mypalette, popup = popupTable(sydney_green_score,zcol = c(2,8,9)))
  
  
  ?popupTable
  united <- nsw %>%
    st_union() 
  
  plot(united)
  
  
  
