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
  library(mapview)
  library(leaflet)
  library(leafpop)

# [1] ---- Grab 2016 Shapefile ----

  sf <- read_sf("data/shapefiles/2016")
  
  nsw <- sf %>%
    filter(STE_CODE16 == 1)
  
  greenscore <- readRDS("data/created/green_score.rds")
  crimescore <- readRDS("data/created/crime_score.rds")
  suburbs <- readRDS("data/created/suburbs.rds")
  
  # Visualise Green Score
  
  sydney_only <- suburbs %>%
    filter(gccsa_code == "1GSYD")
  
  nsw_green_score <- nsw %>%
    left_join(greenscore, by = c("SSC_NAME16" = "suburb_name"))
  
  
  # Visualise Crime Score
  
  nsw_crime_score <- nsw %>%
    left_join(crimescore, by = c("SSC_NAME16" = "suburb_name"))
  
  sydney_crime_score <- nsw_crime_score %>%
    semi_join(sydney_only, by = c("SSC_NAME16" = "suburb_name"))
  
  sydney_crime_score_total <- sydney_crime_score %>%
    group_by(SSC_NAME16) %>%
    summarise(crime_score = sum(crime_score),
           crime_score_log = log(crime_score))
  
  mypalette<-brewer.pal(11,"RdYlGn")
  mypalette<-terrain.colors(13)
  
  greenpalette = colorRampPalette(c('darkred','yellow','darkgreen'))
  
  ?colorRampPalette
  
  sydney_green_score <- nsw_green_score %>%
    semi_join(sydney_only, by = c("SSC_NAME16" = "suburb_name"))
  
  mapview(sydney_green_score, zcol="green_score_decile", col.regions = greenpalette,popup = popupTable(sydney_green_score,zcol = c(2,8,9)))
  
  mapview(sydney_crime_score_total, zcol="crime_score_log", popup = leafpop::popupTable(sydney_crime_score_total))
  
  ?mapview
  
  ?popupTable
  united <- nsw %>%
    st_union() 
  
  plot(united)
  
  
  
