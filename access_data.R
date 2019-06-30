# Access Data

# [0] ---- Load packages ----

  library(readr)
  library(tidyverse)
  library(rdrop2)
  library(feather)
  library(httpuv)

# [1] ---- Load data ----

  drop_auth()
  drop_dir()
  
  drop_download("ilab2019/2017.feather")
  
  data_2017 <- read_feather("2017.feather")