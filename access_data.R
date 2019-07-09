# Access Data

# [0] ---- Load packages ----

  library(readr)
  library(tidyverse)
  library(rdrop2)
  library(feather)
  library(httpuv)

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