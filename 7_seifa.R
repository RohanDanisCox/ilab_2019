# SEIFA data

# [0] ---- Load packages ----

  library(readr)
  library(readxl)
  library(tidyverse)
  library(rdrop2)
  library(feather) # I don't think this will be used
  library(httpuv)
  library(sp)
  library(rgdal)
  library(rgeos)
  library(maptools)
  library(ggmap) - # probably won't use this
  library(broom) # broom might already be in tidyverse
  library(sf)
  library(RColorBrewer)
  library(leaflet)
  library(leafpop)

# [1] ---- Load and transform the SEIFA data for 2016 ----

  seifa_2016_raw <- read_xlsx("data/seifa/2016 ssc.xlsx", sheet = 2, skip = 6,
                          col_names = c("suburb_code",
                                        "suburb_name",
                                        "relative_socio_economic_disadvantage_index",
                                        "relative_socio_economic_disadvantage_decile",
                                        "relative_socio_economic_adv_disadv_index",
                                        "relative_socio_economic_adv_disadv_decile",
                                        "economic_resources_index",
                                        "economic_resources_decile",
                                        "education_and_occupation_index",
                                        "education_and_occupation_decile",
                                        "usual_resident_population",
                                        "use_with_caution"),
                          col_types = c("text","text","numeric","numeric","numeric","numeric","numeric",
                                        "numeric","numeric","numeric","numeric","text")) %>%
    filter(suburb_code < 20000 & suburb_code != "© Commonwealth of Australia 2018") %>%
    select(c(1,2,11,3,5,7,9,12))
  
  seifa_2016_excluded <- read_xlsx("data/seifa/2016 ssc.xlsx", sheet = 7, skip = 6,
                                   col_names = c("suburb_code",
                                                 "suburb_name",
                                                 "usual_resident_population",
                                                 "relative_socio_economic_disadvantage_index",
                                                 "relative_socio_economic_adv_disadv_index",
                                                 "economic_resources_index",
                                                 "education_and_occupation_index"),
                                   col_types = c("text","text","numeric","text","text","text","text")) %>%
    filter(suburb_code < 20000 & suburb_code != "© Commonwealth of Australia 2018") %>%
    anti_join(seifa_2016_raw, by = "suburb_code") %>%
    mutate(use_with_caution = "Y") %>%
    mutate(relative_socio_economic_disadvantage_index = NA_integer_,
           relative_socio_economic_adv_disadv_index = NA_integer_,
           economic_resources_index = NA_integer_,
           education_and_occupation_index = NA_integer_)
  
  seifa_2016 <- seifa_2016_raw %>%
    bind_rows(seifa_2016_excluded) %>%
    arrange(suburb_code)
 
# [2] ---- Load and transform the SEIFA data for 2011 ----
   
  seifa_2011_suburb_names <- read_xlsx("data/seifa/2011 ssc.xlsx", sheet = 6, skip = 6,
                                       col_names = c("1","2","3","4","5","6","7","8","9","10",
                                                     "11","12","13","14","15","16"),
                                       col_types = "text") %>%
    select(suburb_code_2011 = 1,suburb_name_2011 = 2)
  
  seifa_2011_raw <- read_xlsx("data/seifa/2011 ssc.xlsx", sheet = 2, skip = 6,
                          col_names = c("suburb_code_2011",
                                        "relative_socio_economic_disadvantage_index",
                                        "relative_socio_economic_disadvantage_decile",
                                        "relative_socio_economic_adv_disadv_index",
                                        "relative_socio_economic_adv_disadv_decile",
                                        "economic_resources_index",
                                        "economic_resources_decile",
                                        "education_and_occupation_index",
                                        "education_and_occupation_decile",
                                        "usual_resident_population"),
                          col_types = c("text","numeric","numeric","numeric","numeric","numeric",
                                        "numeric","numeric","numeric","numeric")) %>%
    filter(suburb_code_2011 < 20000 & suburb_code_2011 != "© Commonwealth of Australia 2013") %>%
    left_join(seifa_2011_suburb_names, by = "suburb_code_2011") %>%
    select(c(1,11,10,2,4,6,8))
  
  seifa_2011_excluded <- read_xlsx("data/seifa/2011 ssc.xlsx", sheet = 7, skip = 6,
                                   col_names = c("suburb_code_2011",
                                                 "suburb_name_2011",
                                                 "usual_resident_population",
                                                 "relative_socio_economic_disadvantage_index",
                                                 "relative_socio_economic_adv_disadv_index",
                                                 "economic_resources_index",
                                                 "education_and_occupation_index"),
                                   col_types = c("text","text","numeric","text","text","text","text")) %>%
    filter(suburb_code_2011 < 20000 & suburb_code_2011 != "© Commonwealth of Australia 2013") %>%
    anti_join(seifa_2011_raw, by = "suburb_code_2011") %>%
    mutate(relative_socio_economic_disadvantage_index = NA_integer_,
           relative_socio_economic_adv_disadv_index = NA_integer_,
           economic_resources_index = NA_integer_,
           education_and_occupation_index = NA_integer_) 
  
  seifa_2011_unmatched <- seifa_2011_raw %>%
    bind_rows(seifa_2011_excluded) %>%
    arrange(suburb_code_2011)

# [2] ---- Load and transform the SEIFA data for 2006 ----
  
  seifa_2006_suburb_names <- read_xlsx("data/seifa/2006 ssc.xlsx", sheet = 6, skip = 6,
                                       col_names = c("1","2","3","4","5","6","7","8","9","10",
                                                     "11","12","13","14","15","16","17"),
                                       col_types = "text") %>%
    select(suburb_code_2006 = 1,suburb_name_2006 = 2)
  
  seifa_2006_raw <- read_xlsx("data/seifa/2006 ssc.xlsx", sheet = 2, skip = 6,
                              col_names = c("suburb_code_2006",
                                            "relative_socio_economic_disadvantage_index",
                                            "relative_socio_economic_disadvantage_decile",
                                            "relative_socio_economic_adv_disadv_index",
                                            "relative_socio_economic_adv_disadv_decile",
                                            "economic_resources_index",
                                            "economic_resources_decile",
                                            "education_and_occupation_index",
                                            "education_and_occupation_decile",
                                            "usual_resident_population"),
                              col_types = c("text","numeric","numeric","numeric","numeric","numeric",
                                            "numeric","numeric","numeric","numeric")) %>%
    filter(suburb_code_2006 < 20000 & suburb_code_2006 != "© Commonwealth of Australia 2008") %>%
    left_join(seifa_2006_suburb_names, by = "suburb_code_2006") %>%
    select(c(1,11,10,2,4,6,8))
  
  seifa_2006_excluded <- read_xlsx("data/seifa/2006 ssc.xlsx", sheet = 7, skip = 6,
                                   col_names = c("suburb_code_2006",
                                                 "suburb_name_2006",
                                                 "usual_resident_population",
                                                 "reason - comprised of excluded cds",
                                                 "reason - unclassified ssc",
                                                 "reason - other territory ssc"),
                                   col_types = c("text","text","numeric","numeric","numeric","numeric")) %>%
    filter(suburb_code_2006 < 20000 & suburb_code_2006 != "© Commonwealth of Australia 2008") %>%
    anti_join(seifa_2006_raw, by = "suburb_code_2006") %>%
    mutate(relative_socio_economic_disadvantage_index = NA_integer_,
           relative_socio_economic_adv_disadv_index = NA_integer_,
           economic_resources_index = NA_integer_,
           education_and_occupation_index = NA_integer_) %>%
    select(-c(4,5,6))
  
  seifa_2006 <- seifa_2006_raw %>%
    bind_rows(seifa_2006_excluded) %>%
    arrange(suburb_code_2006)
  
# [3] ---- Matching 2006 to 2011 ----
  
  suburb_names_2011 <- seifa_2011_unmatched %>%
    select(1:2) %>%
    mutate(suburb_name_2006 = str_replace(suburb_name_2011," \\(NSW\\)","")) %>%
    mutate(suburb_name_2006 = str_replace(suburb_name_2006," - NSW",""))
  
  suburb_names_2006 <- seifa_2006 %>%
    select(1:2)
  
  write_csv(suburb_names_2011,"data/created/suburb_names_2011.csv")
  write_csv(suburb_names_2006,"data/created/suburb_names_2006.csv")
  
  drop_upload("data/created/suburb_names_2011.csv","ilab2019/")
  drop_upload("data/created/suburb_names_2006.csv","ilab2019/")
  
  not_matched_11 <- suburb_names_2011 %>%
    anti_join(suburb_names_2006, by = "suburb_name_2006")
  
  not_matched_06 <- suburb_names_2006 %>%
    anti_join(suburb_names_2011, by = "suburb_name_2006")
  
# [4] ---- Matching 2011 to 2016 ----
  
  mapping_2011_2016_raw <- read_xls("data/suburb_comparison/cg_ssc_2011_ssc_2016.xls", sheet = 4, skip = 7,
                                    col_names = c("suburb_code_2011",
                                            "suburb_name_2011",
                                            "suburb_code_2016",
                                            "suburb_name_2016",
                                            "ratio",
                                            "percentage"),
                                    col_types = c("text","text","text","text","numeric","numeric"))
  
  mapping_2011_2016 <- mapping_2011_2016_raw %>%
    group_by(suburb_code_2011,suburb_name_2011) %>%
    mutate(rank = rank(desc(ratio))) %>%
    filter(rank == 1) %>%
    ungroup()
  
  
  seifa_2011 <- seifa_2011_unmatched %>%
    left_join(mapping_2011_2016, by = c("suburb_code_2011","suburb_name_2011"))
  
  
# [4] ---- Save off crime score ----

write_rds(crime_score,"data/created/crime_score.rds")


