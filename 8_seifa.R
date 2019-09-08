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
    arrange(suburb_code) %>%
    mutate(year = 2016) %>%
    select(1,2,9,3,4,5,6,7)
 
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

# [3] ---- Load and transform the SEIFA data for 2006 ----
  
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
  
  seifa_2006_unmatched <- seifa_2006_raw %>%
    bind_rows(seifa_2006_excluded) %>%
    arrange(suburb_code_2006)
  
# [4] ---- Load and transform the SEIFA data for 2001 ---- 
  
  seifa_2001_raw <- read_xlsx("data/seifa/2001 ssc.xlsx", sheet = 1,
                              col_names = TRUE) %>%
    mutate(suburb_code_2001 = as.character(Suburb_Code), 
           suburb_name_2006 = Suburb_Name,
           usual_resident_population = as.numeric(SEIFA_2001_Population),
           relative_socio_economic_disadvantage_index = as.numeric(Disadvantage),
           relative_socio_economic_adv_disadv_index = as.numeric(Advantage_Disadvantage),
           economic_resources_index = as.numeric(Econ_Resource),
           education_and_occupation_index = as.numeric(Educ_Occupation)) %>%
    select(46,47,48,49,50,51,52) %>%
    filter(suburb_code_2001 < 20000)
  
  seifa_2006_suburb_names1 <- seifa_2006_suburb_names %>%
    filter(suburb_code_2006 < 20000)
  
  seifa_2001_unmatched <- seifa_2001_raw %>%
    left_join(seifa_2006_suburb_names1, by = "suburb_name_2006") %>%
    filter(!is.na(suburb_code_2006)) 
  
# [5] ---- Matching 2011 to 2016 ----
  
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
    left_join(mapping_2011_2016, by = c("suburb_code_2011","suburb_name_2011")) %>%
    mutate(year = 2011) %>%
    select(suburb_code = 8, suburb_name = 9,13,3,4,5,6,7)
  
# [6] ---- Matching 2006 to 2016 ----
  
  mapping_2006_2016_raw <- read_xlsx("data/suburb_comparison/cg_ssc_2006_ssc_2016.xlsx", sheet = 4, skip = 7,
                                     col_names = c("suburb_code_2006",
                                                   "suburb_name_2006",
                                                   "suburb_code_2016",
                                                   "suburb_name_2016",
                                                   "ratio",
                                                   "percentage"),
                                     col_types = c("text","text","text","text","numeric","numeric"))
  
  mapping_2006_2016 <- mapping_2006_2016_raw %>%
    group_by(suburb_code_2006,suburb_name_2006) %>%
    mutate(rank = rank(desc(ratio))) %>%
    filter(rank == 1) %>%
    ungroup()
  
  seifa_2006 <- seifa_2006_unmatched %>%
    left_join(mapping_2006_2016, by = c("suburb_code_2006","suburb_name_2006")) %>%
    mutate(year = 2006) %>%
    select(suburb_code = 8,suburb_name = 9,13,3,4,5,6,7)

# [7] ---- Matching 2001 to 2016 ----

  seifa_2001 <- seifa_2001_unmatched %>%
    left_join(mapping_2006_2016, by = c("suburb_code_2006","suburb_name_2006")) %>%
    mutate(year = 2001) %>%
    select(suburb_code = 9,suburb_name = 10,14,3,4,5,6,7)

# [8] ---- Combining Seifa Scores ---- 
  
  seifa_wide <- seifa_2016 %>%
    bind_rows(seifa_2011) %>%
    bind_rows(seifa_2006) %>%
    bind_rows(seifa_2001) %>%
    mutate(suburb_code = as.numeric(suburb_code)) %>%
    arrange(suburb_code, desc(year)) 
  
  # Make long
  
  seifa_long <- seifa_wide %>%
    gather("index","value",-c(suburb_code,suburb_name,year,usual_resident_population))
  
# [8] ---- Visualise example ---- 
  
  haberfield <- seifa_long %>%
    filter(suburb_name == "Haberfield")
  
  ggplot(haberfield, aes(year,value,colour = index)) +
    geom_line()
  
# [9] ---- Save off seifa scores ----

write_rds(seifa_wide,"data/created/seifa_scores.rds")
