# Crime Data

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
  library(ggmap) - # probably won't use this
  library(broom) # broom might already be in tidyverse
  library(sf)
  library(RColorBrewer)
  library(leaflet)
  library(leafpop)

# [1] ---- Load the crime data ----

  crime <- read_csv("data/crime/crime by suburb.csv") 

# [2] ---- Establish a matching criteria to base suburb names ----

  crime_suburbs <- crime %>%
    rename(suburb_crime = Suburb) %>%
    distinct(suburb_crime)
  
  suburb_base <- readRDS("data/created/suburbs.rds")
  
  missing_from_suburb_base <- suburb_base %>%
    anti_join(crime_suburbs, by = c("suburb_name" = "suburb_crime")) %>%
    select(suburb_name)
  
  fix1 <- missing_from_suburb_base %>%
    mutate(suburb_crime = str_replace(suburb_name," \\(NSW\\)","")) %>%
    mutate(suburb_crime = str_replace(suburb_crime," - NSW","")) 
  
  crime_suburbs_1 <- crime_suburbs %>%
    left_join(fix1, by = "suburb_crime") %>%
    mutate(suburb_name = case_when(is.na(suburb_name) ~ suburb_crime,
                                   !is.na(suburb_name) ~ suburb_name))
  
  suburb_match_crime <- crime_suburbs_1 %>%
    select(suburb_base = suburb_name,suburb_crime) %>%
    write_csv("data/created/suburb_match_crime.csv")
  
  crime_1 <- crime %>%
    left_join(crime_suburbs_1, by = c("Suburb" = "suburb_crime")) %>%
    select(292,2:291)

# [3] ---- Investigate crime ----

  crime_completeness <- crime_1 %>%
    gather(key = "month_year", value = "value", 4:291) %>%
    separate(month_year,into = c("month","year"),sep = "-") %>%
    mutate(year = case_when(year <90 ~ as.numeric(paste0("20",year)),
                            year >90 ~ as.numeric(paste0("19",year))))
  
  crime_summary <- crime_completeness %>%
    group_by(suburb_name,`Offence category`) %>%
    summarise(number = sum(value))
  
  crime_types <- crime_completeness %>%
    group_by(`Offence category`,Subcategory) %>%
    summarise(number = sum(value))
  
  violent_crime <- c("Abduction and kidnapping","Arson","Assault","Homicide","Malicious damage to property","Robbery","Sexual offences","Theft")
  drug_alcohol_sex_gaming_crime <- c("Drug offences","Liquor offences","Betting and gaming offences","Pornography offences","Prostitution offences")
  other_crime <- c("Against justice procedures","Blackmail and extortion","Disorderly conduct","Intimidation, stalking and harassment",
                   "Other offences","Other offences against the person","Prohibited and regulated weapons offences","Transport regulatory offences")
  
  violent_crime_suburb <- crime_completeness %>%
    filter(`Offence category` %in% violent_crime) %>% 
    group_by(suburb_name, year) %>%
    summarise(violent_crime = sum(value))
  
  dasg_crime_suburb <- crime_completeness %>%
    filter(`Offence category` %in% drug_alcohol_sex_gaming_crime) %>% 
    group_by(suburb_name, year) %>%
    summarise(dasg_crime = sum(value))
  
  other_crime_suburb <- crime_completeness %>%
    filter(`Offence category` %in% other_crime) %>% 
    group_by(suburb_name, year) %>%
    summarise(other_crime = sum(value))
  
  overall_crime  <- violent_crime_suburb %>%
    left_join(dasg_crime_suburb, by = c("suburb_name", "year")) %>%
    left_join(other_crime_suburb, by = c("suburb_name", "year"))
  
  crime_score <- overall_crime %>%
    mutate(crime_score = (10*violent_crime)+(5*dasg_crime)+other_crime) %>%
    mutate(log_crime_score = case_when(crime_score == 0 ~ 0,
                                       crime_score >=1 ~ 1+log(crime_score)))

# [4] ---- check the distribution ----
  
  ggplot(crime_score, aes(log_crime_score)) +
    geom_density()
  
# [5] ---- add back the suburb_code  ----
  
  suburb_code <- suburb_base %>%
    select(1:2)
  
  crime_score1 <- crime_score %>%
    left_join(suburb_code, by = "suburb_name") %>%
    select(8,1:7)

# [6] ---- Save off crime score ----

write_rds(crime_score1,"data/created/crime_score.rds")
  