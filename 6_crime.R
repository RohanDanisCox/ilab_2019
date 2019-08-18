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

# [2] ---- Investigate the crime data ----
names(crime)

crime_suburbs <- crime %>%
  rename(suburb_name = Suburb) %>%
  group_by(suburb_name) %>%
  summarise(n = n())

crime_completeness <- crime %>%
  rename(suburb_name = Suburb) %>%
  gather(key = "month_year", value = "value", 4:291) %>%
  separate(month_year,into = c("month","year"),sep = "-") %>%
  mutate(year = case_when(year <90 ~ as.numeric(paste0("20",year)),
                          year >90 ~ as.numeric(paste0("19",year))))

crime_non_zero <- crime_completeness %>%
  filter(value >0)

summary(crime_completeness)

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
                                     crime_score > 0 ~ log(crime_score)))


class(violent_crime_suburb$month_year)

?separate
  crime_completeness %>%
  group_by(`Offence category`) %>%
  summarise(number = sum(value)) %>%
  select(`Offence category`)

violent_crime[11:21,]

crime_completeness %>% dim

?gather
  

# compare suburbs with suburb base
suburbs <- readRDS("data/created/suburbs.rds")
crime_suburbs <- crime_suburbs %>%
  select(1) %>%
  mutate(crime_matched = TRUE)

suburb_match <- suburbs %>%
  select(1:13)

full_join <- suburb_match %>%
  full_join(crime_suburbs)

write_csv(full_join, "data/created/matched.csv")

drop_upload("data/created/matched.csv","ilab2019/")

not_in_base <- suburbs %>%
  anti_join(crime_suburbs, by = "suburb_name")

not_in_crime <- crime_suburbs %>%
  anti_join(suburbs, by = "suburb_name")

