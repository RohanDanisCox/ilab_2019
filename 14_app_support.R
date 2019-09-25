## APP support

master <- readRDS("suburb_investigation_app/data/master.rds") %>% 
  filter(!gccsa_code %in% c(19499,19799))

map <- readRDS("data/nsw_sa4.rds")

sf <- read_sf("data/shapefiles/2016")

suburbs <- readRDS("data/created/suburbs.rds")

suburbs1 <- suburbs %>%
  select(1,2,10,11)

nsw1 <- nsw %>% 
  mutate(suburb_code = as.numeric(SSC_CODE16),
         suburb_name = SSC_NAME16,
         area = AREASQKM16) %>%
  select(suburb_code,suburb_name,area)

map <- nsw1 %>%
  left_join(suburbs1) %>%
  select(suburb_code,suburb_name,sa4_code,sa4_name,area)

saveRDS(map, "suburb_investigation_app/data/map.rds")

haberfield <- master %>%
  filter(suburb_name == "Haberfield")

ggplot(haberfield,aes(x = year,y = house_median_suburb)) +
  geom_line(colour = "blue") + 
  geom_line(data = haberfield,mapping = aes(x = year, y = house_median_nsw,colour = "red"))


nsw_results <- master %>%
  group_by(year) %>%
  summarise(violent_crime = mean(violent_crime,na.rm=TRUE),
            dasg_crime = mean(dasg_crime,na.rm=TRUE),
            other_crime = mean(other_crime,na.rm=TRUE),
            crime_score = mean(crime_score,na.rm=TRUE),
            log_crime_score = mean(log_crime_score,na.rm=TRUE),
            education_score = mean(education_score,na.rm=TRUE),
            green_score = mean(green_score,na.rm=TRUE),
            confirmed_population = mean(confirmed_population,na.rm=TRUE),
            working_age_proportion = mean(working_age_proportion,na.rm=TRUE))

saveRDS(nsw_results, "suburb_investigation_app/data/nsw_results.rds")


names(master)
