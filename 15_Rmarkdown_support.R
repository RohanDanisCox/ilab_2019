# RMarkdown File Support

# [0] ---- Load packages ----

  library(readr)
  library(tidyverse)
  library(rdrop2)
  library(httpuv)
  library(sp)
  library(rgdal)
  library(rgeos)
  library(maptools)
  library(ggmap)
  library(broom)
  library(sf)
  library(RColorBrewer)
  #library(mapview)
  library(leaflet)
  library(leafpop)
  library(pryr)
  library(ggrepel)
  library(randomForest)

# [1] ---- Obtain data ----

  master <- readRDS("data/created/master.rds") %>%
    filter(year >= 2000)
  
  data_raw <- master %>%
    select(suburb_code,suburb_name,year,sa2_name,sa3_name,sa4_name,gccsa_name,suburb_area_sqkm,
           violent_crime,dasg_crime,log_crime_score, # Crime
           education_score, # education
           green_score_decile, # green space
           usual_resident_population,working_age_proportion,senior_citizen_proportion, # Demographics 
           confirmed_journeys,public_transport_proportion,motor_vehicle_proportion, bicycle_walking_proportion, # Transport
           confirmed_dwellings, house_and_semi_proportion, unit_proportion, dwelling_density, # Dwellings
           seifa_econ_disadvantage, seifa_econ_adv_disadv, # SEIFA
           seifa_econ_resources, seifa_education_occupation, # SEIFA
           median_land_value,median_land_value_per_sqm, # Land Values
           aria_overall, aria_education, aria_health, aria_shopping, aria_public_transport, aria_financial_postal, # ARIA
           house_median = house_median_suburb, unit_median = apartment_median_suburb, land_median_suburb, annual_turnover) # Property Prices
  
  # map
  map <- readRDS("data/created/maps.rds")
  
  map_sa4 <- map %>%
    group_by(sa4_name,gccsa_name) %>%
    summarise(geometry = st_union(geometry)) %>%
    ungroup()
  
  sydney <- map_sa4 %>%
    filter(gccsa_name == "Greater Sydney") %>%
    st_simplify(preserveTopology = TRUE,dTolerance = 0.001)
    
  rest <- map_sa4 %>%
    mutate(sa4_name = case_when(gccsa_name == "Greater Sydney" ~ "Greater Sydney",
                         TRUE ~ sa4_name)) %>%
    group_by(sa4_name) %>%
    summarise(geometry = st_union(geometry)) %>%
    st_simplify(preserveTopology = TRUE,dTolerance = 0.001)
  
  # get the center of each suburb
  
  centroid <- map %>%
    mutate(geometry = st_centroid(geometry))
  
  saveRDS(sydney,"rmarkdown/map_sydney.rds")
  saveRDS(rest,"rmarkdown/map_rest.rds")
  saveRDS(centroid,"rmarkdown/centroids.rds")
  
# [2] ---- Build out the dataset ----

  index <- data_raw %>%
    select(suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name,year,house_median) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name) %>%
    mutate(base_price = first(house_median)) %>%
    ungroup() %>%
    filter(!is.na(base_price))
  
  na_check <- index %>%
    filter(is.na(house_median)) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    summarise(n = n()) %>%
    ungroup() #%>%
    #filter(n > 1)
  
  index_1 <- index %>%
    anti_join(na_check, by = c("suburb_name", "sa2_name", "sa3_name", "sa4_name","gccsa_name"))
  
  index_2 <- index_1 %>%
    mutate(index = (house_median / base_price) * 100) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    mutate(top = last(index))
  
  index_3 <- index_2 %>%
    group_by(year) %>%
    mutate(mean_nsw = mean(house_median)) %>%
    ungroup() %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    mutate(base_nsw = first(mean_nsw)) %>% 
    ungroup() %>%
    mutate(nsw_control_index = (house_median / base_nsw) *100)
  
  # Control for price vs NSW and create a new control index
  
  index_4 <- index_3 %>%
    mutate(control = house_median / mean_nsw) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    mutate(base_control = first(control)) %>% 
    ungroup() %>%
    mutate(control_index = (control / base_control) *100)
  
  # Change index to data
  
  data <- index_4
  
  # Do the same for units
  
  master_unit <- readRDS("data/created/master.rds") %>%
    filter(year >= 2001)
  
  data_raw_unit <- master_unit %>%
    select(suburb_code,suburb_name,year,sa2_name,sa3_name,sa4_name,gccsa_name,suburb_area_sqkm,
           violent_crime,dasg_crime,log_crime_score, # Crime
           education_score, # education
           green_score_decile, # green space
           usual_resident_population,working_age_proportion,senior_citizen_proportion, # Demographics 
           confirmed_journeys,public_transport_proportion,motor_vehicle_proportion, bicycle_walking_proportion, # Transport
           confirmed_dwellings, house_and_semi_proportion, unit_proportion, dwelling_density, # Dwellings
           seifa_econ_disadvantage, seifa_econ_adv_disadv, # SEIFA
           seifa_econ_resources, seifa_education_occupation, # SEIFA
           median_land_value,median_land_value_per_sqm, # Land Values
           aria_overall, aria_education, aria_health, aria_shopping, aria_public_transport, aria_financial_postal, # ARIA
           house_median = house_median_suburb, unit_median = apartment_median_suburb, land_median_suburb, annual_turnover) # Property Prices
  
  units_1 <- data_raw_unit %>%
    select(suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name,year,unit_median) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name) %>%
    mutate(base_price = first(unit_median)) %>%
    ungroup() %>%
    filter(!is.na(base_price))
  
  unit_check <- units_1 %>%
    filter(is.na(unit_median)) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    summarise(n = n()) %>%
    ungroup() #%>%
  #filter(n > 1)
  
  units_2 <- units_1 %>%
    anti_join(unit_check, by = c("suburb_name", "sa2_name", "sa3_name", "sa4_name","gccsa_name"))
  
  units_3 <- units_2 %>%
    mutate(index = (unit_median / base_price) * 100) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    mutate(top = last(index))
  
  units_4 <- units_3 %>%
    group_by(year) %>%
    mutate(mean_nsw = mean(unit_median)) %>%
    ungroup() %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    mutate(base_nsw = first(mean_nsw)) %>% 
    ungroup() %>%
    mutate(nsw_control_index = (unit_median / base_nsw) *100)
  
  units_5 <- units_4 %>%
    mutate(control = unit_median / mean_nsw) %>%
    group_by(suburb_name,suburb_name,sa2_name,sa3_name,sa4_name,gccsa_name) %>%
    mutate(base_control = first(control)) %>% 
    ungroup() %>%
    mutate(control_index = (control / base_control) *100)
  
  units <- units_5
  
  saveRDS(units,"rmarkdown/units.rds")
  
# [3] ---- Develop Visuals ----
  
# [3][1] ---- A Story of Growth ----
  
  ggplot(data,aes(x = year, y = index, group = suburb_name)) + 
    geom_line(alpha = 0.05) +
    ggtitle("NSW Property - A Story of Growth") +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5)) +
    geom_hline(yintercept=100, linetype="dashed", color = "red") + 
    scale_y_continuous(breaks = c(100,500,1000)) + 
    labs(x = "Year", y = "Price Index from year 2000") +
    annotate("rect", xmin=2001.1, xmax=2001.9, ymin=0, ymax=Inf, alpha=0.1, fill="green") + 
    annotate("rect", xmin=2008.7, xmax=2009.3, ymin=0, ymax=Inf, alpha=0.1, fill="green") + 
    annotate("rect", xmin=2011.9, xmax=2019, ymin=0, ymax=Inf, alpha=0.1, fill="green") +
    annotate("rect", xmin=2002.4, xmax=2008.3, ymin=0, ymax=Inf, alpha=0.1, fill="red") + 
    annotate("rect", xmin=2009.8, xmax=2010.9, ymin=0, ymax=Inf, alpha=0.1, fill="red")
  
  data %>% 
    filter(year == 2000) %>% 
    summarise(median = median(house_median)) %>% 
    round(0) %>% 
    pull() %>%
    format(digits = 2, big.mark = ",")
  
# [3][2] ---- Regional NSW Moved First ----
  
  ggplot(data,aes(x = year, y = index, group = suburb_name, colour = gccsa_name)) + 
    geom_line(alpha = 0.1) +
    scale_y_continuous(breaks = c(100,500,1000)) +
    ggtitle("Regional NSW Moved First") +
    labs(x = "Year", y = "Price Index from year 2000", colour = "Region") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom") + 
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
  
# [3][3] ---- Which Regions Grew the Most? ----
  
  growth_by_sa4 <- data %>%
    filter(year == 2019) %>%
    group_by(sa4_name,gccsa_name) %>%
    summarise(average = mean(index)) 

  table_1 <- growth_by_sa4 %>%
    rename(SA4 = sa4_name,
           `Average Price Index in 2019` = average,
           `Greater Sydney or Rest of NSW` = gccsa_name) %>%
    arrange(desc(`Average Price Index in 2019`))
  
  rest_growth <- rest %>%
    left_join(growth_by_sa4)
  
  ggplot(rest_growth) +
    geom_sf(mapping = aes(fill = average)) +
    scale_fill_continuous(type = "viridis",limits = c(200,600)) +
    ggtitle("Average Price Index - Rest of NSW") +
    labs(fill = "Average Price Index in 2019") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom", legend.key.width = unit(0.7,"cm"))
  
  sydney_growth <- sydney %>%
    left_join(growth_by_sa4)
  
  ggplot(sydney_growth) +
    geom_sf(mapping = aes(fill = average)) +
    scale_fill_continuous(type = "viridis",limits = c(200,600)) +
    ggtitle("Average Price Index - Greater Sydney") +
    labs(fill = "Average Price Index in 2019") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom", legend.key.width = unit(0.7,"cm"))
  
# [3][4] ---- Which Regional Suburbs Grew the Most? ----
  
  top_rest <- data %>%
    filter(year == 2019) %>%
    filter(gccsa_name == "Rest of NSW") %>% 
    arrange(desc(top)) %>%
    head(10)
  
  top_rest_points <- centroid %>%
    semi_join(top_rest) %>%
    mutate(x = st_coordinates(geometry)[,1],
           y = st_coordinates(geometry)[,2]) 
  
  ggplot(data %>% 
           filter(gccsa_name == "Rest of NSW") %>% 
           arrange(desc(top)) %>%
           head(200),
         aes(x = year, y = index, group = suburb_name, colour = sa4_name)) + 
    geom_line() +
    geom_text_repel(data = data %>% 
                      filter(gccsa_name == "Rest of NSW") %>% 
                      arrange(desc(top)) %>%
                      head(200) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name), 
                    direction = "y",hjust = -0.5, size= 3, segment.alpha = 0.4) + 
    scale_x_continuous(expand = c(0.1,0.1,0.3,0)) +
    scale_y_continuous(breaks = c(100,500,1000)) +
    ggtitle("Top 10 - Rest of NSW") +
    labs(x = "Year", y = "Price Index from year 2000", colour = "SA4") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom") + 
    guides(colour = guide_legend(override.aes = list(alpha = 1)))
  
  # Where are they?
  ggplot(rest) +
    geom_sf() +
    geom_sf(data = rest %>% filter(sa4_name %in% c("Capital Region",
                                                   "Hunter Valley exc Newcastle",
                                                   "Richmond - Tweed",
                                                   "Coffs Harbour - Grafton",
                                                   "Newcastle and Lake Macquarie",
                                                   "Riverina")),
            mapping = aes(fill = sa4_name)) +
    geom_point(data = top_rest_points,mapping = aes(x,y), colour = "red") +
    geom_label_repel(data = top_rest_points,mapping = aes(x,y,label = suburb_name)) +
    ggtitle("Top 10 - Rest of NSW") +
    labs(fill = "SA4") +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom",
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
# [3][5] ---- Which Sydney Suburbs Grew the Most? ----
  
  top_syd <- data %>%
    filter(year == 2019) %>%
    filter(gccsa_name == "Greater Sydney") %>% 
    arrange(desc(top)) %>%
    head(10)
  
  top_syd_points <- centroid %>%
    semi_join(top_syd) %>%
    mutate(x = st_coordinates(geometry)[,1],
           y = st_coordinates(geometry)[,2]) 
  
  ggplot(data %>% 
           filter(gccsa_name == "Greater Sydney") %>% 
           arrange(desc(top)) %>%
           head(200),
         aes(x = year, y = index, group = suburb_name, colour = sa4_name)) + 
    geom_line() +
    geom_text_repel(data = data %>% 
                      filter(gccsa_name == "Greater Sydney") %>% 
                      arrange(desc(top)) %>%
                      head(200) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name), 
                    direction = "y",hjust = -0.5, size= 3, segment.alpha = 0.4) + 
    scale_x_continuous(expand = c(0.1,0.1,0.3,0)) +
    scale_y_continuous(breaks = c(100,500,1000)) +
    ggtitle("Top 10 - Greater Sydney") +
    labs(x = "Year", y = "Price Index from year 2000", colour = "SA4") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom") + 
    guides(colour = guide_legend(override.aes = list(alpha = 1),
                                 nrow=2,byrow=TRUE))
  
  ggplot(sydney) +
    geom_sf() +
    geom_sf(data = sydney %>% filter(sa4_name %in% c("Central Coast",
                                                   "Sydney - Eastern Suburbs",
                                                   "Sydney - North Sydney and Hornsby",
                                                   "Sydney - Northern Beaches")),
            mapping = aes(fill = sa4_name)) +
    geom_point(data = top_syd_points,mapping = aes(x,y), colour = "red") +
    geom_label_repel(data = top_syd_points,
                    mapping = aes(x,y,label = suburb_name),
                    direction = "both", size= 3, segment.alpha = 0.4) +
    scale_x_continuous(expand = c(0.1,0.1,0.3,0)) +
    ggtitle("Top 10 - Greater Sydney") +
    labs(fill = "SA4") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom",
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  ## Zoom in further
  
  ggplot(sydney) +
    geom_sf() +
    geom_sf(data = sydney %>% filter(sa4_name %in% c("Central Coast",
                                                     "Sydney - Eastern Suburbs",
                                                     "Sydney - North Sydney and Hornsby",
                                                     "Sydney - Northern Beaches")),
            mapping = aes(fill = sa4_name)) +
    coord_sf(xlim = c(151, 151.4), ylim = c(-34, -33.7), expand = FALSE) +
    geom_point(data = top_syd_points,mapping = aes(x,y), colour = "red") +
    geom_label_repel(data = top_syd_points,
                     mapping = aes(x,y,label = suburb_name),
                     direction = "both", size= 3, segment.alpha = 0.4) +
    scale_x_continuous(expand = c(0.1,0.1,0.3,0)) +
    ggtitle("Top 10 - Greater Sydney") +
    labs(fill = "SA4") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom",
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
# [4][1] ---- Apartments Growth ----

  ggplot(units,aes(x = year, y = index, group = suburb_name)) + 
    geom_line(alpha = 0.1) +
    ggtitle("Apartments - A Story of Growth") +
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5)) +
    geom_hline(yintercept=100, linetype="dashed", color = "red") + 
    scale_y_continuous(breaks = c(100,500,1000), limits = c(0,1500)) + 
    labs(x = "Year", y = "Price Index from year 2001") +
    annotate("rect", xmin=2001.1, xmax=2001.9, ymin=0, ymax=Inf, alpha=0.1, fill="green") + 
    annotate("rect", xmin=2008.7, xmax=2009.3, ymin=0, ymax=Inf, alpha=0.1, fill="green") + 
    annotate("rect", xmin=2011.9, xmax=2019, ymin=0, ymax=Inf, alpha=0.1, fill="green") +
    annotate("rect", xmin=2002.4, xmax=2008.3, ymin=0, ymax=Inf, alpha=0.1, fill="red") + 
    annotate("rect", xmin=2009.8, xmax=2010.9, ymin=0, ymax=Inf, alpha=0.1, fill="red")
  
  
# [4][2] ---- Greater Sydney ---- 
  
  top_syd_unit <- units %>%
    filter(year == 2019) %>%
    filter(gccsa_name == "Greater Sydney") %>% 
    arrange(desc(top)) %>%
    head(10)
  
  top_syd_points_unit <- centroid %>%
    semi_join(top_syd_unit) %>%
    mutate(x = st_coordinates(geometry)[,1],
           y = st_coordinates(geometry)[,2]) 
  
  ggplot(units %>% 
           filter(gccsa_name == "Greater Sydney") %>% 
           arrange(desc(top)) %>%
           head(190),
         aes(x = year, y = index, group = suburb_name, colour = sa4_name)) + 
    geom_line() +
    geom_text_repel(data = units %>% 
                      filter(gccsa_name == "Greater Sydney") %>% 
                      arrange(desc(top)) %>%
                      head(190) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name), 
                    direction = "y",hjust = -0.5, size= 3, segment.alpha = 0.4) + 
    scale_x_continuous(expand = c(0.1,0.1,0.3,0)) +
    scale_y_continuous(breaks = c(100,500,1000)) +
    ggtitle("Top 10 - Greater Sydney") +
    labs(x = "Year", y = "Price Index from year 2001", colour = "SA4") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom") + 
    guides(colour = guide_legend(override.aes = list(alpha = 1),
                                 nrow=2,byrow=TRUE))
  
  ggplot(sydney) +
    geom_sf() +
    geom_sf(data = sydney %>% filter(sa4_name %in% c("Central Coast",
                                                     "Sydney - City and Inner South",
                                                     "Sydney - Outer South West",
                                                     "Sydney - Outer West and Blue Mountains",
                                                     "Sydney - Inner West",
                                                     "Sydney - South West")),
            mapping = aes(fill = sa4_name)) +
    geom_point(data = top_syd_points_unit,mapping = aes(x,y), colour = "red") +
    geom_label_repel(data = top_syd_points_unit,
                     mapping = aes(x,y,label = suburb_name),
                     direction = "both", size= 3, segment.alpha = 0.4) +
    scale_x_continuous(expand = c(0.1,0.1,0.3,0)) +
    ggtitle("Top 10 Suburbs - Apartments - Greater Sydney") +
    labs(fill = "SA4") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom",
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  unit_growth_by_sa4 <- units %>%
    filter(year == 2019) %>%
    group_by(sa4_name,gccsa_name) %>%
    summarise(average = mean(index)) 
  
  unit_sydney_growth <- sydney %>%
    left_join(unit_growth_by_sa4)
  
  ggplot(unit_sydney_growth) +
    geom_sf(mapping = aes(fill = average)) +
    scale_fill_continuous(type = "viridis",limits = c(200,350)) +
    ggtitle("Average Price Index - Greater Sydney") +
    labs(fill = "Average Price Index in 2019") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom", legend.key.width = unit(0.7,"cm"))
  
#  [3][6] ---- Getting Rich off the Lockout ----
  
  #Pyrmont (including the Star City Casino) , 
  #Ultimo, Chippendale, Haymarket, Surry Hills, Elizabeth Bay, Rushcutters Bay and Darlinghurst
  
  ggplot(data %>% 
           filter(suburb_name %in% c("Pyrmont",
                                     "Ultimo",
                                     "Chippendale",
                                     "Haymarket",
                                     "Surry Hills",
                                     "Elizabeth Bay",
                                     "Rushcutters Bay",
                                     "Darlinghurst",
                                     "Wooloomooloo")),
         aes(x = year, y = index, group = suburb_name, colour = sa4_name)) + 
    geom_line() +
    geom_text_repel(data = data %>% 
                      filter(suburb_name %in% c("Pyrmont",
                                                "Ultimo",
                                                "Chippendale",
                                                "Haymarket",
                                                "Surry Hills",
                                                "Elizabeth Bay",
                                                "Rushcutters Bay",
                                                "Darlinghurst",
                                                "Wooloomooloo")) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name), 
                    direction = "y",hjust = -0.5, size= 3, segment.alpha = 0.4) + 
    scale_x_continuous(expand = c(0.1,0.1,0.3,0)) +
    scale_y_continuous(breaks = c(100,500,1000)) +
    ggtitle("Top 10 - Greater Sydney") +
    labs(x = "Year", y = "Price Index from year 2000", colour = "SA4") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom") + 
    guides(colour = guide_legend(override.aes = list(alpha = 1),
                                 nrow=2,byrow=TRUE))
  
  
  ### Try with units
  
  table(lockout$sa3_name)
  
  lockout <- units %>%
    filter(sa3_name %in% c("Sydney Inner City")) %>%
    mutate(lockout_flag = case_when(suburb_name %in% c("Pyrmont",
                                                       "Ultimo",
                                                       "Chippendale",
                                                       "Haymarket",
                                                       "Surry Hills",
                                                       "Elizabeth Bay",
                                                       "Rushcutters Bay",
                                                       "Darlinghurst",
                                                       "Woolloomooloo") ~ "Lockout",
                                    TRUE ~ "No Lockout"))
  
  ggplot(units %>%
           filter(suburb_name %in% c("Pyrmont",
                                     "Ultimo",
                                     "Chippendale",
                                     "Haymarket",
                                     "Surry Hills",
                                     "Elizabeth Bay",
                                     "Rushcutters Bay",
                                     "Darlinghurst",
                                     "Woolloomooloo")), 
         aes(x = year, y = index, group = suburb_name)) + 
    geom_line() +
    geom_text_repel(data = units %>%
                      filter(suburb_name %in% c("Pyrmont",
                                                "Ultimo",
                                                "Chippendale",
                                                "Haymarket",
                                                "Surry Hills",
                                                "Elizabeth Bay",
                                                "Rushcutters Bay",
                                                "Darlinghurst",
                                                "Woolloomooloo")) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name), 
                    direction = "y",hjust = -0.5, size= 3, segment.alpha = 0.4) + 
    geom_vline(xintercept = 2014.15, linetype="dashed", color = "red") +
    scale_x_continuous(expand = c(0.1,0.1,0.3,0)) +
    scale_y_continuous(breaks = c(100,500,1000)) +
    ggtitle("Lockout Impact") +
    labs(x = "Year", y = "Price Index from year 2001", colour = "SA4") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom") + 
    guides(colour = guide_legend(override.aes = list(alpha = 1),
                                 nrow=2,byrow=TRUE))
  
  
  lockout_agg <- units %>%
    filter(sa4_name %in% c("Sydney - Inner West",
                           "Sydney - City and Inner South",
                           "Sydney - Eastern Suburbs")) %>%
    group_by(sa3_name, year) %>%
    summarise(average_index = mean(index))
    
  
  ggplot(lockout_agg, aes(x = year, y = average_index, group = sa3_name, colour = sa3_name)) + 
    geom_line() +
    geom_text_repel(data = lockout_agg %>% 
                      group_by(sa3_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = sa3_name), 
                    direction = "y",hjust = -0.5, size= 3, segment.alpha = 0.4) + 
    geom_vline(xintercept = 2014.15, linetype="dashed", color = "red") +
    scale_x_continuous(expand = c(0.1,0.1,0.3,0)) +
    scale_y_continuous(breaks = c(100,200,300,400,500)) +
    ggtitle("Top 10 - Greater Sydney") +
    labs(x = "Year", y = "Price Index from year 2001", colour = "SA4") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position = "none")

  
  
  
# [4] ---- Trash ----
  
  ggplot(data,aes(x = year, y = index, group = suburb_name, colour = gccsa_name)) + 
    geom_line(alpha = 0.2) +
    geom_text(data = data %>% 
                group_by(suburb_name) %>%
                arrange(desc(year)) %>%
                slice(1) %>%
                filter(index > 900),
              aes(x = year + 0.2, label = suburb_name, hjust = 0)) + 
    expand_limits(x = max(data$year) + 3)
  
  
  # Graph of all suburbs - highlighting top few - Using ggrepel instead
  ggplot(index_2,aes(x = year, y = index, group = suburb_name, colour = gccsa_name)) + 
    geom_line(alpha = 0.2) +
    geom_text_repel(data = index_2 %>% 
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1) %>%
                      filter(index > 800),
                    aes(label = suburb_name)) + 
    expand_limits(x = max(index_2$year) + 2) +
    ggtitle("Everywhere Makes Money") +
    theme_minimal()+
    theme(legend.position="bottom")
  
  # Graph of just top 10 suburbs in greater sydney
  ggplot(data = index_2 %>% 
           filter(gccsa_name == "Greater Sydney") %>% 
           arrange(desc(top)) %>%
           head(200),
         aes(x = year, y = index, group = suburb_name, colour = sa4_name)) + 
    geom_line(alpha = 0.2) +
    geom_text_repel(data = index_2 %>% 
                      filter(gccsa_name == "Greater Sydney") %>% 
                      arrange(desc(top)) %>%
                      head(200) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name)) + 
    expand_limits(x = max(index_2$year) + 3)
  
  
  # Graph of just top 10 suburbs outside sydney
  ggplot(data = index_2 %>% 
           filter(gccsa_name == "Rest of NSW") %>% 
           arrange(desc(top)) %>%
           head(200),
         aes(x = year, y = index, group = suburb_name, colour = sa4_name)) + 
    geom_line(alpha = 0.2) +
    geom_text_repel(data = index_2 %>% 
                      filter(gccsa_name == "Rest of NSW") %>% 
                      arrange(desc(top)) %>%
                      head(200) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name)) + 
    expand_limits(x = max(index_2$year) + 3)
  
  
  # Trying to create the previous 2 graphs but faceted
  ggplot(data = index_2 %>%
           arrange(desc(top)) %>%
           head(400),
         aes(x = year, y = index, group = suburb_name, colour = sa4_name)) + 
    geom_line(alpha = 0.2) +
    facet_wrap( ~ gccsa_name) +
    geom_text_repel(data = index_2 %>% 
                      arrange(desc(top)) %>%
                      head(400) %>%
                      group_by(suburb_name) %>%
                      arrange(desc(year)) %>%
                      slice(1),
                    aes(x = year + 0.2, label = suburb_name)) + 
    expand_limits(x = max(index_2$year) + 3)
  
  ggplot(index_3,aes(x = year, y = nsw_control_index, group = suburb_name, colour = gccsa_name)) + 
    geom_line(alpha = 0.3)
  
  ggplot(index_4,aes(year,control_index, group = suburb_name)) + 
    geom_line(alpha = 0.2) +
    geom_text(data = index_4 %>% 
                group_by(suburb_name) %>%
                arrange(desc(year)) %>%
                slice(1),
              aes(x = year + 0.2, label = suburb_name, hjust = 0)) + 
    expand_limits(x = max(index_4$year) + 3)
  
  saveRDS(index_4,"rmarkdown/rmarkdown_data.rds")
  
  
# [4] ---- A bit of modelling ---- 
  
  ## Add back nsw_control_index to data 
  
  data_2 <- index_3 %>%
    select(suburb_name,year,nsw_control_index) %>%
    left_join(data)
  
  # Simple linear model
  simple_linear_model <- lm(data = data_2,formula = nsw_control_index ~ year +
                              log_crime_score + 
                              green_score_decile +
                              education_score + 
                              usual_resident_population + 
                              dwelling_density + 
                              seifa_econ_disadvantage + 
                              seifa_econ_adv_disadv + 
                              seifa_econ_resources + 
                              seifa_education_occupation + 
                              median_land_value_per_sqm + 
                              working_age_proportion)
  
  summary(simple_linear_model)
  
  # Random Forest
  
  random_forest_data <- data_2 %>%
    select(3,2,12:17,19:21,23:29)
  
  base_random_forest <- randomForest(data = random_forest_data,
                                     y = random_forest_data[,1],
                                     x = random_forest_data[,2:18])
    
    ?randomForest
  
  
  # to package up
  
  rmarkdown::render_site(input = "rmarkdown")
  