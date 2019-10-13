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
  library(parsnip)

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
  
  
# [3] ---- Load Data ----
  
  data <- readRDS("rmarkdown/rmarkdown_data.rds")
  units <- readRDS("rmarkdown/units.rds")
  sydney <- readRDS("rmarkdown/map_sydney.rds")
  rest <- readRDS("rmarkdown/map_rest.rds")
  centroid <- readRDS("rmarkdown/centroids.rds")
  
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
  
#  [4][3] ---- Getting Rich off the Lockout ----
  
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

  
#  [5][1] ---- Variable Importance ----
  
  data_set_year <- tibble(`Data Set` = c("Property Sale Information",
                                         "Property Land Information",
                                         "Recorded Crime by Offence",
                                         "SEIFA",
                                         "Census",
                                         "NSW Government & Non-Government Schools",
                                         "Mesh Blocks",
                                         "Metro ARIA"),
                          `Timeframe` = c("1990 - 2019",
                                          "2012 - 2018",
                                          "1995 - 2018",
                                          "2001, 2006, 2011, 2016",
                                          "2006, 2011, 2016",
                                          "2016",
                                          "2016",
                                          "2014"),
                          Explanation = c("Yearly values were calculated using the average of a rolling 12-month window (calculated quarterly from the previous 12 months of data).",
                                          "Values updated annually by NSW Valuer General - Extrapolated from 2018 to 2019.",
                                          "Yearly values were aggregated from monthly totals - Extrapolated from 2018 to 2019.",
                                          "Interpolated values between 2001 and 2016 and then extrapolated from 2016 - 2019.",
                                          "Interpolated values between 2006 and 2016 and then extrapolated from 2016 - 2019.",
                                          "Government & Non-Government schools were combined with Government schools to build a proxy of education, but this comes from a single year reference point. The calculated value was allocated to all years.",
                                          "Meshblocks are relevant for the Green Space calculation, but provide only a single year. This value was allocated to all years.",
                                          "Only a single year is available and so this value was allocated to all years going forward."))
  
  saveRDS(data_set_year,"rmarkdown/data_set_year.rds")
  
  data_set_allocation <- tibble(variable = c("violent_crime","dasg_crime","log_crime_score", # Crime
                                             "education_score", # education
                                             "green_score","green_score_decile", # green space
                                             "usual_resident_population","seifa_econ_disadvantage","seifa_econ_adv_disadv", # SEIFA
                                             "seifa_econ_resources", "seifa_education_occupation", # SEIFA
                                             "working_age_proportion","senior_citizen_proportion", # Census
                                             "confirmed_journeys","public_transport_proportion", # Census
                                             "motor_vehicle_proportion","bicycle_walking_proportion", # Census
                                             "confirmed_dwellings", "house_and_semi_proportion", # Census
                                             "unit_proportion", "dwelling_density",  # Census
                                             "median_land_value","median_land_value_per_sqm", # Land Values
                                             "aria_overall", "aria_education", "aria_health",
                                             "aria_shopping", "aria_public_transport", "aria_financial_postal", # ARIA
                                             "annual_turnover","year"),
                                data_set = c("Recorded Crime by Offence","Recorded Crime by Offence","Recorded Crime by Offence",
                                             "NSW Government & Non-Government Schools",
                                             "Mesh Blocks","Mesh Blocks",
                                             "SEIFA","SEIFA","SEIFA","SEIFA","SEIFA",
                                             "Census","Census","Census","Census","Census",
                                             "Census","Census","Census","Census","Census",
                                             "Property Land Information","Property Land Information",
                                             "Metro ARIA","Metro ARIA","Metro ARIA","Metro ARIA","Metro ARIA","Metro ARIA",
                                             "Property Sale Information","Time"))
  
  saveRDS(data_set_allocation,"rmarkdown/data_set_allocation.rds")
  
  #  [5][2] ---- From 2001 ----
  
  house_data <- data %>%
    select(suburb_name,year,nsw_control_index) %>%
    left_join(data_raw, by = c("suburb_name", "year"))
  
  unit_data <- units %>%
    select(suburb_name,year,nsw_control_index) %>%
    left_join(data_raw, by = c("suburb_name", "year"))
  
  house_data_2001_rf <- house_data %>%
    filter(year >= 2001) %>%
    select(nsw_control_index,year,
           violent_crime,dasg_crime,log_crime_score,
           #education_score,#green_score_decile,
           usual_resident_population,
           #working_age_proportion,senior_citizen_proportion,confirmed_journeys,
           #public_transport_proportion,#motor_vehicle_proportion,bicycle_walking_proportion,
           #confirmed_dwellings,#house_and_semi_proportion,unit_proportion,dwelling_density,
           seifa_econ_disadvantage,seifa_econ_adv_disadv,seifa_econ_resources,seifa_education_occupation,
           #median_land_value_per_sqm,
           #aria_overall,aria_education,aria_health,aria_shopping,
           #aria_public_transport,aria_financial_postal,
           annual_turnover)
  
  unit_data_2001_rf <- unit_data %>%
    filter(year >= 2001) %>%
    select(nsw_control_index,year,
           violent_crime,dasg_crime,log_crime_score,
           #education_score,#green_score_decile,
           usual_resident_population,
           #working_age_proportion,senior_citizen_proportion,confirmed_journeys,
           #public_transport_proportion,#motor_vehicle_proportion,bicycle_walking_proportion,
           #confirmed_dwellings,#house_and_semi_proportion,unit_proportion,dwelling_density,
           seifa_econ_disadvantage,seifa_econ_adv_disadv,seifa_econ_resources,seifa_education_occupation,
           #median_land_value_per_sqm,
           #aria_overall,aria_education,aria_health,aria_shopping,
           #aria_public_transport,aria_financial_postal,
           annual_turnover)
  
  rf_reg <- rand_forest(mode = "regression",min_n = 50) %>%
    set_engine("randomForest")
  
  house_2001_rf_fit <- rf_reg %>%
    fit(data = house_data_2001_rf, formula = nsw_control_index ~ .)
  
  unit_2001_rf_fit <- rf_reg %>%
    fit(data = unit_data_2001_rf, formula = nsw_control_index ~ .)
  
  rf_imp_house_2001 <- as_tibble(house_2001_rf_fit$fit$importance, rownames = "variable") %>%
    mutate(importance = (IncNodePurity - min(IncNodePurity)) / (max(IncNodePurity) - min(IncNodePurity))) %>%
    left_join(data_set_allocation, by = "variable")
  
  rf_imp_unit_2001 <- as_tibble(unit_2001_rf_fit$fit$importance, rownames = "variable") %>%
    mutate(importance = (IncNodePurity - min(IncNodePurity)) / (max(IncNodePurity) - min(IncNodePurity))) %>%
    left_join(data_set_allocation, by = "variable")
  
  saveRDS(house_2001_rf_fit,"rmarkdown/house_2001_rf_fit.rds")
  saveRDS(unit_2001_rf_fit,"rmarkdown/unit_2001_rf_fit.rds")
  
  ggplot(rf_imp_house_2001,aes(x = reorder(variable,importance),y = importance,fill = data_set)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    ggtitle("Houses - Variable Importance 2001 - 2019") +
    labs(y = "Normalised Variable Importance", x = "Variable", fill = "Data Set") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  ggplot(rf_imp_unit_2001,aes(x = reorder(variable,importance),y = importance,fill = data_set)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    ggtitle("Apartments - Variable Importance 2001 - 2019") +
    labs(y = "Normalised Variable Importance", x = "Variable", fill = "Data Set") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  round(max(house_2001_rf_fit$fit$rsq),3)*100
  round(max(unit_2001_rf_fit$fit$rsq),3)*100
  
  #  [5][3] ---- From 2006 ----
  
  house_data_2006_rf <- house_data %>%
    filter(year >= 2006) %>%
    select(nsw_control_index,year,
           violent_crime,dasg_crime,log_crime_score,
           #education_score,#green_score_decile,
           usual_resident_population,
           working_age_proportion,senior_citizen_proportion,confirmed_journeys,
           public_transport_proportion,motor_vehicle_proportion,bicycle_walking_proportion,
           confirmed_dwellings,house_and_semi_proportion,unit_proportion,dwelling_density,
           seifa_econ_disadvantage,seifa_econ_adv_disadv,seifa_econ_resources,seifa_education_occupation,
           #median_land_value_per_sqm,
           #aria_overall,aria_education,aria_health,aria_shopping,
           #aria_public_transport,aria_financial_postal,
           annual_turnover)
  
  unit_data_2006_rf <- unit_data %>%
    filter(year >= 2006) %>%
    select(nsw_control_index,year,
           violent_crime,dasg_crime,log_crime_score,
           #education_score,#green_score_decile,
           usual_resident_population,
           working_age_proportion,senior_citizen_proportion,confirmed_journeys,
           public_transport_proportion,motor_vehicle_proportion,bicycle_walking_proportion,
           confirmed_dwellings,house_and_semi_proportion,unit_proportion,dwelling_density,
           seifa_econ_disadvantage,seifa_econ_adv_disadv,seifa_econ_resources,seifa_education_occupation,
           #median_land_value_per_sqm,
           #aria_overall,aria_education,aria_health,aria_shopping,
           #aria_public_transport,aria_financial_postal,
           annual_turnover)
  
  house_2006_rf_fit <- rf_reg %>%
    fit(data = house_data_2006_rf, formula = nsw_control_index ~ .)
  
  unit_2006_rf_fit <- rf_reg %>%
    fit(data = unit_data_2006_rf, formula = nsw_control_index ~ .)
  
  rf_imp_house_2006 <- as_tibble(house_2006_rf_fit$fit$importance, rownames = "variable") %>%
    mutate(importance = (IncNodePurity - min(IncNodePurity)) / (max(IncNodePurity) - min(IncNodePurity))) %>%
    left_join(data_set_allocation, by = "variable")
  
  rf_imp_unit_2006 <- as_tibble(unit_2006_rf_fit$fit$importance, rownames = "variable") %>%
    mutate(importance = (IncNodePurity - min(IncNodePurity)) / (max(IncNodePurity) - min(IncNodePurity))) %>%
    left_join(data_set_allocation, by = "variable")
  
  saveRDS(house_2006_rf_fit,"rmarkdown/house_2006_rf_fit.rds")
  saveRDS(unit_2006_rf_fit,"rmarkdown/unit_2006_rf_fit.rds")
  
  ggplot(rf_imp_house_2006,aes(x = reorder(variable,importance),y = importance,fill = data_set)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    ggtitle("Houses - Variable Importance 2006 - 2019") +
    labs(y = "Normalised Variable Importance", x = "Variable", fill = "Data Set") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  ggplot(rf_imp_unit_2006,aes(x = reorder(variable,importance),y = importance,fill = data_set)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    ggtitle("Apartments - Variable Importance 2006 - 2019") +
    labs(y = "Normalised Variable Importance", x = "Variable", fill = "Data Set") +
    theme_minimal()+
    theme(plot.title = element_text(hjust=0.5)) +
    theme(legend.position="bottom") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  round(max(house_2006_rf_fit$fit$rsq),3)*100
  round(max(unit_2006_rf_fit$fit$rsq),3)*100
  
#  [5][4] ---- Linear Model ----
  
  house_data_2006_lm <- house_data %>%
    filter(year >= 2006) %>%
    select(nsw_control_index,year,
           #violent_crime,dasg_crime,log_crime_score,
           #education_score,#green_score_decile,
           usual_resident_population,
           working_age_proportion,senior_citizen_proportion,confirmed_journeys,
           public_transport_proportion,motor_vehicle_proportion,bicycle_walking_proportion,
           confirmed_dwellings,house_and_semi_proportion,unit_proportion,dwelling_density,
           seifa_econ_disadvantage,seifa_econ_adv_disadv,seifa_econ_resources,seifa_education_occupation,
           #median_land_value_per_sqm,
           #aria_overall,aria_education,aria_health,aria_shopping,
           #aria_public_transport,aria_financial_postal,
           #annual_turnover
           )
  
  scaled_house_data_2006_lm <- as_tibble(scale(house_data_2006_lm))
    
  house_2006_lm <- lm(data = scaled_house_data_2006_lm,formula = nsw_control_index ~ year + usual_resident_population +
         working_age_proportion + senior_citizen_proportion + confirmed_journeys + 
         public_transport_proportion + motor_vehicle_proportion + bicycle_walking_proportion + 
         confirmed_dwellings + house_and_semi_proportion + unit_proportion + dwelling_density + 
         seifa_econ_disadvantage + seifa_econ_adv_disadv + seifa_econ_resources + 
         seifa_education_occupation)
  
  house_lm <- summary(house_2006_lm)
  
  saveRDS(house_lm,"rmarkdown/house_lm.rds")
  
  house_2006_lm_coef <- as_tibble(house_lm[["coefficients"]],rownames = "Variable") %>%
    mutate_if(is.numeric,round,4) %>%
    mutate(`House Estimate` = Estimate,
           `House P-Value` = `Pr(>|t|)`) %>%
    select(1,6,7)
  
  unit_data_2006_lm <- unit_data %>%
    filter(year >= 2006) %>%
    select(nsw_control_index,year,
           #violent_crime,dasg_crime,log_crime_score,
           #education_score,#green_score_decile,
           usual_resident_population,
           working_age_proportion,senior_citizen_proportion,confirmed_journeys,
           public_transport_proportion,motor_vehicle_proportion,bicycle_walking_proportion,
           confirmed_dwellings,house_and_semi_proportion,unit_proportion,dwelling_density,
           seifa_econ_disadvantage,seifa_econ_adv_disadv,seifa_econ_resources,seifa_education_occupation,
           #median_land_value_per_sqm,
           #aria_overall,aria_education,aria_health,aria_shopping,
           #aria_public_transport,aria_financial_postal,
           #annual_turnover
    )
 
  scaled_unit_data_2006_lm <- as_tibble(scale(unit_data_2006_lm))
  
  unit_2006_lm <- lm(data = scaled_unit_data_2006_lm,formula = nsw_control_index ~ year + usual_resident_population +
                        working_age_proportion + senior_citizen_proportion + confirmed_journeys + 
                        public_transport_proportion + motor_vehicle_proportion + bicycle_walking_proportion + 
                        confirmed_dwellings + house_and_semi_proportion + unit_proportion + dwelling_density + 
                        seifa_econ_disadvantage + seifa_econ_adv_disadv + seifa_econ_resources + 
                        seifa_education_occupation)
  
  unit_lm <- summary(unit_2006_lm)
  
  unit_2006_lm_coef <- as_tibble(unit_lm[["coefficients"]],rownames = "Variable") %>%
    mutate_if(is.numeric,round,4) %>%
    mutate(`Apartment Estimate` = Estimate,
           `Apartment P-Value` = `Pr(>|t|)`) %>%
    select(1,6,7)
  
  lm_coef <- house_2006_lm_coef %>%
    left_join(unit_2006_lm_coef, by = "Variable")
  
  saveRDS(unit_lm,"rmarkdown/unit_lm.rds")
  
  
  ??cell_spec
  summary(simple_linear_model)

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
  
  data_set_year <- tibble(`Data Set` = c("Property Sale Information",
                                        "Property Land Information",
                                        "Recorded Crime by Offence",
                                        "SEIFA",
                                        "Census",
                                        "NSW Government & Non-Government Schools",
                                        "Mesh Blocks",
                                        "Metro ARIA"),
                         `Timeframe` = c("1990 - 2019",
                                    "2012 - 2018",
                                    "1995 - 2018",
                                    "2001, 2006, 2011, 2016",
                                    "2006, 2011, 2016",
                                    "2016",
                                    "2016",
                                    "2014"),
                         Explanation = c("Yearly values were calculated using the average of a rolling 12-month window (calculated quarterly from the previous 12 months of data)",
                                         "Values updated annually by NSW Valuer General - Extrapolated from 2018 to 2019",
                                         "Yearly values were aggregated from monthly totals - Extrapolated from 2018 to 2019",
                                         "Interpolated values between 2001 and 2016 and then extrapolated from 2016 - 2019",
                                         "Interpolated values between 2006 and 2016 and then extrapolated from 2016 - 2019",
                                         "Government & Non-Government schools were combined with Government schools to build a proxy of education, but this comes from a single year reference point. The calculated value was allocated to all years",
                                         "Meshblocks are relevant for the Green Space calculation, but provide only a single year. This value was allocated to all years",
                                         "Only a single year is available and so this value was allocated to all years going forward"))
  
  ## Add back nsw_control_index to data 
  slice(ordered,63366)
  
  house_data <- data %>%
    select(suburb_name,year,nsw_control_index) %>%
    left_join(data_raw, by = c("suburb_name", "year"))
  
  unit_data <- units %>%
    select(suburb_name,year,nsw_control_index) %>%
    left_join(data_raw, by = c("suburb_name", "year"))
  
  
  ## Correlation
  
  house_data_numeric <- house_data %>%
    select(3,2,10:37,40:41)
  cor_full <- as.data.frame(cor(house_data_numeric, use = "pairwise.complete.obs"))
  cor <- as_tibble(cor_full, rownames = "variable")
  
  ggplot(cor, aes(y = nsw_control_index,x = variable)) +
    geom_bar(stat = "identity") + 
    coord_flip()
  
  # Simple linear model
  simple_linear_model <- lm(data = house_data,formula = nsw_control_index ~ year +
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
  ordered <- data_raw %>%
    arrange(year)
  
  check <- ordered %>%
    map_df(~(data.frame(first = which(!is.na(.x)),
                        na_count = sum(is.na(.x)))),
           .id = "variable")
  
  check_1 <- check %>%
    group_by(variable) %>%
    summarise(first=first(first))
  
  house_data_rf <- house_data_numeric %>%
    select(nsw_control_index,year,log_crime_score,education_score,
           green_score_decile,usual_resident_population,working_age_proportion,     
           senior_citizen_proportion,confirmed_journeys,public_transport_proportion,
           motor_vehicle_proportion,bicycle_walking_proportion,confirmed_dwellings,      
           house_and_semi_proportion,unit_proportion,dwelling_density,
           seifa_econ_disadvantage,seifa_econ_adv_disadv,seifa_econ_resources,     
           seifa_education_occupation,
           #median_land_value_per_sqm,
           #aria_overall,aria_education,aria_health,aria_shopping,
           #aria_public_transport,aria_financial_postal,
           annual_turnover)
  
  rf_reg <- rand_forest(mode = "regression", mtry = 4, trees = 200, min_n = 50) %>%
    set_engine("randomForest")
  
  rf_fit <- rf_reg %>%
    fit(data = house_data_rf, formula = nsw_control_index ~ .)
  
  rf_importance <- as_tibble(rf_fit$fit$importance, rownames = "variable") %>%
    mutate(importance = IncNodePurity) 
  
  ggplot(rf_importance,aes(x = variable,y = importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip()
  
  ?rand_forest
  plot(rf_xy_fit$fit$importance)
  
  varImpPlot(rf_xy_fit)
  randomForest(Species ~ ., data=iris[-na.row,])
  
  base_random_forest <- randomForest(data = house_data_numeric,
                                     y = house_data_numeric[,1],
                                     x = house_data_numeric[,2:32])
    
  base_random_forest <- randomForest(data = house_data_numeric,
                                     nsw_control_index ~ year + violent_crime)
  
  varImpPlot(base_random_forest)
    ?rand_forest
  
  # to package up
  
  rmarkdown::render_site(input = "rmarkdown")
  