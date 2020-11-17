#
# Similarity App
#

# [0] ---- Load packages ----
    library(shiny)
    library(sf)
    library(leaflet)
    library(leafpop)
    library(rgdal)
    library(dplyr)
    library(ggplot2)
    library(shinycssloaders)
    library(markdown)
    library(RColorBrewer)
    library(pdist)
    library(shinyjs)
    library(purrr)
    library(stringr)
    library(shinyWidgets)
    library(tibble)
    library(tidyr)

# [1] ---- Load global data ----

    map <- readRDS("data/simple_map.rds")
    
    choices <- readRDS("data/choices.rds")
    
    suburb_choice <- choices %>% distinct(suburb_name) %>% arrange(suburb_name) %>% pull()
    
    unscaled_data <- readRDS("data/unscaled_data.rds") %>%
        select(suburb_name,sa2_name,sa3_name,sa4_name,year,suburb_area_sqkm,log_crime_score,education_score,green_score_decile,
               usual_resident_population,working_age_proportion,senior_citizen_proportion,
               public_transport_proportion,motor_vehicle_proportion,bicycle_walking_proportion,
               house_and_semi_proportion,unit_proportion,dwelling_density,
               seifa_econ_disadvantage,seifa_econ_adv_disadv,
               seifa_econ_resources,seifa_education_occupation,
               median_land_value_per_sqm,house_median_suburb,apartment_median_suburb) ##### LIMITED TO CERTAIN VARIABLES - COULD ADJUST
    
    select_scaled_data <- readRDS("data/select_scaled_data.rds") %>%
        select(suburb_name,sa2_name,sa3_name,sa4_name,suburb_area_sqkm,log_crime_score,education_score,green_score_decile,
               usual_resident_population,working_age_proportion,senior_citizen_proportion,
               public_transport_proportion,motor_vehicle_proportion,bicycle_walking_proportion,
               house_and_semi_proportion,unit_proportion,dwelling_density,
               seifa_econ_disadvantage,seifa_econ_adv_disadv,
               seifa_econ_resources,seifa_education_occupation,
               median_land_value_per_sqm,house_median_suburb,apartment_median_suburb)              ##### LIMITED TO CERTAIN VARIABLES - COULD ADJUST
    
    select_scaler <- readRDS("data/select_scaling_data.rds") %>%
        filter(variable %in% c("suburb_area_sqkm","log_crime_score","education_score","green_score_decile",
                               "usual_resident_population","working_age_proportion","senior_citizen_proportion",
                               "public_transport_proportion","motor_vehicle_proportion","bicycle_walking_proportion",
                               "house_and_semi_proportion","unit_proportion","dwelling_density",
                               "seifa_econ_disadvantage","seifa_econ_adv_disadv",
                               "seifa_econ_resources","seifa_education_occupation",
                               "median_land_value_per_sqm","house_median_suburb","apartment_median_suburb")) ##### LIMITED TO CERTAIN VARIABLES - COULD ADJUST
    
    comparison_scaled_data <- readRDS("data/comparison_scaled_data.rds") %>%
        select(suburb_name,sa2_name,sa3_name,sa4_name,year,suburb_area_sqkm,log_crime_score,education_score,green_score_decile,
               usual_resident_population,working_age_proportion,senior_citizen_proportion,
               public_transport_proportion,motor_vehicle_proportion,bicycle_walking_proportion,
               house_and_semi_proportion,unit_proportion,dwelling_density,
               seifa_econ_disadvantage,seifa_econ_adv_disadv,
               seifa_econ_resources,seifa_education_occupation,
               median_land_value_per_sqm,house_median_suburb,apartment_median_suburb)               ##### LIMITED TO CERTAIN VARIABLES - COULD ADJUST
    
    comparison_scaler <- readRDS("data/comparison_scaling_data.rds") %>%
        filter(variable %in% c("suburb_area_sqkm","log_crime_score","education_score","green_score_decile",
                               "usual_resident_population","working_age_proportion","senior_citizen_proportion",
                               "public_transport_proportion","motor_vehicle_proportion","bicycle_walking_proportion",
                               "house_and_semi_proportion","unit_proportion","dwelling_density",
                               "seifa_econ_disadvantage","seifa_econ_adv_disadv",
                               "seifa_econ_resources","seifa_education_occupation",
                               "median_land_value_per_sqm","house_median_suburb","apartment_median_suburb")) ##### LIMITED TO CERTAIN VARIABLES - COULD ADJUST
    
    data_sources <- readRDS("data/data_sources.rds")

## Function to calculate similarity despite NA's
    rdist_na <- function(X,Y){
        if (!is.matrix(X)) 
            X = as.matrix(X)
        if (!is.matrix(Y)) 
            Y = as.matrix(Y)
        distances <- matrix(pdist(X,Y)@dist, ncol=nrow(X), byrow = TRUE)
        #count NAs
        na.count <- sapply(1:nrow(X),function(i){rowSums(is.na(Y) | is.na(X[i,]))})
        #scaling to number of cols
        distances * sqrt(ncol(X)/(ncol(X) - na.count))
    }

# [2] ---- Define UI ----

    ui <- navbarPage("Suburb Similarity",
                     tabPanel("Introduction",
                              titlePanel("Investigating Suburb Similarity"),
                              fluidRow(column(10, offset = 1,
                                              includeMarkdown("data/markdown.Rmd"),
                                              dataTableOutput("intro_table")))
                     ),
                     tabPanel("Compare to Suburbs",
                              sidebarLayout(
                                  sidebarPanel(htmlOutput("calculate_2"),
                                               br(),
                                               htmlOutput("year"),
                                               selectizeInput("suburb", label = "Search for a suburb:", choices = suburb_choice),
                                               htmlOutput("number_2"),
                                               materialSwitch(inputId = "include_2", label = "Exclude Price Metrics:", value = FALSE, status = "danger")
                                  ),
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                      withSpinner(leafletOutput(outputId = 'map_2', height = 600)),
                                      withSpinner(dataTableOutput(outputId = 'table_2'))
                                  )
                              )
                     ),
                     tabPanel("Compare to Selection",
                              sidebarLayout(
                                  sidebarPanel(
                                      shinyjs::useShinyjs(),
                                      id = "side-panel",
                                      htmlOutput("calculate", inline = TRUE),
                                      htmlOutput("reset", inline = TRUE),
                                      br(),
                                      br(),
                                      materialSwitch(inputId = "include", label = "Exclude Price Metrics:", value = FALSE, status = "danger"),
                                      htmlOutput("number"),
                                      htmlOutput("size"),
                                      htmlOutput("crime"),
                                      htmlOutput("education"),
                                      htmlOutput("green"),
                                      htmlOutput("population"),
                                      htmlOutput("working"),
                                      htmlOutput("seniors"),
                                      htmlOutput("public_transport"),
                                      htmlOutput("motor_vehicle"),
                                      htmlOutput("bicycle_walking"),
                                      htmlOutput("house"),
                                      htmlOutput("unit"),
                                      htmlOutput("density"),
                                      htmlOutput("seifa_1"),
                                      htmlOutput("seifa_2"),
                                      htmlOutput("seifa_3"),
                                      htmlOutput("seifa_4"),
                                      htmlOutput("land_sqm"),
                                      htmlOutput("house_median"),
                                      htmlOutput("unit_median")
                                  ),
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                      withSpinner(leafletOutput(outputId = 'map_1', height = 600)),
                                      withSpinner(dataTableOutput(outputId = 'table_1'))
                                  )
                              )
                     )
                     )
    
# [3] ---- Define Server ----
    
    server <- function(input, output, session) {

# [4] ---- Intro Page ----
        
        output$intro_table <- renderDataTable(escape = FALSE, data_sources,
                                              options = list(lengthChange = FALSE,
                                                             searching = FALSE,
                                                             paging = FALSE,
                                                             info = FALSE))

# [5] ---- Establish the Reactive UI components for the 'Suburb' tab ----    
        
    output$calculate_2 = renderUI({
        actionButton(inputId = "calculate_2", label = "Calculate Similarity")
    })
    output$number_2 = renderUI({
        sliderInput(inputId = "number_2",
                    label = "Number of Suburbs Displayed:",
                    min = 10,
                    max = 500,
                    value = 10)
    })
    updateSelectizeInput(session, "suburb", choices = suburb_choice, server = TRUE)
        
    output$year = renderUI({
        selectInput(inputId = "year",
                    label = "Year:",
                    choices = c(2019,2018,2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007,2006),
                    selected = 1)
    })                               
        
# [6] ---- Establish the Reactive UI components for the 'Select' tab ----
    
    selected_suburb <- reactive({
        if(is.null(input$year)) {
            unscaled_data %>% 
                filter(suburb_name == input$suburb) %>%
                filter(year == 2019)
        }
        else{
            unscaled_data %>% 
                filter(suburb_name == input$suburb) %>%
                filter(year == input$year)
        }
    })    
    
    output$calculate = renderUI({
        actionButton(inputId = "calculate", label = "Calculate Similarity")
    })
    output$reset = renderUI({
        actionButton(inputId = "reset", label = paste0("Reset to ",input$suburb," in ", ifelse(is.null(input$year),2019,input$year)))
    })
    output$number = renderUI({
        sliderInput(inputId = "number",
                    label = "Number of Suburbs Displayed:",
                    min = 10,
                    max = 500,
                    value = 10)
    })
    output$size = renderUI({
        sliderInput(inputId = "size",
                    label = "Size (Km2):",
                    min = 0,
                    max = 20000,
                    value = selected_suburb() %>% select(suburb_area_sqkm) %>% pull(),
                    step = 10)
    })
    output$crime = renderUI({
        sliderInput(inputId = "crime",
                    label = "Crime Score:",
                    min = select_scaler %>% filter(variable == "log_crime_score") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "log_crime_score") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(log_crime_score) %>% pull())
    })
    output$education = renderUI({
        sliderInput(inputId = "education",
                    label = "Education Score:",
                    min = select_scaler %>% filter(variable == "education_score") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "education_score") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(education_score) %>% pull())
    })
    output$green = renderUI({
        sliderInput(inputId = "green",
                    label = "Green Score:",
                    min = select_scaler %>% filter(variable == "green_score_decile") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "green_score_decile") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(green_score_decile) %>% pull())
    })
    output$population = renderUI({
        sliderInput(inputId = "population",
                    label = "Population:",
                    min = select_scaler %>% filter(variable == "usual_resident_population") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "usual_resident_population") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(usual_resident_population) %>% pull())
    })
    output$working = renderUI({
        sliderInput(inputId = "working",
                    label = "Working Age Proportion:",
                    min = select_scaler %>% filter(variable == "working_age_proportion") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "working_age_proportion") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(working_age_proportion) %>% pull())
    })
    output$seniors = renderUI({
        sliderInput(inputId = "seniors",
                    label = "Senior Citizen Proportion:",
                    min = select_scaler %>% filter(variable == "senior_citizen_proportion") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "senior_citizen_proportion") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(senior_citizen_proportion) %>% pull())
    })
    output$public_transport = renderUI({
        sliderInput(inputId = "public_transport",
                    label = "Proportion of Journeys to Work via Public Transport:",
                    min = select_scaler %>% filter(variable == "public_transport_proportion") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "public_transport_proportion") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(public_transport_proportion) %>% pull())
    })
    output$motor_vehicle = renderUI({
        sliderInput(inputId = "motor_vehicle",
                    label = "Proportion of Journeys to Work via Motor Vehicle:",
                    min = select_scaler %>% filter(variable == "motor_vehicle_proportion") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "motor_vehicle_proportion") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(motor_vehicle_proportion) %>% pull())
    })
    output$bicycle_walking = renderUI({
        sliderInput(inputId = "bicycle_walking",
                    label = "Proportion of Journeys to Work by Bike or Walking:",
                    min = select_scaler %>% filter(variable == "bicycle_walking_proportion") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "bicycle_walking_proportion") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(bicycle_walking_proportion) %>% pull())
    })
    output$house = renderUI({
        sliderInput(inputId = "house",
                    label = "Proportion of Dwellings as Houses:",
                    min = select_scaler %>% filter(variable == "house_and_semi_proportion") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "house_and_semi_proportion") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(house_and_semi_proportion) %>% pull())
    })
    output$unit = renderUI({
        sliderInput(inputId = "unit",
                    label = "Proportion of Dwellings as Units:",
                    min = select_scaler %>% filter(variable == "unit_proportion") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "unit_proportion") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(unit_proportion) %>% pull())
    })
    output$density = renderUI({
        sliderInput(inputId = "density",
                    label = "Dwelling Density (Dwellings per km2):",
                    min = select_scaler %>% filter(variable == "dwelling_density") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "dwelling_density") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(dwelling_density) %>% pull())
    })
    output$seifa_1 = renderUI({
        sliderInput(inputId = "seifa_1",
                    label = "SEIFA - Socio-Economic Disadvantage:",
                    min = select_scaler %>% filter(variable == "seifa_econ_disadvantage") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "seifa_econ_disadvantage") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(seifa_econ_disadvantage) %>% pull())
    })
    output$seifa_2 = renderUI({
        sliderInput(inputId = "seifa_2",
                    label = "SEIFA - Socio-Economic Advantage/Disadvantage:",
                    min = select_scaler %>% filter(variable == "seifa_econ_adv_disadv") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "seifa_econ_adv_disadv") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(seifa_econ_adv_disadv) %>% pull())
    })
    output$seifa_3 = renderUI({
        sliderInput(inputId = "seifa_3",
                    label = "SEIFA - Economic Resources:",
                    min = select_scaler %>% filter(variable == "seifa_econ_resources") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "seifa_econ_resources") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(seifa_econ_resources) %>% pull())
    })
    output$seifa_4 = renderUI({
        sliderInput(inputId = "seifa_4",
                    label = "SEIFA - Education & Occupation:",
                    min = select_scaler %>% filter(variable == "seifa_education_occupation") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "seifa_education_occupation") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(seifa_education_occupation) %>% pull())
    })
    output$land_sqm = renderUI({
        sliderInput(inputId = "land_sqm",
                    label = "Land Value per Square Metre:",
                    min = select_scaler %>% filter(variable == "median_land_value_per_sqm") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "median_land_value_per_sqm") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(median_land_value_per_sqm) %>% pull())
    })
    output$house_median = renderUI({
        sliderInput(inputId = "house_median",
                    label = "Median House Value:",
                    min = select_scaler %>% filter(variable == "house_median_suburb") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "house_median_suburb") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(house_median_suburb) %>% pull())
    })
    output$unit_median = renderUI({
        sliderInput(inputId = "unit_median",
                    label = "Median Unit Value:",
                    min = select_scaler %>% filter(variable == "apartment_median_suburb") %>% select(min) %>% pull(),
                    max = select_scaler %>% filter(variable == "apartment_median_suburb") %>% select(max) %>% pull(),
                    value = selected_suburb() %>% select(apartment_median_suburb) %>% pull())
    })
    

# [7] ---- Calculate the Suburb Similarity Tab ----
    
    chosen_suburb <- eventReactive(input$calculate_2,{
        
        scaled_values <- comparison_scaled_data %>%
            filter(year == input$year) %>%
            filter(suburb_name == input$suburb)
        
        other_suburbs <- comparison_scaled_data %>%
            filter(year == 2019) %>%
            filter(suburb_name != input$suburb)
        
        if (input$include_2 == FALSE) {
            na_count <- other_suburbs %>%
                mutate(na_count = rowSums(is.na(other_suburbs))) %>%
                select(na_count)
        
            dist <- as.data.frame(rdist_na(scaled_values[,6:25],other_suburbs[,6:25])) %>%
                dplyr::rename(distance = V1) %>%
                cbind(na_count) %>%
                mutate(similarity = round(1/(1+(distance/(20-na_count))),4)) %>%
                select(distance,similarity)
        }
        else {
            na_count_temp <- other_suburbs %>%
                select(-c(median_land_value_per_sqm,house_median_suburb,apartment_median_suburb))
            
            na_count <- na_count_temp %>%
                mutate(na_count = rowSums(is.na(na_count_temp))) %>%
                select(na_count)
            
            dist <- as.data.frame(rdist_na(scaled_values[,6:22],other_suburbs[,6:22])) %>%
                dplyr::rename(distance = V1) %>%
                cbind(na_count) %>%
                mutate(similarity = round(1/(1+(distance/(17-na_count))),4)) %>%
                select(distance,similarity)
        }
            
        combined <- other_suburbs %>% 
                select(suburb_name,sa2_name,sa3_name,sa4_name) %>%
                cbind(dist) 
            
        combined
    })
    
    number_of_suburbs_2 <- eventReactive(input$calculate_2,{
        input$number_2
    })
    
    output$table_2 <- renderDataTable({
        chosen_suburb() %>%
            arrange(desc(similarity)) %>%
            head(n=number_of_suburbs_2())
    })
    
    output$map_2 <- renderLeaflet({
        leaflet(map) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            #addProviderTiles(providers$Wikimedia) %>%
            setView(146.9211,-33.2532, zoom = 6)
    })
    
    observeEvent(input$calculate_2,{
        top_n <- chosen_suburb() %>%
            arrange(desc(similarity)) %>%
            head(n=number_of_suburbs_2())
        map_small <- map %>%
            left_join(top_n,by = c("suburb_name","sa2_name","sa3_name","sa4_name")) %>%
            filter(!is.na(similarity)) %>%
            arrange(desc(similarity))
        map_presentation <- map_small %>%
            set_names(~ str_replace_all(.,"_"," ") %>%
                          str_to_title()) #%>%
            #dplyr::rename(geometry = Geometry)
        top <- map_small %>%
            st_centroid(geometry)
        top_lat <- top$geometry[[1]][1]
        top_lng <- top$geometry[[1]][2]
        
        pal <- colorNumeric(palette = c("white","red"),domain = chosen_suburb()$similarity)
        
        leafletProxy("map_2") %>%
            clearShapes() %>%
            flyTo(top,lng = top_lat,lat = top_lng,zoom = 10) %>%
            addPolygons(data = map_small,
                        weight = 1, 
                        fillColor = ~pal(similarity), 
                        color = "black",
                        opacity = 1,
                        fillOpacity = 0.8,
                        popup = leafpop::popupTable(map_presentation,  zcol = c(2,8,11:16,18:20,22:28,30,37,38,42),feature.id = FALSE, row.numbers = FALSE))
    })
    
# [8] ---- Calculate the Similarity scores tab ----
    
    observeEvent(input$reset, {
        shinyjs::reset("side-panel")
    })
    
    new_values <- eventReactive(input$calculate,{
        
        new <- tibble(new_values = c(input$size,input$crime,input$education,input$green,
                                    input$population,input$working,input$seniors,input$public_transport,
                                    input$motor_vehicle,input$bicycle_walking,input$house,
                                    input$unit,input$density,input$seifa_1,input$seifa_2,
                                    input$seifa_3,input$seifa_4,input$land_sqm,input$house_median,
                                    input$unit_median)) %>%
            cbind(select_scaler) %>%
            mutate(scaled_value = (new_values - mean) / sd) %>%
            select(scaled_value) %>% 
            rownames_to_column %>%
            gather(variable, value, -rowname) %>% 
            spread(rowname, value) %>%
            select(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`)
        
        suburb <- select_scaled_data
        
        if (input$include == FALSE) {
            na_count <- suburb %>%
                mutate(na_count = rowSums(is.na(suburb))) %>%
                select(na_count)
            
            dist <- as.data.frame(rdist_na(new,suburb[,5:24])) %>%
                dplyr::rename(distance = V1) %>%
                cbind(na_count) %>%
                mutate(similarity = round(1/(1+(distance/(20-na_count))),4)) %>%
                select(distance,similarity)  
        }
        else {
            na_count_temp <- suburb %>%
                select(-c(median_land_value_per_sqm,house_median_suburb,apartment_median_suburb))
                
            na_count <- na_count_temp %>%
                mutate(na_count = rowSums(is.na(na_count_temp))) %>%
                select(na_count)
            
            dist <- as.data.frame(rdist_na(new[,1:17],suburb[,5:21])) %>%
                dplyr::rename(distance = V1) %>%
                cbind(na_count) %>%
                mutate(similarity = round(1/(1+(distance/(17-na_count))),4)) %>%
                select(distance,similarity)
        }
            
        combined <- suburb %>% 
                select(suburb_name,sa2_name,sa3_name,sa4_name) %>%
                cbind(dist) 
            
        combined
    })
    
    number_of_suburbs <- eventReactive(input$calculate,{
        input$number
    })
    
    output$table_1 <- renderDataTable({
        new_values() %>%
            arrange(desc(similarity)) %>%
            head(n=number_of_suburbs())
    })
        
    output$map_1 <- renderLeaflet({
        leaflet(map,options = leafletOptions(minZoom = 6)) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            #addProviderTiles(providers$Wikimedia) %>%
            setView(146.9211,-33.2532, zoom = 6) 
    })
    
    observeEvent(input$calculate,{
        top_n <- new_values() %>%
            arrange(desc(similarity)) %>%
            head(n=number_of_suburbs())
        map_small <- map %>%
            left_join(top_n,by = c("suburb_name","sa2_name","sa3_name","sa4_name")) %>%
            filter(!is.na(similarity)) %>%
            arrange(desc(similarity)) 
        map_presentation <- map_small %>%
            set_names(~ str_replace_all(.,"_"," ") %>%
                          str_to_title()) #%>%
            #dplyr::rename(geometry = Geometry)
        top <- map_small %>%
            st_centroid(geometry)
        top_lat <- top$geometry[[1]][1]
        top_lng <- top$geometry[[1]][2]
        
        pal <- colorNumeric(palette = c("white","red"),domain = new_values()$similarity)
        
        leafletProxy("map_1") %>%
            clearShapes() %>%
            flyTo(top,lng = top_lat,lat = top_lng,zoom = 10) %>%
            addPolygons(data = map_small,
                        weight = 1, 
                        fillColor = ~pal(similarity), 
                        color = "black",
                        opacity = 1,
                        fillOpacity = 0.8,
                        popup = leafpop::popupTable(map_presentation,  zcol = c(2,8,11:16,18:20,22:28,30,37,38,42),feature.id = FALSE, row.numbers = FALSE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
