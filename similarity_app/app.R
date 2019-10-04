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

# [1] ---- Load global data ----

map <- readRDS("data/simple_map.rds")

scaled_data <- readRDS("data/scaled_data.rds") %>%
    select(1,2,5,6,7,8,9,10,12,13,14,16,17,18,19,20,21,22,24,31,32)             ##### NEED TO REMOVE THIS LATER

scaler <- readRDS("data/scaling_data.rds") %>%
    filter(variable %in% c("suburb_area_sqkm","log_crime_score","education_score","green_score_decile",
                           "usual_resident_population","working_age_proportion","senior_citizen_proportion",
                           "public_transport_proportion","motor_vehicle_proportion","bicycle_walking_proportion",
                           "house_and_semi_proportion","unit_proportion","dwelling_density",
                           "relative_socio_economic_disadvantage_index","relative_socio_economic_adv_disadv_index",
                           "economic_resources_index","education_and_occupation_index",
                           "median_land_value_per_sqm","house_median_suburb","apartment_median_suburb")) ##### NEED TO REMOVE THIS LATER

data <- map %>%
    st_drop_geometry()

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

#pal_similarity <- colorNumeric(palette = c("white","darkred"),domain = data$similarity_score)

# [2] ---- Define UI ----

ui <- navbarPage("Suburb Similarity",
                 tabPanel("Introduction",
                          titlePanel("Welcome to the Suburb Similarity Application"),
                          fluidRow(column(12,
                                          includeMarkdown("data/markdown.Rmd")
                                          )
                                   )
                          ),
                 tabPanel("Selections",
                          sidebarLayout(
                              sidebarPanel(
                                  htmlOutput("calculate", inline = TRUE), # might be able to fix this with the inline argument - set to TRUE
                                  htmlOutput("reset", inline = TRUE), # might be able to fix this with the inline argument - set to TRUE
                                  htmlOutput("number", inline = TRUE),
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
                                  withSpinner(dataTableOutput(outputId = 'test'))
                              )
                          )
                 ),
                 tabPanel("Suburbs",
                          sidebarLayout(
                              sidebarPanel(
                              ),
                              # Show a plot of the generated distribution
                              mainPanel(
                                  withSpinner(leafletOutput(outputId = 'map_2', height = 800))
                              )
                              )
                          )
                 )

# [2] ---- Define Server ----
server <- function(input, output) {

    #### Establish the Reactive UI components
    
    output$calculate = renderUI({
        actionButton(inputId = "calculate", label = "Calculate Similarity")
    })
    output$reset = renderUI({
        actionButton(inputId = "reset", label = "Reset Values")
    })
    output$number = renderUI({
        selectInput(inputId = "number",
                    label = "Number of Suburbs Displayed:",
                    choices = c(10,50,100,500,1000),
                    selected = 10)
    )}
    output$size = renderUI({
        sliderInput(inputId = "size",
                    label = "Size (Km2):",
                    min = 0,
                    max = 20000,
                    value = scaler %>% filter(variable == "suburb_area_sqkm") %>% select(mean) %>% pull(),
                    step = 10)
    })
    output$crime = renderUI({
        sliderInput(inputId = "crime",
                    label = "Crime Score:",
                    min = scaler %>% filter(variable == "log_crime_score") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "log_crime_score") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "log_crime_score") %>% select(mean) %>% pull())
    })
    output$education = renderUI({
        sliderInput(inputId = "education",
                    label = "Education Score:",
                    min = scaler %>% filter(variable == "education_score") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "education_score") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "education_score") %>% select(mean) %>% pull())
    })
    output$green = renderUI({
        sliderInput(inputId = "green",
                    label = "Green Score:",
                    min = scaler %>% filter(variable == "green_score_decile") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "green_score_decile") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "green_score_decile") %>% select(mean) %>% pull())
    })
    output$population = renderUI({
        sliderInput(inputId = "population",
                    label = "Population:",
                    min = scaler %>% filter(variable == "usual_resident_population") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "usual_resident_population") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "usual_resident_population") %>% select(mean) %>% pull())
    })
    output$working = renderUI({
        sliderInput(inputId = "working",
                    label = "Working Age Proportion:",
                    min = scaler %>% filter(variable == "working_age_proportion") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "working_age_proportion") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "working_age_proportion") %>% select(mean) %>% pull())
    })
    output$seniors = renderUI({
        sliderInput(inputId = "seniors",
                    label = "Senior Citizen Proportion:",
                    min = scaler %>% filter(variable == "senior_citizen_proportion") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "senior_citizen_proportion") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "senior_citizen_proportion") %>% select(mean) %>% pull())
    })
    output$public_transport = renderUI({
        sliderInput(inputId = "public_transport",
                    label = "Proportion of Journeys to Work via Public Transport:",
                    min = scaler %>% filter(variable == "public_transport_proportion") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "public_transport_proportion") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "public_transport_proportion") %>% select(mean) %>% pull())
    })
    output$motor_vehicle = renderUI({
        sliderInput(inputId = "motor_vehicle",
                    label = "Proportion of Journeys to Work via Motor Vehicle:",
                    min = scaler %>% filter(variable == "motor_vehicle_proportion") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "motor_vehicle_proportion") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "motor_vehicle_proportion") %>% select(mean) %>% pull())
    })
    output$bicycle_walking = renderUI({
        sliderInput(inputId = "bicycle_walking",
                    label = "Proportion of Journeys to Work by Bike or Walking:",
                    min = scaler %>% filter(variable == "bicycle_walking_proportion") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "bicycle_walking_proportion") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "bicycle_walking_proportion") %>% select(mean) %>% pull())
    })
    output$house = renderUI({
        sliderInput(inputId = "house",
                    label = "Proportion of Dwellings as Houses:",
                    min = scaler %>% filter(variable == "house_and_semi_proportion") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "house_and_semi_proportion") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "house_and_semi_proportion") %>% select(mean) %>% pull())
    })
    output$unit = renderUI({
        sliderInput(inputId = "unit",
                    label = "Proportion of Dwellings as Units:",
                    min = scaler %>% filter(variable == "unit_proportion") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "unit_proportion") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "unit_proportion") %>% select(mean) %>% pull())
    })
    output$density = renderUI({
        sliderInput(inputId = "density",
                    label = "Dwelling Density (Dwellings per km2):",
                    min = scaler %>% filter(variable == "dwelling_density") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "dwelling_density") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "dwelling_density") %>% select(mean) %>% pull())
    })
    output$seifa_1 = renderUI({
        sliderInput(inputId = "seifa_1",
                    label = "SEIFA - Socio-Economic Disadvantage:",
                    min = scaler %>% filter(variable == "relative_socio_economic_disadvantage_index") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "relative_socio_economic_disadvantage_index") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "relative_socio_economic_disadvantage_index") %>% select(mean) %>% pull())
    })
    output$seifa_2 = renderUI({
        sliderInput(inputId = "seifa_2",
                    label = "SEIFA - Socio-Economic Advantage/Disadvantage:",
                    min = scaler %>% filter(variable == "relative_socio_economic_adv_disadv_index") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "relative_socio_economic_adv_disadv_index") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "relative_socio_economic_adv_disadv_index") %>% select(mean) %>% pull())
    })
    output$seifa_3 = renderUI({
        sliderInput(inputId = "seifa_3",
                    label = "SEIFA - Economic Resources:",
                    min = scaler %>% filter(variable == "economic_resources_index") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "economic_resources_index") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "economic_resources_index") %>% select(mean) %>% pull())
    })
    output$seifa_4 = renderUI({
        sliderInput(inputId = "seifa_4",
                    label = "SEIFA - Education & Occupation:",
                    min = scaler %>% filter(variable == "education_and_occupation_index") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "education_and_occupation_index") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "education_and_occupation_index") %>% select(mean) %>% pull())
    })
    output$land_sqm = renderUI({
        sliderInput(inputId = "land_sqm",
                    label = "Land Value per Square Metre:",
                    min = scaler %>% filter(variable == "median_land_value_per_sqm") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "median_land_value_per_sqm") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "median_land_value_per_sqm") %>% select(mean) %>% pull())
    })
    output$house_median = renderUI({
        sliderInput(inputId = "house_median",
                    label = "Median House Value:",
                    min = scaler %>% filter(variable == "house_median_suburb") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "house_median_suburb") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "house_median_suburb") %>% select(mean) %>% pull())
    })
    output$unit_median = renderUI({
        sliderInput(inputId = "unit_median",
                    label = "Median Unit Value:",
                    min = scaler %>% filter(variable == "apartment_median_suburb") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "apartment_median_suburb") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "apartment_median_suburb") %>% select(mean) %>% pull())
    })
    
    ##### Calculate the Similarity scores
    
    observeEvent(input$reset,{
        updateSliderInput(session,'number',value = 10)
        updateSliderInput(session,'crime',value = 0)
        updateSliderInput(session,'education',value = 0)
        updateSliderInput(session,'green',value = 0)
        updateSliderInput(session,'population',value = 0)
        updateSliderInput(session,'working',value = 0)
        updateSliderInput(session,'seniors',value = 0)
        updateSliderInput(session,'public_transport',value = 0)
        updateSliderInput(session,'motor_vehicle',value = 0)
        updateSliderInput(session,'bicycle_walking',value = 0)
        updateSliderInput(session,'house',value = 0)
        updateSliderInput(session,'unit',value = 0)
        updateSliderInput(session,'density',value = 0)
        updateSliderInput(session,'seifa_1',value = 0)
        updateSliderInput(session,'seifa_2',value = 0)
        updateSliderInput(session,'seifa_3',value = 0)
        updateSliderInput(session,'land_sqm',value = 0)
        updateSliderInput(session,'house_median',value = 0)
        updateSliderInput(session,'unit_median',value = 0)
    })
    
    new_values <- eventReactive(input$calculate,{
        new <- tibble(new_values = c(input$size,input$crime,input$education,input$green,
                                     input$population,input$working,input$seniors,input$public_transport,
                                     input$motor_vehicle,input$bicycle_walking,input$house,
                                     input$unit,input$density,input$seifa_1,input$seifa_2,
                                     input$seifa_3,input$seifa_4,input$land_sqm,input$house_median,
                                     input$unit_median)) %>%
            cbind(scaler) %>%
            mutate(scaled_value = (new_values - mean) / sd) %>%
            select(scaled_value) %>% 
            t()
        
        suburb <- scaled_data
        
        z <- as.data.frame(rdist_na(new,suburb[,2:21])) %>%
            mutate(divisor = 1/V1) %>%
            mutate(similarity = (divisor - min(divisor, na.rm = TRUE))/ (max(divisor, na.rm = TRUE) - min(divisor, na.rm = TRUE)))
        
        combined <- suburb %>% 
            select(suburb_name) %>%
            cbind(z) 
        
        combined
    })
    
    output$test <- renderDataTable({
        new_values() %>%
            arrange(desc(similarity)) %>%
            head(input$number)
    })
    
    output$map_1 <- renderLeaflet({
        leaflet(map,options = leafletOptions(minZoom = 6)) %>%
            addTiles() %>%
            setView(146.9211,-33.2532, zoom = 6) 
    })
    
    observeEvent(input$calculate,{
        top_n <- new_values() %>%
            arrange(desc(similarity)) %>%
            head(input$number)
        map_small <- map %>%
            left_join(top_n,by = "suburb_name") %>%
            filter(!is.na(similarity)) %>%
            arrange(desc(similarity))
        top <- map_small %>%
            st_centroid(geometry)
        top_lat <- top$geometry[[1]][1]
        top_lng <- top$geometry[[1]][2]
        
        pal <- colorNumeric(palette = c("white","red"),domain = new_values()$similarity)
        
        leafletProxy("map_1") %>%
            clearShapes() %>%
            flyTo(top,lng = top_lat,lat = top_lng,zoom = 9) %>%
            addPolygons(data = map_small,
                        weight = 1, 
                        fillColor = ~pal(similarity), 
                        color = "black",
                        opacity = 1,
                        fillOpacity = 0.8,
                        popup = popupTable(map_small,  zcol = c(2,4:10,43),feature.id = FALSE, row.numbers = FALSE))
    })
    
    output$map_2 <- renderLeaflet({
        leaflet(map) %>%
            addTiles() %>%
            setView(146.9211,-33.2532, zoom = 6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
