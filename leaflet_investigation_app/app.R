#
# Leaflet version of the investigation app
#

# [0] ---- Load packages ----
    library(shiny)
    library(sf)
    library(leaflet)
    library(leafpop)
    library(dplyr)
    library(shinycssloaders)
    library(purrr)
    library(stringr)

# [1] ---- Load global data ----
 
    map <- readRDS("data/simple_map.rds")
    map_data <- readRDS("data/map_data.rds")
    pal_crime_1 <- colorNumeric(palette = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),domain = map$log_crime_score)
    pal_crime_2 <- colorNumeric(palette = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),domain = map$violent_crime)
    pal_crime_3 <- colorNumeric(palette = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),domain = map$dasg_crime)
    pal_education_1 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$education_score)
    pal_green_1 <- colorNumeric(palette = c("#F21A00","#EBCC2A","darkgreen"),domain = map$green_score_decile)
    pal_green_2 <- colorNumeric(palette = c("#F21A00","#EBCC2A","darkgreen"),domain = map$green_score)
    pal_transport_1 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$public_transport_proportion)
    pal_transport_2 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$motor_vehicle_proportion)
    pal_transport_3 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$bicycle_walking_proportion)
    pal_census_1 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$usual_resident_population)
    pal_census_2 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$working_age_proportion)
    pal_census_3 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$senior_citizen_proportion)
    pal_seifa_1 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$seifa_econ_disadvantage)
    pal_seifa_2 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$seifa_econ_adv_disadv)
    pal_seifa_3 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$seifa_econ_resources)
    pal_seifa_4 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$seifa_education_occupation)
    pal_housing_1 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$house_and_semi_proportion)
    pal_housing_2 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$unit_proportion)
    pal_housing_3 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$dwelling_density)
    pal_prices_1 <- colorNumeric(palette = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),domain = map$median_land_value_per_sqm)
    pal_prices_2 <- colorNumeric(palette = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),domain = map$house_median_suburb)
    pal_prices_3 <- colorNumeric(palette = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),domain = map$apartment_median_suburb)
    pal_aria_1 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$aria_overall)
    pal_aria_2 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$aria_education)
    pal_aria_3 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$aria_health)
    pal_aria_4 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$aria_shopping)
    pal_aria_5 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$aria_public_transport)
    pal_aria_6 <- colorNumeric(palette = c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2"),domain = map$aria_financial_postal)
    
    data_sources <- readRDS("data/data_sources.rds")
    
# [2] ---- Define UI ----

    ui <- navbarPage("Leaflet Investigator",
                     tabPanel("Introduction",
                              titlePanel("Welcome to the Leaflet Investigator"),
                              fluidRow(column(10, offset = 1,
                                              includeMarkdown("data/markdown.Rmd"),
                                              dataTableOutput("intro_table")))
                              ),
                     tabPanel("Investigator",
                              tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                              withSpinner(leafletOutput("map")),
                              absolutePanel(top = 150,left = 30,width = 300,
                                            draggable = TRUE,
                                            htmlOutput("variable"))
                              )
                     )

# [2] ---- Define Server ----
    server <- function(input, output) {
        
        output$intro_table <- renderDataTable(escape = FALSE, data_sources,
                                              options = list(lengthChange = FALSE,
                                                             searching = FALSE,
                                                             paging = FALSE,
                                                             info = FALSE))
        
        output$variable = renderUI({
            selectInput(inputId = "variable",
                        label = "Variable Group:",
                        choices =  c("Choose a Variable Group", "Crime","Education","Green Space",
                                     "Transport","Census","SEIFA","Housing","Prices","ARIA"),
                        selected = 1)
            })
    
        output$map <- renderLeaflet({
            leaflet(map,options = leafletOptions(minZoom = 6)) %>%
                #addTiles() %>%
                addProviderTiles(providers$Wikimedia) %>%
                setView(148.9211,-32.2532, zoom = 6)
            })
        
        #### If statement for Dynamic Leaflet ####
        map_data_proper <- map_data %>%
            set_names(~ str_replace_all(.,"_"," ") %>%
                      str_to_title())
        
        observeEvent(input$variable,{
            if(input$variable == "Crime"){
                leafletProxy("map") %>%
                clearShapes() %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_crime_1(log_crime_score), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,8:10), feature.id = FALSE, row.numbers = FALSE),
                                group = "Crime Score") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_crime_2(violent_crime), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,8:10), feature.id = FALSE, row.numbers = FALSE),
                                group = "Violent Crime") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_crime_3(dasg_crime), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,8:10), feature.id = FALSE, row.numbers = FALSE),
                                group = "Drug, Alcohol, Sex & Gambling Crime") %>%
                    addLayersControl(baseGroups = c("Crime Score",
                                                    "Violent Crime",
                                                    "Drug, Alcohol, Sex & Gambling Crime"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            } 
            else if(input$variable == "Education"){
                leafletProxy("map") %>%
                    clearShapes() %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_education_1(education_score), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,11), feature.id = FALSE, row.numbers = FALSE),
                                group = "Education") %>%
                    addLayersControl(baseGroups = c("Education"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            } 
            else if(input$variable == "Green Space"){
                map <- map %>%
                    filter(!is.na(green_score_decile))
                map_data_proper <- map_data %>%
                    filter(!is.na(green_score_decile)) %>% 
                    set_names(~ str_replace_all(.,"_"," ") %>%
                                  str_to_title())
                leafletProxy("map") %>%
                    clearShapes() %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_green_1(green_score_decile), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,12,13), feature.id = FALSE, row.numbers = FALSE),
                                group = "Green Space - Decile") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_green_2(green_score), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,12,13), feature.id = FALSE, row.numbers = FALSE),
                                group = "Green Space - Score") %>%
                    addLayersControl(baseGroups = c("Green Space - Decile",
                                                    "Green Space - Score"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            } 
            else if(input$variable == "Transport"){
                leafletProxy("map") %>%
                    clearShapes() %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_transport_1(public_transport_proportion), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,18:20), feature.id = FALSE, row.numbers = FALSE),
                                group = "Journeys to Work by Public Transport") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_transport_2(motor_vehicle_proportion), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,18:20), feature.id = FALSE, row.numbers = FALSE),
                                group = "Journeys to Work by Motor Vehicle") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_transport_3(bicycle_walking_proportion), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,18:20), feature.id = FALSE, row.numbers = FALSE),
                                group = "Journeys to Work by Bicyle or Walking") %>%
                    addLayersControl(baseGroups = c("Journeys to Work by Public Transport",
                                                    "Journeys to Work by Motor Vehicle",
                                                    "Journeys to Work by Bicyle or Walking"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            } 
            else if(input$variable == "Census"){
                leafletProxy("map") %>%
                    clearShapes() %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_census_1(usual_resident_population), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,14:16), feature.id = FALSE, row.numbers = FALSE),
                                group = "Population") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_census_2(working_age_proportion), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,14:26), feature.id = FALSE, row.numbers = FALSE),
                                group = "Proportion Working Age (Age 15-65)") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_census_3(senior_citizen_proportion), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,14:16), feature.id = FALSE, row.numbers = FALSE),
                                group = "Proportion of Seniors (Age 65+)") %>%
                    addLayersControl(baseGroups = c("Population",
                                                    "Proportion Working Age (Age 15-65",
                                                    "Proportion of Seniors (Age 65+)"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            }
            else if(input$variable == "SEIFA"){
                leafletProxy("map") %>%
                    clearShapes() %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_seifa_1(seifa_econ_disadvantage), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,25:28), feature.id = FALSE, row.numbers = FALSE),
                                group = "SEIFA - Economic Disadvantage") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_seifa_2(seifa_econ_adv_disadv), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,25:28), feature.id = FALSE, row.numbers = FALSE),
                                group = "SEIFA - Economic Advantage / Disadvantage") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_seifa_3(seifa_econ_resources), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,25:28), feature.id = FALSE, row.numbers = FALSE),
                                group = "SEIFA - Economic Resources") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_seifa_4(seifa_education_occupation), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,25:28), feature.id = FALSE, row.numbers = FALSE),
                                group = "SEIFA - Education & Occupation") %>%
                    addLayersControl(baseGroups = c("SEIFA - Economic Disadvantage",
                                                    "SEIFA - Economic Advantage / Disadvantage",
                                                    "SEIFA - Economic Resources",
                                                    "SEIFA - Education & Occupation"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            } 
            else if(input$variable == "Housing"){
                leafletProxy("map") %>%
                    clearShapes() %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_housing_1(house_and_semi_proportion), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,22:24), feature.id = FALSE, row.numbers = FALSE),
                                group = "Proportion of Houses") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_housing_2(unit_proportion), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,22:24), feature.id = FALSE, row.numbers = FALSE),
                                group = "Proportion of Units") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_housing_3(dwelling_density), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,22:24), feature.id = FALSE, row.numbers = FALSE),
                                group = "Dwelling Density (per square km)") %>%
                    addLayersControl(baseGroups = c("Proportion of Houses",
                                                    "Proportion of Units",
                                                    "Dwelling Density (per square km)"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            }
            else if(input$variable == "Prices"){
                prices_1 <- map %>%
                    filter(!is.na(median_land_value_per_sqm))
                prices_1_data <- map_data %>%
                    filter(!is.na(median_land_value_per_sqm)) %>%
                    set_names(~ str_replace_all(.,"_"," ") %>%
                                  str_to_title())
                prices_2 <- map %>%
                    filter(!is.na(house_median_suburb))
                prices_2_data <- map_data %>%
                    filter(!is.na(house_median_suburb)) %>%
                    set_names(~ str_replace_all(.,"_"," ") %>%
                                  str_to_title())
                prices_3 <- map %>%
                    filter(!is.na(apartment_median_suburb))
                prices_3_data <- map_data %>%
                    filter(!is.na(apartment_median_suburb)) %>%
                    set_names(~ str_replace_all(.,"_"," ") %>%
                                  str_to_title())
                leafletProxy("map") %>%
                    clearShapes() %>%
                    addPolygons(data = prices_1,
                                weight = 1, 
                                fillColor = ~pal_prices_1(median_land_value_per_sqm), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(prices_1_data, zcol = c(2,4:6,30,37,38), feature.id = FALSE, row.numbers = FALSE),
                                group = "Land Value per Square Metre") %>%
                    addPolygons(data = prices_2,
                                weight = 1, 
                                fillColor = ~pal_prices_2(house_median_suburb), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(prices_2_data, zcol = c(2,4:6,30,37,38), feature.id = FALSE, row.numbers = FALSE),
                                group = "Median House Price") %>%
                    addPolygons(data = prices_3,
                                weight = 1, 
                                fillColor = ~pal_prices_3(apartment_median_suburb), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(prices_3_data, zcol = c(2,4:6,30,37,38), feature.id = FALSE, row.numbers = FALSE),
                                group = "Median Apartment Price") %>%
                    addLayersControl(baseGroups = c("Land Value per Square Metre",
                                                    "Median House Price",
                                                    "Median Apartment Price"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            }
            else if(input$variable == "ARIA"){
                map <- map %>%
                    filter(!is.na(aria_overall))
                map_data_proper <- map_data %>%
                    filter(!is.na(aria_overall)) %>%
                    set_names(~ str_replace_all(.,"_"," ") %>%
                                  str_to_title())
                leafletProxy("map") %>%
                    clearShapes() %>%
                    setView(151.1,-33.85, zoom = 10) %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_aria_1(aria_overall), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,31:36), feature.id = FALSE, row.numbers = FALSE),
                                group = "ARIA - Overall") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_aria_2(aria_education), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,31:36), feature.id = FALSE, row.numbers = FALSE),
                                group = "ARIA - Access to Education Services") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_aria_3(aria_health), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,31:36), feature.id = FALSE, row.numbers = FALSE),
                                group = "ARIA - Access to Health Services") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_aria_4(aria_shopping), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,31:36), feature.id = FALSE, row.numbers = FALSE),
                                group = "ARIA - Access to Shopping Services") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_aria_5(aria_public_transport), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,31:36), feature.id = FALSE, row.numbers = FALSE),
                                group = "ARIA - Access to Public Transport Services") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_aria_6(aria_financial_postal), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leafpop::popupTable(map_data_proper, zcol = c(2,4:6,31:36), feature.id = FALSE, row.numbers = FALSE),
                                group = "ARIA - Access to Financial/Postal Services") %>%
                    addLayersControl(baseGroups = c("ARIA - Overall",
                                                    "ARIA - Access to Education Services",
                                                    "ARIA - Access to Health Services",
                                                    "ARIA - Access to Shopping Services",
                                                    "ARIA - Access to Public Transport Services",
                                                    "ARIA - Access to Financial/Postal Services"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            }
        })
        }

    # Run the application 
shinyApp(ui = ui, server = server)
