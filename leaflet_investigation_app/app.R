#
# Leaflet version of the investigation app
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

# [1] ---- Load global data ----
 
    map <- readRDS("data/simple_map.rds")
    map_data <- map %>%
        st_drop_geometry()
    pal_crime_1 <- colorNumeric(palette = c("white","darkred"),domain = map$log_crime_score)
    pal_crime_2 <- colorNumeric(palette = c("white","darkred"),domain = map$violent_crime)
    pal_crime_3 <- colorNumeric(palette = c("white","darkred"),domain = map$dasg_crime)
    pal_seifa_1 <- colorNumeric(palette = c("white","purple"),domain = map$relative_socio_economic_disadvantage_index)
    pal_seifa_2 <- colorNumeric(palette = c("white","purple"),domain = map$relative_socio_economic_adv_disadv_index)
    pal_seifa_3 <- colorNumeric(palette = c("white","purple"),domain = map$economic_resources_index)
    pal_seifa_4 <- colorNumeric(palette = c("white","purple"),domain = map$education_and_occupation_index)
    
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
                              sidebarLayout(
                                  sidebarPanel(
                                      htmlOutput("variable"),
                                      htmlOutput("colours"),
                                      htmlOutput("legend")
                                      ),
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                      withSpinner(leafletOutput(outputId = 'map', height = 800))
                                      )
                                  )
                              )
                     )

# [2] ---- Define Server ----
    server <- function(input, output) {
        
        output$intro_table <- renderDataTable(escape = FALSE, data_sources)
        
        output$variable = renderUI({
            selectInput(inputId = "variable",
                        label = "Variable Group:",
                        choices =  c("Choose a Variable Group", "Crime","SEIFA","Transport"),
                        selected = 1)
            })
        
        output$colours = renderUI({
            selectInput("colors", "Color Scheme:",
                        rownames(subset(brewer.pal.info, category %in% c("seq", "div"))))
            })
        
        output$legend = renderUI({
            checkboxInput("legend", "Show legend", TRUE)
            })
    
        output$map <- renderLeaflet({
            leaflet(map) %>%
                addTiles() %>%
                setView(146.9211,-33.2532, zoom = 6)
            })
        
        #### If statement for Dynamic Leaflet ####
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
                                popup = leaflet::popupTable(map_data, zcol = c(2,4:6,8:10), feature.id = FALSE, row.numbers = FALSE),
                                group = "Crime Score") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_crime_2(violent_crime), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leaflet::popupTable(map_data, zcol = c(2,4:6,8:10), feature.id = FALSE, row.numbers = FALSE),
                                group = "Violent Crime") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_crime_3(dasg_crime), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leaflet::popupTable(map_data, zcol = c(2,4:6,8:10), feature.id = FALSE, row.numbers = FALSE),
                                group = "Drug, Alcohol, Sex & Gambling Crime") %>%
                    addLayersControl(baseGroups = c("Crime Score",
                                                    "Violent Crime",
                                                    "Drug, Alcohol, Sex & Gambling Crime"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            } 
            else if(input$variable == "SEIFA"){
                leafletProxy("map") %>%
                    clearShapes() %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_seifa_1(relative_socio_economic_disadvantage_index), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leaflet::popupTable(map_data, zcol = c(2,4:6,24:27), feature.id = FALSE, row.numbers = FALSE),
                                group = "SEIFA - Economic Disadvantage") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_seifa_2(relative_socio_economic_adv_disadv_index), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leaflet::popupTable(map_data, zcol = c(2,4:6,24:27), feature.id = FALSE, row.numbers = FALSE),
                                group = "SEIFA - Economic Advantage / Disadvantage") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_seifa_3(economic_resources_index), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leaflet::popupTable(map_data, zcol = c(2,4:6,24:27), feature.id = FALSE, row.numbers = FALSE),
                                group = "SEIFA - Economic Resources") %>%
                    addPolygons(data = map,
                                weight = 1, 
                                fillColor = ~pal_seifa_4(education_and_occupation_index), 
                                color = "black",
                                opacity = 1,
                                fillOpacity = 0.8,
                                popup = leaflet::popupTable(map_data, zcol = c(2,4:6,24:27), feature.id = FALSE, row.numbers = FALSE),
                                group = "SEIFA - Education & Occupation") %>%
                    addLayersControl(baseGroups = c("SEIFA - Economic Disadvantage",
                                                    "SEIFA - Economic Advantage / Disadvantage",
                                                    "SEIFA - Economic Resources",
                                                    "SEIFA - Education & Occupation"),
                                     options = layersControlOptions(collapsed = FALSE),
                                     position = "topright")
            } 
        })
        }

    # Run the application 
shinyApp(ui = ui, server = server)
