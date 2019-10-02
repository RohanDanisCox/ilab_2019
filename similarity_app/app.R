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
    select(1,2,5,6,7)             ##### NEED TO REMOVE THIS LATER

scaler <- readRDS("data/scaling_data.rds") %>%
    filter(variable %in% c("suburb_area_sqkm","log_crime_score","education_score","green_score_decile")) ##### NEED TO REMOVE THIS LATER

data <- map %>%
    st_drop_geometry()

## Function to calculate similarity despite NA's
rdist_na <- function(X,Y)
{
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
                                  htmlOutput("size"),
                                  htmlOutput("crime"),
                                  htmlOutput("education"),
                                  htmlOutput("green"),
                                  htmlOutput("button")
                              ),
                              # Show a plot of the generated distribution
                              mainPanel(
                                  withSpinner(leafletOutput(outputId = 'map_1', height = 500)),
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
    output$button = renderUI({
        actionButton(inputId = "go", label = "Calculate Simularity")
    })
 
    ##### Here is where I need to calculate the similarity scores
    
    new_values <- eventReactive(input$go,{
        #if(is.null(input$go)){
        #    return()
        #}
        new <- tibble(new_values = c(input$size,input$crime,input$education,input$green)) %>%
            cbind(scaler) %>%
            mutate(scaled_value = (new_values - mean) / sd) %>%
            select(scaled_value) %>% 
            t()
        suburb <- scaled_data
        z <- as.data.frame(rdist_na(new,suburb[,2:5])) %>%
            mutate(divisor = 1/V1) %>%
            mutate(normalise = (divisor - min(divisor, na.rm = TRUE))/ (max(divisor, na.rm = TRUE) - min(divisor, na.rm = TRUE)))
        combined <- suburb %>% 
            select(suburb_name) %>%
            cbind(z) %>%
            arrange(desc(normalise)) %>%
            head(10)
        combined
    })
    
    output$test <- renderDataTable({
        new_values()
    })
    
    output$map_1 <- renderLeaflet({
        map_small <- map %>%
            left_join(new_values(),by = "suburb_name") %>%
            filter(!is.na(normalise))
        
        pal <- colorNumeric(palette = c("white","red"),domain = map_small$normalise)
        
        leaflet(map_small) %>%
            addTiles() %>%
            setView(146.9211,-33.2532, zoom = 6) %>%
            addPolygons(data = map_small,
                        weight = 1, 
                        fillColor = ~pal(normalise), 
                        color = "black",
                        opacity = 1,
                        fillOpacity = 0.8,
                        popup = popupTable(map_small,  zcol = c(2,4:6,40:43),feature.id = FALSE, row.numbers = FALSE))
    })
    
    output$map_2 <- renderLeaflet({
        leaflet(map) %>%
            addTiles() %>%
            setView(146.9211,-33.2532, zoom = 6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
