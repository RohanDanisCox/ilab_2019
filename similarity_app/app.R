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

# [1] ---- Load global data ----

map <- readRDS("data/simple_map.rds")

scaled_data <- readRDS("data/scaled_data.rds") %>%
    select(1,2,5,6)             ##### NEED TO REMOVE THIS LATER

scaler <- readRDS("data/scaling_data.rds") %>%
    filter(variable %in% c("suburb_area_sqkm","log_crime_score","education_score")) ##### NEED TO REMOVE THIS LATER

data <- map %>%
    st_drop_geometry()

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
                                  htmlOutput("button")
                              ),
                              # Show a plot of the generated distribution
                              mainPanel(
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
                    min = scaler %>% filter(variable == "suburb_area_sqkm") %>% select(min) %>% pull(),
                    max = scaler %>% filter(variable == "suburb_area_sqkm") %>% select(max) %>% pull(),
                    value = scaler %>% filter(variable == "suburb_area_sqkm") %>% select(mean) %>% pull())
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
    output$button = renderUI({
        actionButton(inputId = "go", label = "Calculate Simularity")
    })
 
    ##### Here is where I need to calculate the similarity scores
    
    new_values <- eventReactive(input$go,{
        #if(is.null(input$go)){
        #    return()
        #}
        tibble(new_values = c(input$size,input$crime,input$education)) %>%
            cbind(scaler) %>%
            mutate(scaled_value = (new_values - mean) / sd) 
    })
    
    output$test <- renderDataTable({
        new_values()
    })
    
    output$map_1 <- renderLeaflet({
        leaflet(map) %>%
            addTiles() %>%
            setView(146.9211,-33.2532, zoom = 6)
    })
    
    output$map_2 <- renderLeaflet({
        leaflet(map) %>%
            addTiles() %>%
            setView(146.9211,-33.2532, zoom = 6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
