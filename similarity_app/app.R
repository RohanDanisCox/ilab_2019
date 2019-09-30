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

data <- map %>%
    st_drop_geometry()

slider_options <- data %>%
    select(7:39) %>%
    map_df(~(data.frame(min = round(min(.x, na.rm = TRUE),0),
                        max = round(max(.x, na.rm = TRUE),0),
                        mean = round(mean(.x,na.rm = TRUE),0),
                        na_count = sum(is.na(.x)))),
           .id = "variable")

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
                                  htmlOutput("education")
                              ),
                              # Show a plot of the generated distribution
                              mainPanel(
                                  withSpinner(leafletOutput(outputId = 'map_1', height = 800))
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
                    min = slider_options %>% filter(variable == "suburb_area_sqkm") %>% select(min) %>% pull(),
                    max = slider_options %>% filter(variable == "suburb_area_sqkm") %>% select(max) %>% pull(),
                    value = slider_options %>% filter(variable == "suburb_area_sqkm") %>% select(mean) %>% pull())
    })
    output$crime = renderUI({
        sliderInput(inputId = "crime",
                    label = "Crime Score:",
                    min = slider_options %>% filter(variable == "log_crime_score") %>% select(min) %>% pull(),
                    max = slider_options %>% filter(variable == "log_crime_score") %>% select(max) %>% pull(),
                    value = slider_options %>% filter(variable == "log_crime_score") %>% select(mean) %>% pull())
    })
    output$education = renderUI({
        sliderInput(inputId = "education",
                    label = "Education Score:",
                    min = min(data$education_score, na.rm = TRUE),
                    max = max(data$education_score, na.rm = TRUE),
                    value = mean(data$education_score, na.rm = TRUE))
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
