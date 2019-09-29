#
# Leaflet version of the investigation app
#

# [0] ---- Load packages ----
    library(shiny)
    library(sf)
    library(leaflet)
    library(rgdal)
    library(dplyr)
    library(ggplot2)
    library(shinycssloaders)
    library(markdown)
    library(RColorBrewer)

# [1] ---- Load global data ----
 
    map <- readRDS("data/simple_map.rds")
    #map_data <- map %>%
        #st_drop_geometry()

    
# [2] ---- Define UI ----

    ui <- navbarPage("Leaflet Investigator",
                     tabPanel("Introduction",
                              titlePanel("Welcome to the Leaflet Investigator"),
                              fluidRow(column(12,
                                              includeMarkdown("data/leaflet_markdown.Rmd")))),
                     tabPanel("Investigator",
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput(inputId = "variable",
                                                   label = "Variable:",
                                                   choices = c("log_crime_score","education_score", "green_score_decile"),
                                                   selected = 1),
                                      selectInput("colors", "Color Scheme",
                                                  rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                      ),
                                      selectInput(inputId = "colour",
                                                  label = "Colour:",
                                                  choices = c("black","red","blue", "green")),
                                      checkboxInput("legend", "Show legend", TRUE)
                                      ),
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                      withSpinner(leafletOutput(outputId = 'map', height = 800))
                                      )
                                  )
                              )
                     )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
        leaflet(data = map) %>%
            addTiles() %>%
            setView(147.9211,-33.2532, zoom = 6) %>%
            addPolygons(layerId = ~NAME_1, weight = 1, fillColor = "grey", color = "black",
                        opacity = 1, fillOpacity = 0.6)
    })

# Run the application 
shinyApp(ui = ui, server = server)
