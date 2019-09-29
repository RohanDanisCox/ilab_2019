#
# Leaflet version of the investigation app
#

library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(shinycssloaders)

# [1] ---- Load global data ----
 
    map <- readRDS("data/map.rds")
    
# [2] ---- Define UI ----

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
        ),

        # Show a plot of the generated distribution
        mainPanel(
            withSpinner(leafletOutput(outputId = 'map', height = 800))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
        leaflet(map) %>%
            addTiles() %>%
            setView(147.9211,-33.2532, zoom = 6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
