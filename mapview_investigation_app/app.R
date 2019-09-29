#
# Mapview Version of the Investigation App
#

    library(shiny)
    library(shinythemes)
    library(leaflet)
    library(mapview)
    library(rgdal)
    library(shinycssloaders)

# [1] ---- Load global data ----

    map_built <- readRDS("data/map_built.rds")

# [2] ---- Define UI ----
    ui <- fluidPage(
        
        # Application title
        titlePanel("MapView Investigation App - Prototype"),
        
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

# [2] ---- Server ----
server <- function(input, output) {
    
    output$map <- renderLeaflet({
        map_built@map
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
