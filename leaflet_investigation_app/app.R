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
 
    leaf_map <- readRDS("data/leaf_map.rds")
    map_data <- map %>%
        st_drop_geometry()

    
# [2] ---- Define UI ----

    ui <- navbarPage("Leaflet Investigator",
                     tabPanel("Introduction",
                              titlePanel("Welcome to the Leaflet Investigator"),
                              fluidRow(column(12,
                                              includeMarkdown("data/leaflet_markdown.Rmd")))),
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
        
        output$variable = renderUI({
            selectInput(inputId = "variable",
                        label = "Variable:",
                        choices =  map_data %>% select(3:4) %>% names(),
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
            leaf_map
        })
        }

    # Run the application 
shinyApp(ui = ui, server = server)
