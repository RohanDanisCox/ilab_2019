
# INVESTIGATIONS APP - useful video here: https://vimeo.com/rstudioinc/review/131218530/212d8a5a7a/#t=42m2s 

# [0] ---- Load packages ----
library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(markdown)

# [1] ---- Load global data ----
    suburb_data <- readRDS("data/suburb_data.rds")
    
    sa3_data <- readRDS("data/sa3_data.rds")
    
    sa4_data <- readRDS("data/sa4_data.rds")
    
    nsw_data <- readRDS("data/nsw_data.rds")
    
    choices <- readRDS("data/choices.rds")
    
    map <- readRDS("data/map.rds")

# [2] ---- Define UI for application ----
    ui <- navbarPage("Suburb Investigator",
                     tabPanel("Introduction",
                              titlePanel("Welcome to the Suburb Investigator"),
                              fluidRow(column(12,
                                              includeMarkdown("data/test.Rmd")))),
                     tabPanel("Investigator",
                              sidebarLayout(
                                  sidebarPanel(
                                      htmlOutput("sa4_selector"),
                                      htmlOutput("sa3_selector"),
                                      htmlOutput("suburb_selector"),
                                      htmlOutput("variable_selector"),
                                      withSpinner(plotOutput("plot_3", height = "320px"))
                                      ),
                                  # Show a plot of the generated distribution
                                  mainPanel(
                                      withSpinner(plotOutput("plot_1", height = "350px")),
                                      withSpinner(plotOutput("plot_2", height = "350px"))
                                      )
                                  )
                              )
                     )

# [3] ---- Define server logic ----
    
    server <- function(input, output) {
        
        # create reactive input values 
        output$sa4_selector = renderUI({
            selectInput(inputId = "sa4", #name of input
                        label = "SA4:", #label displayed in UI
                        choices = list("Greater Sydney" = list("Central Coast",
                                                               "Sydney - Baulkham Hills and Hawkesbury",
                                                               "Sydney - Blacktown",
                                                               "Sydney - City and Inner South",
                                                               "Sydney - Eastern Suburbs",
                                                               "Sydney - Inner South West",
                                                               "Sydney - Inner West",                   
                                                               "Sydney - North Sydney and Hornsby",
                                                               "Sydney - Northern Beaches",           
                                                               "Sydney - Outer South West",
                                                               "Sydney - Outer West and Blue Mountains",
                                                               "Sydney - Parramatta",                  
                                                               "Sydney - Ryde",          
                                                               "Sydney - South West",
                                                               "Sydney - Sutherland"),
                                       "Rest of NSW" = list("Capital Region",
                                                            "Central West", 
                                                            "Coffs Harbour - Grafton",
                                                            "Far West and Orana",
                                                            "Hunter Valley exc Newcastle",    
                                                            "Illawarra",                         
                                                            "Mid North Coast",            
                                                            "Murray",                          
                                                            "New England and North West",            
                                                            "Newcastle and Lake Macquarie",       
                                                            "Richmond - Tweed",                     
                                                            "Riverina",                         
                                                            "Southern Highlands and Shoalhaven")),
                        selected = 1)
        })
        
        output$sa3_selector = renderUI({
            selectInput(inputId = "sa3", #name of input
                        label = "SA3:", #label displayed in UI
                        choices = choices %>% filter(sa4_name == input$sa4) %>% distinct(sa3_name) %>% arrange(sa3_name),
                        selected = 1)
        })
        output$suburb_selector = renderUI({
            selectInput(inputId = "suburb",
                        label = "Suburb:",
                        choices = choices %>% filter(sa3_name == input$sa3) %>% distinct(suburb_name) %>% arrange(suburb_name),
                        selected = 1)
        })
        output$variable_selector = renderUI({
            selectInput(inputId = "variable",
                        label = "Variable:",
                        choices = suburb_data %>% select(8:40) %>% names(),
                        selected = 1)
        })
        
        # get the reactive values which will be used in the plots
        
        suburb_plot_data <- reactive({suburb_data %>%
                filter(suburb_name == input$suburb)})
        
        sa3_plot_data <- reactive({sa3_data %>%
                filter(sa3_name == input$sa3)})
        
        sa4_plot_data <- reactive({sa4_data %>%
                filter(sa4_name == input$sa4)})
        
        # create the plots
        
        output$plot_1 <- renderPlot({
            ggplot(suburb_plot_data(),aes(x = year,y = !!as.symbol(input$variable),colour="black")) +
                geom_line(size = 2) +
                geom_line(data = sa3_plot_data(),
                          mapping = aes(x = year,y = !!as.symbol(input$variable), colour = "blue"),
                          linetype = "dashed") +
                geom_line(data = sa4_plot_data(),
                          mapping = aes(x = year,y = !!as.symbol(input$variable), colour = "darkgreen"),
                          linetype = "dashed") +
                geom_line(data = nsw_data,
                          mapping = aes(x = year,y = !!as.symbol(input$variable), colour = "red"),
                          linetype = "dashed") +
                theme_minimal(base_size = 16) +
                scale_color_identity(name = "Geography",
                                     breaks = c("black","blue","darkgreen","red"),
                                     labels = c("Suburb","SA3","SA4","NSW"),
                                     guide = "legend") + 
                labs(x = "Year", y = as.character(input$variable))
        })
        
        output$plot_2 <- renderPlot({
            ggplot(suburb_data,aes(!!as.symbol(input$variable))) +
                geom_density() +
                geom_vline(data = suburb_plot_data(),
                           aes(xintercept = !!as.symbol(input$variable)), 
                           color = "#FC4E08", linetype = "dashed", size = 1)+
                theme_minimal(base_size = 16)
        })
        
        output$plot_3 <- renderPlot({
            
            map_subset <- map %>%
                filter(sa4_name == input$sa4) %>%
                mutate(fill = case_when(suburb_name == input$suburb ~ "suburb",
                                          suburb_name != input$suburb & sa3_name == input$sa3 ~ "sa3",
                                          TRUE ~ "No"))
            
            ggplot() +
                geom_sf(data = map_subset, aes(fill = fill)) +
                scale_fill_manual(values = c("white","pink","red"), guide = FALSE)
            
        })
    }

# Run the application
shinyApp(ui = ui, server = server)
