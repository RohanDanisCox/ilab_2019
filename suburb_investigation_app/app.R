
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
    
    data_sources <- readRDS("data/data_sources.rds")
    
    suburb_choice <- choices %>% distinct(suburb_name) %>% arrange(suburb_name) %>% pull()

# [2] ---- Define UI for application ----
    ui <- navbarPage("Suburb Investigator",
                     tabPanel("Introduction",
                              titlePanel("Welcome to the Suburb Investigator"),
                              fluidRow(column(10, offset = 1,
                                              includeMarkdown("data/markdown.Rmd"),
                                              dataTableOutput("intro_table")))),
                     tabPanel("Investigator",
                              sidebarLayout(
                                  sidebarPanel(
                                      selectizeInput("foo", label = "test",choices = NULL,
                                                     multiple = TRUE, options = list(maxItems = 3)),
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
    
    server <- function(input, output, session) {
        
        output$intro_table <- renderDataTable(escape = FALSE, data_sources,
                                              options = list(lengthChange = FALSE,
                                                             searching = FALSE,
                                                             paging = FALSE,
                                                             info = FALSE))
        # trying selectize
        updateSelectizeInput(session, "foo", choices = suburb_choice, server = TRUE)
        
        # create reactive input values 
     
        output$variable_selector = renderUI({
            selectInput(inputId = "variable",
                        label = "Variable:",
                        choices = suburb_data %>% select(8:40) %>% names(),
                        selected = 1)
        })
        
        # get the reactive values which will be used in the plots
        
        nsw_plot_data <- reactive({nsw_data})
        
        suburb_plot_data <- reactive({
            if(length(input$foo) == 1) {
                suburb_data %>%filter(suburb_name == input$foo[[1]])
            }
            else if(length(input$foo) == 2) {
                suburb_data %>%filter(suburb_name == input$foo[[1]] | suburb_name == input$foo[[2]])
            }
            else if(length(input$foo) == 3) {
                suburb_data %>%filter(suburb_name == input$foo[[1]] | suburb_name == input$foo[[2]] | suburb_name == input$foo[[3]])
            }
            })
        
        sa3_plot_data <- reactive({sa3_data %>%
                filter(sa3_name == input$sa3)
            })
        
        sa4_plot_data <- reactive({sa4_data %>%
                filter(sa4_name == input$sa4)
            })
        
        # create the plots
        first_plot <- reactive({
            if(length(input$foo) < 1) {
                ggplot(nsw_data,aes(x = year, y = !!as.symbol(input$variable))) + 
                    geom_line(linetype = "dashed") + 
                    labs(x = "Year", y = as.character(input$variable))
            }
            else if(length(input$foo) >= 1){
                ggplot(nsw_data,aes(x = year, y = !!as.symbol(input$variable))) + 
                    geom_line(linetype = "dashed") + 
                    geom_line(data = suburb_plot_data(),
                              mapping = aes(x = year,y = !!as.symbol(input$variable), colour = suburb_name)) +
                    theme_minimal(base_size = 16) +
                    #scale_color_manual(name = "Geography",
                    #breaks = c("black","blue","darkgreen","red"),
                    #labels = c("Suburb","SA3","SA4","NSW"),
                    #guide = "legend") + 
                    labs(x = "Year", y = as.character(input$variable))
                }
            })
        
        second_plot <- reactive({
            if(length(input$foo) < 1) {
                ggplot(suburb_data,aes(!!as.symbol(input$variable))) +
                    geom_density() +
                    theme_minimal(base_size = 16)
            }
            else if(length(input$foo) >= 1){
                ggplot(suburb_data,aes(!!as.symbol(input$variable))) +
                    geom_density() +
                    geom_vline(data = suburb_plot_data(),
                               aes(xintercept = !!as.symbol(input$variable)), 
                               color = "#FC4E08", linetype = "dashed", size = 1)+
                    theme_minimal(base_size = 16)
            }
        })
        

        output$plot_1 <- renderPlot({
            first_plot()
            })
            
        output$plot_2 <- renderPlot({
            second_plot()
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
