
# INVESTIGATIONS APP

# [0] ---- Load packages ----
library(shiny)
library(tidyverse)

# [1] ---- Load global data ----
    master <- readRDS("data/master.rds") %>% 
        filter(!gccsa_code %in% c(19499,19799))
    
    nsw_results <- readRDS("data/nsw_results.rds")
    
    map <- readRDS("data/map.rds")

# [2] ---- Define UI for application ----
    ui <- fluidPage(
        
        # Application title
        titlePanel("Suburb Investigator"),
        
        # Select Box
        sidebarLayout(
            sidebarPanel(
                htmlOutput("gccsa_selector"),
                htmlOutput("sa4_selector"),
                htmlOutput("sa3_selector"),
                htmlOutput("suburb_selector"),
                htmlOutput("variable_selector")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("plot_1", height = "280px"),
                plotOutput("plot_2", height = "280px"),
                plotOutput("plot_3", height = "280px")
            )
        )
    )

# [3] ---- Define server logic ----
    
    server <- function(input, output) {
        
        output$gccsa_selector = renderUI({
            selectInput(inputId = "gccsa", #name of input
                        label = "GCCSA:", #label displayed in UI
                        choices = master %>% distinct(gccsa_name) %>% arrange(gccsa_name), # calls unique values from subset
                        selected = 1)
        })
        
        output$sa4_selector = renderUI({
            selectInput(inputId = "sa4", #name of input
                        label = "SA4:", #label displayed in UI
                        choices = master %>% filter(gccsa_name == input$gccsa) %>% distinct(sa4_name) %>% arrange(sa4_name), # calls unique values from subset
                        selected = 1)
        })
        output$sa3_selector = renderUI({
            selectInput(inputId = "sa3", #name of input
                        label = "SA3:", #label displayed in UI
                        choices = master %>% filter(sa4_name == input$sa4) %>% distinct(sa3_name) %>% arrange(sa3_name),
                        selected = 1)
        })
        output$suburb_selector = renderUI({
            selectInput(inputId = "suburb",
                        label = "Suburb:",
                        choices = master %>% filter(sa3_name == input$sa3) %>% distinct(suburb_name) %>% arrange(suburb_name),
                        selected = 1)
        })
        output$variable_selector = renderUI({
            selectInput(inputId = "variable",
                        label = "Variable:",
                        choices = master %>% select(19:99) %>% names(),
                        selected = 1)
        })
        
        output$plot_1 <- renderPlot({
            
            plot_data <- master %>%
                filter(suburb_name == input$suburb)
            
            ggplot(plot_data,aes_string(x = "year",y = input$variable)) +
                geom_line() +
                geom_line(data = nsw_results,mapping = aes_string(x = "year",y = input$variable), colour = "red")
        })
        
        output$plot_2 <- renderPlot({
            
            plot_data <- master %>%
                filter(suburb_name == input$suburb)
            
            ggplot(master,aes_string(input$variable)) +
                geom_density() +
                geom_vline(data = plot_data,aes_string(xintercept = input$variable), color = "#FC4E08", linetype = "dashed", size = 1)
        })
        
        output$plot_3 <- renderPlot({
            
            map_subset <- map %>%
                filter(sa4_name == input$sa4) %>%
                mutate(suburb = case_when(suburb_name == input$suburb ~ "Yes",
                                          TRUE ~ "No"))
            
            ggplot() +
                geom_sf(data = map_subset, aes(fill = suburb)) +
                scale_fill_manual(values = c("white","red"), guide = FALSE)
            
        })
    }

# Run the application
shinyApp(ui = ui, server = server)