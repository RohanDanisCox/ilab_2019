# Suburb Investigation

# [0] ---- Load packages ----
  library(shiny)
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(shinycssloaders)
  library(markdown)
  library(leaflet)
  library(leafpop)
  library(stringr)
  library(scales)
  library(purrr)

# [1] ---- Load global data ----

  suburb_data <- readRDS("data/suburb_data.rds")
  
  nsw_data <- readRDS("data/nsw_data.rds") %>%
    mutate(nsw = "-- NSW --")
  
  choices <- readRDS("data/choices.rds")
  
  map <- readRDS("data/map.rds")
  
  data_sources <- readRDS("data/data_sources.rds")
  
  suburb_choice <- choices %>% distinct(suburb_name) %>% arrange(suburb_name) %>% pull()
  
  log_var <- c("Median_House_Price","Median_Apartment_Price","Usual_Resident_Population",
               "Dwelling_Density","Annual_Turnover","Median_Land_Value","Median_Land_Value_Per_Sq_M",
               "Violent_Crime","DASG_Crime")
  
  small_var <-c("Crime","Education", "Green_Decile","Working_Age",
                "Senior_Citizens","Journey_to_Work_by_Public_Transport","Journey_to_Work_by_Motor_Vehicle",
                "Journey_to_Work_by_Bicycle_or_Walking","Proportion_of_Houses","Proportion_of_Units","ARIA_Overall_Services",
                "ARIA_Education_Services","ARIA_Health_Services","ARIA_Shopping_Services","ARIA_Public_Transport_Services",
                "ARIA_Financial_Postal_Services")

# [2] ---- Define UI for application ----
  
  ui <- navbarPage("Suburb Investigation",
                   tabPanel("Introduction",
                            titlePanel("Investigating Suburbs in NSW"),
                            fluidRow(column(10, offset = 1,
                                            includeMarkdown("data/markdown.Rmd"),
                                            dataTableOutput("intro_table")))),
                   tabPanel("Investigate",
                            sidebarLayout(
                              sidebarPanel(
                                selectizeInput("suburb", label = "Search for up to 3 suburbs:",choices = NULL,
                                               multiple = TRUE, options = list(maxItems = 3)),
                                htmlOutput("variable_selector"),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                withSpinner(leafletOutput("plot_3", height = "320px"))
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
    updateSelectizeInput(session, "suburb", choices = suburb_choice, server = TRUE)
    
    # create reactive input values 
    
    output$variable_selector = renderUI({
      selectInput(inputId = "variable",
                  label = "Choose a variable to inspect:",
                  choices = suburb_data %>% 
                    select(Median_House_Price,Median_Apartment_Price,Usual_Resident_Population,SEIFA_Socio_Economic_Disadvantage,
                            SEIFA_Socio_Economic_Advantage_Disadvantage,SEIFA_Economic_Resources,SEIFA_Education_and_Occupation,
                            Dwelling_Density,Annual_Turnover,Median_Land_Value,Median_Land_Value_Per_Sq_M,
                            Crime,Violent_Crime,DASG_Crime,Education,Green_Decile,Working_Age,
                            Senior_Citizens,Journey_to_Work_by_Public_Transport,Journey_to_Work_by_Motor_Vehicle,
                            Journey_to_Work_by_Bicycle_or_Walking,Proportion_of_Houses,Proportion_of_Units,ARIA_Overall_Services,
                            ARIA_Education_Services,ARIA_Health_Services,ARIA_Shopping_Services,ARIA_Public_Transport_Services,
                            ARIA_Financial_Postal_Services) %>% 
                    names() %>% str_replace_all("_"," "),
                  selected = 1)
    })
    
    # get the reactive values which will be used in the plots
    
    suburb_plot_data <- reactive({
      suburb_data %>%filter(suburb_name %in% input$suburb)
    })
    
    # create the plots
    first_plot <- reactive({
      y_var <- input$variable %>% str_replace_all(" ","_")
      if(y_var %in% small_var) {
        if(length(input$suburb) < 1) {
          ggplot(nsw_data,aes(x = year, y = !!as.symbol(y_var), colour = nsw)) + 
            geom_line(linetype = "dashed", size = 1) + 
            theme_minimal(base_size = 16) + 
            labs(x = "Year", y = as.character(input$variable),title = paste0(input$variable," in NSW")) + 
            scale_colour_manual(name = "", values = c("#000000", "#009E73", "#56B4E9", "#D55E00")) +
            scale_y_continuous(labels = comma_format(accuracy = 0.01))
        }
        else if(length(input$suburb) >= 1){
          ggplot(nsw_data,aes(x = year, y = !!as.symbol(y_var), colour = nsw)) + 
            geom_line(linetype = "dashed", size = 1) + 
            geom_line(data = suburb_plot_data(),
                      mapping = aes(x = year,y = !!as.symbol(y_var), colour = suburb_name), size = 1) +
            theme_minimal(base_size = 16) +
            labs(x = "Year", y = as.character(input$variable),
                title = paste0(input$variable," in NSW")) +
            scale_colour_manual(name = "", values = c("#000000", "#009E73", "#56B4E9", "#D55E00")) + 
            scale_y_continuous(labels = comma_format(accuracy = 0.01))
        }
      }
      else {
        if(length(input$suburb) < 1) {
          ggplot(nsw_data,aes(x = year, y = !!as.symbol(y_var), colour = nsw)) + 
            geom_line(linetype = "dashed", size = 1) + 
            theme_minimal(base_size = 16) + 
            labs(x = "Year", y = as.character(input$variable),title = paste0(input$variable," in NSW")) + 
            scale_colour_manual(name = "", values = c("#000000", "#009E73", "#56B4E9", "#D55E00")) +
            scale_y_continuous(labels = comma)
        }
        else if(length(input$suburb) >= 1){
          ggplot(nsw_data,aes(x = year, y = !!as.symbol(y_var), colour = nsw)) + 
            geom_line(linetype = "dashed", size = 1) + 
            geom_line(data = suburb_plot_data(),
                      mapping = aes(x = year,y = !!as.symbol(y_var), colour = suburb_name), size = 1) +
            theme_minimal(base_size = 16) +
            labs(x = "Year", y = as.character(input$variable),
                 title = paste0(input$variable," in NSW")) +
            scale_colour_manual(name = "", values = c("#000000", "#009E73", "#56B4E9", "#D55E00")) + 
            scale_y_continuous(labels = comma)
        }
      }
    })
    
    second_plot <- reactive({
      y_var <- input$variable %>% str_replace_all(" ","_")
      if(y_var %in% log_var) {
        if(length(input$suburb) < 1) {
          ggplot(suburb_data,aes(!!as.symbol(y_var))) +
            geom_density() +
            theme_minimal(base_size = 16) +
            labs(x = paste0("Log of ",as.character(input$variable)), y = "",
                 title = paste0("Density Plot for ",input$variable," in NSW")) + 
            scale_x_continuous(trans='log', labels = comma)
        }
        else if(length(input$suburb) >= 1){
          ggplot(suburb_data,aes(!!as.symbol(y_var))) +
            geom_density() +
            geom_vline(data = suburb_plot_data(),
                       aes(xintercept = !!as.symbol(y_var), colour = suburb_name), 
                       linetype = "dashed", size = 1, alpha = 0.5)+
            theme_minimal(base_size = 16) + 
            labs(x = paste0("Log of ",as.character(input$variable)), y = "",
                 title = paste0("Density Plot for ",input$variable," in NSW")) +
            scale_colour_manual(name = "", values = c("#009E73", "#56B4E9", "#D55E00")) +
            scale_x_continuous(trans='log', labels = comma) +
            guides(colour = guide_legend(override.aes = list(alpha = 1)))
        }
      }
      else{
        if(length(input$suburb) < 1) {
          ggplot(suburb_data,aes(!!as.symbol(y_var))) +
            geom_density() +
            theme_minimal(base_size = 16) +
            labs(x = as.character(input$variable), y = "",
                 title = paste0("Density Plot for ",input$variable," in NSW")) + 
            scale_x_continuous(labels = comma_format(accuracy = 0.1))
        }
        else if(length(input$suburb) >= 1){
          ggplot(suburb_data,aes(!!as.symbol(y_var))) +
            geom_density() +
            geom_vline(data = suburb_plot_data(),
                       aes(xintercept = !!as.symbol(y_var), colour = suburb_name), 
                       linetype = "dashed", size = 1, alpha = 0.5)+
            theme_minimal(base_size = 16) + 
            labs(x = as.character(input$variable), y = "",
                 title = paste0("Density Plot for ",input$variable," in NSW")) +
            scale_colour_manual(name = "", values = c("#009E73", "#56B4E9", "#D55E00")) + 
            scale_x_continuous(labels = comma_format(accuracy = 0.1)) + 
            guides(colour = guide_legend(override.aes = list(alpha = 1)))
        }
      }
    })
    
    output$plot_1 <- renderPlot({
      first_plot()
    })
    
    output$plot_2 <- renderPlot({
      second_plot()
    })
    
    output$plot_3 <- renderLeaflet({
      leaflet(map,options = leafletOptions(minZoom = 6)) %>%
        addProviderTiles(providers$Wikimedia) %>%
        setView(148.9211,-32.2532, zoom = 6)
    })
    
    observeEvent(input$suburb,{
      map_subset <- map %>%
        filter(suburb_name %in% input$suburb)
      map_presentation <- map_subset %>%
          set_names(~ str_replace_all(.,"_"," ") %>%
                        str_to_title()) %>%
          rename(geometry = Geometry)
      
      length_check <- length(input$suburb)
      
      if(length_check== 1) {
          top <- map_subset %>%
              filter(suburb_name == input$suburb[[1]]) %>%
              st_centroid(geometry)
          top_lat <- top$geometry[[1]][1]
          top_lng <- top$geometry[[1]][2]
      }
      else if(length_check== 2)  {
          top <- map_subset %>%
              filter(suburb_name == input$suburb[[2]]) %>%
              st_centroid(geometry)
          top_lat <- top$geometry[[1]][1]
          top_lng <- top$geometry[[1]][2]
      }
      else if(length_check== 3)  {
          top <- map_subset %>%
              filter(suburb_name == input$suburb[[3]]) %>%
              st_centroid(geometry)
          top_lat <- top$geometry[[1]][1]
          top_lng <- top$geometry[[1]][2]
      }    
      
      leafletProxy("plot_3") %>%
        clearShapes() %>%
        addPolygons(data = map_subset,
                    weight = 1, 
                    fillColor = "red", 
                    color = "black",
                    opacity = 1,
                    fillOpacity = 0.8,
                    popup = leafpop::popupTable(map_presentation, zcol = c(2,4,6,7,8),feature.id = FALSE, row.numbers = FALSE)) %>%
        flyTo(top,lng = top_lat,lat = top_lng,zoom = 10)
    })
  }
  # [4] ---- Run the Application ----
  
    shinyApp(ui = ui, server = server)
