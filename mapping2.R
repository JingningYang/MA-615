#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(leaflet)
library(RColorBrewer)
library(lattice)
library(dplyr)
library(scales)
library(shiny)
library(tidyverse)
library(leaflet.extras)
library(DT)

data <- read.csv("restaurant.csv")
Data <- filter(data, Latitude != 0 & Longitude != 0)

ui <- fluidPage(

    # Application title
    navbarPage("Exploring Boston restaurants", id="nav",
               tabsetPanel(tabPanel("Interactive map", 
                                    fluidRow(
                                      column(3,
                                             selectInput("Res", "Select the Restaurant", Data$BusinessName)),
                                      column(2,
                                             selectInput("off", "Type of Offering", Data$DESCRIPT)),
                                      leafletOutput("map", width = "1000", height = "1000")
             )),
             tabPanel("Data Exploration", 
                      fluidRow(
                        column(3,
                               selectInput("Re", "Select the Restaurant", c("All",as.character(Data$BusinessName)))),
                        column(2,
                               selectInput("of", "Type of Offering", c("All",as.character(Data$DESCRIPT)))),
                        column(2,
                               selectInput("zip", "Zipcode", c("All", as.character(data$ZIP)))),
                        hr(),
                        DT::dataTableOutput("inf")
             )))))

server <- function(input, output, session) {
    pal <- colorFactor(
        palette = c('gold', 'grey', 'orange red', 'red', 'dark red', 'purple', 'blue'),
        domain = Data$CITY)
    
    #create the map
    output$map <- renderLeaflet({
        leaflet(Data)%>% 
            addTiles() %>% 
            setView(lng = -73.85, lat = 40.45, zoom = 7) %>%
            addCircles(data = Data, lat = ~ Latitude, lng = ~ Longitude, weight = 1, radius = 100, 
                       label = ~as.character(paste0("Restaurant: ", sep = " ", BusinessName)), color = ~pal(CITY), fillOpacity = 0.5)
    })
    observeEvent(input$Res, {
      leafletProxy("map")%>%
        clearGroup("Markers") %>%
        addMarkers(data = Data[Data$BusinessName == input$Res, ], ~Longitude, ~Latitude, group = "Markers")
    })
    observeEvent(input$off, {
      leafletProxy("map")%>%
        clearGroup("Markers") %>%
        addMarkers(data = Data[Data$DESCRIPT == input$off, ], ~Longitude, ~Latitude, group = "Markers")
    })
  
    ###### Data exploration
    output$inf <- DT::renderDataTable({
      data <- Data
      if(input$Re != "All"){
        data <- data[data$BusinessName == input$Re, ]
      }
      if(input$of != "All"){
        data <- data[data$DESCRIPT == input$of, ]
      }
      if(input$zip != "All"){
        data<- data[data$ZIP == input$zip, ]
      }
      data
    })
    }
    
  
# Run the application 
shinyApp(ui = ui, server = server)
