#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggmap)
library(maptools)
library(maps)
library(mapproj)
library(shiny)

mapWorld <- map_data("world")


ui <- fluidPage(

    # Application title
    titlePanel("Different World Map Projections"),
    selectInput("select", label = "Type of Projection", choices = c(
        "Projection: Cylinder" = "cylindrical" ,
        "Projection: Mercator" = "mercator",
        "Projection: Sinusoidal" = "sinusoidal",
        "Projection: Gnomonic" = "gnomonic",
        "Projection: Gilbert" = "gilbert",
        "Projection: Mollweide" = "mollweide"
    )),

        mainPanel(
            plotOutput("pl")
            ))

server <- function(input, output, session) {
    output$pl <- renderPlot({
        ggplot(mapWorld, aes(x=long, y=lat, group=group))+
            geom_polygon(fill="white", color="black") +
            coord_map(xlim=c(-180,180), ylim=c(-60, 90)) +
            coord_map(toString(input$select), xlim=c(-180,180), ylim=c(-60, 90)) 
        })
}


# Run the application 
shinyApp(ui = ui, server = server)
