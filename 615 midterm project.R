#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(psych)
library(shiny)
library(gplots)
library(ggplot2)
library(RColorBrewer)
library(ggfortify)
library(readxl)
library(cluster)
library(DT)
library(fpc)

# Define UI for application that draws a histogram
ui <- fluidPage(
        headerPanel("World value dataset exploration"),
        sidebarLayout(
            sidebarPanel(
              h5("Since my goal is reduce number of variables(selected survey questions) in my data by 
                 extracting important one from the data, thus, I will using PCA to do factor extraction for this dataset."),
              checkboxInput(inputId = "parallel",label="Parallel Scree plot"),
              checkboxInput(inputId = "regular", label = "Regular Scree plot"),
              sliderInput("factor", "Number of Factors we can choose based on the Scree plot", value = 0, min = 0, max = 11),
              helpText("After we decided the number of factors used in PCA, we can check the output of PCA 
                         and compare them with each other for the best choice that we can explain it well.
                         Now, try to move the sidebar and choose the button below and see how plots shows different"),
              hr(), 
              h4("For fun, we can plot PCA in many different ways to understand our data:"),
              checkboxInput(inputId = "interesting", label = "Interesting plot"),
              checkboxInput(inputId = "int", label = "Interesting Cluster Plot"),
              helpText("Please slide down the page and maybe wait a second for the appearance of plots~")
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("Raw data",DT::dataTableOutput("Raw")),
            tabPanel("Matrix table",
                     tags$h4("Matrix table that show correlation between selected survey questions after
                             we delete non-information questions, select survey questions and numeric answers of our selected questions."),
                     tableOutput("table")),
            tabPanel("Scree Plot", plotOutput("screeplot")),
            tabPanel("Summary", 
                     tags$h4("For easier to explain the output of factor extraction, we can using 
                             orthogonal rotation to decreasing noice for factors as much as possible."),
                     tags$h5("Here is the output of orthogonal rotationed PCA, we can try understand 
                             the correlation between survey questions and components numberically"),
                     verbatimTextOutput("summary")),
            tabPanel("PCA result", plotOutput("result"), tags$h4("This is a graph drarws the relationship
                                                                 between selected questions and components well
                                                                 graphically and easier to understand than understand 
                                                                 it based on numbers."),
                     tags$h4("From the result of PCA, we have general idea about how can we explain data from
                         using PCA, and understand undergoing realtionship between those varibles")),
            tabPanel("Interesting plots", tags$h3("Click plots button down the left side"), plotOutput("intertest"), plotOutput("intt")),
            tabPanel("Conclusion", tags$h3("According to the results and the questionnaires, We can find the questions that load 
            highly on factor 1 are V4(Important in life:Family) with the highest loading of 0.71, and lowest loading of 0.4 is V10(Feeling of happiness). Factor 2 
            are mianly explained by V64(Most important:first choice) and V60(Aim of country:first choice) with loading of 0.76 and 0.75. Factor 3 are mainly explained 
            by V11(state of health) with 0.86 and the loweest loading of 0.64 is V10(Feeling of happiness)."), 
                     tags$h3("Based on the obsersation, we can summarize the factor 1 as hapiness people consider a lot are important in life and label factor 2 as the expectation from people to country, factor 3 as causes of healthy people."))
          )
        )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #Read data and numeric selected columns:
    data <- read_excel("F00007693-WV6_Data_United_States_2011_Excel_v20180912.xlsx")
    newdata <- data[vapply(data, function(x) length(unique(x)) > 1, logical(1L))]
    x <- c(1:8,57,59,61)
    newdata[x] <- apply(newdata[x],2,function(x) as.numeric(as.factor(x)))
    pc2 <- reactive({
      principal(newdata[x], nfactors = input$factor, rotate="none")
      })
    pc3 <- reactive({
      principal(newdata[x], nfactors = input$factor, rotate = "varimax")
      })

    #create output object, and name it so it corresponds to the ui output function ID:
    #create the raw data frame:
    output$Raw <- DT::renderDataTable({
        data
    })
    #create the matrix table for selected:
     output$table <- renderTable({
         cor(newdata[x])
     })
    #plot scree plot
     output$screeplot <- renderPlot({
       if(input$parallel) {
         x <- c(1:8,57,59,61)
         newdata[x] <- apply(newdata[x],2,function(x) as.numeric(as.factor(x)))
         fa.parallel(newdata[x], fm='minres', fa='fa', main = "Scree Plot")
       }
       if(input$regular){
         
         x <- c(1:8,57,59,61)
         newdata[x] <- apply(newdata[x],2,function(x) as.numeric(as.factor(x)))
         pc <- principal(newdata[x], nfactors = 11, rotate="none")
         plot(pc$values, type="b", main = "Scree Plot") 
         }
     })
    #plot table PC:
      output$summary <- renderPrint({
        print.psych(pc3(), cut=0.3, sort = TRUE) 
      })
    #plot result of PCA:
     output$result <- renderPlot({
         fa.diagram(pc3(),simple=TRUE)
     })
     #plot further interesting plot:
     output$intertest <- renderPlot({
       if(input$interesting){
         autoplot(prcomp(newdata[x]), scale=0)
       }
     })
     output$intt <- renderPlot({
       if(input$int){
         clus <- kmeans(newdata[x], centers = 3)
         plotcluster(newdata[x], clus$cluster)
         clusplot(newdata[x], clus$cluster, color = TRUE, shade = TRUE, labels = 3, lines = 0)
       }
     })
    
}
  
# Run the application 
shinyApp(ui = ui, server = server)
