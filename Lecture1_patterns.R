library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

x <- rnorm(10000, mean = 0, sd = 1)
y <- rnorm(10000, mean = 5, sd = 2)
z <- x^2 + y

data <- data.frame(x = x, y= y, z= z)


ui <- fluidPage(
  
  titlePanel("Lecture 1"),
  h5('Today, we are going to talk about sth...'),
  
  
  wellPanel(
    h2('There are several patterns for you to inspect:'),
    
  sidebarLayout(
    
    
    sidebarPanel(
      
      
        selectInput(inputId = "y", 
                     label = "Y:",
                     choices = c("x", "y", "z"),
                     selected = "x"),
        selectInput(inputId = "x", 
                    label = "X:",
                    choices = c("x", "y", "z"),
                    selected = "y")
    ),
    mainPanel(
      plotOutput(outputId = "Scatterplot")
      
    )
  ))
)  

server <- function(input, output) {
  output$Scatterplot <- renderPlot({
      ggplot(data, aes_string(input$x,input$y)) + geom_point()
    
  })
  
}
shinyApp(ui = ui, server = server)
  
  
  
  