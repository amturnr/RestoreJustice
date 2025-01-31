#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
data(mtcars)

ui <- fluidPage(
  titlePanel("MTCARS"),
  selectInput("Columns","Columns",
              names(mtcars), multiple = TRUE),
  verbatimTextOutput("dfStr")
)

server <- function(input, output) {
  Dataframe2 <- reactive({
    mtcars[,input$Columns] 
  })
  output$dfStr <- renderPrint({
    str(Dataframe2())
  })
}

shinyApp(ui, server)