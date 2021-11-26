###############################################################################
## Libraries
library(shiny)
library(tidyverse)
###############################################################################
ui <- fluidPage(
  fluidRow(
    column(3,
           numericInput("lambda1", label = "lambda1", value = 3),
           numericInput("lambda2", label = "lambda2", value = 5),
           numericInput("n", label = "n", value = 10, min = 0),
           actionButton("simulate", "Simulate!")
    ),
    column(9, plotOutput("plt"))
  )
)

server <- function(input, output, session) {
  timer <- reactiveTimer(500)
  x1 <- reactive({
    input$simulate
    rexp(input$n, input$lambda1)
  })
  x2 <- reactive({
    input$simulate
    rexp(input$n, input$lambda2)
  })
  output$plt <- renderPlot(ggplot() + geom_point(aes(x = x1(), y = x2())))
}
shinyApp(ui, server)