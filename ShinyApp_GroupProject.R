# Shiny App Group Project
## Rebecca Rogers

# Shiny app that shows the histograms and summary of mtcars variables
ui <- fluidPage(
  titlePanel("Mtcars Scatter Plot"),
  selectInput("var1", "Variable 1", choices = names(mtcars)),
  plotOutput("plot"),
  verbatimTextOutput("code")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(mtcars, aes(x = .data[[input$var1]])) +
      geom_histogram(fill = "purple") +
      ggtitle("MtCars Scatter Plot")
  })
  output$code <- renderPrint({
    summary(mtcars[[input$var1]])
  })
}

shinyApp(ui = ui, server = server)

