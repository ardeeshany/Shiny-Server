library(shiny)
test<-function(n){
  return(rnorm(n))
}

ui<-fluidPage(
  sliderInput(inputId = "num",label = "Hello",min = 0,max = 100,value = 25),
  plotOutput(outputId = "hist"),
  tableOutput(outputId = "table")
)

server <- function(input,output){
  output$hist <- renderPlot({
    title <- "histogram"
    hist(test(input$num) , main=title)
    })
  output$table <- renderTable(test(input$num))
}

shinyApp(ui = ui, server = server)
