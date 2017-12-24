library(shiny)
test<-function(n){
  return(rnorm(n))
}

ui<-fluidPage(
  sliderInput(inputId = "num",label = "histogram",min = 0,max = 100,value = 25),
 
  plotOutput(outputId = "hist"),
  tableOutput(outputId = "table")
)

server <- function(input,output){
 
   data <- reactive(test(input$num))

    output$hist <- renderPlot({
    title <- "histogram"
    hist(data() , main=title)
    })
  output$table <- renderTable(data())
}
shinyApp(ui = ui, server = server)





