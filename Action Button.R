







#######################
#######################
#######################
#######################
#######################
####################### eventReactive: to delay reactions until a user clicks the action button.
library(shiny)

ui<-fluidPage(
  
  sliderInput("x",label="Input",min = 0,max = 100,value = 5),
  ## Action buttons  
  actionButton(inputId = "go",label = "Action"),
  ##  
  plotOutput("plot")
)

server <- function(input,output){
  
  data <- eventReactive(input$go, {
    runif(input$x)
  })
  
  output$plot <- renderPlot({
    hist(data() , main="Histogram")
  })
  
}
shinyApp(ui = ui, server = server)





#######################
#######################
#######################
#######################
#######################
####################### reactiveValues() : To build several action buttons that control the "same object" (same location) ; combine obserEvent()   
library(shiny)
test<-function(n){
  return(rnorm(n))
}

ui<-fluidPage(
  sliderInput(inputId = "num",label = "histogram",min = 0,max = 100,value = 25),
  
  ## Action buttons  
  actionButton(inputId = "go1",label = "Uniform"),
  actionButton(inputId = "go2",label = "Normal"),
  actionButton(inputId = "reset",label = "Clear"),
  ##  
  
  plotOutput(outputId = "hist"),
  tableOutput(outputId = "table"),
  plotOutput("plot")
)

server <- function(input,output){
  
  data <- reactive(test(input$num))
  
  ## Action Button
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$go1, {
    v$data <- runif(100)
  })
  observeEvent(input$go2, {
    v$data <- rnorm(100)
  })
  observeEvent(input$reset, {
    v$data <- NULL
  })  
  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    hist(v$data)
  })
  
  ##
  
  
  output$hist <- renderPlot({
    title <- "histogram"
    hist(data() , main=title)
  })
  output$table <- renderTable(data())
}
shinyApp(ui = ui, server = server)






