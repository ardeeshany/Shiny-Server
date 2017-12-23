#######################
#######################
#######################
#######################
#######################
####################### observeEvent(): triggers code to run on the server
library(shiny)

ui<-fluidPage(
  
  sliderInput("x",label="Input",min = 0,max = 100,value = 5),
  ## Action buttons  
  actionButton(inputId = "go",label = "Action")
)

server <- function(input,output){

  
### by each click, it prints the input value in R session.     
  observeEvent(input$go, {
    print(as.numeric(input$x))
  })

  
}
shinyApp(ui = ui, server = server)











#######################
#######################
#######################
#######################
#######################
####################### eventReactive(): to delay reactions until a user clicks the action button.
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
####################### Creat a list of reactive values
####################### reactiveValues() : Manage state; To build several action buttons that control the "same object" (same location) ; combine obserEvent()   
library(shiny)
test<-function(n){
  return(rnorm(n))
}

ui<-fluidPage(

  ## Action buttons  
  actionButton(inputId = "go1",label = "Uniform"),
  actionButton(inputId = "go2",label = "Normal"),
  actionButton(inputId = "reset",label = "Clear"),
  ##  
  
<<<<<<< HEAD:Templates/Action Button.R
  plotOutput("plot")
=======
  plotOutput("plot"),
  plotOutput("plot2")
>>>>>>> 2a54dd78f491d6981d781549a0b4feb15077085e:Action Button.R
)

server <- function(input,output){
  
<<<<<<< HEAD:Templates/Action Button.R
  # it can be more than one ractive values like : reactiveValues(data = NULL,data2=NULL)
  v <- reactiveValues(data = NULL)
=======
  # we can initiate values for data or data2.
  v <- reactiveValues(data = NULL,data2=NULL)
>>>>>>> 2a54dd78f491d6981d781549a0b4feb15077085e:Action Button.R
  
  observeEvent(input$go1, {
    v$data <- runif(100)
  })
  observeEvent(input$go2, {
    v$data2 <- rnorm(100)
  })

  
  observeEvent(input$reset, {
    v$data <- NULL
    v$data2 <- NULL
  })  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    hist(v$data)
  })
<<<<<<< HEAD:Templates/Action Button.R
=======
  output$plot2 <- renderPlot({
    if (is.null(v$data2)) return()
    hist(v$data2)
  })
>>>>>>> 2a54dd78f491d6981d781549a0b4feb15077085e:Action Button.R

}
shinyApp(ui = ui, server = server)






