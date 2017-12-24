#######################
#######################
#######################
#######################
####################### CSS: Cascading Style Sheets : what makes the appearance of HTML page interestnig to look.


### Shiny's general CSS classes come from the "bootstrap 3" framework.

### To learn more about CSS & HTML: www.codecademy.com/tracks/web
### and css articles in shiny.rstudio.com

library(shiny)
test<-function(n){
  return(rnorm(n))
}

ui<-fluidPage(
  
  
  # 1- Pick the theme ... with external CSS
  theme = "bootswatch-cerulean.css",
  
  # 2- write global CSS with tags$head(), tags$style(), HTML(), 
  # or includeCSS(): save the CSS as a file in your app directory and include it

  tags$head("HELLO"),
  
  # 3- write indiviual CSS in a tag's style attribute
  tags$h1(" Hello World", style="color:red;"),
  
  ## Shiny has 12 panel functions or grouping some elements together (as a panel), 
  ## e.g wellpanel(), mainpanle() and etc.
  
  wellPanel(
    fluidRow(
      column(2,offset=3, sliderInput(inputId = "num",label = "histogram",min = 0,max = 100,value = 25)),
      column(3,offset=3, tableOutput(outputId = "table"))
    )),
  
  fluidRow(
    column(5,offset=2,plotOutput(outputId = "hist"))
  )
  
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


