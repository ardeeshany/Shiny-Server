#######################
#######################
#######################
#######################
#######################
####################### observeEvent(): triggers code to run on the server


### To run, do not command Enter ; click on the Run App in the top right of this panel.
library(shiny)

ui<-fluidPage(
  # image    "src" stands for source
  tags$img(src='logo.png',width="100px",height="100px")
)

server <- function(input,output){}
shinyApp(ui = ui, server = server)

