#######################
#######################
#######################
#######################
#######################
####################### observeEvent(): triggers code to run on the server






### To run, do not command Enter ; click on the Run App in the top right of this panel.



library(shiny)

ui<-fluidPage(
  
  # soem functions like h1 comes with a wrapper function
  tags$h1("First level Header"),
  tags$h2("2nd level Header"),
  tags$h3("3rd level Header"),
  tags$h4("Fourth level Header"),
  h5("Fifth level Header"),
  
  
  # paragrapgh
  
  tags$p("This is a",
         strong("Shiny"),      # Bold
         em("app"),            # Italic
         "by me."),
  
  # Line Breack
  br(),
  br(),
  hr(),
  tags$br(),
  
  # horizental line
  tags$hr(),
  
  # link
  tags$a(href = "https://www.google.com" , "GOOGLE"),
  
  # image    "src" stands for source : can be in www fodlder or be a web link
  tags$img(src='logo.png',width="100px",height="100px"),
  
  # text format computer code
  tags$br(),
  tags$code("This is my code in Shiny")
)

server <- function(input,output){}
shinyApp(ui = ui, server = server)







### Add raw html code with HTML(): If you have a HTML chink code, you can come it in HTML(##chink code) in the fluid page and it knows
### it is a HTML code not R code.
# library(shiny)
# 
# ui<-fluidPage(
#  HTML(
#    "<h1> My Shiny App <h1>"
#  ) 
# )
# 
# server <- function(input,output){}
# shinyApp(ui = ui, server = server)

