32

####################### 3 important stackable panels
#######################
#######################
####################### Assemble layers of panels, z coordinate
####################### tabsetPanel() , navlistPanel() << tabPanel()


### tabsetPanel(): stack of layers; combines tabs into a single panel , using with tabPanel() function.
library(shiny)

ui<-fluidPage(
  tabsetPanel(
    tabPanel("tab1","Hi, You should know it"),
    tabPanel("tab2","test 2")
  )

)

server <- function(input,output){}
shinyApp(ui = ui, server = server)












#######################
#######################
#######################
####################### Example of tabsetPanel() ; tab navigation
library(shiny)

ui <- fluidPage(title = "Random generator",
                tabsetPanel(              
                  tabPanel(title = "Normal data",
                           plotOutput("norm"),
                           actionButton("renorm", "Resample")
                  ),
                  tabPanel(title = "Uniform data",
                           plotOutput("unif"),
                           actionButton("reunif", "Resample")
                  ),
                  tabPanel(title = "Chi Squared data",
                           plotOutput("chisq"),
                           actionButton("rechisq", "Resample")
                  )
                )
)

server <- function(input, output) {
  
  rv <- reactiveValues(
    norm = rnorm(500), 
    unif = runif(500),
    chisq = rchisq(500, 2))
  
  observeEvent(input$renorm, { rv$norm <- rnorm(500) })
  observeEvent(input$reunif, { rv$unif <- runif(500) })
  observeEvent(input$rechisq, { rv$chisq <- rchisq(500, 2) })
  
  output$norm <- renderPlot({
    hist(rv$norm, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard normal distribution")
  })
  output$unif <- renderPlot({
    hist(rv$unif, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard uniform distribution")
  })
  output$chisq <- renderPlot({
    hist(rv$chisq, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a Chi Square distribution with two degree of freedom")
  })
}

shinyApp(server = server, ui = ui)











#######################
#######################
#######################
####################### Example of navlistPanel() ; sidebar navigation
library(shiny)

ui <- fluidPage(title = "Random generator",
                navlistPanel(              
                  tabPanel(title = "Normal data",
                           plotOutput("norm"),
                           actionButton("renorm", "Resample")
                  ),
                  tabPanel(title = "Uniform data",
                           plotOutput("unif"),
                           actionButton("reunif", "Resample")
                  ),
                  tabPanel(title = "Chi Squared data",
                           plotOutput("chisq"),
                           actionButton("rechisq", "Resample")
                  )
                )
)

server <- function(input, output) {
  
  rv <- reactiveValues(
    norm = rnorm(500), 
    unif = runif(500),
    chisq = rchisq(500, 2))
  
  observeEvent(input$renorm, { rv$norm <- rnorm(500) })
  observeEvent(input$reunif, { rv$unif <- runif(500) })
  observeEvent(input$rechisq, { rv$chisq <- rchisq(500, 2) })
  
  output$norm <- renderPlot({
    hist(rv$norm, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard normal distribution")
  })
  output$unif <- renderPlot({
    hist(rv$unif, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard uniform distribution")
  })
  output$chisq <- renderPlot({
    hist(rv$chisq, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a Chi Square distribution with two degree of freedom")
  })
}

shinyApp(server = server, ui = ui)









####################### Use Prepackaged Layout
#######################
#######################
####################### sidebarLayout()
####################### sidebarPanel(), mainPanel() : divide app into two sections
library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "num", 
                  label = "Choose a number", 
                  value = 25, min = 1, max = 100),
      textInput(inputId = "title", 
                label = "Write a title",
                value = "Histogram of Random Normal Values")
    ),
    mainPanel(
      plotOutput("hist")
    )
  )
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num), main = input$title)
  })
}

shinyApp(ui = ui, server = server)














#######################
#######################
####################### 
####################### navbarPage() : replaces fluidPage() ; The entire app is a stack (images in www).
####################### do not need to combine fluidpage( navlistPanel())
####################### navbarPage() < navbarMenu() : combines tab links into a dropdown menu

library(shiny)

ui <- navbarPage(title = "Random generator",
                 tabPanel(title = "Normal data",
                          plotOutput("norm"),
                          actionButton("renorm", "Resample")
                 ),
                 navbarMenu(title = "Other data",
                            tabPanel(title = "Uniform data",
                                     plotOutput("unif"),
                                     actionButton("reunif", "Resample")
                            ),
                            tabPanel(title = "Chi Squared data",
                                     plotOutput("chisq"),
                                     actionButton("rechisq", "Resample")
                            )
                 )
)      
)

server <- function(input, output) {
  
  rv <- reactiveValues(
    norm = rnorm(500), 
    unif = runif(500),
    chisq = rchisq(500, 2))
  
  observeEvent(input$renorm, { rv$norm <- rnorm(500) })
  observeEvent(input$reunif, { rv$unif <- runif(500) })
  observeEvent(input$rechisq, { rv$chisq <- rchisq(500, 2) })
  
  output$norm <- renderPlot({
    hist(rv$norm, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard normal distribution")
  })
  output$unif <- renderPlot({
    hist(rv$unif, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard uniform distribution")
  })
  output$chisq <- renderPlot({
    hist(rv$chisq, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a Chi Square distribution with two degree of freedom")
  })
}

shinyApp(server = server, ui = ui)
















#######################
#######################
#######################
####################### Shinydashboard is a package with many more interesting appearance tools
####################### e.g. dashboardPage()
####################### look at rstudio.github.io/shinydashboard
