## app.R ##
library(shiny)
library(stats)
library(XLConnect)
library(xlsx)
library(readxl)
library(openxlsx)
library(ConvCalendar)
library(shinydashboard)
library(truncnorm)
library(graphics)
library(reshape2)
library(plotly)
library(ggplot2)



## modules ##
source('ui_color.R')

source('Header.R')
source('Sidebar.R')
source('Body.R')

source('Data_Import.R')
source('ui_outside_functions.R')

source('Module/Module_Class.R')
  source('Module/Submodule/C_Hist.R')
  source('Module/Submodule/C_Scatter.R')
  source('Module/Submodule/C_Box.R')
  source('Module/Submodule/C_Category.R')
  source('Module/Submodule/C_Progress.R')
source('Module/Module_Student.R')
source('Module/Module_Summary.R')



## UI ##
ui <- dashboardPage( skin = "blue", 
 
HeaderUI("mod_header"),                   
SidebarUI("mod_sidebar"),
BodyUI("mod_body")


)


## Server ##
server <- function(input, output,session) {

callModule(Header,"mod_header")
callModule(Sidebar,"mod_sidebar")
callModule(Body,"mod_body")
  
}


shinyApp(ui, server) #,enableBookmarking = "url")
