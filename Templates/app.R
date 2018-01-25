## app.R ##
library(shiny)
library(stats)
library(XLConnect)
library(xlsx)
library(readxl)
library(openxlsx)
library(shinydashboard)
library(ConvCalendar)
library(truncnorm)
library(graphics)
library(reshape2)
library(plotly)
library(ggplot2)



#source('Simulation_Data.R')
source('Data_Import.R')
source('ui_outside_functions.R')
source('ui_Header.R')
source('ui_Sidebar.R')
source('Class_Module.R')
source('Student_Module.R')
source('Summary_Module.R')


ui <- dashboardPage( skin = "blue", 
 
HeaderUI("mod_header"),                   

SidebarInput("mod_sidebar"),


#############################
#
# Body
#
#############################
dashboardBody(

tabItems(
    
    
##########
# Tab: Class
##########         
    
      tabItem(tabName="CP",
            selectInput("CPG","گروه",
                        c("گروه را انتخاب کنید",
                          "یک"="CP1",
                          "دو"="CP2",
                          "سه"="CP3")),

            
## CP2            
            conditionalPanel(
              condition = "input.CPG == 'CP2' ",
              ClassUI("mod_CP2",date_P2,names_P2)
                            ),
            
## CP3
            conditionalPanel(
              condition = "input.CPG == 'CP3' ",
              ClassUI("mod_CP3",date_P3,names_P3)
            )
            
    ),
    
    
    
    
##########
# Tab: Student
##########  
tabItem(tabName="Student",    
StudentUI("mod_student",names_all)
),




##########
# Tab: Summary
##########  
    tabItem(tabName = "Summary",
SummaryUI("mod_summary")
    )

  )
)


)



#################################
#################################   Server
#################################



server <- function(input, output,session) {

  
callModule(Header,"mod_header")
  
callModule(Sidebar,"mod_sidebar")  
  

#########
# Summary
#########

callModule(Summary,"mod_summary")  


#######
## Class
#######
  
callModule(Class,"mod_CP2",DP2M,date_P2,names_P2,CP2M_Mean)

callModule(Class,"mod_CP3",DP3M,date_P3,names_P3,CP3M_Mean)  


}


shinyApp(ui, server 
         #enableBookmarking = "url"
         )
