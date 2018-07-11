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



#source('Simulation_Data.R')
source('Data_Import.R')
source('ui_outside_functions.R')
source('Header.R')
source('Sidebar.R')
source('ui_color.R')
source('Module/Module_Class.R')
source('Module/Module_Student.R')
source('Module/Module_Summary.R')



#Submodule
source('Module/Submodule/C_Hist.R')
source('Module/Submodule/C_Scatter.R')
source('Module/Submodule/C_Box.R')
source('Module/Submodule/C_Category.R')
source('Module/Submodule/C_Progress.R')

ui <- dashboardPage( skin = "blue", 
 
HeaderUI("mod_header"),                   

SidebarUI("mod_sidebar"),


#############################
#
# Body
#
#############################
dashboardBody(

#ColorUI("mode_color"),  
    
tabItems(

  
  
  
#PassUI("mod_pass")
## Login module;

# tabItem(tabName = "Login",
# 
# tagList(
#   tags$head(
#     tags$link(rel="stylesheet", type="text/css",href="style.css"),
#     tags$script(type="text/javascript", src = "md5.js"),
#     tags$script(type="text/javascript", src = "passwdInputBinding.js")
#   )
# ),
# 
# div(class = "login",
#     style="text-align:right; font-weight:bold",
#     box(height = "500%",
#     #title = "راوی، روایتگر قصه های شما",
#     status = "primary",width = "250%",
#     uiOutput("uiLogin"),
#     textOutput("pass"),
#     uiOutput("txt"))
# )
#         ),
  
##########
# Tab: Summary
##########  
  tabItem(tabName = "Summary",
          SummaryUI("mod_summary")
  ),  
      
    
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
)

  )
)


)



#################################
#################################   Server
#################################



server <- function(input, output,session) {

    #USER1 <- callModule(Pass,"mod_pass")
  
  #source('Module/Module_Pass.R',local = TRUE)
  
  # observe({
  # 
  #   
  #   if (USER$Logged == TRUE) {    

      
      
    callModule(Header,"mod_header")
  
    callModule(Sidebar,"mod_sidebar")  
  

#########
# Summary
#########

callModule(Summary,"mod_summary")  


#######
## Class
#######
  
callModule(Class,"mod_CP2",DP2M,date_P2,names_P2)
# 
callModule(Class,"mod_CP3",DP3M,date_P3,names_P3)  


   # }
   #})
  
  

}


shinyApp(ui, server 
         #enableBookmarking = "url"
         )
