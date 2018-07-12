BodyUI <- function(id){
  

  ns <- NS(id)

  
dashboardBody(
    
    
    #ColorUI("mode_color"),  
    
tabItems(
      
      
#### Login ####      
      
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
# ),
#### Login ####  

## Summmary ##
tabItem(tabName = "Summary",
              SummaryUI(ns("mod_summary"))),  


## Class > P ##      
tabItem(tabName="CP",
          
          box(width = "130%",collapsible = TRUE,status = "primary",
                  
              tabsetPanel(selected = NULL,
                    
                    tabPanel(title="گروه را انتخاب کنید",icon = icon("mail-forward")),
                              
                    tabPanel(title="یک"),
                              
                    tabPanel(title="دو",
                      ClassUI(ns("mod_CP2"),date_P2,names_P2)),
                              
                    tabPanel(title="سه",
                      ClassUI(ns("mod_CP3"),date_P3,names_P3))
              
                         ))
              ),
      
## Student ##      
tabItem(tabName="Student",   
              StudentUI(ns("mod_student"),names_all))
      
    ))

}

Body <- function(input,output,session){
  
#### Login ####  
  #USER1 <- callModule(Pass,"mod_pass")
  #source('Module/Module_Pass.R',local = TRUE)
  # observe({
  # 
  #   
  #   if (USER$Logged == TRUE) {   
#### Login ####    
  
  callModule(Summary,"mod_summary")  
  callModule(Class,"mod_CP2",DP2M,date_P2,names_P2)
  callModule(Class,"mod_CP3",DP3M,date_P3,names_P3)  
  callModule(Student,"mod_student")  

#}  
  
}