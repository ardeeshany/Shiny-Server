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

tabItem(tabName = "Summary", M_SummaryUI(ns("summary"))),  
tabItem(tabName = "CP",M1_ClassUI(ns("cp"))), 
tabItem(tabName = "C3",M1_ClassUI(ns("c3"))), 
tabItem(tabName="Student", M_StudentUI(ns("student"),names_all))
      
       )
    )
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
  
callModule(M_Summary,"summary")
callModule(M1_Class,"cp") 
callModule(M1_Class,"c3") 
callModule(M_Student,"student")  

#}  
  
}