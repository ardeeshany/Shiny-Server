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
tabItem(tabName = "C12",M1_ClassUI(ns("c12"))), 
tabItem(tabName = "C11",M1_ClassUI(ns("c11"))), 
tabItem(tabName = "C10",M1_ClassUI(ns("c10"))), 
tabItem(tabName="Student", M_StudentUI(ns("student"),names_all))
      
       )
    )
}

Body <- function(input,output,session, outputadrs="RAAVI/RAAVI/DATA/school1"){
  
  
          callModule(M_Summary,"summary")
  vals <- callModule(M1_Class,"c12",outputcls = sprintf("%s/12",outputadrs)) 
          callModule(M1_Class,"c11",outputcls = sprintf("%s/11",outputadrs))
          callModule(M1_Class,"c10",outputcls = sprintf("%s/10",outputadrs))
          callModule(M_Student,"student",Vals=vals)    
  
#### Login ####  
  #USER1 <- callModule(Pass,"mod_pass")
  #source('Module/Module_Pass.R',local = TRUE)
  # observe({
  # 
  #   
  #   if (USER$Logged == TRUE) {   
#### Login ####    
  


#}  
  
}