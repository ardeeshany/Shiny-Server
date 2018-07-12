M_ClassUI <- function(id){

  ns <- NS(id)
  
  
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
  )
  
}




M_Class <- function(input,output,session){

callModule(Class,"mod_CP2",DP2M,date_P2,names_P2)
callModule(Class,"mod_CP3",DP3M,date_P3,names_P3)  

}
