M3_ClassUI <- function(id,date,names){

  ns <- NS(id)

  tabsetPanel(
                        
     tabPanel(title="تحلیل را انتخاب کنید",icon = icon("mail-forward")),
     C_BoxUI(ns("Box"),date,names),
     C_HistUI(ns("Hist"),date,names),
     C_ScatterUI(ns("Scatter"),date,names),                       
     C_CatUI(ns("Category"),date,names),
     C_ProgUI(ns("Progress"),date,names),
     C_LoadUI(ns("Load")) 
                
  )}




M3_Class <- function(input,output,session,Data,date,names){

     callModule(C_Box,"Box",Data,date,names)
     callModule(C_Hist,"Hist",Data)
     callModule(C_Scatter,"Scatter",Data,date,names)
     callModule(C_Cat,"Category",Data,date,names)
     callModule(C_Prog,"Progress",Data,date,names)
D <- callModule(C_Load,"Load")

}
