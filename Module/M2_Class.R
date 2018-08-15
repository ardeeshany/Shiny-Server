M2_ClassUI <- function(id,date,names){

  ns <- NS(id)
  
  
  tabsetPanel(selected = NULL,
        tabPanel(title="درس را انتخاب کنید",icon = icon("mail-forward")),
        tabPanel(title = "ریاضی", M3_ClassUI(ns("riazi"),date,names)),
        tabPanel(title = "فیزیک", M3_ClassUI(ns("fizik"),date,names)),
        tabPanel(title = "شیمی", M3_ClassUI(ns("shimi"),date,names))
                        
                                    )}


M2_Class <- function(input,output,session,Data,date,names){

  callModule(M3_Class,"riazi",Data,date,names)
  callModule(M3_Class,"fizik",Data,date,names)
  callModule(M3_Class,"shimi",Data,date,names)

}
