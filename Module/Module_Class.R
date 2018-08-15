ClassUI <- function(id,date,names){

  ns <- NS(id)
  
  
  tabsetPanel(selected = NULL,
            tabPanel(title="درس را انتخاب کنید",icon = icon("mail-forward")),
            
tabPanel(title = "ریاضی" ,
 tabsetPanel(
                        
    tabPanel(title="تحلیل را انتخاب کنید",icon = icon("mail-forward")),
    C_BoxUI(ns("C_Box"),date,names),
    C_HistUI(ns("C_Hist"),date,names),
    C_ScatterUI(ns("C_Scatter"),date,names),                       
    C_CatUI(ns("C_Category"),date,names),
    C_ProgUI(ns("C_Progress"),date,names),
    C_LoadUI(ns("C_Load")) 
                
  )
      ),
            
            
            tabPanel( title = "فیزیک" ,
                      tabsetPanel(
                        tabPanel(title = "سیب",
                                 box(
                                   title = "Box title", width = 6, status = "primary",
                                   "Box content"
                                 )
                        )
                      )
            )
)
}




Class <- function(input,output,session,Data,date,names){

callModule(C_Box,"C_Box",Data,date,names)
callModule(C_Hist,"C_Hist",Data)
callModule(C_Scatter,"C_Scatter",Data,date,names)
callModule(C_Cat,"C_Category",Data,date,names)
callModule(C_Prog,"C_Progress",Data,date,names)
callModule(C_Load,"C_Load")

}
