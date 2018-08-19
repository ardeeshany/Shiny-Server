
M2_ClassUI <- function(id){

  ns <- NS(id)
  
  tabsetPanel(selected = NULL,
        tabPanel(title="درس را انتخاب کنید",icon = icon("mail-forward")),
        tabPanel(title = "ریاضی", M3_ClassUI(ns("riazi"))),
        tabPanel(title = "فیزیک", M3_ClassUI(ns("fizik"))),
        tabPanel(title = "شیمی", M3_ClassUI(ns("shimi")))
                        
                                    )}


M2_Class <- function(input,output,session,outputlev,class,level){

  
      V1 <- callModule(M3_Class,"riazi",outputDir = sprintf("%s/riazi",outputlev),class,level,course="ریاضی")
      V2 <- callModule(M3_Class,"fizik",outputDir = sprintf("%s/fizik",outputlev),class,level,course="فیزیک")
       callModule(M3_Class,"shimi",outputDir = sprintf("%s/shimi",outputlev),class,level,course="shimi")
      
      V <- reactive({ 
        rbind(V1(),V2())
                   })
      
      return(V)      
}
