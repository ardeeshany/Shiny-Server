M2_ClassUI <- function(id){

  ns <- NS(id)
  
  
  tabsetPanel(selected = NULL,
        tabPanel(title="درس را انتخاب کنید",icon = icon("mail-forward")),
        tabPanel(title = "ریاضی", M3_ClassUI(ns("riazi"))),
        tabPanel(title = "فیزیک", M3_ClassUI(ns("fizik"))),
        tabPanel(title = "شیمی", M3_ClassUI(ns("shimi")))
                        
                                    )}


M2_Class <- function(input,output,session){

  callModule(M3_Class,"riazi",outputDir = "RAAVI/RAAVI/Data/School1/Pish/Level2/riazi")
  callModule(M3_Class,"fizik",outputDir = "RAAVI/RAAVI/Data/School1/Pish/Level2/riazi")
  callModule(M3_Class,"shimi",outputDir = "RAAVI/RAAVI/Data/School1/Pish/Level2/riazi")

}
