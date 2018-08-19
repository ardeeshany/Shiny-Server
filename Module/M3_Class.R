M3_ClassUI <- function(id){

  ns <- NS(id)

  tabsetPanel(
                        
     tabPanel(title="تحلیل را انتخاب کنید",icon = icon("mail-forward")),
     M0_BoxUI(ns("Box")),
     M0_HistUI(ns("Hist")),
     M0_ScatterUI(ns("Scatter")),                       
     M0_CatUI(ns("Category")),
     M0_ProgUI(ns("Progress")),
     M0_LoadUI(ns("Load")) 
                
  )}




M3_Class <- function(input,output,session,outputDir){

        callModule(M0_Box,"Box",vals)
        callModule(M0_Hist,"Hist",vals)
        callModule(M0_Scatter,"Scatter",vals)
        callModule(M0_Cat,"Category",vals)
        callModule(M0_Prog,"Progress",vals)
vals <- callModule(M0_Load,"Load",outputDir)

# V <- reactive({
#   vals
#   # M <- tidyr::gather(cbind(name=vals[["names"]],vals[["now"]]),key,value,-name)
#   # return(M)
# })

return(vals)

}
