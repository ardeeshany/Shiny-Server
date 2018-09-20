M_StudentUI <- function(id,names){
  
  ns <- NS(id)
  
  tagList(          
    
    fluidRow(
      
      column(1,
             div(style=" display:inline-block; width: 300px; margin-top: 15px;",
                 selectizeInput(inputId=ns("Student_input"), label= "نام دانش آموز را وارد کنید", choices= names , selected = NULL, multiple = FALSE,
                                options = NULL))
      ),
      column(1, offset=3,
             div(style="display:inline-block; margin-top: 40px",
                 actionButton(inputId = ns("STinp_act"),label = "انتخاب"))
             
      )
    ),
  
  conditionalPanel( condition = "input.STinp_act%2==1", ns=ns,
  tabsetPanel(
    tabPanel(title="تحلیل را انتخاب کنید",icon = icon("mail-forward")),
    M0_St_ScatterUI(ns("Scatter"))
            ))
    )}

M_Student <- function(input,output,session,Vals,add="RAAVI/RAAVI/Data"){
  
  callModule(M0_St_Scatter,"Scatter",Vals)
  
 
#M <- tidyr::gather(cbind(name=rownames(DP2M),DP2M),key,value,-name)
#   saveData <- function(data,fileName){
#     # Create a unique file name
#     filePath <- file.path(tempdir(), sprintf("%s.xlsx",fileName)) # Write the data to a temporary file locally
#     #colnames(data)[1] <- "نام"
#     write.xlsx(x = data, file = filePath, row.names = FALSE)
#     drop_upload(filePath, path = add, autorename = TRUE,mode = "add")
#   }
#   
# observeEvent(input$act,{
#   saveData(as.data.frame(Vals()),"Tidy")
# })
  
}




