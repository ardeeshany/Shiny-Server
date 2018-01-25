StudentUI <- function(id,names){
  
  ns <- NS(id)
  

tagList(          
     
          fluidRow(
            
            column(1,
                   div(style=" display:inline-block; width: 300px; margin-top: 15px;",
                       selectizeInput(inputId=ns("Student_input"), label="نام دانش آموز", choices= names , selected = NULL, multiple = TRUE,
                                      options = NULL))
            ),
            column(1, offset=3,
                   div(style="display:inline-block; margin-top: 40px",
                       actionButton(inputId = ns("STinp_act"),label = "آنالیز"))
                   
            )
          ),
          
          conditionalPanel( condition = "input.STinp_act%2==1", ns=ns,
                            tabsetPanel(
                              
                              tabPanel(title="تحلیل را انتخاب کنید",icon = icon("mail-forward")
                              ),
                              tabPanel(title="خلاصه",
                                       box(
                                         title = "Box title1", width = 6, status = "primary",
                                         "Box content"
                                       )      
                              )
                            )
          )
)

}

Student <- function(input,output,session){
  
}