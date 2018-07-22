ClassUI <- function(id,date,names){

  ns <- NS(id)
  
  
  tabsetPanel(selected = NULL,
            tabPanel(title="درس را انتخاب کنید",icon = icon("mail-forward")
            ),
            
            
            tabPanel( title = "ریاضی" ,
                      
                      tabsetPanel(
                        
                        tabPanel(title="تحلیل را انتخاب کنید",icon = icon("mail-forward")
                        ),
                        

#### Boxplot
C_BoxUI(ns("C_Box"),date,names),
                      
### Histogram                       
C_HistUI(ns("C_Hist"),date,names),

#### Scatterplot
C_ScatterUI(ns("C_Scatter"),date,names),                       
                        

#### DataTable
C_CatUI(ns("C_Category"),date,names),
   
#### Progress
C_ProgUI(ns("C_Progress"),date,names),

C_LoadUI(ns("C_Load")) 
                      
)


########################
                      
            ),
            
            
            
            ###
            ### Physics
            ###
            
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






#############################################
#
#
#       Server Logic
#
#
#############################################









Class <- function(input,output,session,Data,date,names){
  ################
  #Class P2
  ################
  
## Box Plot
callModule(C_Box,"C_Box",Data,date,names)
  
### Histogram
callModule(C_Hist,"C_Hist",Data)
  
### ScatterPlot
callModule(C_Scatter,"C_Scatter",Data,date,names)
  
### DataTable 
callModule(C_Cat,"C_Category",Data,date,names)
  
### Progress 
callModule(C_Prog,"C_Progress",Data,date,names)

callModule(C_Load,"C_Load")
 
}
