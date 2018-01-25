source('ui_Sidebar.R')

SummaryUI <- function(id){

  ns <- NS(id)

tagList(    
  fluidRow(
  box(
    title = "Box title", width = 6, status = "primary",
    "Box content"
  ),
  box(
    status = "warning", width = 6,
    "Box content"
  )
),


# layout based on column priority; "column" 
fluidRow(
  column(width = 4,
         box(
           title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
           "Box content"
         ),
         box(
           width = NULL, background = "black",
           "A box with a solid black background"
         )
  ),
  
  column(width = 4,
         box(
           title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
           "Box content"
         ),
         box(
           title = "Title 5", width = NULL, background = "light-blue",
           "A box with a solid light-blue background"
         )
  ),
  
  column(width = 4,
         box(
           title = "Title 2", width = NULL, solidHeader = TRUE,
           "Box content"
         ),
         box(
           title = "Title 6", width = NULL, background = "maroon",
           "A box with a solid maroon background"
         )
  )
),
## InfoBox and ValueBox         
infoBoxOutput(ns("progressBox"),width = 3),
valueBoxOutput(ns("approvalBox"),width = 3)
##         

)

}




Summary <- function(input,output,session){
  # infoBox
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", "80%" , icon = icon("list"),
      color = "purple", fill = FALSE ,width = 1
    )
  })
  
  # valueBox    
  output$approvalBox <- renderValueBox({
    valueBox(
      "70%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow",width = 2
    )
  }) 
}