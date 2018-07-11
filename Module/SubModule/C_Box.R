C_BoxUI <- function(id,date,names){
  
  ns <- NS(id)
  
  tabPanel(title = "نمودار جعبه ای",
           
           fluidRow(
             
             column(1,
                    div(style="display:inline-block; width: 150px; margin-top: 15px;",
                        selectInput(inputId = ns("Bx_SeI1"),label = "زمان ابتدا",choices = date,selected = tail(date,4)[1]))
             ),
             column(1, offset=1,
                    div(style="display:inline-block; width: 150px; margin-top: 15px;",
                        selectInput(inputId = ns("Bx_SeI2"),label = "زمان انتها",choices = date,selected = tail(date,1)))
             ),
             column(1, offset=1,
                    div(style="display:inline-block; margin-top: 40px;",
                        actionButton(inputId = ns("Bx_Ac"),label = "آنالیز"))
             )
           ),
           plotOutput(ns("Bx")),icon=icon("square")
  )
}







######################
#
# Server Logic
#
######################

C_Box <- function(input,output,session,Data,date,names){
  
  Mean <- apply(Data,2,mean)  
  
  melt_Data_Bx <- reactive({
    min=which(date==input$Bx_SeI1)
    max=which(date==input$Bx_SeI2)
    
    if(min < max){
      d <- as.data.frame(Data[,min:max])
    }
    if(min==max){
      d <- as.data.frame(Data[,max])
    }
    if(min > max){
      d <- as.data.frame(Data)
    }
    d <- melt(as.matrix(d))
    colnames(d) <- c("Student","Day","value")
    d$Day <- factor(d$Day)
    d
  })
  
  Reac_CP2M_Bx <- eventReactive(input$Bx_Ac, {
    min=which(date==input$Bx_SeI1)
    max=which(date==input$Bx_SeI2)
    if(min <=max){
      ggplot(melt_Data_Bx() , aes(x=Day,y=value,fill=Day)) + geom_boxplot(outlier.size=4) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
        labs(title = "Grade", x = "Date")+
        scale_x_discrete(labels=date[which(date==input$Bx_SeI1):which(date==input$Bx_SeI2)])+
        theme(axis.text.x = element_text(size=15,colour="red",angle=90, hjust=1,vjust=.5)) }
    else{
      ggplot(melt_Data_Bx() , aes(x=Day,y=value,fill=Day)) + geom_boxplot(outlier.size=4) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
        labs(title = "Grade", x = "Date")+
        scale_x_discrete(labels=date)
    }
  })
  output$Bx <- renderPlot(Reac_CP2M_Bx())
}