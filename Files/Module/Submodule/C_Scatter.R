C_ScatterUI <- function(id,date,names){
  
  ns <- NS(id)
  
  tabPanel(title = "دانش آموزان", icon=icon("user-circle"),
           
### For Error Message          
           tags$head(
             tags$style(HTML("
                             .shiny-output-error-validation {
                             color: red;
                             font-size: 16px;
                             margin-top: 10px;
                             margin-left: 10px;
                             }
                             "))
             ),
###


            fluidRow(                   
             
             column(1,
                    dropdownButton(
                      
                      label = "دانش آموزان مورد نظر را انتخاب کنید", status = "primary", width = 80,
                      
                      
                      div(style="display:inline-block; margin-top: 20px; ",
                          checkboxInput(ns('St_all'), 'تمام / هیچ')),
                      checkboxGroupInput(inputId = ns("St_ChG"), label = "", choices = c(names)))
                    
             ),
             
             column(1,offset=2,
                    div(style="display:inline-block; margin-top:20px;",
                        actionButton(inputId = ns("St_Ac"),label = "آنالیز"))
             ),

             
             column(1, 
                    div(style="display:inline-block;margin-top:20px; ",
                        checkboxInput(ns('St_Mean'), 'میانگین'))
             ),
             
             column(1,                      
                    div(style="display:inline-block;margin-top:20px;",
                        checkboxInput(inputId = ns("St_chbI"),label = "تجمیع نمودارها",value = FALSE))
             ),
             
             column(1,
                    div(style="display:inline-block; margin-top:5px; margin-left:50px  ",
                        radioButtons(ns('St_rb'), "نوع تخمین" ,
                                     choices = c("خطی"="lm","غیرخطی"="loess")))
             )
             
           ),
           
           
           
           plotlyOutput(ns("St"))
  )
}







######################
#
# Server Logic
#
######################

C_Scatter <- function(input,output,session,Data,date,names){

  
  print("SCATTER PLOOOOT")
  
  Mean <- apply(Data,2,mean)
  print(Mean)
  
  
  observe({
    updateCheckboxGroupInput(
      session, 'St_ChG', choices = names,
      selected = if (input$St_all) names
    )
  })
  
  melt_Data_St <- reactive({
 
    print("MELT DATA MEAN ddddddddd")
    
    validate(
      need((input$St_Mean==TRUE)||!is.null(input$St_ChG),"حداقل باید یک نمودار را انتخاب کنید")
    )
    
    if(input$St_Mean==TRUE){
      
      print("ST_MEAN ==== TRUE ")
      
      if(is.null(input$St_ChG)==TRUE){
        print("d NULL")
        d <- t(as.matrix(Mean))
        rownames(d) <- "میانگین"
      }else{
        
        print("d NOOOOO NULL ")
        d <- as.data.frame(Data[which(rownames(Data) %in% input$St_ChG),,drop=FALSE])      
        d <- rbind(d,Mean)
        rownames(d) <- c(rownames(d)[-length(rownames(d))],"میانگین")
      }
      
      colnames(d) <- 1:dim(d)[2]    
    
    }else{
      
      print("ST_MEAN ==== FALSE ")
      
      d <- as.data.frame(Data[which(rownames(Data) %in% input$St_ChG),,drop=FALSE]) 
      colnames(d) <- 1:dim(d)[2] 
    }
    
    d <- melt(as.matrix(d))
    colnames(d) <- c("Student","Day","value")
    print(d)
    return(d)
  })
  

  
  Reac_CP2_Sc <- eventReactive(input$St_Ac, {
    
    m <- reactive({lm(value ~ Day, melt_Data_St())})

    text <- reactive({ coef(m())[1] })
    
    if(input$St_chbI == FALSE){
      p <- ggplot(melt_Data_St(), aes(Day, value)) + geom_point(aes(color = Student)) +
        stat_smooth(aes(color = Student),method = input$St_rb) +
        facet_wrap( ~ Student,as.table = FALSE, scales = "free_x")+
        theme(axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"),
              axis.text.x  = element_text(face="bold",angle=45, vjust=0.5, size=5)) +
        labs(title="تحلیل زمانی دانش آموزان",color="دانش آموزان") +
        scale_x_discrete(name ="تاریخ امتحان", limits=date) +
        annotate('text',x = 9,y = 18,label= text())+
        xlab("زمان") + ylab("نمره")
    }
    else{
      p <- ggplot(melt_Data_St(), aes(Day, value)) + geom_point(aes(color = Student)) +
        stat_smooth(aes(color = Student),method = input$St_rb,se=FALSE) +
        theme(axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"),
              axis.text.x  = element_text(face="bold",angle=45, vjust=0.5, size=5)) +
        labs(title="تحلیل زمانی دانش آموزان",color="دانش آموزان") +
        scale_x_discrete(name ="تاریخ امتحان", limits=date) +
        annotate('text',x = 9,y = 18,label= text())+
        #geom_text(aes(color=Student),position = position_dodge(width = 1),label = text(), parse = TRUE)+
        xlab("زمان") + ylab("نمره")
    }
    
    
    gg <- ggplotly(p)
    gg
    
  })
  
  output$St <- renderPlotly(Reac_CP2_Sc())
  
}
