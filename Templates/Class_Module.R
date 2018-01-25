library(shiny)
library(ggplot2)

ClassUI <- function(id,date,names){

  ns <- NS(id)
  
  
  tabsetPanel(selected = NULL,
              
            tabPanel(title="درس را انتخاب کنید",icon = icon("mail-forward")
            ),
            
            tabPanel( title = "ریاضی" ,
                      
                      tabsetPanel(
                        
                        tabPanel(title="تحلیل را انتخاب کنید",icon = icon("mail-forward")
                        ),
                        
                        ###############
                        #### Boxplot
                        ###############
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
                        ),
                        
                        ################
                        #### Histogram
                        ################
                        tabPanel(title = "هیستوگرام",
                                 
                                 fluidRow(            
                                   
                                   column(1,
                                          div(style="display:inline-block; width: 150px; margin-top: 15px;",
                                              selectInput(inputId = ns("Hg_SeI"),label = "زمان",choices = date,selected =tail(date,n=1)))
                                   ),
                                   
                                   column(1,offset=1,
                                          div(style="display:inline-block; width: 150px; margin-top: 15px;",
                                              textInput(inputId = ns("Hg_bin"),label = "بین",value = 10))
                                   ),
                                   column(1,offset=1,
                                          
                                          div(style="display:inline-block;margin-top: 40px;",
                                              actionButton(inputId = ns("Hg_Ac"),label = "آنالیز"))
                                   )
                                 ),
                                 
                                 
                                 plotOutput(ns("Hg")),icon=icon("bar-chart")
                        ),
                        
                        
                        ###################
                        #### Scatterplot
                        ###################
                        tabPanel(title = "دانش آموزان",
                                 
                                 fluidRow(                   
                                   
                                   column(1,
                                          dropdownButton(
                                            
                                            label = "دانش آموزان مورد نظر را انتخاب کنید", status = "primary", width = 80,
                                            
                                            
                                            div(style="display:inline-block; margin-top: 20px;",
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
                                   )
                                 ),
                                 
                                 
                                 
                                 plotlyOutput(ns("St")),icon=icon("bar-chart")
                        )
                        
                      )
                      
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









Class <- function(input,output,session,Data,date,names,Mean){
  ################
  #Class P2
  ################
  ## Box Plot
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
  
  
  ### Histogram
  melt_Data_Hg <- reactive({
    d <- as.data.frame(Data[,input$Hg_SeI])
    d <- melt(as.matrix(d))
    colnames(d) <- c("Student","Day","value")
    d
  })
  Reac_Hg <- eventReactive(input$Hg_Ac, {
    ggplot(melt_Data_Hg(),aes(value)) + geom_histogram(aes(y=..density..),bins=input$Hg_bin,colour="black", fill="green")+
      geom_density(aes(y=..density..),alpha=0.5, fill="#FF6666",colour="red",lwd=1.5)
  })
  
  output$Hg <- renderPlot(Reac_Hg())
  
  
  ### ScatterPlot
  
  observe({
    updateCheckboxGroupInput(
      session, 'St_ChG', choices = names,
      selected = if (input$St_all) names
    )
  })
  
  melt_Data_St <- reactive({
    
    if(input$St_Mean==TRUE){
      
      if(is.null(d)==TRUE){
        d <-  Mean
        rownames(d) <- "میانگین"
      }
      
      else{
        d <- as.data.frame(Data[which(rownames(Data) %in% input$St_ChG),,drop=FALSE])      
        d <- rbind(d,Mean)
        rownames(d) <- c(rownames(d)[-length(rownames(d))],"میانگین")
      }
      
      colnames(d) <- 1:dim(d)[2]    
    }else{
      d <- as.data.frame(Data[which(rownames(Data) %in% input$St_ChG),,drop=FALSE]) 
      colnames(d) <- 1:dim(d)[2] 
    }
    
    d <- melt(as.matrix(d))
    colnames(d) <- c("Student","Day","value")
    d
  })
  
  Reac_CP2_Sc <- eventReactive(input$St_Ac, {
    
    
    if(input$St_chbI == FALSE){
      p <- ggplot(melt_Data_St(), aes(Day, value)) + geom_point(aes(color = Student)) +
        stat_smooth(aes(color = Student),method = "loess") +
        facet_wrap( ~ Student,as.table = FALSE, scales = "free_x")+
        theme(axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"),
              axis.text.x  = element_text(face="bold",angle=45, vjust=0.5, size=5)) +
        labs(title="تحلیل زمانی دانش آموزان",color="دانش آموزان") +
        scale_x_discrete(name ="تاریخ امتحان", limits=date) +
        xlab("زمان") + ylab("نمره")
    }
    else{
      p <- ggplot(melt_Data_St(), aes(Day, value)) + geom_point(aes(color = Student)) +
        stat_smooth(aes(color = Student),method = "loess",se=FALSE) +
        theme(axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"),
              axis.text.x  = element_text(face="bold",angle=45, vjust=0.5, size=5)) +
        labs(title="تحلیل زمانی دانش آموزان",color="دانش آموزان") +
        scale_x_discrete(name ="تاریخ امتحان", limits=date) +
        xlab("زمان") + ylab("نمره")
    }
    
    
    gg <- ggplotly(p)
    gg
    
  })
  
  output$St <- renderPlotly(Reac_CP2_Sc())
}