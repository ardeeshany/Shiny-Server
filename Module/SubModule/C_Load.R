

C_LoadUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel(title = "وارد کردن دیتا",
       
           fluidRow(
          
          column(width = 3,
          br(),
          box(width="200%",  
          titlePanel("ویرایش و ذخیره اطلاعات"),
           #sidebarLayout(
           #   sidebarPanel(
           #     helpText("Shiny app based on an example given in the rhandsontable package.", 
           #              "Right-click on the table to delete/insert rows.", 
           #              "Double-click on a cell to edit"),
               
               wellPanel(
                uiOutput(outputId = ns("f_set")),
                uiOutput(outputId = ns("f_new"))
               ),
               br(),
               
               
               
               wellPanel(
                 h3("Table options"),
                 radioButtons(ns("useType"), "Use Data Types", c("TRUE", "FALSE"))
               ),
               br(), 
               
               wellPanel(
                 h3("ذخیره کردن دیتا"), 
                 div(class='row', 
                     div(class="col-sm-6", 
                         actionButton(ns("save"), "Save")),
                     div(class="col-sm-6",
                         radioButtons(ns("fileType"), "File type", c("R", "xlsx")))
                 )
               )
               
             )),
             
             column(width = 9,
             br(),
             box(collapsible = TRUE,title = "تغییر دیتا",collapsed = TRUE,width = "200%",status = "info",
                 fluidRow(
                   column(width = 6,
                          wellPanel(
                          h3("ستون"),
                          div(style="width:55%;",
                          uiOutput(ns("ui_newcolname"))),
                          div(style="display:inline-block;width:30%;",
                          actionButton(ns("addcolumn"), "اضافه کردن ستون")),
                          #radioButtons(ns("newcolumntype"), "Type", c("integer", "double", "character")),
                          div(style="width:50%;",
                          uiOutput(ns("ui_removecolname"))),
                          div(style="display:inline-block;width:30%;",
                          actionButton(ns("removecolumn"),"حذف کردن ستون")))
                   ),
                   column(width = 6,
                          wellPanel(
                          div(style="display:inline-block;width:55%;",
                          uiOutput(ns("ui_newrowname"))),
                          div(style="display:inline-block;width:30%;",
                          actionButton(ns("addrow"),"اضافه کردن سطر"))),
                          #radioButtons(ns("newrowtype"), "Type", c("integer", "double", "character")),
                          wellPanel(
                          div(style="display:inline-block;width:50%;",
                          uiOutput(ns("ui_removerowname"))),
                          div(style="display:inline-block;width:30%;",
                          actionButton(ns("removerow"),"حذف کردن سطر")))
                   )
                   
                 )
               ),
               br(),
               box(status="primary",width="200%",collapsible = TRUE,collapsed = FALSE,
               actionButton(ns("cancel"), "Cancel last action"),
               br(), br(), 
               
               rHandsontableOutput(ns("hot")),
               br(),
               
               wellPanel(
                 uiOutput(ns("message"), inline=TRUE)
               ))
               
               
                 #h3("اضافه کردن ستون"),
  )
           )
  )
  
}







######################
#
# Server Logic
#
######################

C_Load <- function(input,output,session,outdir=getwd(),outfilename="Test_SAVE"){
  
  
   # DF <- eventReactive(input$f_new,{
   #   M <- readxl::read_excel(input$f_new$datapath)
   #   return(M)
   # }) 
  
  
   # DF <- data.frame(Value = 1:10, Status = TRUE, Name = LETTERS[1:10],
   #                  Date = seq(from = Sys.Date(), by = "days", length.out = 10),
   #                  stringsAsFactors = FALSE)
   
   
   
   DF2 <- as.data.frame(readxl::read_excel("Data_P2.xlsx"))
   DF3 <- as.data.frame(readxl::read_excel("Data_P3.xlsx")) 
   
   
   
   DF_tot <- reactive({
     A <- list(DF2,DF3)
     names(A) <- c("DF2","DF3") 
     return(A)
     })

   
             
   output$f_set <- renderUI({
     selectInput(inputId = session$ns("f_set"),label = "دیتا برای آنالیز",choices = names(DF_tot()))
     })

   
   
   output$f_new <- renderUI({
     textInput(inputId = session$ns("f_name"),label = "نام دیتا")
     fileInput(inputId = session$ns("f_new"),label = "آپلود کردن فایل جدید")
                           })
  # 
  #  observeEvent(input$f_new,{
  #   
  #    if(is.null(input$f_name))
  #       cat(paste0("please select a name"))
  #    else{
  #       File_name <- input$f_name
  #       File <- as.data.frame(read.xlsx(input$f_new$datapath,1))
  #       assigan(File_name,File)
  #        }
  # })

  
  values <- reactiveValues()
  names <- reactiveValues()
  
  observeEvent(input$f_set,{
    values[["now"]] <- DF_tot()[[input$f_set]][,-1]
    values[["names"]] <- DF_tot()[[input$f_set]][,1]
    values[["dates"]] <- colnames(DF_tot()[[input$f_set]])
  })
  
  
  # Changing the table, Save previous work Handsontable
  observe({
    if (!is.null(input$hot)) {
      #values[["previous"]] <- isolate(values[["now"]]) # current table values
      values[["now"]] = hot_to_r(input$hot)   # DF is the r format of the table value
    }
  })
  
  
  ## Add column
  output$ui_newcolname <- renderUI({
    textInput(session$ns("newcolumnname"), "", sprintf("newcol%s", 1+ncol(values[["now"]])))
  })
  
  observeEvent(input$addcolumn, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    newcolumn <- eval(parse(text=sprintf('%s(nrow(DF))', "integer" ))) #isolate(input$newcolumntype))))
    values[["now"]] <- setNames(cbind(DF, newcolumn, stringsAsFactors=FALSE), c(names(DF), isolate(input$newcolumnname)))
  })

  
  ## Add row
  output$ui_newrowname <- renderUI({
    textInput(session$ns("newrowname"), "سطر", sprintf("newrow%s", 1+nrow(values[["now"]])))
  })
  
  observeEvent(input$addrow, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    values[["names"]] <- c(values[["names"]],input$newrowname)
    A <- rbind(DF,NA)
    values[["now"]] <- A
  })
  
  
  
  ## remove column
  output$ui_removecolname <- renderUI({
    selectInput(session$ns("removecolnamelist"), "",choices = colnames(values[["now"]]))
  })
  
  observeEvent(input$removecolumn, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    values[["dates"]] <- colnames(DF)
    count <- which(values[["dates"]]==input$removecolnamelist)
    A <- DF[,-count]
    colnames(A) <- values[["dates"]][-count]
    values[["now"]] <- A
    #values[["now"]] <- subset(DF, select=-c(input$removecolnamelist))
  })
   
  
  
  ## remove row
  output$ui_removerowname <- renderUI({
    selectInput(session$ns("removerownamelist"), "سطر",choices = values[["names"]])
  })
  
  observeEvent(input$removerow, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    count <- which(values[["names"]]==input$removerownamelist)
    A <- DF[-count,]
    values[["names"]] <- values[["names"]][-count]
    values[["now"]] <- A
  })
   
  
## Output  
  output$hot <- renderRHandsontable({
    if (!is.null(values[["now"]]))
      rhandsontable(values[["now"]], useTypes = as.logical(input$useType), stretchH = "all",rowHeaders = values[["names"]],rowHeaderWidth = 100)
  })
  
  
## Save 
  observeEvent(input$save, {
    fileType <- isolate(input$fileType)
    finalDF <- cbind(values[["names"]],values[["now"]])
    if(fileType == "R"){
      dput(finalDF, file=file.path(outdir, sprintf("%s.txt", outfilename)))
    }
    else{
      write.xlsx(x = finalDF, file = file.path(outdir, sprintf("%s.xlsx", outfilename)),
                row.names = FALSE) # ,sheetName = "TestSheet")
    }}
  )
  
  ## Cancel last action    
  observeEvent(input$cancel, {
    if(!is.null(isolate(values[["previous"]]))) values[["now"]] <- isolate(values[["previous"]])
  })
  
  
  ## Message
  output$message <- renderUI({
    if(input$save==0){
      helpText(sprintf("This table will be saved in folder \"%s\" once you press the Save button.", outdir))
    }else{
      outfile <- ifelse(isolate(input$fileType)=="ASCII", "table.txt", "table.rds")
      fun <- ifelse(isolate(input$fileType)=="ASCII", "dget", "readRDS")
      list(helpText(sprintf("File saved: \"%s\".", file.path(outdir, outfile))),
           helpText(sprintf("Type %s(\"%s\") to get it.", fun, outfile)))
    }
  })
  

}



# 
# df = data.frame(subject=c("Subject A", "Subject B", "Subject C", "Subject D"),id=c(1:4))
# df
# df <- data.frame(lapply(df, as.character),
#                  stringsAsFactors = FALSE)
# df <- as.list(df)
# 
# my_new_list <- split(df$id, df$subject)
# my_new_list
# 
# my_new_list <- with(df, split(id, subject))
