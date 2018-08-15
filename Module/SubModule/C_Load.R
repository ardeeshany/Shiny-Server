
C_LoadUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel(title = "وارد کردن دیتا",
       
           fluidRow(

### Left Panel   ----             
          div(style="text-align:center;", 
          column(width = 3,
          br(),
          box(width="200%",title = "ویرایش اطلاعات",status="primary",

               wellPanel(
                radioButtons(inputId = ns("up_rmv"),label = "",choices = c("آپلود","پاک"),selected = "آپلود",inline = TRUE),
                uiOutput(outputId = ns("f_upload"))),
                br(),
               
               wellPanel(
                 h3("ذخیره کردن دیتا"), 
                 div(class='row', 
                     textInput(inputId = ns("save_name"),label = "",value = Sys.Date(),
                               placeholder = "نام دیتا را وارد کنید"),
                         actionButton(ns("save"), "ذخیره کردن دیتا در ابر"),
                     
                       uiOutput(ns("message"), inline=TRUE)
                     
                     # div(class="col-sm-6",
                     #     radioButtons(ns("fileType"), "File type", c("R", "xlsx")))
                 )
               )
               
             ))),
             

### Left Panel   ----   

### Right Panel   ----       
                 
               div(style="text-align:center;",         
               column(width = 9,
               div(style="margin-top: 3%;",
               box(status="primary",width="200%",collapsible = TRUE,collapsed = FALSE,
               fluidRow(
               column(width = 4,
               div(style="display:inline-block;width:90%;",
               uiOutput(outputId = ns("f_set")))),
               column(width = 3,
               div(style="display:inline-block;width:30%;margin-top:15%;",
               actionButton(ns("cancel"), "Cancel last action")))),
               rHandsontableOutput(ns("hot")),
               br(),
               

               box(collapsible = TRUE,title = "تغییر دیتا",collapsed = TRUE,width = "200%",status = "info",
                   fluidRow(
                     column(width = 3,
                      wellPanel(div(style="width:100%;",
                                  uiOutput(ns("ui_newcolname"))),
                              div(style="display:inline-block;",
                                  actionButton(ns("addcolumn"), "اضافه کردن ستون")))),
                            #radioButtons(ns("newcolumntype"), "Type", c("integer", "double", "character")),
                     
                     column(width = 3,       
                     wellPanel(div(style="width:100%;",
                                  uiOutput(ns("ui_removecolname"))),
                              div(style="display:inline-block;",
                                  actionButton(ns("removecolumn"),"حذف کردن ستون")))),

                     column(width = 3,
                     wellPanel(div(style="display:inline-block;width:100%;",
                                  uiOutput(ns("ui_newrowname"))),
                              div(style="display:inline-block;",
                                  actionButton(ns("addrow"),"اضافه کردن سطر")))),
                            #radioButtons(ns("newrowtype"), "Type", c("integer", "double", "character")),
                     column(width = 3,       
                     wellPanel(div(style="display:inline-block;width:100%;height:60%;",
                                  uiOutput(ns("ui_removerowname"))),
                              div(style="display:inline-block;",
                                  actionButton(ns("removerow"),"حذف کردن سطر"))))
                     )
               )
               
               
              )
               
               
                 #h3("اضافه کردن ستون"),
  )
           ))

### Right Panel   ----       

))
}







######################
#
# Server Logic
#
######################

list.files()

C_Load <- function(input,output,session,outdir=getwd(),outputDir = "RAAVI/RAAVI/Data"){
  
  
  
  
  saveData <- function(data,fileName){
    #data <- t(data)
    # Create a unique file name
    #fileName <- sprintf("%s_%s.xlsx", as.integer(Sys.time()), digest::digest(data))
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), sprintf("%s.xlsx",fileName))
    #write.xlsx(x = data, file = filePath,row.names = FALSE)
    colnames(data)[1] <- "نام"
    write.xlsx(x = data, file = filePath, row.names = FALSE)
    # Upload the file to Dropbox
    drop_upload(filePath, path = outputDir,autorename = TRUE)
  }

   observeEvent(input$f_new,{
     D_new <-read.xlsx(input$f_new$datapath)
     saveData(D_new,input$f_name)
     })
  
   observeEvent(input$remove_f,{
   ind <- which(File()$name==input$f_remove)
   drop_delete(path = File()$path[ind])
   })
   
  
  
  File <- reactive({
    input$save
    input$f_new
    input$remove_f
    input$f_new
    filesInfo <- drop_dir(outputDir)
    filenames <- unlist(strsplit(filesInfo$name,"[.]"))[c(TRUE,FALSE)] # select odd elemnts
    filePaths <- filesInfo$path_display
    return(list(name=filenames,path=filePaths))
  }) 
   
  output$f_set <- renderUI({
     selectInput(inputId = session$ns("f_set"),label = "دیتا برای آنالیز",choices = File()$name)
     })

   
   output$f_upload <- renderUI({
     if(input$up_rmv=="آپلود"){
       Date <- as.OtherDate(Sys.Date(),"persian")[1:3]
       A <- textInput(inputId = session$ns("f_name"),label = "نام دیتا",
                    value = sprintf("%s-%s-%s",Date[3],Date[2],Date[1]))
       B <- fileInput(inputId = session$ns("f_new"),label = "آپلود کردن فایل جدید")
       return(list(A,B))
     }else{
       A <- selectInput(inputId = session$ns("f_remove"),label = "نام دیتا",choices = File()$name) 
       B <- actionButton(session$ns("remove_f"), "پاک کردن")
       return(list(A,B))
     }
   })
   
  
  
  values <- reactiveValues(tot=NULL)
  names <- reactiveValues()
  
  # observeEvent(input$f_set,{
  #   values[["now"]] <- DF_tot()[[input$f_set]][,-1]
  #   values[["names"]] <- DF_tot()[[input$f_set]][,1]
  #   values[["dates"]] <- colnames(DF_tot()[[input$f_set]])
  # })
  
  
  observeEvent(input$f_set,{
    ind <- which(File()$name==input$f_set)
    Temp <- file.path(tempdir(),"Test.xlsx")
    drop_download(path = File()$path[ind],local_path = Temp,overwrite = TRUE)
    D <- read.xlsx(xlsxFile = Temp)
    #D <- drop_read_csv(File()$path[ind])
    values[["now"]] <- D[,-1]
    values[["names"]] <-D[,1]
    values[["dates"]] <- colnames(D)
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
    textInput(session$ns("newcolumnname"), "", format(Sys.Date(),format="%d-%m"))   # sprintf("newcol%s", 1+ncol(values[["now"]]))
  })
  
  observeEvent(input$addcolumn, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    newcolumn <- 0 #eval(parse(text=sprintf('%s(nrow(as.data.frame(DF)))', "integer" ))) #isolate(input$newcolumntype))))
    values[["now"]] <- setNames(cbind(DF, newcolumn), c(names(DF), isolate(input$newcolumnname)))
  })

  
  ## Add row
  output$ui_newrowname <- renderUI({
    textInput(session$ns("newrowname"), "", sprintf("newrow%s", 1+nrow(values[["now"]])))
  })
  
  observeEvent(input$addrow, {
    DF <- values[["now"]]
    values[["previous"]] <- DF
    values[["names"]] <- c(values[["names"]],input$newrowname)
    A <- rbind(DF,0)
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
    selectInput(session$ns("removerownamelist"),"",choices = values[["names"]])
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
      rhandsontable(values[["now"]], useTypes = FALSE, stretchH = "all",rowHeaders = values[["names"]],rowHeaderWidth = 100)
  })
  
  output$message <- renderUI({
      helpText(sprintf(""))
  })
  
## Save 
  observeEvent(input$save, {
    
    finalDF <- cbind(values[["names"]],values[["now"]])
    outfilename <- input$save_name 
    saveData(finalDF,sprintf("%s.xlsx", outfilename))
    
    output$message <- renderUI({
      if(input$save==0){
        helpText(sprintf("This table will be saved in folder \"%s\" once you press the Save button.", outdir))
      }else{
        div(style="text-align:right;",
        helpText(sprintf("فایل \"%s\" با موفقیت ذخیره شد", outfilename)))
      }
    })
    })
  
  
  ## Cancel last action    
  observeEvent(input$cancel, {
    if(!is.null(isolate(values[["previous"]]))) values[["now"]] <- isolate(values[["previous"]])
  })
  

  #return(list(data=cbind(values[["names"]],values[["now"]]),date=values[["date"]]))
  
}
