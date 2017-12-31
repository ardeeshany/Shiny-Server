## app.R ##
library(shiny)
library(shinydashboard)


### For Dynamic content with dropdownMenuOutput
messageData=matrix(NA,3,2)
colnames(messageData)=c("from","message")
messageData[,1]=1:3
messageData[,2]=c("test1","test2","test3")
###





ui <- dashboardPage( skin = "blue", 
 
  
#############################
#
# Header
#
#############################
dashboardHeader(title =  tags$a(href='http://github.com/ardeeshany',
                          tags$img(src='logo.png',height='60',width='80')),
               
                ### long Title
                 # "Example of a long title that needs more space",
                #  titleWidth = 450,

                
### Dynamic content ; render in server function              
               dropdownMenuOutput("messageMenu"),
                  
### Messages menus ; not render in server function, a message menu needs values for from and message.
                   dropdownMenu(type = "messages",
                               messageItem(
                                 from = "دپارتمان فروش",
                                 message = "در این ماه مشکلی موجود نمی باشد"
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2014-12-01"
                               )
                  ),

### Notification menus ; not render in server function, a text notification
dropdownMenu(type = "notifications",
             notificationItem(
               text = "5 new users today",
               icon("users")
             ),
             notificationItem(
               text = "12 items delivered",
               icon("truck"),
               status = "success"
             ),
             notificationItem(
               text = "Server load at 86%",
               icon = icon("exclamation-triangle"),
               status = "warning"
             )
),

### Tasks menus ; not render in server function, a progress bar and a text label. 
dropdownMenu(type = "tasks", badgeStatus = "success",
             taskItem(value = 90, color = "green",
                      "Documentation"
             ),
             taskItem(value = 17, color = "blue",
                      "Project X"
             ),
             taskItem(value = 75, color = "yellow",
                      "Server deployment"
             ),
             taskItem(value = 80, color = "red",
                      "Overall project"
             )
)
                  ),
 






#############################
#
# Sidebar
#
#############################
dashboardSidebar( 
    width = 230,
    helpText(h1(strong("RAAVI"))),
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
      
      menuItem("خلاصه", tabName = "Summary", icon = icon("bar-chart")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "red"),
      menuItem("Charts", icon = icon("bar-chart-o"), startExpanded = FALSE,
               menuSubItem("Sub-item 1", tabName = "subitem1"),
               menuSubItem("Sub-item 2", tabName = "subitem2")
      ),
      menuItem("Source code", icon = icon("file-code-o"),       # link to external content
               href = "https://github.com/ardeeshany/"),
      
### Dynamic item  , needs a render function in server function
      menuItemOutput("menuitem"),
      tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),

      # The dynamically-generated user panel
      uiOutput("userpanel")
      
    )
  ),







#############################
#
# Body
#
#############################
  dashboardBody(
#    bookmarkButton(),
## font of header  
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
##


    tabItems(
      # First tab content
     tabItem( tabName = "subitem1",
              box(
                title = "Box title", width = 6, status = "primary",
                "Box content"
              )
     ),
     tabItem( tabName = "subitem2",
              box(
                title = "Box title", width = 8, status = "info",
                "Box content"
              )
     ),
       tabItem(tabName = "Summary",
           # layout based on row priority; "width"
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
              )
              ),
      
      tabItem(tabName = "dashboard",
              # fluidRow(
              #   box(title = "Histogram", status="info", # boxes can gave title and color
              #       solidHeader = TRUE, collapsible = TRUE, background = "blue",
              #       plotOutput("plot1", height = 250),
              #       tableOutput("table1")), # box() : the main building blocks of dashboard pages
              # 
              #   # box(title = "Inputs", status = "danger",
              #   #   "Box content here", br(), "More box content",
              #   #   sliderInput("slider", "Number of observations:", 1, 100, 50),
              #   #   textInput("txt",label = "Text",value = 2)
              #   # )
              #   
              #   tabBox(
              #     tabPanel("Tab1",  sliderInput("slider", "Number of observations:", 1, 100, 50)),
              #     tabPanel("Tab2",  textInput("txt",label = "Text",value = 2))
              #   )
              #   
              #   
              # ),
            
              #' tags$head(
              #'   tags$style(HTML("
              #'                   @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
              #'                   
              #'                   h1 {
              #'                   font-family: 'Lobster', cursive;
              #'                   font-weight: 500;
              #'                   line-height: 1.1;
              #'                   color: #48ca3b;
              #'                   }
              #'                   
              #'                   "))
              #'   ),
              
         #     headerPanel("New Application"),
              
         
## InfoBox and ValueBox         
         infoBoxOutput("progressBox",width = 3),
         valueBoxOutput("approvalBox",width = 3),
##         
         
           
            tabBox( title = "Inputs", width= 10,
            
              tabPanel("Histogram", 
                      plotOutput("plot1",height=300),
                      sliderInput("slider", "Number of observations:", 1, 100, 50)
                      
              ),
              tabPanel("Rnorm",
                      tableOutput("table1"),
                      textInput("txt",label = "Text",value = 2)
                )
              )
              
              
      ),
              
              
              
              
              
              
              
      #         fluidRow(
      #           tabBox(
      #             tabPanel("Histogram", plotOutput("plot1", height = 250)),
      #             tabPanel("Rnorm",  tableOutput("table1"))
      #             )
      #             # title = "Histogram", status="info", # boxes can gave title and color
      #             #   solidHeader = TRUE, collapsible = TRUE, background = "blue",
      #         )
      # 
      #         
      # ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
)
)

server <- function(input, output,session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$table1 <- renderTable({rnorm(input$txt)}, bordered = TRUE)
  
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })

    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  })
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("calendar"))
    )
  })
  
  
# Dynamic item in sidebar  
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  })
  
  
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

output$userpanel <- renderUI({
  # session$user is non-NULL only in authenticated sessions
  if (!is.null(session$user)) {
    sidebarUserPanel(
      span("Logged in as ", session$user),
      subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
  }
})
  
}

shinyApp(ui, server 
         #enableBookmarking = "url"
         )
