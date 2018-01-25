#? A problem with Dynamic input ... I think it is not run in server. 
#? For running we do not use ns() for input names.


#############################
#
# Sidebar
#
#############################
SidebarInput <- function(id){
  

  ns <- NS(id)

  dashboardSidebar(
    width = 230,
    helpText(h1(strong("RAAVI"))),
    
    sidebarMenu(id = "menu1",              
              
              
              # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
              #                   label = "Search..."),
              
              
              
              menuItem("خلاصه", tabName = "Summary", icon = icon("bar-chart")
              ),
              
              
              
              menuItem("کلاس",tabname="Class", icon = icon("bar-chart-o"), startExpanded = FALSE,
                       menuSubItem("پیش", tabName = "CP"),
                       menuSubItem("سوم", tabName = "C3")
              ),
              
              menuItem("دانش آموز", tabName = "Student", icon = icon("bar-chart")
              ),
              
              
              menuItem("دبیر", tabName = "Teacher", icon = icon("bar-chart")
              ),
              
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              
              
              
              
              menuItem("Widgets", icon = icon("th"), tabName = "widgets",
                       badgeLabel = "new", badgeColor = "red"),
              
              
              
              menuItem("Source code", icon = icon("file-code-o"),       # link to external content
                       href = "https://github.com/ardeeshany/"),


              ### Dynamic item : Instead of menuItem, use menuItemOutput
              ### It needs a render function in server function
              menuItemOutput(ns("menuitem")),
              tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),


              # The dynamically-generated user panel
              uiOutput("userpanel")
        
  )   
    )
}

Sidebar <- function(input,output,session){
  
  output$menuitem <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("calendar"))
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