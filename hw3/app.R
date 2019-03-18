library(shiny)
library(shinydashboard)
library(RMySQL)
library(data.table)
con = dbConnect(MySQL(), user = "root", password = "", dbname = "willy", host = '140.136.150.100')
student <- fread("students_big5.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                tableOutput("students")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$searchButton, {
    print(input$searchText)
    sql <- paste("SELECT * FROM students WHERE student_id='", input$searchText,"';", sep = " ")
    res <- dbGetQuery(con, sql)
    print(res)
    output$students <- renderTable({
      return(student)
    })
  })
  output$students <- renderTable({
    return(student)
  })
}

main <- function() {
  
  
  students <- dbReadTable(con, name = "students")
  
  shinyApp(ui, server)
  #dbDisconnect(con)
}

if(interactive()) {
  main()
}
