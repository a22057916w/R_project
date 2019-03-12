library(shinydashboard)
library(RMySQL)
library(data.table)

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

  output$students <- renderTable({
    return(student)
  })
}

main <- function() {
  #shinyApp(ui, server)
  con = dbConnect(MySQL(), user = "root", password = "", dbname = "willy", host = '140.136.150.100')
  students <- dbReadTable(con, name = "students")
}

if(interactive()) {
  main()
}
