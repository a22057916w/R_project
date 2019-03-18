library(shiny)
library(shinydashboard)
library(RMySQL)
library(data.table)

con = dbConnect(MySQL(), user = "root", password = "", dbname = "willy", host = '140.136.150.100')
dbSendQuery(con, "SET NAMES big5")
students <- dbGetQuery(con, "select * from students limit 5000") 
#students <- dbReadTable(con, name = "students")
#print(students)


answer <- scan("Data2.txt", what = "character", nlines = 1, encoding = "big5")     # read the answer on the first line
answer <- iconv(answer, to = "UTF-8")
answer <- substr(answer, 11, 30)      # parsing the answer
print(answer)

header <- dashboardHeader(title = "Student Info")

sidebar <- dashboardSidebar(
  sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search..."),
  sidebarMenu(id = "tab",
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
  
)

body <- dashboardBody(
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


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  observe({
    print(input$tab)
    if(input$tab == "dashboard") {
      output$students <- renderTable({
        return(students)
      })
    }
  })
  
  # searchButtom
  observeEvent(input$searchButton, {
    
    if(!grepl("\\D", input$searchText)) {
      sql <- paste("SELECT * FROM students WHERE student_id='", input$searchText,"';", sep = " ")
      res <- dbGetQuery(con, sql)
      res$standard_answer <- answer
      output$students <- renderTable({
        return(res)
      })
    }
    else {
      res <- students[students$student_name == input$searchText,]
      res$standard_answer <- answer
      output$students <- renderTable({
        return(res)
      })
    }
  })
  
  output$students <- renderTable({
    return(students)
  })
}

main <- function() {
  
  shinyApp(ui, server)
  #dbDisconnect(con)
}

if(interactive()) {
  main()
}
