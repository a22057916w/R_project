library(shiny)
library(shinydashboard)
library(RMySQL)
library(data.table)

con = dbConnect(MySQL(), user = "root", password = "", dbname = "willy", host = '140.136.150.100')
dbSendQuery(con, "SET NAMES big5")
students_5000 <- dbGetQuery(con, "select * from students limit 5000") 
students_all <- dbReadTable(con, name = "students")
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
              menuItem("Charts", tabName = "charts", icon = icon("chart-bar"))
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
    tabItem(tabName = "charts",
            plotOutput(outputId = "distPlot")
    )
  )
)


ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  observe({
    print(input$tab)
    if(input$tab == "dashboard") {
      output$students <- renderTable({
        return(students_5000)
      })
    }
  })
  
  # searchButtom
  observeEvent(input$searchButton, {
    
    if(!grepl("\\D", input$searchText)) {
      sql <- paste("SELECT * FROM students WHERE student_id='", input$searchText,"';", sep = " ")
      res <- dbGetQuery(con, sql)
      
      if(nrow(res) > 0) {
        res$standard_answer <- answer
        output$students <- renderTable({
          return(res)
        })
      }
      else {
        showNotification("Can not find any result", type ="error")
        output$students <- renderTable({
          return(students_5000)
        })
      }
    }
    else {
      res <- students_all[students_all$student_name == input$searchText,]
      
      if(nrow(res) > 0) {
        res$standard_answer <- answer
        output$students <- renderTable({
          return(res)
        })
      }
      else {
        showNotification("Can not find any result", type ="error")
        output$students <- renderTable({
          return(students_5000)
        })
      }
    }
  })
  
  output$students <- renderTable({
    return(students_5000)
  })
  
  output$distPlot <- renderPlot({
    x <- students_all$student_score
    hist(x, 
         main="Histogram of Score",         
         xlab="Score",                      
         ylab="Number of people",
         breaks = 20,
         col = "#75AADB", 
         border = "white")
  })
}

main <- function() {
  
  shinyApp(ui, server)
  #dbDisconnect(con)
}

if(interactive()) {
  main()
}
