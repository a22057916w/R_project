library(shiny)
library(shinydashboard)
library(RMySQL)
library(data.table)

student <- fread("students_big5.csv")

main <- function() {
  
  con = dbConnect(MySQL(), user = "root", password = "", dbname = "willy", host = '140.136.150.100')
  students <- dbReadTable(con, name = "students")
  NO <- 20190017
  sql <- paste("SELECT * FROM students WHERE student_id='", NO,"';", sep = " ")
  res <- dbGetQuery(con, sql)
  print(res)
  #dbDisconnect(con)
}

if(interactive()) {
  main()
}
