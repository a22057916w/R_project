main <- function() {

  library(tmcn)
  library(RMySQL)
  library(data.table)
  options(max.print = 999999)     # increasing the ouptut row number

  students <- fread("students_utf8.csv")
  #str(students)
  con = dbConnect(MySQL(), user = "root", password = "", dbname = "willy", host = '140.136.150.100')

  dbWriteTable(con, value = students, name = "students", row.names = FALSE, overwrite = TRUE)
  
}

if(interactive()) {
  main()
}
