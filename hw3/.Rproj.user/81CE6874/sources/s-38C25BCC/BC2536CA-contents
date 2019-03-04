main <- function() {
  
  library(tmcn)
  library(RMySQL)
  options(max.print = 999999)     # increasing the ouptut row number
  
  #students <- read.csv("students.csv")
  #print(students)
  con = dbConnect(MySQL(), user = "root", password = "", dbname = "willy", host = '140.136.150.100')
  print(con)
}

if(interactive()) {
  main()
}