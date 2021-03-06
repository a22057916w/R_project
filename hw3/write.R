read <- function() {
  
  answer <- scan("Data2.txt", what = "character", nlines = 1, encoding = "big5")     # read the answer on the first line
  answer <- iconv(answer, to = "UTF-8")
  answer <- substr(answer, 11, 30)      # parsing the answer
  print(answer)
  
  data <- scan("Data2.txt", what = "character", skip = 1, encoding = "big5")       # skip the first line which is the standard answer
  data <- iconv(data, to = 'UTF-8')
  
  # initiating the variables for data which read the context from Test2019.txt
  st.no <- vector(mode = "list", length = length(data))
  st.name <- vector(mode = "list", length = length(data))
  st.ans <- vector(mode = "list", length = length(data))
  
  # assigning the values from Test2019.txt to variables
  for(i in 1:length(data)) {
    st.no[i] <- substr(data[i], 1, 6)
    st.name[i] <- substr(data[i], 7, 9)
    st.ans[i] <- substr(data[i], 10, 29)
  }
  
  return(list(answer, st.no, st.name, st.ans))      # using list to store different data type
}

parse <- function(parm) {
  
  # assignning the parameter to varaibles
  ans <- as.vector(unlist(parm[1]))
  st.no <- as.vector(unlist(parm[2]))
  st.name <- as.vector(unlist(parm[3]))
  st.ans <- as.vector(unlist(parm[4]))
  
  st.score <- vector("numeric", length(st.no))    # recording the scores
  
  # calculate the score of each student
  for(i in 1:length(st.no)) {
    diff <- 0     # recording wrong answer
    space <- 0      # recording empty answer
    
    # comparing through each student's answer
    for(j in 1:nchar(st.ans[i])) {
      if(substr(st.ans[i], j, j) != substr(ans, j, j)) {
        if(substr(st.ans[i], j, j) == "*")
          space <- space + 1
        diff <- diff + 1
      }
    }
    
    st.score[i] <- 100 - (diff * 5) - (diff - space) * 1.25     # calculate the score
    if(st.score[i] < 0)
      st.score[i] <- 0
  }
  #print(st.score)
  
  seq <- sort(st.score)     # increasing ordered sequence
  
  range <- list(top = 0, front = 0, mean = 0, rear = 0, bottom = 0)     # for five standrads
  ratio <- c(88, 75, 50, 25, 12) / 100      # the ratio for five standrads
  
  # calculate the five standards
  for(i in 1:5) 
    range[[i]] <- floor(seq[ceiling(length(st.no) * ratio[i])])
  
  st.range <- vector("character", length(st.no))      # to compute the standard of each student
  
  # acquire each student's standard
  for(i in 1:length(st.range)) 
    for(j in 1:length(range)) {
      if(st.score[i] >= range[[j]]) {
        st.range[i] <- names(range)[j]
        break
      }
      st.range[i] <- names(range)[5]
      
    }
  
  # create a data frame to be stored as a csv file
  students <- data.frame(student_id = st.no, student_name = st.name, student_score = st.score, student_range = st.range, student_answer = st.ans)
  return(students)
  
}

# ******************************* main script *****************************
main <- function() {
  
  library(tmcn)
  options(max.print = 999999)     # increasing the ouptut row number
  
  parm <- read()     # read Test2019.txt, return a list of student No., name and answers
  students <- parse(parm)     # parsing the data from test2019.txt
  write.csv(students, file = "students_utf8.csv", row.names = FALSE, fileEncoding = "UTF-8")    # writing data to a csv file
  write.csv(students, file = "students_big5.csv", row.names = FALSE)  
}

if(interactive()) {
  main()
}