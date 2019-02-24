read <- function() {
  
  data <- scan("Test2019.txt", what = "character", skip = 1, encoding = "big5")
  data <- iconv(data, to = 'UTF-8')
  
  # initiating the variables for data which read the context from Test2019.txt
  st.no <- vector(mode = "list", length = length(data))
  st.name <- vector(mode = "list", length = length(data))
  st.ans <- vector(mode = "list", length = length(data))
  
  # assigning the values from Test2019.txt to variables
  for(i in 1:length(data)) {
    st.no[i] <- substr(data[i], 1, 8)
    st.name[i] <- substr(data[i], 9, 11)
    st.ans[i] <- substr(data[i], 12, 31)
  }

  return(list(st.no, st.name, st.ans))      # using list to store different data type
}


# ******************************* main script *****************************
main <- function() {

  library(tmcn)
  options(max.print = 999999)     # increasing the ouptut row number

  ref <- read()     # read Test2019.txt, return a list of student No., name and answers
  st.no <- as.vector(unlist(ref[1]))
  st.name <- ref[2]
  st.ans <- ref[3]
  print(class(st.no[1]))
}

if(interactive()) {
  main()
}