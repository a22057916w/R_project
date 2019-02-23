library(readr)
library(tmcn)
#options(stringsAsFactors = FALSE)
#Sys.setlocale(category = "LC_CTYPE", locale = "Chinese (Traditional)")
data <- scan("Test2019.txt", what = "character", skip = 1, nline = 2, encoding = "big5")
x <- '123'
x2 <- iconv('中文', to = 'UTF-8')
#Sys.getlocale()
#Sys.setlocale(locale = "Chinese")
print(data)

