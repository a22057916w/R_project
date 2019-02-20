data <- scan("Test2019.txt", what = "character", skip = 1, nline = 1, encoding="UTF-8")

print(data)

x1 <- "幹"
writeLines(x1, "abc.txt")
