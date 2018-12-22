# first assignment

# ******************** global variables ***********************
.row <- 3
.col <- 3
.min <- 0
.max <- 100


# ******************** functions ******************************
set.matrix <- function() {
  matrix1 <- matrix(runif(.row * .col, min = .min, max = .max), nrow = .row, ncol = .col)
  return(matrix1)
}

init <- function() {

}
# ********************* main script ***************************

# initialize variables
#var <- c(row = 3, col = 3, min = 0, max = 100)
#for(i in 1:4) {
#  cat("Enter ", names(var)[i], ": ", sep = "")
#  var[[i]] <- as.numeric(readLines(file("stdin"), n = 1))
#}
#.row <- var[[1]]
#.col <- var[[2]]
#.min <- var[[3]]
#.max <- var[[4]]
my.matrix <- set.matrix()
print(my.matrix)
cat(.row, .col)
# find solutions
tmp <- my.matrix
print(tmp)
num <- 0
print(tmp[0, 2])
for(i in 1:.row-1) {
  print(i)
  for(j in i+1:.row) {
    num <- 1/tmp[i,i] * tmp[j, i]
    #cat(i, j, "   ")
    for(k in i:.col) {
      tmp[i, k] <- tmp[i, k] * num
    }
    for(k in i:.col) {
      tmp[j, k] <- tmp[j, k] - tmp[i, k]
    }
  }
}
print(tmp)
