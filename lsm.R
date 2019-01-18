
lsm <- function(x, y) {
  power <- 12
  m <- mpfr(matrix(0, nrow = power, ncol = power + 1), 80)
  mrow <- nrow(m)
  mcol <- ncol(m)
  # initialize A for Ax = B
  for(i in 1:mrow) {
    for(j in 1:(mcol-1)) {
      coefficient <- mpfr(0, 80)      #  to calculate the value of m[i, j]
      for(k in 1:length(x))     # iterates all x
        coefficient <- coefficient + x[k] ^ ((j - 1) + (i - 1))       # sum all Xs for different power ((j - 1) + (i - 1))
      m[i, j] <- coefficient      # assign coefficient result to m[i, j]
    }
  }
  print(m)

  # initialize B for Ax = B
  for(i in 1:mrow) {
    coefficient <- 0      #  to calculate the value of last column
    for(j in 1:length(x))     # iterates all x and y
      coefficient <- coefficient + y[j] * (x[j] ^ (i - 1))       # sum all Xs and Ys for different power x ^(i - 1)
    m[i, mcol] <- coefficient     # assign coefficient result to m[i, mcol]
  }

  source("matrix_solution.R")     # include file matrix_solution.R
  sol <- c(matrix.solution(m))      #function(matriix.solution(x)) in matrix_solution.R

  require(polynom)
  p <- as.polynomial(sol)
  return(p)
}

main <- function() {
  require("Rmpfr")
  x <- mpfr(runif(100, 0, 2*pi), 80)
  y <- sin(x)
  fnc <- lsm(x, y)
  plot(fnc)
}

if(!interactive()) {
    main()
}
