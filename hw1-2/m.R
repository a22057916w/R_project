# first assignment

# ******************** self-defined functions ***********************
swap <- function(m, r1, c1) {
  r2 <- r1
  for(i in r1:nrow(m)) {
    if(abs(m[i, c1]) > abs(m[r1, c1])) {
      r2 <- i
      break
    }
  }
  for(i in c1:ncol(m)) {
    tmp <- m[r1, i]
    m[r1, i] <- m[r2, i]
    m[r2, i] <- tmp
  }
  return(m)
}

check <- function(sol, mat) {
  max <- 0
  for(i in 1:nrow(mat)) {
    col <- 0
    for(j in 1:(nrow(sol)))
      col <- col + mat[i, j] * sol[j, 1]
    col <- abs(col - mat[i, ncol(mat)])
    if(max < col)
      max <- col
  }
  print(max)
  if(max < 1e-6)
    return(TRUE)
  else
    return(FALSE)
}

# ********************* main script ***************************
main <- function(m) {
  require("Rmpfr")
  # set maxima print out lines
  options(max.print = 999999)

  # initialize variables
  var <- c(row = 3, col = 3, min = 0, max = 100)
  for(i in 1:4) {
    if(i == 2)
      next
    cat("Enter ", names(var)[i], ": ", sep = "")
    con <- file("stdin")      # Establish connections to cmd
    var[[i]] <- as.numeric(readLines(con, n = 1))     # read data from cmd
    close(con)      # Close the connections
  }
  mrow <- var[[1]]
  mcol <- var[[1]] + 1
  mmin <- var[[3]]
  mmax <- var[[4]]
  my.matrix <- matrix(as.double(runif(mrow * mcol, min = mmin, max = mmax)), nrow = mrow, ncol = mcol)
  # End the initialization

  #cat("The original matrix:","\n")
  #print(my.matrix)      # Show original matrix
  #cat("\n")

  # Find solutions
  tmp <- my.matrix
  num <- 1
  # First step: Triangle matrix
  for(i in 1:(mrow-1)) {      # Low Triangle matrix approach
    for(j in (i+1):mrow) {      # Subtracting all rows, downward
      if(tmp[i, i] == 0) {      # Check if the first coefficient is zero
        tmp = swap(tmp, i, i)     # If so, swap the rows, there may be a chance the whole column is zero
      }
      if(tmp[j, i] != 0) {      # To prevent from the NaN in case the first coefficient of the other row is zero
        num <- 1/tmp[i,i] * tmp[j, i] # Manipulating the first coefficient to eliminate all coefficients for other rows
        for(k in i:mcol) {
          tmp[i, k] <- tmp[i, k] * num      # Manipulating the major_row(i) for all columns
          tmp[j, k] <- tmp[j, k] - tmp[i, k]      # Subtracting to major_row(i) for all columns
        }
      }
    }
  }
  #cat("After Triangle matrix:", "\n")
  #print(tmp)
  #cat("\n")

  # Step 2: Gaussian elimination, reverse Step 1
  num <- 1
  for(i in mrow:2) {
    for(j in (i-1):1) {
      num <- 1/tmp[i,i] * tmp[j, i]
      for(k in i:mcol) {
        tmp[i, k] <- tmp[i, k] * num
        tmp[j, k] <- tmp[j, k] - tmp[i, k]
      }
    }
  }
  # Step 3: Make coefficient as 1 and get the solutions
  for(i in 1:mrow) {
    if(tmp[i, i] != 0) {      # Make sure no figures divided by zero
      tmp[i, mcol] <- tmp[i ,mcol] / tmp[i, i]
      tmp[i, i] <- tmp[i, i] / tmp[i, i]
    }
  }
  #cat("After Gaussian elimination:", "\n")
  #print(tmp)
  #cat("\n")

  # Check the solutions
  sol <- 1
  for(i in 1:mrow) {
    if(tmp[i, i] == 0 && tmp[i, mcol] == 0) {
      print("There are infinitely many solutions.")
      sol <- Inf
      break
    } else if(tmp[i, i] == 0 && tmp[i, mcol] != 0) {
      print("There is no solutions.")
      sol <- 0
      break
    }
  }
  if(sol == 1) {
    x <- matrix(tmp[, mcol], nrow = mrow, ncol = 1)
    if(check(x, my.matrix)) {
      cat("The solution is:", "\n")
      print(x)
    }
    else
      cat("Insufficient accuracy(above 10^-6)")
  }
}

if(!interactive()) {
    main()
}
