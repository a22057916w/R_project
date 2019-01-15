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
  if(max < 1e-5)
    return(TRUE)
  else
    return(FALSE)
}

matrix.solution <- function(m) {

  # check if m exists
  if(!exists("m")) {
    cat("Error: Can't find a parameter.")
  }

  # Find solutions
  mrow <- nrow(m)
  mcol <- ncol(m)
  tmp <- m
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
          tmp[j, k] <- round(tmp[j, k] - tmp[i, k], digit = 18)      # Subtracting to major_row(i) for all columns
        }
      }
    }
  }

  # Step 2: Gaussian elimination, reverse Step 1
  num <- 1
  for(i in mrow:2) {
    for(j in (i-1):1) {
      num <- 1/tmp[i,i] * tmp[j, i]
      for(k in i:mcol) {
        tmp[i, k] <- tmp[i, k] * num
        tmp[j, k] <- round(tmp[j, k] - tmp[i, k], digit = 18)
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
    #if(check(x, m))
      return(x)
    #else
      #cat("Insufficient accuracy(above 10^-6)")
  }
  return(NULL)
}
