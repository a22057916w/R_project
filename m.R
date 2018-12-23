# first assignment

# ******************** self-defined functions ***********************
swap <- function(m, r1, c1) {
  r2 <- 0
  for(i in r1:nrow(m)) {
    if(abs(m[i, c1]) > abs(m[r1, c1])) {
      r2 <- i
      break
    }
  }
  for(i in c1:ncol(m)) {
    tmp = m[r1, i]
    m[r1, i] = m[r2, i]
    m[r2, i] = tmp
  }
    return(m)
}

# ********************* main script ***************************
main <- function() {
  # initialize variables
  var <- c(row = 3, col = 3, min = 0, max = 100)
  for(i in 1:4) {
    cat("Enter ", names(var)[i], ": ", sep = "")
    con <- file("stdin")      # Establish connections to cmd
    var[[i]] <- as.numeric(readLines(con, n = 1))
    close(con)      # Close the connections
  }
  mrow <- var[[1]]
  mcol <- var[[2]]
  mmin <- var[[3]]
  mmax <- var[[4]]
  my.matrix <- matrix(runif(mrow * mcol, min = mmin, max = mmax), nrow = mrow, ncol = mcol)
  # End the initialization

  cat("The original matrix:","\n")
  print(my.matrix)      # Show original matrix
  cat("\n")

  # Find solutions
  tmp <- my.matrix
  num <- 1
  # First step: Triangle matrix
  for(i in 1:(mrow-1)) {      # Low Triangle matrix approach
    for(j in (i+1):mrow) {      # Subtracting all rows, downward
      if(tmp[i, i] == 0) {      # Check if the first coefficient is zero
        tmp = swap(tmp, i, j)     # If so, swap the rows
      }
      num <- 1/tmp[i,i] * tmp[j, i] # Manipulating the first coefficient to eliminate all coefficients for other rows
      for(k in i:mcol) {
        tmp[i, k] <- tmp[i, k] * num      # Manipulating the major_row(i) for all columns
        tmp[j, k] <- round(tmp[j, k] - tmp[i, k], digit = 5)      # Subtracting to major_row(i) for all columns
      }
    }
  }
  cat("After Triangle matrix:", "\n")
  print(tmp)
  cat("\n")

  # Step 2: Gaussian elimination, reverse Step 1
  num <- 1
  for(i in mrow:2) {
    for(j in (i-1):1) {
      num <- 1/tmp[i,i] * tmp[j, i]
      for(k in i:mcol) {
        tmp[i, k] <- tmp[i, k] * num
        tmp[j, k] <- round(tmp[j, k] - tmp[i, k], digit = 5)
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
  cat("After Gaussian elimination:", "\n")
  print(tmp)
  cat("\n")

  # Check the solutions
  if(tmp[mrow, mcol - 1] == 0 && tmp[mrow, mcol] == 0) {
    print("There are infinitely many solutions")
  } else if(tmp[mrow, mcol -1] == 0 && tmp[mrow, mcol] != 0) {
    print("There is no solutions")
  } else {
    x <- matrix(tmp[, mcol], nrow = mrow, ncol = 1)
    cat("The solution is:", "\n")
    print(x)
  }
}

if(!interactive()) {
    main()
}
