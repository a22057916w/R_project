my.matrix <- matrix(c(0,2,3,1,2,3,4,5,6), nrow = 3, ncol = 3)
print(my.matrix)

swap <- function(m, r1, r2) {
    for(i in 1:3) {
      tmp = m[r1, i]
      m[r1, i] = m[r2, i]
      m[r2, i] = tmp
    }
    return(m)
}

my.matrix <- swap(my.matrix, 2, 3)
print(my.matrix)
print(nrow(my.matrix))
