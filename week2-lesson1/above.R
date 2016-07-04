# returns all the elements of a vector x that are greater than a specified value n
above <- function(x, n = 10) { # n has a default value of 10
  use <- x > n
  x[use]
}
