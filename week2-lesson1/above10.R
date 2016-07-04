# returns all the elements in a vector that are greater than 10
above10 <- function(x) { # where x is a vector
  use <- x > 10
  x[use]
}