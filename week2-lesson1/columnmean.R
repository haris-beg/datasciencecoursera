# Calculate the mean of the columns of a data frame or matrix
columnmean <- function(y, removeNA = TRUE) {
  # calculate number of columns in the data structure passed as argument to function
  nc <- ncol(y)
  
  # create an empty vector having same number of columns as the argument to this function
  means <- numeric(nc)
  
  # fill each column with the mean of the corresponding column of the argument data frame/matrix
  for (i in 1:nc) {
    means[i] <- mean(y[, i], na.rm = removeNA)
  }
  
  # return the vector
  means
  
}