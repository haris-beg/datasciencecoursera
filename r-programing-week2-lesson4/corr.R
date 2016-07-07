corr <- function(directory, threshold = 0) {
    
    # Obtain the list of all files in "directory"
    absfilepath <- "C:/github-repos/datasciencecoursera/r-programing-week2-lesson4/"
    absdirpath <- paste(c(absfilepath, directory), sep="", collapse = "")
    
    # Read the full names of all csv files in "directory"
    fileslist <- list.files(absdirpath, pattern = '\\.csv', full.names = TRUE)

    # Create an empty vector to store the correlations
    correlationvector <- c()
    
    for (filename in fileslist) {
        #Import valid rows from the file into a data frame
        validdataframe <- na.omit(read.csv(filename))
        
        #Count the number of rows in the data frame
        rowcount <- nrow(validdataframe)
        
        # If number of valid rows in monitor file is greater than the threshold
        # then calculate the correlation between sulfate and nitrate columns
        # and append the correlation to the vector to be returned from function
        if (rowcount > threshold) {
            correlation <- cor(validdataframe$sulfate, validdataframe$nitrate)
            correlationvector <- c(correlationvector, correlation)
        }
    }
    
    # Return the vector of correlations
    correlationvector
}