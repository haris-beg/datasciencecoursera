# The main "complete" function
complete <- function(directory, id = 1:332) {
    
    # Create empty data frame for first time call to rbind()
    alldataframe <- data.frame()
    
    # Loop through the 'id' vector to read & append all files into one data frame
    for (fileid in id) {
        filename <- getfilename(fileid, directory)
        csvdataframe <- na.omit(read.csv(filename))
        numofvalidrows <- nrow(csvdataframe)
        alldataframe <- rbind(alldataframe, c(fileid, numofvalidrows))
    }
    
    # Specify column names for the dataframe
    cnames <- c("id", "nobs")
    colnames(alldataframe) <- cnames
    
    # Return the data frame
    alldataframe
    
}

# Function to generate the filename corresponding to a specific id
getfilename <- function(fileid, directory) {
    absfilepath <- "C:/github-repos/datasciencecoursera/r-programing-week2-lesson4/"
    filename <- paste(c(absfilepath, directory, "/", fileprefix(fileid), fileid, ".csv"), sep="", collapse = "")
    filename
}

# The function to generate the exact zeros-prefix for the CSV file name
fileprefix <- function(fileid) {
    if (fileid > 0 & fileid < 10) {
        prefix <- "00"
    }
    else if(fileid >= 10 & fileid < 100) {
        prefix <- "0"
    }
    else {
        prefix <- ""
    }
}