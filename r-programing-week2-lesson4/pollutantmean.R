# The main pollutantmean function
pollutantmean <- function(directory, pollutant, id =1:332) {

    # Create empty data frame for first time call to rbind()
    alldataframe <- data.frame()
    
    # Loop through the 'id' vector to read & append all files into one data frame
    for (fileid in id) {
        filename <- getfilename(fileid)
        newdataframe <- read.csv(filename)
        alldataframe <- rbind(alldataframe, newdataframe)
    }

    # Determine which column to calculate the mean of based in the pollutant passed to the function
    if (identical(pollutant, "sulfate")) {
        meanval <- mean(alldataframe$sulfate, na.rm = TRUE)
    }
    else if (identical(pollutant, "nitrate")) {
        meanval <- mean(alldataframe$nitrate, na.rm = TRUE)
    }

    # Return the calculated mean value
    meanval
}

# Function to generate the filename corresponding to a specific id
getfilename <- function(fileid) {
    absfilepath <- "C:/github-repos/datasciencecoursera/r-programing-week2-lesson4/specdata/"
    filename <- paste(c(absfilepath, fileprefix(fileid), fileid, ".csv"), sep="", collapse = "")
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