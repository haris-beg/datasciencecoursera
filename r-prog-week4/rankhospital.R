## This function reads the outcome-of-care-measures.csv file and returns a 
## character vector with the name of the hospital that has the ranking 
## specified by the num argument.

# Suppress warnings globally
# oldw <- getOption("warn")
options(warn = -1) 

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomeDf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state is valid
    stateMatchDf <- outcomeDf[outcomeDf$State == state,]
    if (nrow(stateMatchDf) == 0) {
        stop("invalid state")
    }

    numIndices <- num
    if (outcome == "heart attack") {
        if (num == "best") {
            ## Obtain the indices for the hospitals having the lowest (best) outcome in the state
            numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 
                                    min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))
        }
        else if (num == "worst") {
            ## Obtain the indices for the hospitals having the highest (worst) outcome in the state
            numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 
                                    max(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))
        }
        else {
            ## Sort the state-specific data frame on the outcome column (primary) and hospital name column (secondary)
            stateMatchDf <- stateMatchDf[with(stateMatchDf, order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
                                                                  Hospital.Name, na.last = TRUE)), ]
        }
    }
    else if (outcome == "heart failure") {
        if (num == "best") {
            ## Obtain the indices for the hospitals having the lowest (best) outcome in the state
            numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == 
                                min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na.rm = TRUE))
        }
        else if (num == "worst") {
            ## Obtain the indices for the hospitals having the highest (worst) outcome in the state
            numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == 
                                    max(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na.rm = TRUE))
        }
        else {
            ## Sort the state-specific data frame on the outcome column (primary) and hospital name column (secondary)
            stateMatchDf <- stateMatchDf[with(stateMatchDf, order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), 
                                                                  Hospital.Name, na.last = TRUE)), ]
        }
    }
    else if (outcome == "pneumonia") {
        if (num == "best") {
            ## Obtain the indices for the hospitals having the lowest (best) outcome in the state
            numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == 
                                    min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm = TRUE))
        }
        else if (num == "worst") {
            ## Obtain the indices for the hospitals having the highest (worst) outcome in the state
            numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == 
                                    max(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm = TRUE))
        }
        else {
            ## Sort the state-specific data frame on the outcome column (primary) and hospital name column (secondary)
            stateMatchDf <- stateMatchDf[with(stateMatchDf, order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), 
                                                                  Hospital.Name, na.last = TRUE)), ]
        }
    }
    else {
        ## Check that outcome is valid
        stop("invalid outcome")
    }
    
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## If more than one hospital matches the criteria, then return the first one in alphabetical sort order
    numHospitals <- sort(numRowDf$Hospital.Name)
    return(numHospitals[1])
    
}

# Unsuppress warnings globally
# options(warn = oldw) 