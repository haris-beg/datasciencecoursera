## This function returns the hospital that has the lowest 30-day mortality
## in a specified state for a specified outcome.
## It checks for valid state and outcome values passed as arguments to this function.
## State must be a standard 2-character abbreviation in the US.
## Outcome must be one of "heart attack", "heart failure", or "pneumonia".

# Suppress warnings globally
oldw <- getOption("warn")
options(warn = -1) 

best <- function(state, outcome) {
    ## Read outcome data
    outcomeDf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state is valid
    stateMatchDf <- outcomeDf[outcomeDf$State == state,]
    if (nrow(stateMatchDf) == 0) {
        stop("invalid state")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    if (outcome == "heart attack") {
        minIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 
                                min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))
    }
    else if (outcome == "heart failure") {
        minIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == 
                                min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na.rm = TRUE))
    }
    else if (outcome == "pneumonia") {
        minIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == 
                                min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm = TRUE))
    }
    else {
        ## Check that outcome is valid
        stop("invalid outcome")
    }
    
    minRow <- stateMatchDf[minIndices,]
    bestHospitals <- sort(minRow$Hospital.Name)
    return(bestHospitals[1])
}
# Unsuppress warnings
options(warn = oldw) 