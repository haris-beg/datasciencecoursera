## The rankall function takes two arguments: an outcome name (outcome) and a 
## hospital ranking (num). The function reads the outcome-of-care-measures.csv
## file and returns a 2-column data frame containing the hospital in each state 
## that has the ranking specified in num.

## Suppress warnings globally
options(warn = -1) 

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomeDf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # Split the outcomeDf data frame by the state
    splitOutcomeDf <- split(outcomeDf, outcomeDf$State)
    
    if (outcome == "heart attack") {
        if (num == "best") {
            ## Get the best hospital in each state for 30-day heart attack death rates.
            sapply(splitOutcomeDf, bestHeartAttackHospitalInState)
        }
        else if (num == "worst") {
            ## Get the worst hospital in each state for 30-day heart attack death rates.
            lapply(splitOutcomeDf, worstHeartAttackHospitalInState)
        }
        else {
            ## Get the hospital in each state with rank equal to 'num' for 30-day heart attack death rates.
        }
    }
    else if (outcome == "heart failure") {
        if (num == "best") {
            ## Get the best hospital in each state for 30-day heart failure death rates.
        }
        else if (num == "worst") {
            ## Get the worst hospital in each state for 30-day heart failure death rates.
        }
        else {
            ## Get the hospital in each state with rank equal to 'num' for 30-day heart failure death rates.
        }
    }
    else if (outcome == "pneumonia") {
        if (num == "best") {
            ## Get the best hospital in each state for 30-day pneumonia death rates.
        }
        else if (num == "worst") {
            ## Get the worst hospital in each state for 30-day pneumonia death rates.
        }
        else {
            ## Get the hospital in each state with rank equal to 'num' for 30-day pneumonia death rates.
        }
    }
    else {
        ## Check that outcome is valid
        stop("invalid outcome")
    }
    
}

bestHeartAttackHospitalInState <- function(stateMatchDf) {
    ## Obtain the indices for the hospitals having the lowest (best) outcome in the state
    numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 
                            min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## If more than one hospital matches the criteria, then return the first one in alphabetical sort order
    numHospitals <- sort(numRowDf$Hospital.Name)
    ## return a vector containing the selected hospital and the state abbreviation
    return(numHospitals[1])
}

worstHeartAttackHospitalInState <- function(stateMatchDf) {
    ## Obtain the indices for the hospitals having the highest (worst) outcome in the state
    numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 
                            max(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## If more than one hospital matches the criteria, then return the first one in alphabetical sort order
    numHospitals <- sort(numRowDf$Hospital.Name)
    ## return a vector containing the selected hospital and the state abbreviation
    return(c(numHospitals[1], numRowDf[1]$State))
}
