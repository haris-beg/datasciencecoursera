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
            results <- lapply(splitOutcomeDf, bestHeartAttackHospitalInState)
            as.data.frame(do.call(rbind, results))
        }
        else if (num == "worst") {
            ## Get the worst hospital in each state for 30-day heart attack death rates.
            results <- lapply(splitOutcomeDf, worstHeartAttackHospitalInState)
            as.data.frame(do.call(rbind, results))
        }
        else {
            ## Get the hospital in each state with rank equal to 'num' for 30-day heart attack death rates.
            results <- lapply(splitOutcomeDf, rankedHeartAttackHospitalInState, num)
            as.data.frame(do.call(rbind, results))
        }
    }
    else if (outcome == "heart failure") {
        if (num == "best") {
            ## Get the best hospital in each state for 30-day heart failure death rates.
            results <- lapply(splitOutcomeDf, bestHeartFailureHospitalInState)
            as.data.frame(do.call(rbind, results))
        }
        else if (num == "worst") {
            ## Get the worst hospital in each state for 30-day heart failure death rates.
            results <- lapply(splitOutcomeDf, worstHeartFailureHospitalInState)
            as.data.frame(do.call(rbind, results))
        }
        else {
            ## Get the hospital in each state with rank equal to 'num' for 30-day heart failure death rates.
            results <- lapply(splitOutcomeDf, rankedHeartFailureHospitalInState, num)
            as.data.frame(do.call(rbind, results))
        }
    }
    else if (outcome == "pneumonia") {
        if (num == "best") {
            ## Get the best hospital in each state for 30-day pneumonia death rates.
            results <- lapply(splitOutcomeDf, bestPneumoniaHospitalInState)
            as.data.frame(do.call(rbind, results))
        }
        else if (num == "worst") {
            ## Get the worst hospital in each state for 30-day pneumonia death rates.
            results <- lapply(splitOutcomeDf, worstPneumoniaHospitalInState)
            as.data.frame(do.call(rbind, results))
        }
        else {
            ## Get the hospital in each state with rank equal to 'num' for 30-day pneumonia death rates.
            results <- lapply(splitOutcomeDf, rankedPneumoniaHospitalInState, num)
            as.data.frame(do.call(rbind, results))
        }
    }
    else {
        ## Check that outcome is valid
        stop("invalid outcome")
    }
    
}

## This function returns the best hospital for heart attack in the passed list
bestHeartAttackHospitalInState <- function(stateMatchDf) {
    ## Obtain the indices for the hospitals having the lowest (best) outcome in the state
    numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 
                            min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## return a vector containing the matching hospital and the state abbreviation
    return(getHospitalAndState(stateMatchDf, numRowDf))
}

## This function returns the worst hospital for heart attack in the passed list
worstHeartAttackHospitalInState <- function(stateMatchDf) {
    ## Obtain the indices for the hospitals having the highest (worst) outcome in the state
    numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 
                            max(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## return a vector containing the matching hospital and the state abbreviation
    return(getHospitalAndState(stateMatchDf, numRowDf))
}

## This function returns the hospital with a specific rank for heart attack in the passed list
rankedHeartAttackHospitalInState <- function(stateMatchDf, rank) {
    ## Sort the state-specific data frame on the outcome column (primary) and hospital name column (secondary)
    stateMatchDf <- stateMatchDf[with(stateMatchDf, order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
                                                          Hospital.Name, na.last = TRUE)), ]
    ## Get the rows corresponding to the rank for the hospital(s)
    numRowDf <- stateMatchDf[rank,]
    ## return a vector containing the matching hospital and the state abbreviation
    return(getHospitalAndState(stateMatchDf, numRowDf))
}

## This function returns the best hospital for heart failure in the passed list
bestHeartFailureHospitalInState <- function(stateMatchDf) {
    ## Obtain the indices for the hospitals having the lowest (best) outcome in the state
    numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == 
                            min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na.rm = TRUE))
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## return a vector containing the matching hospital and the state abbreviation
    return(getHospitalAndState(stateMatchDf, numRowDf))
}

## This function returns the worst hospital for heart failure in the passed list
worstHeartFailureHospitalInState <- function(stateMatchDf) {
    ## Obtain the indices for the hospitals having the highest (worst) outcome in the state
    numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == 
                            max(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na.rm = TRUE))
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## return a vector containing the matching hospital and the state abbreviation
    return(getHospitalAndState(stateMatchDf, numRowDf))
}

## This function returns the hospital with a specific rank for heart failure in the passed list
rankedHeartFailureHospitalInState <- function(stateMatchDf, rank) {
    ## Sort the state-specific data frame on the outcome column (primary) and hospital name column (secondary)
    stateMatchDf <- stateMatchDf[with(stateMatchDf, order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), 
                                                          Hospital.Name, na.last = TRUE)), ]
    ## Get the rows corresponding to the rank for the hospital(s)
    numRowDf <- stateMatchDf[rank,]
    ## return a vector containing the matching hospital and the state abbreviation
    return(getHospitalAndState(stateMatchDf, numRowDf))
}

## This function returns the best hospital for pneumonia in the passed list
bestPneumoniaHospitalInState <- function(stateMatchDf) {
    ## Obtain the indices for the hospitals having the lowest (best) outcome in the state
    numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == 
                            min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm = TRUE))
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## return a vector containing the matching hospital and the state abbreviation
    return(getHospitalAndState(stateMatchDf, numRowDf))
}

## This function returns the worst hospital for pneumonia in the passed list
worstPneumoniaHospitalInState <- function(stateMatchDf) {
    ## Obtain the indices for the hospitals having the highest (worst) outcome in the state
    numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == 
                            max(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm = TRUE))
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## return a vector containing the matching hospital and the state abbreviation
    return(getHospitalAndState(stateMatchDf, numRowDf))
}

## This function returns the hospital with a specific rank for pneumonia in the passed list
rankedPneumoniaHospitalInState <- function(stateMatchDf, rank) {
    ## Sort the state-specific data frame on the outcome column (primary) and hospital name column (secondary)
    stateMatchDf <- stateMatchDf[with(stateMatchDf, order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), 
                                                          Hospital.Name, na.last = TRUE)), ]
    ## Get the rows corresponding to the rank for the hospital(s)
    numRowDf <- stateMatchDf[rank,]
    ## return a vector containing the matching hospital and the state abbreviation
    return(getHospitalAndState(stateMatchDf, numRowDf))
}

## This function returns a vector containing the matching hospital and the state abbreviation
getHospitalAndState <- function(stateMatchDf, numRowDf) {
    ## If more than one hospital matches the criteria, then return the first one in alphabetical sort order
    matchingHospitals <- sort(numRowDf$Hospital.Name)
    ## Get the state abbreviation
    matchingStates <- stateMatchDf$State
    matchingStates <- matchingStates[!is.na(matchingStates)]
    ## return a vector containing the selected hospital and the state abbreviation
    return(c(hospital = matchingHospitals[1], state = matchingStates[1]))
}
