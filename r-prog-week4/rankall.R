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
            sapply(splitOutcomeDf, worstHeartAttackHospitalInState)
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
            sapply(splitOutcomeDf, bestHeartFailureHospitalInState)
        }
        else if (num == "worst") {
            ## Get the worst hospital in each state for 30-day heart failure death rates.
            sapply(splitOutcomeDf, worstHeartFailureHospitalInState)
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

## This function returns the best hospital for heart attack in the passed list
bestHeartAttackHospitalInState <- function(stateMatchDf) {
    ## Obtain the indices for the hospitals having the lowest (best) outcome in the state
    numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 
                            min(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## If more than one hospital matches the criteria, then return the first one in alphabetical sort order
    matchingHospitals <- sort(numRowDf$Hospital.Name)
    ## Get the state abbreviation
    matchingStates <- stateMatchDf$State
    matchingStates <- matchingStates[!is.na(matchingStates)]
    ## return a vector containing the selected hospital and the state abbreviation
    return(c(hospital = matchingHospitals[1], state = matchingStates[1]))
}

## This function returns the worst hospital for heart attack in the passed list
worstHeartAttackHospitalInState <- function(stateMatchDf) {
    ## Obtain the indices for the hospitals having the highest (worst) outcome in the state
    numIndices <- which(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == 
                            max(as.numeric(stateMatchDf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))
    ## Get the rows corresponding to the index(es) for the hospital(s)
    numRowDf <- stateMatchDf[numIndices,]
    ## If more than one hospital matches the criteria, then return the first one in alphabetical sort order
    matchingHospitals <- sort(numRowDf$Hospital.Name)
    ## Get the state abbreviation
    matchingStates <- stateMatchDf$State
    matchingStates <- matchingStates[!is.na(matchingStates)]
    ## return a vector containing the selected hospital and the state abbreviation
    return(c(hospital = matchingHospitals[1], state = matchingStates[1]))
}

## This function returns the hospital with a specific rank for heart attack in the passed list
rankedHeartAttackHospitalInState <- function(stateMatchDf, rank) {
    ## Sort the state-specific data frame on the outcome column (primary) and hospital name column (secondary)
    stateMatchDf <- stateMatchDf[with(stateMatchDf, order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), 
                                                          Hospital.Name, na.last = TRUE)), ]
    ## Get the rows corresponding to the index for the hospital(s)
    numRowDf <- stateMatchDf[rank,]
    ## If more than one hospital matches the criteria, then return the first one in alphabetical sort order
    matchingHospitals <- sort(numRowDf$Hospital.Name)
    ## Get the state abbreviation
    matchingStates <- stateMatchDf$State
    matchingStates <- matchingStates[!is.na(matchingStates)]
    ## return a vector containing the selected hospital and the state abbreviation
    return(c(hospital = matchingHospitals[1], state = matchingStates[1]))
}
