best <- function(state, outcome) {
    ## Read outcome data
    outcomeDf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state is valid
    stateCol <- outcomeDf$State
    stateMatch <- stateCol[stateCol == state]
    if (length(stateMatch) == 0) {
        stop("invalid state")
    }
    
    ## Check that outcome is valid
    if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia") {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
}