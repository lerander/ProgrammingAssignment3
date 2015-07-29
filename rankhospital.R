rankhospital <- function(state, outcome, num = "best") {
        ## Reads outcome data.
        ## Checks that state and outcome are valid.
        ## Returns hospital name in that state with the given rank.
        
        hospfile <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        
        ## Defining columns, and identifying illegal outcome input.
        name_column <- 2
        state_column <- 7
        if (outcome == "heart attack")          outcome_column <- 11
        else if (outcome == "heart failure")    outcome_column <- 17
        else if (outcome == "pneumonia")        outcome_column <- 23
        else stop("Invalid outcome")
        
        ## Move data of interest into a frame
        hospitals <- data.frame("name" = as.character(hospfile[which(hospfile[, state_column] == state), name_column]),
                                "outcome percentage" = hospfile[which(hospfile[, state_column] == state), outcome_column],
                                stringsAsFactors = FALSE)
        
        ## All states have hospitals. If no hospitals, the state was invalid.
        if(nrow(hospitals) == 0) stop("Invalid state")
        
        ## Handling of the special legal values for num
        if(num == "worst") descend <- TRUE else descend <- FALSE
        if(num == "worst" || num == "best") num <- 1
        
        ## Sort the frame by 1. outcome percentage, 2. name. Remove NAs.
        sort_vector <- order(hospitals$outcome.percentage,
                             hospitals$name,
                             na.last = NA,
                             decreasing = descend)
        
        ## return name of the hospital with requested rank
        hospitals$name[sort_vector[num]]
}