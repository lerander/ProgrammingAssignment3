rankhospital <- function(state, outcome, num = "best") {
        ## Reads outcome data.
        ## Checks that state and outcome are valid.
        ## Returns hospital name in that state with the given rank.
        
        hospfile <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        
        ## Map outcome to correct file column through outcome_vector.
        ## If none of the predefined outcomes: the outcome was invalid.
        if (outcome == "heart attack") outcome_vector <- hospfile[which(hospfile[, 7] == state), 11]
        else if (outcome == "heart failure") outcome_vector <- hospfile[which(hospfile[, 7] == state), 17]
        else if (outcome == "pneumonia") outcome_vector <- hospfile[which(hospfile[, 7] == state), 23]
        else stop("Invalid outcome")
        
        ## Move data of interest into a frame
        hospitals <- data.frame("name" = as.character(hospfile[which(hospfile[, 7] == state), 2]),
                                "outcome percentage" = outcome_vector,
                                stringsAsFactors = FALSE)
        
        ## All states have hospitals. If no hospitals, the state was invalid.
        if(nrow(hospitals) == 0) stop("Invalid state")
        
        ## Sort the frame by 1. outcome percentage, 2. name. Remove NAs.
        sort_vector <- order(hospitals$outcome.percentage,
                             hospitals$name,
                             na.last = NA)
        
        ## Handling of the special legal values for num
        if(num == "best") index <- 1
        else if(num == "worst") index <- length(sort_vector)
        else index <- num
        
        ## return name of the hospital with requested rank
        hospitals$name[sort_vector[index]]
}