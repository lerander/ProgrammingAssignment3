rankhospital <- function(state, outcome, num = "best") {
        ## Reads outcome data.
        ## Checks that state and outcome are valid.
        ## Returns hospital name in that state with the given rank.
        ## (Uses 30-day death rate.)
        
        hospfile <- read.csv("outcome-of-care-measures.csv",
                             na.strings = "Not Available")
        
        ## Move data of interest into a frame
        washed_file <- data.frame("name" = hospfile[, 2],
                                  "state" = hospfile[, 7],
                                  "heart attack" = hospfile[, 11],
                                  "heart failure" = hospfile[, 17],
                                  "pneumonia" = hospfile[, 23],
                                  stringsAsFactors = FALSE)
        
        ## Let's move the rows that are in the relevant state to a smaller subset.
        all_in_state <- washed_file[which(washed_file$state == state), ]
        
        ## All states have hospitals. If no hospitals, the state was invalid.
        if(nrow(all_in_state) == 0) stop("Invalid state")
        
        ## Place hospital name and requested outcome in a separate frame.
        ## If none of the predefined outcomes: the outcome was invalid.
        if (outcome == "heart attack") {
                hospitals <- data.frame("name" = as.character(all_in_state$name), 
                                        "death percentage" = all_in_state$heart.attack,
                                        stringsAsFactors = FALSE)
        } else if (outcome == "heart failure") {
                hospitals <- data.frame("name" = as.character(all_in_state$name), 
                                        "death percentage" = all_in_state$heart.failure,
                                        stringsAsFactors = FALSE)
        } else if (outcome == "pneumonia") {
                hospitals <- data.frame("name" = as.character(all_in_state$name), 
                                        "death percentage" = all_in_state$pneumonia,
                                        stringsAsFactors = FALSE)
        } else {
                stop("Invalid outcome")
        }
        
        ## Sort the frame by 1. death percentage, 2. name. Remove NAs.
        sort_vector <- order(hospitals$death.percentage,
                             hospitals$name,
                             na.last = NA)
        
        ## Handling of the special legal values for num
        if(num == "worst") {
                index <- length(sort_vector)
        } else if(num == "best") {
                index <- 1
        } else index <- num
        
        hospitals$name[sort_vector[index]]
}