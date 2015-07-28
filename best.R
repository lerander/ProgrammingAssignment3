best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        hospfile <- read.csv("outcome-of-care-measures.csv")
        ## Move data of interest into a frame
        ## (This can not be the most elegant way of doing this...)
        washed_file <- data.frame("name" = hospfile[, 2],
                           "state" = hospfile[, 7],
                           "heart attack" = suppressWarnings(as.numeric(as.character(hospfile[, 11]))),
                           "heart failure" = suppressWarnings(as.numeric(as.character(hospfile[, 17]))),
                           "pneumonia" = suppressWarnings(as.numeric(as.character(hospfile[, 23]))))
        ## Let's only consider the rows that are in the relevant state
        all_in_state <- washed_file[which(washed_file[, 2] == state), ]
       
        ## If no hospitals; then the state was illegal
        if(length(all_in_state[, 1]) == 0) {
                        stop("Invalid state")
        }
        
        ## Create index of the hospitals having the minimum number
        ## of deaths of category "outcome".
        ## If none of the predefined: stop with error message
        if (outcome == "heart attack") {
                minimums <- all_in_state[, 3] == min(all_in_state[, 3], na.rm = TRUE)
        } else if (outcome == "heart failure") {
                minimums <- all_in_state[, 4] == min(all_in_state[, 4], na.rm = TRUE)
        } else if (outcome == "pneumonia") {
                minimums <- all_in_state[, 5] == min(all_in_state[, 5], na.rm = TRUE)
        } else {
                stop("Invalid outcome")
        }

        ## To avoid strange message rgd. factors, we explicitly convert.        
        best_hospitals <- as.character(all_in_state[, 1][minimums])
        
        ## There may be more than one. In such cases we shall
        ## print the alphabetically first one
        best_hospitals[sort.list(best_hospitals)[1]]
}