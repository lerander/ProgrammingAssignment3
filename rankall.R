rankall <- function(outcome, num = "best") {
        ## For each state, finds the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        ## Defining columns. Stopping if illegal outcome input.
        name_column <- 2
        state_column <- 7
        if (outcome == "heart attack")          outcome_column <- 11
        else if (outcome == "heart failure")    outcome_column <- 17
        else if (outcome == "pneumonia")        outcome_column <- 23
        else stop("Invalid outcome")
        
        hospfile <- read.csv("outcome-of-care-measures.csv",
                             na.strings = "Not Available",
                             stringsAsFactors = FALSE)
        
        ## Move data of interest into a frame
        hospitals <- na.omit(data.frame("name" = hospfile[, name_column],
                                        "state" = hospfile[, state_column],
                                        "outcome" = hospfile[, outcome_column],
                                        stringsAsFactors = FALSE))
        
        ## Order the data. Handle "worst"
        hospitals <- hospitals[order(hospitals[, 2],
                                     hospitals[, 3]*(1-2*(num == "worst")),
                                     hospitals[, 1]),]
        
        if(!is.numeric(num)) num <- 1 ## Best/worst is concidered as 1.
        
        hospitalcount <- nrow(hospitals) ## To avoid counting several times later on.

        
        if(num < 1 || num > hospitalcount) stop("Illegal num")
        
        state = ""
        states <- unique(hospitals$state)
        
        #Prepare result containing all the states.
        result <- data.frame(hospital = NA,
                             state = states,
                             row.names = states)
        
        # This may not be so stupid after all. (Not spesific R code, however.)
        for(i in (1:(hospitalcount - num + 1))) { ## Spin through data set.
                if (hospitals$state[i] != state) { # Whenever a new state is reached ...
                        state <- hospitals$state[i] ## We prepare for updating result for this state.
                        if (hospitals$state[i + num - 1] == state) { ## Check that the offseted row is same state ...
                                result[state, 1] <- hospitals$name[i + num - 1] # ... and update the result.
                        }
                }
        }
        
        print(result)
}