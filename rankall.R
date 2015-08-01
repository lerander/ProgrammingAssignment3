rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        ## Defining columns, and identifying illegal outcome input.
        name_column <- 2
        state_column <- 7
        if (outcome == "heart attack")          outcome_column <- 11
        else if (outcome == "heart failure")    outcome_column <- 17
        else if (outcome == "pneumonia")        outcome_column <- 23
        else stop("Invalid outcome")
        
        hospfile <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        
        ## Move data of interest into a frame
        hospitals <- data.frame("name" = hospfile[, name_column],
                                "state" = hospfile[, state_column],
                                "outcome percentage" = hospfile[, outcome_column],
                                stringsAsFactors = FALSE)
        
        completes <- hospitals[complete.cases(hospitals), ]
        
        ## when worst, we reverse ordering for outcome, but not for the others.
        if(num == "worst") {
                sort_vector <- order(completes$state,
                                     - completes$outcome.percentage,
                                     completes$name)
        } else {
                sort_vector <- order(completes$state,
                                     completes$outcome.percentage,
                                     completes$name)
        }
        
        if(num == "worst" || num == "best") num <- 1
        
        ## return name of the hospital with requested rank
        ordered_completes <- data.frame("state" = completes$state[sort_vector],
                                        "name" = completes$name[sort_vector],
                                        "outcome" = completes$outcome.percentage[sort_vector])
        
        ## Now we have an ordered frame containing complete cases of state, name and outcome
        
        ## Easy way to solve this, implemented below:
        ## Loop from 1 to length of the frame
        ## Look at the state. If it is "new", count (num - 1) from this. If state is still
        ## the same: store to result. (If not, store state and NA)
        state = "NN"
        name = "Initial"
        ## Had some data.frame-problems. therefore populate a matrix
        ## first, and present as data.frame afterwards. Should be fixed.
        ## Also: predefining the size is obviously a hack.
        result <- matrix(nrow=54, ncol=2, byrow=TRUE, dimnames = list(NULL, c("hospital", "state")))
        
        counter <- 0
        
        # Should really find a more elegant way to do this!
        for(stateindex in (1:nrow(ordered_completes))) {
                if (ordered_completes[stateindex, 1] != state) { #First row with a new state.
                        state <- ordered_completes[stateindex, 1]
                        counter <- counter + 1
                        if (ordered_completes[stateindex + num - 1, 1] == state && ## offset with num-1, and check if same state
                            stateindex + num - 1 <= nrow(ordered_completes)) { ## Don't move beyond last entry
                                result[counter, ] <- c(c(as.character(ordered_completes[stateindex + num - 1, 2]),
                                                         as.character(state)))
                        }
                        else if (state != "NN") { ## Push NA and state to result. (The dummy value should not be pushed.)
                                result[counter, ] <- c(NA, c(as.character(state)))
                        }
                }
        }
        print(as.data.frame(result))
}