outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

best <- function(state, outcome) {
        ## Read outcome data
        out <- read.csv("outcome-of-care-measures.csv")
        ## Check that state and outcome are valid
        if(!state && !outcome){
                
        }
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}