best <- function(state, outcome) {
        ## Read outcome data
        out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        out <- out[c(2,7,11,17,23)]
        names(out) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
        ## Check that state and outcome are valid
        states <- unique(out[, 2])
        if(!state %in% states){
                stop("invalid state")
        }
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia") ){
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        fil_out <- out[out$state == state & out[outcome] != "Not Available", ]
        hos <- fil_out[, outcome]
        num_hos <- which.min(hos)
        fil_out[num_hos, ]$name
        
}