rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        out <- out[out$state == state & out[outcome] != "Not Available", ]
        out[outcome] <- as.data.frame(sapply(out[outcome], as.numeric))
        
        out <- out[order(out$name, decreasing = FALSE), ]
        
        out <- out[order(out[outcome], decreasing = FALSE),]
        hos <- out[, outcome]
        if(num=="best"){
                num <- which.min(hos)
        }
        else if(num=="worst"){
                num <- which.max(hos)
        }
        
        out[num, ]$name
}