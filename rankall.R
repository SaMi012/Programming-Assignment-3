rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        out <- out[c(2,7,11,17,23)]
        names(out) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
        
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia") ){
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        data <- out[out[outcome] != 'Not Available', ]
        
        data[outcome] <- as.data.frame(sapply(data[outcome], as.numeric))
        data <- data[order(data$name, decreasing = FALSE), ]
        data <- data[order(data[outcome], decreasing = FALSE), ]
        
        getHospByRank <- function(df, s, n) {
                df <- df[df$state==s, ]
                vals <- df[, outcome]
                if( n == "best" ) {
                        rowNum <- which.min(vals)
                } else if( n == "worst" ) {
                        rowNum <- which.max(vals)
                } else {
                        rowNum <- n
                }
                df[rowNum, ]$name
        }
        
        states <- data[, 2]
        states <- unique(states)
        newdata <- data.frame("hospital"=character(), "state"=character())
        for(st in states) {
                hosp <- getHospByRank(data, st, num)
                newdata <- rbind(newdata, data.frame(hospital=hosp, state=st))
        }
        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        newdata <- newdata[order(newdata['state'], decreasing = FALSE), ]
        newdata
        
}