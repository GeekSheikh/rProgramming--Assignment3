#Data From https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip

#Load Libraries
library(dplyr)
library(plyr)

rankall <- function(outcome, num = "best"){
  #read data
  dfoutcome <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  #Error Handling
  outcome <- tolower(outcome)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  states <- dfoutcome$State
  validnums <- c("best","worst")
  
  if(outcome %in% outcomes == FALSE) stop("invalid outcome")
  if(outcome == "heart attack"){
    outcome <- "heartattack" 
    index <- 4L
  }else if(outcome == "heart failure"){
    outcome <- "heartfailure" 
    index <- 5L
  }else if(outcome == "pneumonia"){
    index <- 6L
  }

  #simplify data
  dfoutcome <- dfoutcome[,c(1,2,7,11,17,23)]
  dfoutcome <- dplyr::rename(dfoutcome, Provider_ID = Provider.Number,
                             Hospital_Name = Hospital.Name,
                             "heartattack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                             "heartfailure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                             "pneumonia" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  dfoutcome <- transform(dfoutcome, heartattack = as.numeric(dfoutcome[,4]))
  dfoutcome <- transform(dfoutcome, heartfailure = as.numeric(dfoutcome[,5]))
  dfoutcome <- transform(dfoutcome, pneumonia = as.numeric(dfoutcome[,6]))
  
  #Find Best Hospital Matching input Criteria
  dfoutcome <- dfoutcome[order(dfoutcome$State, dfoutcome$Hospital_Name),]
  dfoutcome$StateRank <- unlist(tapply(
    dfoutcome[,index], dfoutcome[,3], function(x) 
      rank(x, ties.method = "first", na.last=T)))
  if (!is.numeric(num)) {
    if(num %in% validnums == FALSE) stop(NA)    
    if(num == "worst") {
      sorted <- dfoutcome[order(dfoutcome[,3], desc(dfoutcome[,index])),]
      hospital <- sorted[!duplicated(sorted$State), c("Hospital_Name","State")]
    }
    if (num == "best") num <- 1
  }else{
    if(num > max(dfoutcome$StateRank)) stop(NA)
    hospital <- data.frame(dfoutcome[dfoutcome$StateRank == num, c("Hospital_Name","State")])
  }

  hospital <- dplyr::rename(hospital, hospital = Hospital_Name, state = State)
  rownames(hospital) <- NULL
  hospital
}

