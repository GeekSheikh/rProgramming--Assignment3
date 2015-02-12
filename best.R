#Data From https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip

#Load Libraries
library(dplyr)
library(plyr)

best <- function(state, outcome){
#read data
dfoutcome <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")

#Error Handling
outcome <- tolower(outcome)
outcomes <- c("heart attack", "heart failure", "pneumonia")
states <- dfoutcome$State

if (outcome %in% outcomes == FALSE) stop("invalid outcome")
if (state %in% states == FALSE) stop("invalid state")

#simplify data
dfoutcome <- dfoutcome[,c(1,2,7,11,17,23)]
dfoutcome <- dplyr::rename(dfoutcome, Provider_ID = Provider.Number,
                         Hospital_Name = Hospital.Name,
                         "heart attack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                         "heart failure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                         "pneumonia" = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)

#Find Best Hospital Matching input Criteria
dfoutcome <- dfoutcome[dfoutcome$State == state & dfoutcome[,outcome] != "Not Available", ]
v <- dfoutcome[,outcome]
row <- which.min(v)
hopital <- dfoutcome[row,"Hospital_Name"]
}
