## 1 Plot the 30-day mortality rates for heart attack

## Read the outcome data into R via the read.csv function and look at the first 
## few rows.

## > outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## > head(outcome)

library(readr)
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

head(outcome)


## There are many columns in this dataset. You can see how many by typing ncol
## (outcome) (you can see the number of rows with the nrow function). 

ncol(outcome)

nrow(outcome)


## In addition,  you can see the names of each column by typing names(outcome) (the names are also in the PDF document.

names(outcome)

## To make a simple histogram of the 30-day death rates from 
## heart attack (column 11 in the outcome dataset),run outcome[, 11] <- as.numeric(outcome[, 11])

outcome[, 11] <- as.numeric(outcome[,11])

## You may get a warning about NAs being introduced; that is okay > hist(outcome[, 11]) 

hist(outcome[, 11])

## Because we originally read the data in as character (by specifying colClasses = "character" we need to                                                          coerce the column to be numeric. You may get a warning about NAs being introduced but that is okay.
## coerce the column to be numeric. You may get a warning about NAs being introduced but that is okay.


## 2 Finding the best hospital in a state

## Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
## outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
## in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
## be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
## outcome should be excluded from the set of hospitals when deciding the rankings.
## Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
## be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,
## and “f” are tied for best, then hospital “b” should be returned).

##The function should use the following template.
##best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
##}

## The function should check the validity of its arguments. If an invalid state value is passed to best, the
## function should throw an error via the stop function with the exact message “invalid state”. If an invalid
## outcome value is passed to best, the function should throw an error via the stop function with the exact
##message “invalid outcome”.

data_new2 <- outcome[, c(2, 7, 11, 17, 23)]

best <- function(state, outcome) {
  
  data_state <- data_new[which(data_new[, 2] == state), ]
  
  if (all(state != data_new[, 2])) {
    stop("invalid state")
  }
  
  if (outcome == "heart attack") {
      data_updated <- data_state[, 1:3]
    } else if (outcome == "heart failure") {
      data_updated <- data_state[, c(1, 2, 4)]
    } else if (outcome == "pneumonia") {
      data_updated <- data_state[, c(1, 2, 5)]
    } else {
      stop("invalid outcome")
    }
  
  data_updated[, 3] <- suppressWarnings(as.numeric(data_updated[, 3]))
  
  # Delete NAs from the outcome column
  
  non_na_indices <- which(!is.na(data_updated[, 3]))
  data_updated <- data_updated[non_na_indices, ]
  
  # Order the data frame in ascending order according to the outcome column
  # In case of a tie, the candidates are ordered following the alphabetical order of the hospital name
  
  data_ordered <- data_updated[with(data_updated, order(data_updated[, 3], data_updated[, 1])), ]
  
  # Output the hospital name in the desired state with the lowest outcome
  
  data_ordered[1, 1]
  
}



## 3 Ranking hospitals by outcome in a state

## Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument. For example, the call
## rankhospital("MD", "heart failure", 5)
## would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
## for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
## (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data on a particular outcome should
## be excluded from the set of hospitals when deciding the rankings.

## Handling ties. It may occur that multiple hospitals have the same 30-day mortality rate for a given cause
## of death. In those cases ties should be broken by using the hospital name. For example, in Texas (“TX”),
## the hospitals with lowest 30-day mortality rate for heart failure are shown here.

## > head(texas)
## Hospital.Name Rate Rank
## 3935 FORT DUNCAN MEDICAL CENTER 8.1 1
## 4085 TOMBALL REGIONAL MEDICAL CENTER 8.5 2
## 4103 CYPRESS FAIRBANKS MEDICAL CENTER 8.7 3
## 3954 DETAR HOSPITAL NAVARRO 8.7 4
## 4010 METHODIST HOSPITAL,THE 8.8 5
## 3962 MISSION REGIONAL MEDICAL CENTER 8.8 6

## Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both have the same 30-day rate
## (8.7). However, because Cypress comes before Detar alphabetically, Cypress is ranked number 3 in this
## scheme and Detar is ranked number 4. One can use the order function to sort multiple vectors in this
## manner (i.e. where one vector is used to break ties in another vector).

## The function should use the following template.

## rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
## }

## The function should check the validity of its arguments. If an invalid state value is passed to rankhospital,
## the function should throw an error via the stop function with the exact message “invalid state”. If an invalid
## outcome value is passed to rankhospital, the function should throw an error via the stop function with
## the exact message “invalid outcome”.


data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Select the necessary fields (variables/columns) in the following order:
# hospital name, state, 30-day mortality rate from heart attack, heart failure and pneumonia

data_new <- data[, c(2, 7, 11, 17, 23)]

# Note: Now, 2nd col becomes 1st, 7th 2nd, 11th 3rd, 17th 4th, 23rd 5th


# Create the function "rankhospital" 

rankhospital <- function(state, outcome, num = "best") {
  
  # Select the desired state
  
  data_state <- data_new[which(data_new[, 2] == state), ]
  
  if (all(state != data_new[, 2])) {
    stop("invalid state")
  }
  
  # Select the desired outcome
  
  if (outcome == "heart attack") {
    data_updated <- data_state[, 1:3]
  } else if (outcome == "heart failure") {
    data_updated <- data_state[, c(1, 2, 4)]
  } else if (outcome == "pneumonia") {
    data_updated <- data_state[, c(1, 2, 5)]
  } else {
    stop("invalid outcome")
  }
  
  # Convert the outcome column to numeric type
  
  data_updated[, 3] <- suppressWarnings(as.numeric(data_updated[, 3]))
  
  # Delete NAs from the outcome column
  
  non_na_indices <- which(!is.na(data_updated[, 3]))
  data_updated <- data_updated[non_na_indices, ]
  
  # Order the data frame in ascending order according to the outcome column
  # In case of a tie, the candidates are ordered following the alphabetical order of the hospital name
  
  data_ordered <- data_updated[with(data_updated, order(data_updated[, 3], data_updated[, 1])), ]
  
  # Output the hospital name according to the desired rank ("best", "worst" or numerical value)
  
  if (num == "best") {
    data_ordered[1, 1]
  } else if (num == "worst") {
    data_ordered[nrow(data_ordered), 1]
  } else {
    if (num <= nrow(data_ordered)) {
      data_ordered[num, 1]
    } else {
      stop("NA")
    }
  }
  
}



## 4 Ranking hospitals in all states

## Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. For example the function call
## rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates. The function should return a value
## for every state (some may be NA). The first column in the data frame is named hospital, which contains
## the hospital name, and the second column is named state, which contains the 2-character abbreviation for
## the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings.
## Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
## that the rankhospital function handles ties.

## The function should use the following template.
## rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
## NOTE: For the purpose of this part of the assignment (and for efficiency), your function should NOT call
## the rankhospital function from the previous section.
## The function should check the validity of its arguments. If an invalid outcome value is passed to rankall,
## the function should throw an error via the stop function with the exact message “invalid outcome”. The num
## variable can take values “best”, “worst”, or an integer indicating the ranking (smaller numbers are better).
## If the number given by num is larger than the number of hospitals in that state, then the function should
### return NA.


rankall <- function(outcome, num = "best") {
  
  # Select the desired outcome
  
  if (outcome == "heart attack") {
    data_updated <- data_new[, 1:3]
  } else if (outcome == "heart failure") {
    data_updated <- data_new[, c(1, 2, 4)]
  } else if (outcome == "pneumonia") {
    data_updated <- data_new[, c(1, 2, 5)]
  } else {
    stop("invalid outcome")
  }
  
  
  # Convert the outcome column to numeric type
  
  data_updated[, 3] <- suppressWarnings(as.numeric(data_updated[, 3]))
  
  # Delete NAs from the outcome column
  
  non_na_indices <- which(!is.na(data_updated[, 3]))
  data_complete <- data_updated[non_na_indices, ]
  
  # Order the data frame in ascending order by state, outcome then hospital name column
  
  data_ordered <- data_complete[with(data_complete, order(data_complete[, 2], data_complete[, 3], data_complete[, 1])), ]
  
  # Create a list of data frames, each one corresponding to a specif state
  
  data_by_state <- vector(mode = "list")
  
  unique_states <- unique(data_ordered[, 2])
  
  for (s in 1:length(unique_states)) {
    data_by_state[[s]] <- filter(data_ordered, data_ordered[, 2] == unique_states[s])
  }
  
  # Create a frame containing one hospital per state according to the desired rank ("best", "worst" or numerical value)
  
  ranking <- data.frame(nrow = length(unique_states), ncol = 2)
  colnames(ranking) <- c("hospital", "state")
  
  if (num == "best") {
    
    for (s in 1:length(unique_states)) {
      ranking[s, 1] <- data_by_state[[s]][1, 1]
      ranking[s, 2] <- data_by_state[[s]][1, 2] 
    }
    
  } else if (num == "worst") {
    
    for (s in 1:length(unique_states)) {
      ranking[s, 1] <- data_by_state[[s]][nrow(data_by_state[[s]]), 1]
      ranking[s, 2] <- data_by_state[[s]][nrow(data_by_state[[s]]), 2] 
    } 
    
  } else {
    
    for (s in 1:length(unique_states)) {
      
      if (nrow(data_by_state[[s]]) >= num) {
        
        ranking[s, 1] <- data_by_state[[s]][num, 1]
        ranking[s, 2] <- data_by_state[[s]][num, 2]
        
      } else {
        
        ranking[s, 1] <- "<NA>"
        ranking[s, 2] <- data_by_state[[s]][1, 2]
      }
    }
    
  }
  
  # Output the result: ranking data frame
  
  ranking
  
}