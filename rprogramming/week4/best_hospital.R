## week 4 assignment
## best hospital based 30 day mortality rates.
best <- function(state, outcome) {
  # read outcome data
  care_measure_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  # coerce data to numerice
  # heart attack
  care_measure_data[, 11] <- as.numeric(care_measure_data[,11])
  # heart failure
  care_measure_data[, 17] <- as.numeric(care_measure_data[, 17])
  # pneumonia
  care_measure_data[, 23] <- as.numeric(care_measure_data[, 23])
  
  # check state and outcome are valid
  actualStates <- unique(care_measure_data$State)
  if (presentInList(actualStates, state) == FALSE) {
    stop("invalid state")
  }
  
  # check outcomes
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if (presentInList(validOutcomes, outcome) == FALSE) {
    stop("invalid outcome")
  }
  
  outcomeIdx <- getOutcomeIndex(outcome)
  
  # get the sub-data out,
  data_filter <- care_measure_data$State == state & !is.na(care_measure_data[outcomeIdx])
  subData <- care_measure_data[data_filter, c(2, outcomeIdx)]
  minHospitalAndRate <- findMin(subData)
  minHospitalAndRate
}

getOutcomeIndex <- function(outcome) {
  if (outcome == "heart attack")
    return(11)
  else if (outcome == "heart failure")
    return(17)
  else
    return(23)
}

presentInList <- function(actList, value) {
  present <- FALSE
  for (item in actList) {
    if (item == value) {
      present <- TRUE
      break
    }
  }
  return(present)
}

# input is data frame with 2 cols
findMin <- function(hospitalNameRateDataFrame) {
  df <- hospitalNameRateDataFrame
  minVal <- Inf
  minName <- Inf
  rows <- nrow(df)
  for (idx in 1 : rows) {
    name <- df[idx, 1]
    value <- df[idx, 2]
    if (value <= minVal) {
      if (value == minVal) {
        minVal <- value
        if (name < minName) {
          minName <- name
        }
      }
      else {
        minVal <- value
        minName <- name
      }
    }
  }
  
  c(minName, minVal)
}

