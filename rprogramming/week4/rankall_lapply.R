## week 4 assignment
## get the ranked hospital
rankall_lapply <- function(outcome, num = "best") {
  # read outcome data
  care_measure_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  # coerce data to numerice
  # heart attack
  care_measure_data[, 11] <- as.numeric(care_measure_data[,11])
  # heart failure
  care_measure_data[, 17] <- as.numeric(care_measure_data[, 17])
  # pneumonia
  care_measure_data[, 23] <- as.numeric(care_measure_data[, 23])
  
  # check outcomes
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if (presentInList(validOutcomes, outcome) == FALSE) {
    stop("invalid outcome")
  }
  
  
  bestHosp <- getBestHospitalFunction(outcome, num)
  splitOutput <- split(care_measure_data, care_measure_data$State)
  outputListOfVectors <- lapply(splitOutput,  bestHosp)
  # slow method since increase the data.frame row by row.....check the other faster method.
  outputDf <- do.call(rbind, outputListOfVectors)
  colnames(outputDf) <- c("hospital", "state")
  outputDf
}
  
getBestHospitalFunction <- function(outcomeName, rank) {
    outcomeIdx <- getOutcomeIndex(outcomeName)
  getBestHospital <- function(df) {
#     print(summary(df))
    stateName <- df[1,7]
    subData <- df[!is.na(df[outcomeIdx]), c(2, outcomeIdx)]
    orderedData <- order(subData[,2], subData[,1])
    
    if (rank == "best") {
      rank <- 1
    }
    else if (rank == "worst") {
      rank <- length(orderedData)
    }
    
    hospital <- subData[orderedData[rank], 1]
    c(hospital, stateName)
  }
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
