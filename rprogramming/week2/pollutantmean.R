## course -> r-programming
## assignment 2

## 'directory' is a character vector of length 1 indicating the location of csv files.
## 'pollutant' is a character vector of len 1 -> name of pollutant
## 'id' -> integer vector, monitor IDs
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## storing the sum total of pollutant across all the files.
  totalSum <- 0
  ## count of total elements in all the files (disregarding the NAs)
  totalCount <- 0
  
  ## get the files.
  files <- list.files(directory, full.names = TRUE)
  
  ## iterate over each id
  for (eachId in id) {
    ## reading the 'id.csv' file in a data frame.
    fileData <- read.csv(files[eachId])
    ## print(files[eachId])
    
    ## individual sum
    indSum <- 0
    ## individual count
    indCount <- 0
    
    ## this is SLICK ;)?
    pollutantData <- fileData[!is.na(fileData[pollutant]), pollutant]
    indSum <- sum(pollutantData)
    ## print(paste("ind sum for ", eachId, " is ", indSum))
    
    indCount <- length(pollutantData)
    ## print(paste("ind count for ", eachId, " is ", indCount))
    
    totalSum = totalSum + indSum
    totalCount = totalCount + indCount
  }
  
  mean <- totalSum/totalCount
  return(mean)
}