## find correlation in files where complete cases are more then the given threshold
corr <- function(directory, threshold = 0) {
  ## 0 length vector
  correlationVector <- numeric()
  
  fileNames <- list.files(directory, full.names=TRUE)
  
  idx <- 1
  
  for (fileName in fileNames) {
    fileData <- read.csv(fileName)
    ## logical vector for complete cases
    ccCases <- complete.cases(fileData)
    
    ccFileData <- fileData[ccCases, ]
    
    if (nrow(ccFileData) > threshold) {
    ## calculate the correlation between nitrate and sulfate
      corValue <- cor(ccFileData$nitrate, ccFileData$sulfate)
      
      correlationVector[idx] <- corValue
      idx = idx + 1
    }
  }
  
  return(correlationVector)
}