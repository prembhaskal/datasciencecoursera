## course -> r-programming
## assignment 2
## function which reports count of complete cases in files present in the 'directory'
## for each file corresponding to the id.
complete <- function (directory, id=1:332) {
  fileNames <- list.files(directory, full.names=TRUE)
  
  totalFiles <- length(id)
  
  ## cc - completeCases
  ccDataFrame <- data.frame("id" = 1:totalFiles, "nobs" = 1 : totalFiles)
  
  idx <- 1
  
  for (fileId in id) {
    fileData <- read.csv(fileNames[fileId])
    ccCases <- complete.cases(fileData)
    ccData <- fileData[ccCases, ]
    
    ccDataFrame[idx, ] <- c(fileId, nrow(ccData))
    idx = idx + 1
  }
  
  return(ccDataFrame)
}