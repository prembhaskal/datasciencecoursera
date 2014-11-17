# load required libraries and scripts.
library(stringr)
source("run_analysis_functions.R")

print("started the analysis ...")

msrIdVsName <- read.table("UCI HAR Dataset//features.txt", col.names=c("measurementId", "measurementName"),
                        colClasses = c("numeric","character"))

activityLabels <- read.table("UCI HAR Dataset//activity_labels.txt", col.names = c("activityId", "activityName"),
                               colClasses = c("numeric", "character"))
  
trainingData <- combineAndGetTrainingData(msrIdVsName)

testData <- combineAndGetTestData(msrIdVsName)

# merging training and test data
completeData <- rbind(trainingData, testData)

measureAndStdDevData <- extractMeanAndStdDevData(msrIdVsName, completeData)

applyActivityLabels(measureAndStdDevData, activityLabels)

tidyDataSet <- getDataSetWithMeanMeasurements(measureAndStdDevData)

write.table(tidyDataSet, "getDataProject_TidyDataSet.txt", row.names = FALSE, sep = "\t", quote = FALSE)

print("analysis completed!!!")