# course project.
# run analysis functions
combineAndGetTrainingData <- function (msrIdVsName) {
  trainingSet <- read.table("UCI HAR Dataset//train//X_train.txt",col.names = msrIdVsName$measurementName, check.names = FALSE)
  trainingActLabels <- read.table("UCI HAR Dataset//train//y_train.txt", col.names = "activity")
  trainSubjects <- read.table("UCI HAR Dataset//train//subject_train.txt", col.names = "subject")
  trainingData <- cbind(trainingActLabels, trainSubjects, trainingSet)
}

combineAndGetTestData <- function (msrIdVsName) {
  testSet <- read.table("UCI HAR Dataset//test/X_test.txt", col.names = msrIdVsName$measurementName, check.names = FALSE)
  testActivityLabel <- read.table("UCI HAR Dataset//test//y_test.txt", col.names = "activity")
  testSubjects <- read.table("UCI HAR Dataset//test//subject_test.txt", col.names = "subject")
  testData <- cbind(testActivityLabel, testSubjects, testSet)
}

extractMeanAndStdDevData <- function (msrIdVsName, completeData) {
  meanOrStdCol <- str_detect(msrIdVsName$measurementName, fixed("mean()")) | str_detect(msrIdVsName$measurementName, fixed("std()"))
  meanOrStdCols <- msrIdVsName$measurementName[meanOrStdCol]
  measureAndStdDevData <- completeData[, c("activity", "subject", meanOrStdCols)]
}


applyActivityLabels <- function (measureAndStdDevData, activityLabels) {
  # replace activity id in data with activity name.
  # TODO replce the below loop with some apply function or something. (also make it more efficient)
  for (i in 1: nrow(activityLabels)) {
    entry <- activityLabels[i,]
    measureAndStdDevData[measureAndStdDevData$activity == entry$activityId, "activity"] = entry$activityName
  }
}


getDataSetWithMeanMeasurements <- function (measureAndStdDevData) {
  # tidy data set with avergage of each measuremeny grouped by activity and subject.
  totalCols <- ncol(measureAndStdDevData)
  
  tidyDataSet <- aggregate(measureAndStdDevData[,c(3: totalCols)], 
                           by = list("activity" = measureAndStdDevData$activity, "subject" = measureAndStdDevData$subject), 
                           mean) 
}