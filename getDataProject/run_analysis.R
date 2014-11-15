# load required libraries
library(stringr)

# steps to do
# get proper (means combined with measurement and activitylabel) data set for each training and test.
# combine them together.
# do further activities..

msrIdVsName <- read.table("UCI HAR Dataset//features.txt", col.names=c("measurementId", "measurementName"),
                        colClasses = c("numeric","character"))
actLabelsDf <- read.table("UCI HAR Dataset//activity_labels.txt", col.names = c("activityId", "activityName"),
                               colClasses = c("numeric", "character"))
  
# merge the training and test data set into one data frame.
# training data
trainSetDf <- read.table("UCI HAR Dataset//train//X_train.txt",col.names = msrIdVsName$measurementName, check.names = FALSE)
trainActLabelsDf <- read.table("UCI HAR Dataset//train//y_train.txt", col.names = "activity")
trainSubjectsDf <- read.table("UCI HAR Dataset//train//subject_train.txt", col.names = "subject")
trainingDataDf <- cbind(trainActLabelsDf, trainSubjectsDf, trainSetDf)

# test data
testSetDf <- read.table("UCI HAR Dataset//test/X_test.txt", col.names = msrIdVsName$measurementName, check.names = FALSE)
testActivityLabelDf <- read.table("UCI HAR Dataset//test//y_test.txt", col.names = "activity")
testSubjectsDf <- read.table("UCI HAR Dataset//test//subject_test.txt", col.names = "subject")
testDataDf <- cbind(testActivityLabelDf, testSubjectsDf, testSetDf)

# merging training and test data
completeDataDf <- rbind(trainingDataDf, testDataDf)

# extracting only means Or standard deviation of the required columns
meanOrStdCol <- str_detect(msrIdVsName$measurementName, fixed("mean()")) | str_detect(msrIdVsName$measurementName, fixed("std()"))
meanOrStdCols <- msrIdVsName$measurementName[meanOrStdCol]
measureAndStdDevData <- completeDataDf[, c("activity", "subject", meanOrStdCols)]

# replace activity id in data with activity name.
# TODO replce the below loop with some apply function or something. (also make it more efficient)
for (i in 1: nrow(actLabelsDf)) {
  entry <- actLabelsDf[i,]
  measureAndStdDevData[measureAndStdDevData$activity == entry$activityId, "activity"] = entry$activityName
}

# tidy data set with avergage of each measuremeny grouped by activity and subject.
totalCols <- ncol(measureAndStdDevData)

tidyDataSet <- aggregate(measureAndStdDevData[,c(3: totalCols)], 
                         by = list("activity" = measureAndStdDevData$activity, "subject" = measureAndStdDevData$subject), 
                         mean)
write.table(tidyDataSet, "getDataProject_TidyDataSet.txt", row.names = FALSE, sep = "\t", quote = FALSE)
