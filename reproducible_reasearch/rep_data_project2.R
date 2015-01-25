# required libraries
  library(dplyr)
  library(ggplot2)
  library(knitr)

# load the data

  con <- file("StormData.csv.bz2")
  fileData <- readLines(con, 10)
  close(con)

  stormData <- read.csv("StormData.csv.bz2")
  
  summary(stormData)
  dim(stormData)
  
# analyzing types of events
  length(unique(stormData$EVTYPE))

# cleaning the data
# unique activities sorted , write to a file and then analyze
# try to extract actual/proper activity name and replace it with gsub() function.
# converting to uppercase
# replacing common terms with single one
  stormData$EVTYPE <- toupper(stormData$EVTYPE)
  
  stormData$EVTYPE <- stormData$EVTYPE[order(stormData$EVTYPE)]
  write.table(stormData$EVTYPE, "weather_events.txt")
  
  # analysing the file we understand that weather event column has lot of inconsistencies.
  # here we try to clean up some of these.
  stormData$EVTYPE <- gsub("WINDS", "WIND", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*HEAVY SNOW.*", "HEAVY SNOW", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*TSTM.*", "THUNDERSTORM", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*THUNDERSTORM.*", "THUNDERSTORM", stormData$EVTYPE)
  stormData$EVTYPE <- gsub("COLD.*", "COLD", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*DRY MICROBURST.*", "DRY MICROBURST", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*FLASH.*FLOOD.*", "FLOOD", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*HAIL.*", "HAIL", stormData$EVTYPE)
  stormData$EVTYPE <- gsub("PRECI.*TION", "RAIN", stormData$EVTYPE)
  stormData$EVTYPE <- gsub("RAINFALL", "RAIN", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*HEAVY.*RAIN.*", "RAIN", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*WATERSPOUT.*", "WATERSPOUT", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*THU.*TORM.*", "THUNDERSTORM", stormData$EVTYPE)
  stormData$EVTYPE <- gsub("SNOW.*", "SNOW", stormData$EVTYPE)
  stormData$EVTYPE <- gsub("ICE.*", "ICE", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*HURRICANE.*", "HURRICANE", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*HIGH.*WIND.*", "HIGH WIND", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*LIG.*ING.*", "LIGHTNING", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*FREEZING.*", "FREEZING", stormData$EVTYPE)
  stormData$EVTYPE <- gsub("FLOOD.*", "FLOOD", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*TORNADO.*", "TORNADO", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*TORNDAO.*", "TORNADO", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*TROPICAL STORM.*", "TROPICAL STORM", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*HEAVY SURF.*", "HEAVY SURF", stormData$EVTYPE)
  stormData$EVTYPE <- gsub("EXTREME", "EXCESSIVE", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*EXCESSIVE.*HEAT.*", "EXCESSIVE HEAT", stormData$EVTYPE)
  stormData$EVTYPE <- gsub(".*EXCESSIVE.*WIND.*", "EXCESSIVE WIND", stormData$EVTYPE)
  stormData$EVTYPE <- gsub("DRY.*", "DRY", stormData$EVTYPE)
  stormData$EVTYPE <- gsub("WINTER.*", "WINTER", stormData$EVTYPE)
  stormData$EVTYPE <- gsub("RIP CURRENT.*", "RIP CURRENT", stormData$EVTYPE)
  
# finding events vs total fatalities.
  fatalitiesVsEvtype <- aggregate(FATALITIES ~ EVTYPE, data = stormData, sum)
  fatalitiesVsEvtype <- fatalitiesVsEvtype[order(fatalitiesVsEvtype$FATALITIES, decreasing = TRUE), ]
  
  # faster alternative using tapply
#   facilitiesVsEvt <- tapply(stormData$FATALITIES, stormData$EVTYPE, sum)
#   testDat <- data.frame("event" = names(facilitiesVsEvt), "facilities" = facilitiesVsEvt)
#   testDat <- testDat[order(testDat$facilities, decreasing = TRUE), ]
  
# top events causing major fatalities
  topEventForFatalities <- head(fatalitiesVsEvtype, 10)
  
  g <- ggplot(data = topEventForFatalities, aes(x = EVTYPE, y = FATALITIES))
  g <- g + geom_bar(stat = "identity")
  g <- g + labs(x = "event", y = "total fatalities")
  g <- g + ggtitle("top events causing fatalities")
  g <- g + theme(axis.text.x = element_text(angle = 90))
  print(g)
  
# finding events causing injuries.
  injuriesVsEvtype <- aggregate(INJURIES ~ EVTYPE, data = stormData, sum)
  injuriesVsEvtype <- injuriesVsEvtype[order(injuriesVsEvtype$INJURIES, decreasing = TRUE),]
  
# top events causing major injuries.
  topEventForInjuries <- head(injuriesVsEvtype, 10)
# plotting 
  g <- ggplot(data = topEventForInjuries, aes(x = EVTYPE, y = INJURIES))
  g <- g + geom_bar(stat = "identity")
  g <- g + labs(x = "event", y = "total injuries")
  g <- g + ggtitle("top events causing injuries")
  g <- g + theme(axis.text.x = element_text(angle = 90))
  print(g)
  
# convert all to upper case
  stormData$PROPDMGEXP <- toupper(stormData$PROPDMGEXP)
  stormData$CROPDMGEXP <- toupper(stormData$CROPDMGEXP)
# convert non character exponent to 0
  stormData[!grepl("[K|M|H|B|0-9]", stormData$PROPDMGEXP), "PROPDMGEXP"] <- 0
  stormData[!grepl("[K|M|H|B|0-9]", stormData$CROPDMGEXP), "CROPDMGEXP"] <- 0
  
  
# map for character vs exponential values.
  char <- c(0,1,2,3,4,5,6,7,8,9,"H","K","M","B")  
  val <- c(0,1,2,3,4,5,6,7,8,9,2,3,6,9)
  expVsVal <- data.frame(char, val)
  
  colnames(expVsVal) <- c("char", "propexp")
  
  stormData <- merge(x = stormData, y = expVsVal, by.x = "PROPDMGEXP", by.y = "char")
  
  colnames(expVsVal) <- c("char", "cropexp")
  stormData <- merge(x = stormData, y = expVsVal, by.x = "CROPDMGEXP", by.y = "char")
  
  stormData <- mutate(stormData, totalDamage  = PROPDMG * 10 ^ propexp + CROPDMG * 10 ^ cropexp)
  
# verifying the data
  head(stormData[, c("PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "totalDamage")])
  
# group events by total damage
  totalDamageVsEvtType <- aggregate(totalDamage ~ EVTYPE, data = stormData, sum)
# getting the events causing maximum damage
  totalDamageVsEvtType <- totalDamageVsEvtType[order(totalDamageVsEvtType$totalDamage, decreasing = TRUE), ]
  
# considering the damages which are greater than 0
  totalDamageVsEvtType <- totalDamageVsEvtType[totalDamageVsEvtType$totalDamage > 0,]
  
  head(totalDamageVsEvtType, 10)
  
# dividing by billion to fit the scale
  totalDamageVsEvtType$totalDamage <- totalDamageVsEvtType$totalDamage/10^9
# events causing damage
  topEventsForDamage <- head(totalDamageVsEvtType, 10)
  
  g <- ggplot(data = topEventsForDamage, aes(x = EVTYPE, y = totalDamage))
  g <- g + geom_bar(stat = "identity")
  g <- g + labs(x = "event", y = "total damages in Billion Dollars")
  g <- g + ggtitle("top events causing financial damages")
  g <- g + theme(axis.text.x = element_text(angle = 90))
  print(g)
  
  
  ## utility function
  findUniqueAndWrite <- function(eventsList) {
    eventsList <- unique(eventsList)
    eventsList <- eventsList[order(eventsList)]
    write.table(eventsList, "weather_events.txt")
  }
  
  uniqueToFile <- function(eventsList, fileName) {
    eventsList <- unique(eventsList)
    eventsList <- eventsList[order(eventsList)]
    write.table(eventsList, fileName)
  }