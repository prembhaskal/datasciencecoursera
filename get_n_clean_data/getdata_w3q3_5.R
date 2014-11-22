
gdpData <- read.csv("getdata_w3q3_1.csv", as.is = TRUE, skip = 4)

head(gdpData)

colnames(gdpData) <- 1:9
head(gdpData)
gdpData <- gdpData[, c(1,2,4,5)]
head(gdpData)
colnames(gdpData) <- c("code", "rank", "name", "gdp")
head(gdpData)
gdpData$rank <- as.numeric(gdpData$rank)
head(as.numeric(gdpData$gdp))

class(gdpData$gdp)

str_replace_all(gdpData$gdp, ",", "")
library(stringr)
str_replace_all(gdpData$gdp, ",", "")
gdpData$gdp <- str_replace_all(gdpData$gdp, ",", "")

gdpData$gdp <- as.numeric(gdpData$gdp)
head(gdpData)

properData <- gdpData[!is.na(gdpData$rank), ]


edcData <- read.csv("getdata_w3q2_2.csv", as.is = TRUE)
head(edcData, 1)

sortedData <- properData[order(properData$gdp, decreasing = TRUE), ]
head(sortedData)
sortedData <- properData[order(properData$rank, decreasing = TRUE), ]
head(sortedData)
sortedData[13,]
mergedData <- merge(properData, edcData, by.x = "code", by.y = "CountryCode")
colnames(mergedData)
nrow(mergedData)
length(properData$code[properData$code %in% edcData$CountryCode])

tapply(mergedData$rank, mergedData$Income.Group, mean)
qnt <- quantile(mergedData$rank, c(0,1/5, 2/5, 3/5, 4/5, 1))
rnkGp <- cut(mergedData$rank, breaks = qnt, include.lowest = TRUE)
summary(rnkGp)

table(mergedData$rank, rnkGp)
