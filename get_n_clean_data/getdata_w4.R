## question 1
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", "getdata_w4q1.csv")
  dat1 <- read.csv("getdata_w4q1.csv")
  d1names <- names(dat1)
  d1names <- tolower(d1names)
  splitnames <- strsplit(d1names, "wgtp")
  splitnames[123]

## question 2
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv", "getdata_w4q2.csv")
  dat <- read.csv("getdata_w4q2.csv", skip = 4, nrow = 190, colClasses = c("character", "character", "NULL", "character", "character", rep("NULL", 5)))
  colnames(dat) <- c("code", "rank", "name", "gdp")
  dat$gdp <- gsub(",", "", dat$gdp)
  dat$gdp <- as.numeric(dat$gdp)
  mean(dat$gdp)

## question 3
  dat$name[grep("^United", dat$name)]

  
## q4
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv", "getdata_w4q3_edc.csv")
  edcDat <- read.csv("getdata_w4q3_edc.csv")
  mergedDat <- merge(dat, edcDat, by.x = "code", by.y = "CountryCode")
  
  cond <- !is.na(mergedDat$Special.Notes) & grepl("^Fiscal year end: June*", mergedDat$Special.Notes)
  length(mergedDat[cond, "code"])
  
## q5
  library(quantmode)
  amzn = getSymbols("AMZN",auto.assign=FALSE)
  amznDat <- as.data.frame(amzn)
  amznDat <- cbind(date = rownames(amznDat), amznDat)
  rownames(amznDat) <- 1:nrow(amznDat)
  nrow(amznDat[year(amznDat$date) == "2012",])
  nrow(amznDat[year(amznDat$date) == "2012" & wday(amznDat$date) == 2,])
  
  