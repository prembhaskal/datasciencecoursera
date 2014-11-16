# read file line by line, split to form a data.frame.
# a hack... and this is what seems to be needed for this question as per discussion forum.
con <- file("getdata_w2q5.for")
fileData <- readLines(con)
close(con)
len <- length(fileData)
col4Data <-numeric(len - 4)
# skip first 4 lines
for(i in 4 : length(fileData)) {
  line <- fileData[i]
  col4 <- substr(line, 29, 32)
  col4Data[i-4] <- as.numeric(col4)
  if (is.na(col4)) {
    print(col4)
  }
}

print(sum(col4Data))