## ggplot 2 library

library(ggplot2)

## read the data
NEI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")

# filter out the emission data by coal sources.
coalSCC <- SCC[grepl("coal", SCC$Short.Name, ignore.case = TRUE), ]

# merge the source data with the emission data.
coalEmissionData <- merge(x = NEI, y = coalSCC, by.x = "SCC", by.y = "SCC")

# group total emissions by year.
emissionByYear <- aggregate(Emissions ~ year, data = coalEmissionData, sum)

# initiate a png device
png(filename = "plot4.png", width = 800, height = 480, units = "px")

# draw step by step
g <- ggplot(emissionByYear, aes(x = year, y = Emissions))
g <- g + geom_point(size = 3) + geom_line() 
g <- g + labs(x = "year", y = "PM2.5 emissions (in tonnes)")
g <- g + ggtitle("total PM2 Emission by Coal Combustion over years.")
print(g)

dev.off()
