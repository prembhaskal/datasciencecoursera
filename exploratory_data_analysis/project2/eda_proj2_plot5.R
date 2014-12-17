## ggplot 2 library

library(ggplot2)

## read the data
NEI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")

# filter baltimore city data
baltimoreData <- NEI[NEI$fips == "24510", ]

# filter out the emission sources by motor vehicle sources.
motorSCC <- SCC[grepl("motor", SCC$Short.Name, ignore.case = TRUE), ]

# merge baltimore data with the motor vehicle sources.
motorEmissionData <- merge(x = baltimoreData, y = motorSCC, by.x = "SCC", by.y = "SCC")

# initiate a png device
png(filename = "plot5.png", width = 800, height = 480, units = "px")

# draw step by step
g <- ggplot(motorEmissionData, aes(x = year, y = Emissions))
# added smooth to see the trend
g <- g + geom_point() + geom_smooth(method = "lm")
g <- g + labs(x = "year", y = "PM2.5 emissions (in tonnes)")
g <- g + ggtitle("PM2 Emission by Motor Vehicles in Baltimore")
g <- g + ylim(0, .2)
print(g)

dev.off()
