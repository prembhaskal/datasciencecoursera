## read the data
NEI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")


baltimoreData <- NEI[NEI$fips == "24510", ]

## group the total emission per year.
baltimoreEmissions <- aggregate(Emissions ~ year, data = baltimoreData, sum)

# initiate a png device
png(filename = "plot2.png", width = 480, height = 480, units = "px")

with(baltimoreEmissions, plot(year, Emissions, type = "p", 
     xlab = "year", ylab = "PM2.5 emission in Baltimore (in tonnes)"),
     pch = 19, col = "black")

with(baltimoreEmissions, lines(year, Emissions))

dev.off()
