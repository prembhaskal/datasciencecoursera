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

# group total emissions by year.
emissionByYear <- aggregate(Emissions ~ year, data = motorEmissionData, sum)

# initiate a png device
png(filename = "plot5.png", width = 800, height = 480, units = "px")

# draw step by step
# added smooth to see the trend
# limit the y scale to see data clearly.
# also added regression line for the actual data
g <- ggplot(emissionByYear, aes(x = year, y = Emissions))
g <- g + geom_point(size = 3) + geom_line()
g <- g + geom_smooth(data = motorEmissionData, aes(year, Emissions), method = "lm")
g <- g + labs(x = "year", y = "PM2.5 emissions (in tonnes)")
g <- g + ggtitle("total PM2 Emission by Motor Vehicles in Baltimore")
print(g)

dev.off()

## NOTE regression lines suggest that the spike may be due to an outlier.