## ggplot 2 library

library(ggplot2)

## read the data
NEI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")

# filter baltimore city data PLUS California data.
cityData <- NEI[NEI$fips == "24510" | NEI$fips == "06037", ]

# convert fips to city factor.
cityData$fips = factor(cityData$fips, 
                            levels = c("24510", "06037"), 
                            labels = c("Baltimore", "California"))

# filter out the emission data by motor vehicle sources.
motorSCC <- SCC[grepl("motor", SCC$Short.Name, ignore.case = TRUE), ]

# merge the cityData and motor vehicles data together.
motorEmissionData <- merge(x = cityData, y = motorSCC, by.x = "SCC", by.y = "SCC")

# aggreate emissions by year and city.
motorAggData <- aggregate(Emissions ~ year + fips, data = motorEmissionData, sum)

# initiate a png device
png(filename = "plot6.png", width = 800, height = 480, units = "px")

# draw step by step
# outliers ignored, changed y limit to 0:5
g <- ggplot(motorEmissionData, aes(x = year, y = Emissions))
g <- g + geom_point() + geom_smooth(method = "lm")
g <- g + facet_grid( . ~ fips)
g <- g + labs(x = "year", y = "PM2.5 emissions (in tonnes)")
g <- g + ggtitle("PM2 Emission by Motor Vehicles in 2 cities")
g <- g + ylim(0, 5)
print(g)

dev.off()
