## read the data
NEI <- readRDS(file = "summarySCC_PM25.rds")
SCC <- readRDS(file = "Source_Classification_Code.rds")

## group the total emission per year.
emissionsByYear <- aggregate(Emissions ~ year, data = NEI, sum)

# initiate a png device
png(filename = "plot1.png", width = 480, height = 480, units = "px")

## plots for each year
plot(emissionsByYear$year, emissionsByYear$Emissions, type = "p", 
     ylab = "total PM2.5 Emission (in tonnes)", xlab = "year",
     main = "Total PM2.5 Emission across years",
     pch = 19, col = "black")
## connecting line to show the decreasing emission.
with(emissionsByYear, lines(year, Emissions))

## close the device.
dev.off()