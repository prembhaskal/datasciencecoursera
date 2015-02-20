## statistical inference project part 2
## libraries and other stuff
library(ggplot2)

## tooth growth data
library(datasets)

data(ToothGrowth)
head(ToothGrowth)
## summary of the data
summary(ToothGrowth)

nrow(ToothGrowth)

unique(ToothGrowth$supp)
class(ToothGrowth$supp)

unique(paste(ToothGrowth$supp,ToothGrowth$dose))

## the data is about the tooth growth as a affect of 2 supplements VC and OJ, with different doses of 0.5, 1.0 and 2.0

## finding the average length of tooth growth grouped by supplements and doses.
toothGrowthBySuppDoes = aggregate(len ~ supp + dose, data = ToothGrowth, mean)

## plot of average growth Vs supplement_n_does
# g <- ggplot(data = toothGrowthBySuppDoes, aes(x = paste(supp,"-",dose), y = len))
g <- ggplot(data = toothGrowthBySuppDoes, aes(x = dose, y = len))
g <- g + geom_bar(stat = "identity")
g <- g + facet_grid(supp ~ .)
g <- g + labs(x = "dose used", y = "average tooth length")
g <- g + ggtitle("bar plot of average tooth growth vs dose grouped by different suppplements used")
print(g)


## clear figure
dev.off()
