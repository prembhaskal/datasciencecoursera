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


## doing t tests for different supplements taken with same dose.
supp1dose1 <- ToothGrowth[ToothGrowth$supp == "OJ" & ToothGrowth$dose == "0.5",] 
supp2dose1 <- ToothGrowth[ToothGrowth$supp == "VC" & ToothGrowth$dose == "0.5",] 

## since we don't have any information of variance of different group, let us assume it is different.
t.test(supp1dose1$len, supp2dose1$len, paired = FALSE,  var.equal = FALSE)

## DOUBT what is the meaning or interpretation of the values, conf interval obtained by doing the "t-tests".
supp1dose2 <- ToothGrowth[ToothGrowth$supp == "OJ" & ToothGrowth$dose == "1",] 
supp2dose2 <- ToothGrowth[ToothGrowth$supp == "VC" & ToothGrowth$dose == "1",] 

## since we don't have any information of variance of different group, let us assume it is different.
t.test(supp1dose2$len, supp2dose2$len, paired = FALSE,  var.equal = FALSE)

# supp1dose3 <- ToothGrowth[ToothGrowth$supp == "OJ" & ToothGrowth$dose == "2",] 
# supp2dose3 <- ToothGrowth[ToothGrowth$supp == "VC" & ToothGrowth$dose == "2",] 
# 
# ## since we don't have any information of variance of different group, let us assume it is different.
# t.test(supp1dose3$len, supp2dose3$len, paired = FALSE,  var.equal = FALSE)


## doing a two group hypothesis testing assuming that mean of the tooth growth is same ( that is 
## different in mean is 0 -- NULL HYPOTHESIS)

dose1Data <- ToothGrowth[ToothGrowth$dose == "2",]
t.test(len ~ supp, paired = FALSE, var.equal = FALSE, data = dose1Data)

## in our case, n = 10, therefore df = 9, so 
Z_1minusAlpha  = qt(0.975, df = 9)

## since the absolute test Statistic is lesser than Z_1minusAlpha, we fail to reject null hypothesis, so 
## the 2 data groups have same mean.

## clear figure
dev.off()
