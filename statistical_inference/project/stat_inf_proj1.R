## exponential distribution 
## distribution of average of 40 samples.

library(ggplot2)

totalIterations <- 1000
sequence <- seq(1 : totalIterations)

lambda <- 0.2
n <- 40

meanOfAverages <- sapply(X = sequence, function(p) {
  samples <- rexp(n, lambda)
  round(mean(samples), 1)
})

dfMean <- data.frame("mean" = meanOfAverages)

g <- ggplot()
g <- g + geom_density(data = dfMean, aes(x = mean, color = "sample_mean"), size = 2)
g <- g + geom_vline(aes(xintercept = 1/lambda, size = 2, color = "theoretical_mean"))
g <- g + labs(x = "x", y = "density")
g <- g + ggtitle("sample mean vs theoretical mean")
g <- g + xlim(0, 10)
g <- g + scale_color_manual(name = "",
                            values = c("sample_mean" = "red", "theoretical_mean" = "blue"))

print(g)



## the plot shows that the mean of the distribution is around 5, which is the same as the 
# sample mean.


## super imposed plots
# g <- ggplot()
# g <- g + geom_density(data = df, aes(x = mean, xmin = 0, xmax = 10), color = "red")
# g <- g + geom_density(data = dfObs, aes(x = obs, xmin = 0, xmax = 10), color = "blue")
# g <- g + ggtitle("observations vs mean of observations")
# g <- g + labs(x = "x", y = "density")
# g <- g + xlim(0, 10)
# print(g)

dev.off()