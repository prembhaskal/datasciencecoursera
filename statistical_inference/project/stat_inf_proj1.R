## exponential distribution 
## distribution of average of 40 samples.

library(ggplot2)

totalIterations <- 1000
sequence <- seq(1 : totalIterations)

lambda <- 0.2
n <- 40
theoreticalMean <- 1 / lambda
theoreticalStdDeviation <- 1 / lambda
theoreticalVariance <- theoreticalStdDeviation ^ 2

meanOfAverages <- sapply(X = sequence, function(p) {
  samples <- rexp(n, lambda)
  round(mean(samples), 1)
})

dfMean <- data.frame("mean" = meanOfAverages)

g <- ggplot()
g <- g + geom_vline(aes(xintercept = 1/lambda, size = 2, color = "theoretical_mean"))
g <- g + geom_density(data = dfMean, aes(x = mean, color = "sample_mean"), size = 2)
g <- g + labs(x = "x", y = "density")
g <- g + ggtitle("sample mean vs theoretical mean")
g <- g + xlim(0, 10)
g <- g + scale_color_manual(values = c("sample_mean" = "red", "theoretical_mean" = "blue"))

print(g)


## sample variance vs theoretical variance

varOfAverages <- sapply(X = sequence, function(p) {
  samples <- rexp(n, lambda)
  round(var(samples), 1)
})

dfVars <- data.frame("var" = varOfAverages)

g <- ggplot()
g <- g + geom_density(data = dfVars, aes(x = var, color = "sample_variance"), size = 2)
g <- g + geom_vline(aes(xintercept = 1/lambda ^ 2, size = 2, color = "theoretical_variance"))
g <- g + labs(x = "x", y = "density")
g <- g + ggtitle("sample variance vs theoretical variance")
# g <- g + xlim(0, 10)
g <- g + scale_color_manual(name = "",
                            values = c("sample_variance" = "red", "theoretical_variance" = "blue"))

print(g)

## showing that the distribution is approximately normal.
sampleAverages <- function(n, df) {
  varOfAverages <- sapply(X = df, function(p) {
    samples <- rexp(n, lambda)
    round(mean(samples), 1)
  })
}

samples <- c(sampleAverages(20, seq(1 : totalIterations)), 
             sampleAverages(40, seq(1 : totalIterations)),
             sampleAverages(80, seq(1 : totalIterations)))
sampleCounts <- factor(rep(c(20, 40, 80), each = totalIterations))

samplesDf <- data.frame(x = samples, size = sampleCounts)

g <- ggplot(data = samplesDf, aes(x = x))
g <- g + geom_histogram(aes(y = ..density.., fill = size), binwidth = 0.1, colour = "black")##binwidth = 0.3)
g <- g + geom_vline(xintercept = 1/lambda)
g <- g + geom_density(size = 2, data = samplesDf, aes(x = x))
g <- g + facet_grid(. ~ size)
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