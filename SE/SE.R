# An experiment to demonstrate the standard error of the mean
# 1. Population: Large uniform distribution
# 2. Sample 10000 times
# 3. Compute mean and sd of each sample
#    a. Using sd function/formula
#    b. Using SE formula
# 4. Plot the sampling distribution of means
# 5. Compute sd of null/sampling distribution
# 6. Compute the p value

library(dplyr)
library(rafalib)

# 1. Generate population data
population <- runif(10000000, min = 1, max = 10000)

obs.value = 5000
n <- 25
iter <- 1000

# 2,3. Compute population statistics for the null hypothesis
null <- data.frame(matrix(nrow = iter, ncol = 4), stringsAsFactors = FALSE)
colnames(null) <- c("mean", "pop_sd", "raw_sd", "clt_sd")
set.seed(1)
for (i in 1:iter) {
  s <- sample(population, n)
  sd.raw <- sqrt(sum((s - mean(s))^2)/n)
  sd.clt <- sd.raw/sqrt(n)
  null[i,] <- c(mean(s), popsd(s), sd.raw, sd.clt)
}
 
# 4. Plots
mypar(1, 3)
hist(null$mean)
plot(null$raw_sd)
plot(null$clt_sd)

# 5. Mean/sd of sampling distribution
sd_means <- sqrt(sum((null$mean - mean(null$mean))^2) / iter)
abline(h = popsd(null$mean), col = "red")

# 6. Compute z-score and p-value
z <- (obs.value - mean(null$mean)) / popsd(null$mean)
p <- pnorm(abs(z))