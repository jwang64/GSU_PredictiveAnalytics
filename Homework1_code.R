arma_1 <- ARMAacf(ar = c(-1.6,-.64), lag.max = 10)
arma_2 <- ARMAacf(ar = c(.4,.45), lag.max=10)
arma_3 <- ARMAacf(ar = c(1.2,-.85), lag.max=10)

par(mfrow=c(3,1))
barplot(arma_1, main="AR(2) ACF 1", ylab="ACF")
barplot(arma_2, main="AR(2) ACF 2", ylab="ACF")
barplot(arma_3, main="AR(2) ACF 3", ylab="ACF")

library(astsa)
par(mfrow=c(2,1))
plot(oil, main="Time Series of Oil", ylab="Dollars/Barrel")
plot(gas, main="Time Series of Gas", ylab="Cents/Gallon")

par(mfrow=c(2,2))
plot(diff(log(oil)), main="Oil", ylab = "diff(oil)")
acf(diff(log(oil)), 208, main="ACF Diff(Oil)")
plot(diff(log(gas)), main="Gas", ylab= "diff(gas)")
acf(diff(log(gas)), 208, main="ACF Diff(Gas)")
