install.packages("forecast", dependencies=TRUE)
library(forecast)

LRts <- ts(LR, frequency=52, start=c(2013, 1))

par(mfrow=c(3,1))
plot(ses(LRts,32), bty="l")
plot(holt(LRts,32), bty="l")
plot(hw(LRts,32), bty="l")

