
##Run 40000 random exponentials to illustrate the underlying distribution 
##(that we ultimately don't care about)
set.seed=1
rxp = rexp(40000,.2)

g <- ggplot() + aes(rxp) + geom_histogram(binwidth=.5, colour="black", fill="white") 
g <- g + geom_vline(xintercept=mean(rxp),color="red",size = 1)
g
##Run 1000 samples of rexp with n=40 and lambda = .2
mrxp = NULL
for (i in 1 : 1000) mrxp = c(mrxp, mean(rexp(40,.2)))
mean(mrxp)

##Plot the histogram of the samples

g <- ggplot() + aes(mns) + geom_histogram(binwidth=.5, colour="black", fill="white") 
g <- g + geom_vline(xintercept=mean(mns),color="red",size = 1)