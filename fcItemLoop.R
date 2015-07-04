library(forecast)

#retail <- read.csv("http://robjhyndman.com/data/ausretail.csv",header=FALSE)
#retail <- ts(retail[,-1],f=12,s=1982+3/12)

#ns <- ncol(retail)
#h <- 24
#fcast <- matrix(NA,nrow=h,ncol=ns)
#for(i in 1:ns)
#  fcast[,i] <- forecast(retail[,i],h=h)$mean

#write(t(fcast),file="retailfcasts.csv",sep=",",ncol=ncol(fcast))

##wtc <- function(x) {write.table(x, "clipboard", sep="\t", row.names=FALSE)}


##Take DataFrame from LAPPLY and generate parent TS object

fcMulti <- function(x,last_actual,lookback_years) {
     fcTStoYR(fcItemToTS(x),last_actual,lookback_years)  
}

fcItemToTS <- function(x) {
     preTS <- x[x$units!=0,]
     This.start <- unlist(head(preTS[2:3],1))
     This.end <- unlist(tail(preTS[2:3],1))
     This.TS <- ts(preTS$units,start=This.start,frequency=12)
     This.TS
}

fcTStoYR <- function(x,last_actual,lookback_years) {
     f <- 12
     
     #Store length of passed timeseries  
     start.win <- trunc(last_actual)-lookback_years
     end.win <- last_actual
     
     #Store desired range of resulting timeseries
     xend <- end(x)[1]+end(x)[2]/f
     xstart <- start(x)[1]+start(x)[2]/f
     
     ##make sure we have enough data
     if(end.win>xend){ return("data ends too soon")}
     if(start.win<xstart){ return("data starts too late")}
     
     ##use window to subset
     window(x,start=start.win,end=end.win)
}

