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
fcSummary <- function(x) {
     z <- lapply(x, function(x) {unlist(x[11:13])})
     result <- do.call(rbind,z)
     result
}


##Take DataFrame from LAPPLY and generate parent TS object
fcCompare <- function(x,last_actual,lookback_years) {
     lapply(x,fcMulti,last_actual,lookback_years)
}

fcMulti <- function(x,last_actual,lookback_years) {
     x.name <- x[1,1]
     ##print(x.name)
     x <- tryCatch(fcTStoYR(fcItemToTS(x),last_actual,lookback_years) , error=function(e) NULL)
     
     c(item=x.name,x)
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
     fcst <- forecast(window(x,start=start.win,end=end.win),8)
     actual <- window(x,start=c(2014,5),end=c(2014,12))
     c(fcst,forecasted_ROY=round(sum(fcst$mean),0),actual_ROY=round(sum(actual),0),simple_variance=round(sum(fcst$mean)/sum(actual),3))
}

asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.character)], asNumeric))

fcRecastFC <- function(fcObject) {
     st <- start(fcObject$x)
     frq <- frequency(fcObject$x)
     t <- ts(c(fcObject$x,fcObject$mean),start=st,frequency=frq)
     t
}

fcItemAccuracyOverTime <- function(xts,YEAR,itemName,model) {
     fc <- as.list(NULL)
     for(mo in 0:10) {
          t <- window(xts,end=c(YEAR,mo))
          a <- window(xts,start=c(YEAR,1),end=c(YEAR,12))
          actualized <- if(mo>0) {sum(window(t,start=c(YEAR,1)))} else 0
          f <- forecast(t,h=12-mo,model=model)
          ##get total for Act and FCST for accuracy comparison
          a.ttl <- sum(a)
          f.ttl <- sum(f$mean)+actualized
          f <- c(f,accuracy=list(cbind(item=as.character(itemName),year=YEAR,month=mo,accuracy(f$mean,a),act=a.ttl,proj=f.ttl,netAcc=round((1-abs(f.ttl-a.ttl)/a.ttl),3)*100,model=f$method))) 
          fc <- c(fc,list(f))
     }
     #fc
     ##for accuracy summary:
     x <- data.table(do.call("rbind",lapply(fc,function(x) x$accuracy)))
     #row.names(x) = c(paste(itemName,"DEC"),paste(itemName,"JAN"),paste(itemName,"FEB"),paste(itemName,"MAR"),paste(itemName,"APR"),paste(itemName,"MAY"),paste(itemName,"JUN"),paste(itemName,"JUL"),paste(itemName,"AUG"),paste(itemName,"SEP"),paste(itemName,"OCT"))
     x
}


fcAccuracyOverTimeMulti <- function(x,YEAR,model) {
     x.name <- x[1,1]
     fx <- tryCatch((window(fcItemToTS(x),end=c(YEAR,12))), error=function(e) NULL)
     
     if(length(fx)>36) {
          fc <- tryCatch(fcItemAccuracyOverTime(fx,YEAR,x.name,model), error=function(e) NULL)
print(paste(x.name,length(fx)))
          if(!is.null(fc)) fco$appendAcc(fc)
print("done append")
     }
     else return()  #(list(x.name,": not enough history"))
}

fcRunAccuracy <- function(x,YEAR,model) {
     fco <- fcStore()
     lapply(x,fcAccuracyOverTimeMulti,YEAR,model)
##     x <- data.table(do.call("rbind",unlist(x)))
     fco$getAcc()
}

GetSeasonality <- function(x,YEAR) {
     t <- fcItemToTS(x)
     SPR <- window(t,start = c(YEAR,1), end=c(YEAR,7))
     FALL <- window(t,start = c(YEAR,8), end=c(YEAR,12))
     SEASONALITY <- sum(FALL)/sum(SPR)
     data.frame(SPR=as.integer(round(sum(SPR),0)),FALL=as.integer(round(sum(FALL),0)),SEASONALITY=round(SEASONALITY,3))
}
     
