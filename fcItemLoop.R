library(forecast)

retail <- read.csv("http://robjhyndman.com/data/ausretail.csv",header=FALSE)
retail <- ts(retail[,-1],f=12,s=1982+3/12)

ns <- ncol(retail)
h <- 24
fcast <- matrix(NA,nrow=h,ncol=ns)
for(i in 1:ns)
  fcast[,i] <- forecast(retail[,i],h=h)$mean

write(t(fcast),file="retailfcasts.csv",sep=",",ncol=ncol(fcast))

##wtc <- function(x) {write.table(x, "clipboard", sep="\t", row.names=FALSE)}


##Take DataFrame from LAPPLY and generate parent TS object
fcItemToTS <- function(x) {
    preTS <- x[x$units!=0,]
    This.start <- unlist(head(cItem[2:3],1))
    This.end <- unlist(tail(cItem[2:3],1))
    This.TS <- ts(preTS$units,start=This.start,frequency=12)
    This.TS
}

fcTSto2YR < function(x) {
  
  
}