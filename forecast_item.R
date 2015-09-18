


#######################################################################
#### FORECAST AND SEASONALITY EXTRACTION ##############################
#######################################################################
require(forecast)
require(data.table)
require(zoo)

forecast_table <- function(x) {
 
     ##INPUTS:
     freq <- 52
     tend <- c(2014,freq/4) #CUT OFF ACTUALS for simulation
     fend <- c(2016,freq/4) #FORECAST THROUGH this month
     
     
     #tend.dt <- as.Date.yearmon(yearmon(tend[1]+(tend[2]-1)/freq))
     tend.dt <- tend[1]+(tend[2]-1)/freq
     #fend.dt <- as.Date.yearmon(yearmon(fend[1]+(fend[2]-1)/freq))
     fend.dt <- fend[1]+(fend[2]-1)/freq

     item <- x
     
     
     tstart <- item[1,c(POS_YEAR,PERIOD)] #Note exception of .() or list(), must output vector for ts()
     #tstart.dt <- as.Date.yearmon(yearmon(tstart[1]+(tstart[2]-1)/freq))
     tstart.dt <- tstart[1]+(tstart[2]-1)/freq
     
     tactuals_end <- item[.N,c(POS_YEAR,PERIOD)] #Note exception of .() or list(), must output vector for ts()
     #tactuals_end.dt <- as.Date.yearmon(yearmon(tactuals_end[1]+(tactuals_end[2]-1)/freq))
     tactuals_end.dt <- tactuals_end[1]+(tactuals_end[2]-1)/freq
     #Check for critical length of history
          #Check for 24 months of history
          if(tend.dt - tstart.dt < 1) {
               #print("not enough history")
               return(data.table(PERIOD=0,SSN_PROFILE=0,POS_YEAR=as.numeric(0),UNITS_FC="",AF_FLAG="",FC_CONF_80="",MODEL="Not enough history"))
               }
          if(tactuals_end.dt <=  tend.dt) {
               #print("item stopped selling")
               return(data.table(PERIOD=0,SSN_PROFILE=0,POS_YEAR=as.numeric(0),UNITS_FC="",AF_FLAG="",FC_CONF_80="",MODEL="item stopped selling"))
          }
          
     
     ###CREATE TIMESERIES TO PROCESS
     item.ts <- ts(item$units,start = tstart,frequency = freq,end=tend)
     #item.ts <<- item.ts
     ###Get simple seasonality
     if(length(item.ts) >= freq*2) {
               item.dc <- decompose(item.ts,"multiplicative")
          }
          else {
               item.dc <- data.frame(figure = as.numeric(rep(0,freq)))
     }
     #map seasonality by period
     sm <- (tstart[2]:(tstart[2]+freq-1))
     sm[sm>freq] <- (sm[sm>freq]-freq)
     SEASONALITY <- data.table(cbind(sm,item.dc$figure))
     setnames(SEASONALITY,1:2,c("PERIOD","SSN_PROFILE"))
     setkey(SEASONALITY,PERIOD)
     
     h <- (fend.dt - tend.dt)*freq
          #tsdatediff(fend,tend)
     
     #For weekly data, need TBATS conversion for forecast
     #if(freq>12) {item.ts <- tbats(item.ts)}
     item.ts <- tbats(item.ts)
     
     item.fc <- tryCatch(
          forecast(item.ts,h=h,model="ZZM")
          , error=function(e) NULL)
     
     item.fc <<- item.fc
     
     if(class(item.fc) != "forecast") {
          return(data.table(PERIOD=0,SSN_PROFILE=0,POS_YEAR=0L,UNITS_FC="",AF_FLAG="",FC_CONF_80="",MODEL="forecast error"))
     }
     
     
     item.fc2 <- rbindlist(list(
          data.table(units=cbind(as.numeric(round(item.fc$x,0)),AF="A",FC_80CONF=0,model=""))
          ,data.table(units=cbind(as.numeric(round(item.fc$mean,0)),AF="F",FC_80CONF=as.vector(round(item.fc$upper[,1]-item.fc$mean,0)),model=item.fc$method))
     ),use.names=FALSE, fill=FALSE)
     
     ##RESTORE YEAR / MONTH INDEX
     ym <- seq(tstart.dt,fend.dt, by=1/freq) #by='month')
     
     ym <- data.table(cbind(
               POS_YEAR=as.integer(ym)
               ,PERIOD=((ym %% 1)*freq)+1L
               ))
     
     item.fc2 <- cbind(ym,item.fc2)
     setkey(item.fc2,PERIOD)
     item.fc2 <- SEASONALITY[item.fc2][order(POS_YEAR,PERIOD)]
     
     ##had issue with bad forecast returning non-integer year number to .SD
     item.fc2[,POS_YEAR:=as.numeric(POS_YEAR)] 
     return(item.fc2)
     
}

tsdatediff <- function(x,y) {
     ((x[1]+((x[2]-1)/freq)) - (y[1]+((y[2]-1)/freq)))*freq
}

acc <- function(x){ #expects data.frame or similar with col1 = Forecast, Col2 = Actuals
     x <- as.data.frame(x)
     f <- as.integer(x[,1])
     a <- x[,2]
     #print((a))
     
     acc <- tryCatch(
          accuracy(f,a)
          , error = function(e) return(cbind(0,0,0,0,0,0))
     )
     
     #print(class(acc))
     return(as.data.table(
           cbind(acc
          ,NET_ERR=round(abs(sum(f)-sum(a))/sum(a),2)
          ,FC_UNITS=sum(f),ACT_UNITS=sum(a)
     )))
     
}

