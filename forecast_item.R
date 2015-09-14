


#######################################################################
#### FORECAST AND SEASONALITY EXTRACTION ##############################
#######################################################################
require(forecast)
require(data.table)
require(zoo)

forecast_table <- function(x) {

     ##INPUTS:
     freq <- 12
     tend <- c(2014,3) #CUT OFF ACTUALS for simulation
     fend <- c(2016,3) #FORECAST THROUGH this month
     
     
     tend.dt <- as.Date.yearmon(yearmon(tend[1]+(tend[2]-1)/12))
     fend.dt <- as.Date.yearmon(yearmon(fend[1]+(fend[2]-1)/12))
     
     item <- x
     
     
     tstart <- item[1,c(POS_YEAR,POS_MTH_NBR)] #Note exception of .() or list(), must output vector for ts()
     tstart.dt <- as.Date.yearmon(yearmon(tstart[1]+(tstart[2]-1)/12))
     tactuals_end <- item[.N,c(POS_YEAR,POS_MTH_NBR)] #Note exception of .() or list(), must output vector for ts()
     tactuals_end.dt <- as.Date.yearmon(yearmon(tactuals_end[1]+(tactuals_end[2]-1)/12))
     #Check for critical length of history
          #Check for 24 months of history
          if(as.yearmon(tend.dt) - as.yearmon(tstart.dt) < 1) {
               #print("not enough history")
               return(data.table(POS_MTH_NBR=0,SSN_PROFILE=0,POS_YEAR=0L,UNITS_FC="",AF_FLAG="",FC_CONF_80="",MODEL="Not enough history"))
               }
          if(tactuals_end.dt <=  tend.dt) {
               #print("item stopped selling")
               return(data.table(POS_MTH_NBR=0,SSN_PROFILE=0,POS_YEAR=0L,UNITS_FC="",AF_FLAG="",FC_CONF_80="",MODEL="item stopped selling"))
          }
          
     
     ###CREATE TIMESERIES TO PROCESS
     item.ts <- ts(item$units,start = tstart,frequency = 12,end=tend)
     #item.ts <<- item.ts
     ###Get simple seasonality
     if(length(item.ts) > 23) {
               item.dc <- decompose(item.ts,"multiplicative")
          }
          else {
               item.dc <- data.frame(figure = rep(0,12))    
     }
     #map seasonality by month
     sm <- (tstart[2]:(tstart[2]+11))
     sm[sm>12] <- (sm[sm>12]-12)
     SEASONALITY <- data.table(cbind(sm,item.dc$figure))
     setnames(SEASONALITY,1:2,c("POS_MTH_NBR","SSN_PROFILE"))
     setkey(SEASONALITY,POS_MTH_NBR)
     

     h <- tsdatediff(fend,tend)
     
     item.fc <- tryCatch(
          forecast(item.ts,h=h,model="ZZM")
          , error=function(e) NULL)
     
     if(class(item.fc) != "forecast") {
          return(data.table(POS_MTH_NBR=0,SSN_PROFILE=0,POS_YEAR=0L,UNITS_FC="",AF_FLAG="",FC_CONF_80="",MODEL="forecast error"))
     }
     
     
     item.fc2 <- rbindlist(list(
          data.table(units=cbind(as.numeric(round(item.fc$x,0)),AF="A",FC_80CONF=0,model=""))
          ,data.table(units=cbind(as.numeric(round(item.fc$mean,0)),AF="F",FC_80CONF=as.vector(round(item.fc$upper[,1]-item.fc$mean,0)),model=item.fc$method))
     ),use.names=FALSE, fill=FALSE)
     
     ##RESTORE YEAR / MONTH INDEX
     ym <- seq(tstart.dt,fend.dt,by='month')
     ym <- data.table(cbind(POS_YEAR=year(ym),POS_MTH_NBR=month(ym)))
     item.fc2 <- cbind(ym,item.fc2)
     setkey(item.fc2,POS_MTH_NBR)
     
     item.fc2 <- SEASONALITY[item.fc2][order(POS_YEAR,POS_MTH_NBR)]
     
     
     return(item.fc2)
}

tsdatediff <- function(x,y) {
     ((x[1]+((x[2]-1)/12)) - (y[1]+((y[2]-1)/12)))*12
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

