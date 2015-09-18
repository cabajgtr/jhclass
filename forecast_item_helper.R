##INPUT EXPECTATIONS: 
#item[,forecast_table(.SD),by=.(SEGMENT4,TT_CUSTOMER)]
#where data.table "item" contains: SEGMENT4,TT_CUSTOMER,POS_YEAR,POS_MTH_NBR,units

#Sample to Run By Customer and Segment4
item <<- TOYS[#SEGMENT4=="MY PAL SCOUT" # & TT_CUSTOMER == "WMT"
     ,.(units = sum(units))
     ,keyby=.(SEGMENT4,TT_CUSTOMER,POS_YEAR,PERIOD=POS_MTH_NBR)][order(SEGMENT4,TT_CUSTOMER,POS_YEAR,POS_MTH_NBR)]
item[,forecast_table(.SD),by=.(SEGMENT4,TT_CUSTOMER)]

setkey(item,SEGMENT4,POS_YEAR,PERIOD)
setkey(item.test,SEGMENT4,POS_YEAR,PERIOD)


##
##
##
##BY MONTH BY ITEM (TOTAL US)
item <- TOYS[POS_YEAR > 2011#SEGMENT4=="MY PAL SCOUT" # & TT_CUSTOMER == "WMT" ##May want to reduce items to forecast
             ,.(units = sum(units))
             ,keyby=.(SEGMENT4,POS_YEAR,PERIOD=POS_MTH_NBR)][order(SEGMENT4,POS_YEAR,PERIOD)]

##BY WEEK BY ITEM (TOTAL US)
item <- TOYS[POS_YEAR > 2011#SEGMENT4=="MY PAL SCOUT" # & TT_CUSTOMER == "WMT" ##May want to reduce items to forecast
             ,.(units = sum(units))
             ,keyby=.(SEGMENT4,POS_YEAR,PERIOD=POS_WK_NBR)][order(SEGMENT4,POS_YEAR,PERIOD)]

item.test <- item[,forecast_table(.SD),by=.(SEGMENT4)]

#JOIN and MERGE ORIGINAL ACTUALS FOR COMPARISON
item[,POS_YEAR:=as.numeric(POS_YEAR)]
item[,PERIOD:=as.numeric(PERIOD)]

setkey(item,SEGMENT4,POS_YEAR,PERIOD)
setkey(item.test,SEGMENT4,POS_YEAR,PERIOD)
item.test <- item[item.test]

#CREATE SECONDARY TABLE FOR ACCURACY RESULTS              
item.acc <- (item.test[UNITS_FC!=0 & POS_YEAR == 2014])
item.acc.summary <- 
     item.acc[,acc(.SD),by=.(SEGMENT4),.SDcols = c("UNITS_FC","units")]

#setnames(item.fc2,1:7,c("POS_MTH_NBR","SSN_PROFILE","POS_YEAR","UNITS_FC","AF_FLAG","FC_CONF_80","MODEL"))

setkey(item,SEGMENT4,TT_CUSTOMER,POS_YEAR,POS_MTH_NBR)
setkey(item.test,SEGMENT4,TT_CUSTOMER,POS_YEAR,POS_MTH_NBR)
##
##
##
cpb(cbind(
decompose(window(mmlhw.ts,start=c(2011,1)),"mult")$figure
,decompose(window(mmlhw.ts,start=c(2012,1)),"mult")$figure
,decompose(window(mmlhw.ts,start=c(2013,1)),"mult")$figure
))